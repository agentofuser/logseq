(ns frontend.modules.outliner.tree-test
  (:require [cljs.test :refer [deftest is are testing use-fixtures]]
            [frontend.modules.outliner.tree :as tree]))

(defrecord RenderNode [id children])
(defrecord TestNode [id parent left])

(defn build-root-test-node
  [id]
  (->TestNode id :root :root))

(defn build-render-tree
  [[id children :as tree]]
  (let [children (mapv build-render-tree children)]
    (->RenderNode id children)))

(defn build-sql-records
  "build RDS record from memory node struct."
  [tree-record]
  (letfn [(build [acc node queue]
            (let [{:keys [id left parent]} node
                  sql-record (->TestNode id parent left)
                  left (atom (:id node))
                  children (map (fn [c]
                                  (let [node (assoc c :left @left :parent (:id node))]
                                    (swap! left (constantly (:id c)))
                                    node))
                                (:children node))
                  queue (concat queue children)
                  acc (conj acc sql-record)]
              (if (seq queue)
                (build acc (first queue) (rest queue))
                acc)))]
    (let [id (:id tree-record)
          root (assoc tree-record :left id :parent nil)]
      (build nil root '()))))

(def tree [1 [[2 [[3 [[4]
                      [5]]]
                  [6 [[7 [[8]]]]]
                  [9 [[10]
                      [11]]]]]
              [12 [[13]
                   [14]
                   [15]]]
              [16 [[17]]]]])

(def records (-> (build-render-tree tree)
                 (build-sql-records)))

(def db (atom records))

(defn find-node
  ([id]
   (when-let [m (some #(when (= id (:id %)) %)
                      @db)]
     (map->TestNode m)))
  ([parent left]
   (when-let [m (some #(when (and (= parent (:parent %))
                                  (= left (:left %)))
                         %)
                      @db)]
     (map->TestNode m))))

(defn find-children
  [parent-node]
  (let [first-child (tree/-get-down parent-node)]
    (loop [current first-child
           children [first-child]]
      (if-let [node (tree/-get-right current)]
        (recur node (conj children node))
        children))))

(extend-type TestNode
  tree/INode
  (tree/-get-id [this]
    (:id this))

  (tree/-get-parent-id [this]
    (:parent this))

  (tree/-set-parent-id [this parent-id]
    (assoc this :parent parent-id))

  (tree/-get-left-id [this]
    (:left this))

  (tree/-set-left-id [this left-id]
    (assoc this :left left-id))

  (tree/-get-parent [this]
    (find-node (:parent this)))

  (tree/-get-left [this]
    (find-node (:left this)))

  (tree/-get-right [this]
    (find-node (:parent this)
               (:id this)))

  (tree/-get-down [this]
    (find-node (:id this) (:id this)))

  (tree/-save [this]
    (let [id (:id this)]
      (swap! db (fn [db]
                  (-> (remove #(= (:id %) id) db)
                      (conj this))))))

  (tree/-get-children [this]
    nil))

(deftest test-insert-node
  "
  Inert a node between 6 and 9.
  [1 [[2 [[3 [[4]
              [5]]]
          [6 [[7 [[8]]]]]
          [18]         ;; add
          [9 [[10]
              [11]]]]]
      [12 [[13]
           [14]
           [15]]]
      [16 [[17]]]]]
   "
  (reset! db records)
  (let [new-node (->TestNode 18 nil nil)
        left-node (->TestNode 6 2 3)]
    (tree/insert-node new-node left-node)
    (let [children-of-2 (->> (->TestNode 2 1 1)
                             (find-children)
                             (mapv :id))]
      (is (= [3 6 18 9] children-of-2)))))

(deftest test-delete-node
  "
  Inert a node between 6 and 9.
  [1 [[2 [[3 [[4]
              [5]]]
          [6 [[7 [[8]]]]]  ;; delete 6
          [9 [[10]
              [11]]]]]
      [12 [[13]
           [14]
           [15]]]
      [16 [[17]]]]]
   "
  (reset! db records)
  (let [node (->TestNode 6 2 3)]
    (tree/delete-node node)
    (let [children-of-2 (->> (->TestNode 2 1 1)
                             (find-children)
                             (mapv :id))]
      (is (= [3 9] children-of-2)))))

(deftest test-get-node-list-with-cursor
  (reset! db records)
  (let [cursor (-> (build-root-test-node 1)
                   (tree/init-cursor))
        number 7
        {:keys [acc cursor]}
        (tree/get-node-list-with-cursor number cursor)]
    (is (= [1 2 3 4 5 6 7] (mapv :id acc)))

    (let [{:keys [acc cursor]}
          (tree/get-node-list-with-cursor number cursor)]

      (is (= [8 9 10 11 12 13 14] (mapv :id acc)))

      (let [{:keys [acc]}
            (tree/get-node-list-with-cursor number cursor)]
        (is (= [15 16 17] (mapv :id acc)))))))


(comment
  (defn build-node-from-sql-record
    "build node from RDS records"
    [node-id sql-records]
    (letfn [(get-right
              [node-id children]
              (some #(when (= (:left %) node-id)
                       %)
                    children))

            (sort-children
              [parent-node-id children]
              (loop [node-id parent-node-id
                     result []]
                (if-let [node (get-right node-id children)]
                  (let [result (conj result node)]
                    (recur (:id node) result))
                  (do
                    (when (not= (count children) (count result))
                      (throw (js/Error "children data error, ")))
                    result))))

            (get-children
              [node-id]
              (filter #(= (:parent %) node-id) sql-records))

            (build [node-id depth]
              (when (= depth 20)
                (throw (js/Error "Recur depth is too large.")))
              (let [children (some->> (get-children node-id)
                                      (sort-children node-id))
                    children (mapv #(build (:id %) (inc depth)) children)]
                (->RenderNode node-id children)))]
      (build node-id 0)))

  (deftest test-serialize-&-deserialize-tree
    (let [tree-record (build-render-tree tree)
          sql-record (build-sql-records tree-record)
          tree (build-node-from-sql-record 1 sql-record)]
      (is (= tree tree-record)))))