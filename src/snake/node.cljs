(ns snake.node
  (:require [snake.style :as s]
            [clojure.data :as data]))


;; -------------------------------
;; DOM Wrapper
;; -------------------------------

(defn id [node v]
  (set! (.-id node) v)
  node)

(defn append-child [node child]
  (.appendChild node child)
  node)

(defn create [tag]
  (.createElement js/document tag))

(defn remove [node]
  (.remove node)
  node)

(defn by-id [id]
  (.getElementById js/document id))


;; -------------------------------
;; Globals
;; -------------------------------

(def *app* (by-id "app"))


;; -------------------------------
;; Element Pool
;; -------------------------------

(defonce elements (atom {}))

(defn get-node [i]
  (if-let [element (@elements i)]
    element
    (as-> (create "div") elt 
      (id elt (str "element-" i))
      (append-child *app* elt)
      (swap! elements assoc i elt)
      (get-node i))))

(defn relinquish-node [id])

(defn clean-tree [current-ids]
  (let [[dead-ids _ _] (data/diff 
                         (into #{} (keys @elements))
                         current-ids)
        nodes (select-keys @elements dead-ids)]
    (map n/remove (vals nodes))
    (swap! elements select-keys current-ids)))


;; -------------------------------
;; ID Pool
;; -------------------------------

(defonce ID-POOL
  (atom {:counter 0
         :free []}))

(defn get-id []
  (let [{counter :counter free :free} @ID-POOL]
    (if (empty? free)
      (do (swap! ID-POOL update-in [:counter] inc)
          counter)
      (do (swap! ID-POOL update-in [:free] pop) 
          (peek free)))))

(defn relinquish-id [i]
  (swap! ID-POOL update-in [:free] conj i))
