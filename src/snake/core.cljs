(ns snake.core
    (:require [clojure.data :as data]
              [snake.style :as s]
              [snake.node :as n]
              [snake.util :as u]))


(defn floor [n] (.floor js/Math n))

(defn prob [percent]
  (> percent (.random js/Math)))
    

(def TILE 40)
(def VIEW-WIDTH (.-innerWidth js/window))
(def VIEW-HEIGHT (.-innerHeight js/window))
(def COLUMNS (floor (/ VIEW-WIDTH TILE)))
(def ROWS (floor (/ VIEW-HEIGHT TILE)))

(enable-console-print!)

(def app (.getElementById js/document "app"))

(defn ri [ciel]
  (int (.floor js/Math (rand ciel))))

(defn create-snake 
  ([body direction] {:body body 
                     :ids [(n/get-id) (n/get-id) (n/get-id) (n/get-id) (n/get-id)]
                     :type :snake
                     :direction direction})
  ([] (create-snake '((0 1) (0 2) (0 3) (0 4) (0 5)) [1 0]))) 

(defn create-apple 
  ([] (create-apple [(ri COLUMNS) (ri ROWS)]))
  ([loc] {:location loc
          :id (n/get-id)
          :type :apple}))

(defn ate? [{[head & _] :body} apples] 
  (filter #(= head %) (map #(get % :location) apples)))

(defn overlap? [{[head & tail] :body}]
  (contains? (into #{} tail) head))

(defn wrap [floor x ciel]
  (cond
    (< x floor) ciel
    (>= x ciel) floor
    :else x))

(defn move [{:keys [body direction ids] :as snake} grow]
  (let [nx (+ (direction 0) (-> body first (nth 0)))
        ny (+ (direction 1) (-> body first (nth 1)))]
    (cond-> snake
      true
      (assoc-in [:body]
        (conj
          (if grow body (butlast body))
          (list 
            (wrap 0 nx COLUMNS)
            (wrap 0 ny ROWS))))
      grow
      (update-in [:ids] conj (n/get-id)))))
        
(defn low-on-apples? [apples]
  (< (count apples) 10))

(defn lost? [v]
  v)

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:snake (create-snake)
                          :apples (map #(create-apple) (range 10))
                          :type :state}))


(defmulti draw (fn [item] (:type item)))

(defmethod draw :state
  [{snake :snake apples :apples}]
  (draw snake)
  (doseq [apple apples]
    (draw apple)))

(defmethod draw :apple
  [{[x y] :location id :id}]
  (doto (-> id n/get-node s/get-style)
    (s/background-color "red")
    (s/z-index 1)
    (s/top (str (* y TILE) "px"))
    (s/left (str (* x TILE) "px"))))

(defmethod draw :snake
  [{body :body ids :ids}]
  (doseq [[[x y] id] (map vector body ids)
          :let [elt (n/get-node id)]]
    (doto (.-style elt)
      (s/background-color "blue")
      (s/z-index 100)
      (s/box-shadow "inset 0 0 1px white")
      (s/top (str (* y TILE) "px"))
      (s/left (str (* x TILE) "px")))))


(defn updater [e]
  (let [snake (get-in @app-state [:snake])
        apples (get-in @app-state [:apples])
        ate (ate? snake apples)
        lost (overlap? snake)]

    (if-not (empty? ate)
      (do (swap! app-state update-in [:apples] (u/flip filter) #(not= (:location %) (first ate))) 
          (swap! app-state update-in [:snake] move true)) 
      (swap! app-state update-in [:snake] move false)) 

    (when lost
      (swap! app-state assoc-in [:game :lost] lost)
      (swap! app-state assoc-in [:snake] (create-snake)))

    (when (low-on-apples? apples)
      (swap! app-state update-in [:apples] conj (create-apple)))

    (n/clean-tree
      (into #{}
        (concat
          (map #(:id %) (:apples @app-state))
          (get-in @app-state [:snake :ids]))))

    (draw @app-state)))

; (.addEventListener (.getElementById js/document "step") "click" updater)
    
(defonce game-loop 
  (js/setInterval
    updater
    100))

(defn keypress [key]
  (swap! app-state assoc-in [:snake :direction]
    (cond
      (= key :up) [0 -1]
      (= key :down) [0 1]
      (= key :left) [-1 0]
      (= key :right) [1 0]
      :else (get-in @app-state [:snake :direction]))))
  
(def keymap
  {38 :up
   87 :up
   40 :down
   83 :down
   37 :left
   65 :left
   39 :right
   68 :right})

(defonce key-handler
  (.addEventListener (.-body js/document) "keydown" 
    (fn [e]
      (let [code (.-keyCode e)]
        (keypress
          (keymap code))))))

                                   
                   
  
(defn on-js-reload [])
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
