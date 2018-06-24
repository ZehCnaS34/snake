(ns snake.core
    (:require))

(defn floor [n] (.floor js/Math n))

(def TILE 10)
(def VIEW-WIDTH (.-innerWidth js/window))
(def VIEW-HEIGHT (.-innerHeight js/window))
(def COLUMNS (floor (/ VIEW-WIDTH TILE)))
(def ROWS (floor (/ VIEW-HEIGHT TILE)))

(enable-console-print!)

(defonce ID (atom 0))
(defn id []
  (let [id @ID]
    (swap! ID inc)
    id))

(def app (.getElementById js/document "app"))

(defn ri [ciel]
  (int (.floor js/Math (rand ciel))))

(defn create-snake 
  ([body direction] {:body body 
                     :ids [(id) (id)]
                     :type :snake
                     :direction direction})
  ([] (create-snake '((0 1) (0 2)) [1 0]))) 

(defn create-apple 
  ([] (create-apple [(ri COLUMNS) (ri ROWS)]))
  ([loc] {:location loc
          :id (id)
          :type :apple}))

(defn ate? [{[head & _] :body} apples] 
  (filter #(= head %) (map #(get % :location) apples)))

(defn overlap? [{[head & tail] :body}]
  (contains? (into #{} tail) head))

(defn move [{:keys [body direction ids] :as snake} grow]
  (let [nx (+ (direction 0) (-> body first (nth 0)))
        ny (+ (direction 1) (-> body first (nth 1)))]
    (cond-> snake
      true
      (assoc-in [:body]
        (conj
          (if grow body (butlast body))
          (list 
            (cond
              (< nx 0) COLUMNS
              (>= nx COLUMNS) 0
              :else nx)
            (cond
              (< ny 0) ROWS
              (>= ny ROWS) 0
              :else ny))))
      grow
      (update-in [:ids] conj (id)))))
        

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:snake (create-snake)
                          :apples (map #(create-apple) (range 10))
                          :type :state}))

(defonce elements (atom {}))

(defn elt-pool [id]
  (let [elts @elements
        element (elts id)]
    (if (nil? element)
      (let [e (.createElement js/document "div")]
        (set! (.-id e) (str "element-" id))
        (set! (.-position (.-style e)) "absolute")
        (set! (.-height (.-style e)) (str TILE "px"))
        (set! (.-width (.-style e)) (str TILE "px"))
        (.appendChild app e) 
        (swap! elements assoc id e)
        (elt-pool id))
      element)))

(defmulti draw (fn [item] (:type item)))

(defmethod draw :state
  [{:keys [snake apples]}]
  (draw snake)
  (doseq [apple apples]
    (draw apple)))

(defmethod draw :apple
  [{[x y] :location
    id :id}]
  (let [elt (elt-pool id)
        style (.-style elt)]
    (set! (.-backgroundColor style) "red")
    (set! (.-zIndex style) 1)
    (set! (.-top style) (str (* y TILE) "px"))
    (set! (.-left style) (str (* x TILE) "px"))))

(defmethod draw :snake
  [{body :body
    ids :ids}]
  (doseq [[[x y] id] (map vector body ids)]
    (let [elt (elt-pool id)
          style (.-style elt)]
      (set! (.-backgroundColor style) "blue")
      (set! (.-zIndex style) 100)
      (set! (.-boxShadow style) "inset 0 0 1px white")
      (set! (.-top style) (str (* y TILE) "px"))
      (set! (.-left style) (str (* x TILE) "px")))))

(defn flip [f]
  (fn [& xs]
    (apply f (reverse xs))))

(defn updater [e]

  (let [ate (ate? (:snake @app-state) (:apples @app-state))]
    (if (not (empty? ate))
      (do
        (swap! app-state update-in [:apples]
               (flip filter) 
               #(not= (:location %) (first ate))) 
        (swap! app-state update-in [:snake] move true)) 

      (swap! app-state update-in [:snake] move false))) 
    
        

  (swap! app-state assoc-in [:game :lost] (overlap? (:snake @app-state)))

  (if (get-in @app-state [:game :lost])
    (swap! app-state assoc-in [:snake] (create-snake)))

  (draw @app-state))

(.addEventListener (.getElementById js/document "step") "click" updater)
    
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
  

(defonce key-handler
  (.addEventListener (.-body js/document) "keydown" 
    (fn [e]
      (let [code (.-keyCode e)]
        (keypress
          (cond
            (or (= code 38) (= code 87)) :up
            (or (= code 40) (= code 83)) :down
            (or (= code 37) (= code 65)) :left
            (or (= code 39) (= code 68)) :right
            :default (println (str "code=" code))))))))

                                   
                   
  
(defn on-js-reload [])
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
