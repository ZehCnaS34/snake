(ns snake.style)

(defn get-style [node]
  (.-style node))

(defn background-color [style v]
  (set! (.-backgroundColor style) v)
  style)

(defn z-index [style v]
  (set! (.-zIndex style) v)
  style)

(defn top [style v]
  (set! (.-top style) v)
  style)

(defn left [style v]
  (set! (.-left style) v)
  style)

(defn box-shadow [style v]
  (set! (.-boxShadow style) v)
  style)

(defn position [style v]
  (set! (.-position style) "absolute"))

(defn height [style v]
  (set! (.-height style) v))

(defn width [style v]
  (set! (.-width style) v))
