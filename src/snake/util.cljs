(ns snake.util)

(defn flip [f]
  (fn [& xs]
    (apply f (reverse xs))))

