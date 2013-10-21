(ns rcpsp.schedule-view (:use (rcpsp main)) (:import (javax.swing JButton JFrame JPanel JScrollPane)))

;=======================================================================================================================
; Display output
;=======================================================================================================================
(defn times-capacity [ps jobs] (flatten (map (fn [j] (repeat ((:k ps) j) j)) jobs)))

(defn fill-to-capacity [ps t v]
  (let [cap-in-t (+ (:k ps) (z (:oc-jumps ps) t))]
    (if (< (count v) cap-in-t)
      (fill-to-capacity ps t (cons 0 v))
      v)))

(defn col-for-period [ps sts t] (->> (active-in-period ps sts t)
                                     (times-capacity ps)
                                     (fill-to-capacity ps t)))

(defn display-schedule [ps sts]
  (map (partial col-for-period ps sts) (periods-in-schedule ps sts)))

(defn display-residuals [ps sts]
  (map (partial residual-in-period ps sts) (periods-in-schedule ps sts)))

;=======================================================================================================================
; GUI
;=======================================================================================================================
(def scaling-factor 20)
(def scale (partial * scaling-factor))

(defn create-schedule-panel [ps sts]
  (proxy [JPanel] []
    (paintComponent [g]
      (proxy-super paintComponent g)
      (doseq [j (keys sts)]
        (let [stj (sts j)]
          (.fillRect g (scale stj) 0 (scale (get-in ps [:d j])) 10)
          (.drawString g (str j) (scale stj) 30))))))

(defn create-window [ps sts]
  (doto (new JFrame)
    (.setTitle "Schedule")
    (.setLocationRelativeTo nil)
    (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
    (.setContentPane (new JScrollPane (create-schedule-panel ps sts)))
    (.setSize 200 200)
    (.setVisible true)))

(defn update-window [ps sts])
