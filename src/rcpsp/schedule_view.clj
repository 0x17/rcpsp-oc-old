(ns rcpsp.schedule-view (:import (javax.swing JButton JFrame)))

(defn show-window []
  (doto (new JFrame)
    (.setTitle "Test")
    (.setLocationRelativeTo nil)
    (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
    (.add (new JButton "Test"))
    (.pack)
    (.setVisible true)))

(show-window)