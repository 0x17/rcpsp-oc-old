(ns rcpsp.psplib-reader (:use (clojure test set) (rcpsp helpers main)))

(defn parse-nums [line] (read-string (str "(" line ")")))

(defn lines-between-prefixes [start-prefix end-prefix lines]
  (let [start-ix (index-ofp (fn [line] (.startsWith line start-prefix)) lines)
        end-ix (+ start-ix (index-ofp (fn [line] (.startsWith line end-prefix)) (drop start-ix lines)))]
       (elems-between start-ix end-ix lines)))

(def precedence-lines (comp (partial drop 1)
                            (partial lines-between-prefixes
                               "PRECEDENCE RELATIONS:"
                               "***********************************")))

(def reqdur-lines (comp (partial drop 2)
                        (partial lines-between-prefixes
                                 "REQUESTS/DURATIONS:"
                                 "*********************************")))

(def capacity-line (comp first
                         (partial drop 1)
                         (partial lines-between-prefixes
                                  "RESOURCEAVAILABILITIES:"
                                  "*********************************")))

(defn horizon [lines]
  (let [horizon-line (first (drop 6 lines))]
    (read-string (second (map #(.trim %) (.split horizon-line ":"))))))

(defn succ-from-line [job-to-succ prec-line]
  (let [line-lst (parse-nums prec-line)
        jobnr (first line-lst)
        succ (set (drop 3 line-lst))]
    (assoc job-to-succ jobnr succ)))

(defn succ-from-content [content]
  (let [plines (precedence-lines content)]
    (reduce succ-from-line {} (precedence-lines content))))

(defn succ-to-edges [job-to-succ]
  (if (empty? job-to-succ)
    #{}
    (let [j (ffirst job-to-succ)
          succs (second (first job-to-succ))]
      (union (succ-to-edges (rest job-to-succ)) (set (map #(vector j %) succs))))))

(defn parse-reqdur-line [line]
  (let [line-lst (parse-nums line)
        colnames '(:jobnr :mode :duration :R1 :R2 :R3 :R4)]
    (apply hash-map (interleave colnames line-lst))))

(defn parse-capacity-line [line]
  (let [line-lst (parse-nums line)]
    (apply hash-map (interleave '(:R1 :R2 :R3 :R4) line-lst))))

(defn ps-from-content [content]
  (let [edges (->> content succ-from-content succ-to-edges)
        reqdur-hashes (map parse-reqdur-line (reqdur-lines content))
        durations (unite-maps :jobnr :duration reqdur-hashes)
        demands (unite-maps :jobnr :R1 reqdur-hashes)
        jobnums (map :jobnr reqdur-hashes)
        capacities (parse-capacity-line (capacity-line content))]
    (map->projrec
      {:J (set jobnums)
       :E edges
       :d durations
       :k demands
       :K (capacities :R1)
       :oc-jumps {1 0}
       :zmax 10})))

(defn ps-from-file [f] (ps-from-content (read-lines f)))
