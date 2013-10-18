(ns rcpsp.main (:use clojure.set rcpsp.helpers))

;=======================================================================================================================
; Structure
;=======================================================================================================================
(defrecord projrec [J d k E K oc-jumps zmax qlevels qlt])

(defn last-job-sched [sts] (apply max (keys sts)))
(defn last-job-ps [ps] (apply max (:J ps)))

(defn preds [E j] (->> E
                     (select #(= (second %) j))
                     (map first)))

(defn follow [E i] (->> E
                      (select #(= (first %) i))
                      (map second)))

(defn st [d j ftj] (- ftj (d j)))
(defn ft [d j stj] (+ stj (d j)))

(defn z [oc-jumps t] (oc-jumps (apply max (filter #(<= % t) (keys oc-jumps)))))

;=======================================================================================================================
; Time analysis
;=======================================================================================================================
(defn est [ps ests j]
  (assoc ests j (->> (preds (:E ps) j)
                     (map (fn [i] (ft (:d ps) i (ests i))))
                     (cons 1)
                     (apply max))))

(defn ests [ps] (reduce (partial est ps) {0 1} (difference (:J ps) #{0})))
(defn efts [ps] (map2 (partial ft (:d ps)) ests))

(defn minimal-makespan [ps] ((efts (:d ps)) (last-job-ps ps)))

(defn lft [ps lfts j]
  (assoc lfts j (->> (follow (:E ps) j)
                     (map (fn [i] (st (:d ps) i (lfts i))))
                     (cons (minimal-makespan ps))
                     (apply min))))

(defn determine-lfts [ps lfts rest]
  (if (empty? rest)
    lfts
    (let [all-followers-done (first (filter (fn [i] (every? lfts (follow i))) rest))
          new-rest (remove #(= % all-followers-done) rest)]
      (determine-lfts (:J ps) (:d ps) (lft (:J ps) (:d ps) lfts all-followers-done) new-rest))))

(defn lfts [ps]
  (determine-lfts ps {(last-job-ps ps) (minimal-makespan ps)} (difference (:J ps) #{(last-job-ps ps)})))

(defn lsts [ps] (map2 (partial st (:d ps)) (lfts ps)))

(defn xjt [ps sts]
  (fn [j t]
    (let [k (bool->num (zero? ((:d ps) j)))]
      (bool->num (= t (+ k (ft (:d ps) j (sts j))))))))

(defn sts<-xjt [ps xjt]
  (hash-map (mapcat (fn [j] (vector j (first-where (partial = 1) (partial xjt j) nat-nums))) (:J ps))))

;=======================================================================================================================
; Common scheduling functions
;=======================================================================================================================
(defn job-act-in-period? [d sts t j]
  (and (sts j)
       (let [stj (sts j)]
         (<= stj t (dec (ft d j stj))))))

(defn active-in-period [ps sts t] (filter (partial job-act-in-period? (:d ps) sts t) (keys sts)))

(defn residual-in-period [ps sts t]
  (- (+ (:K ps) (z (:oc-jumps ps) t))
     (sum (:k ps) (active-in-period ps sts t))))

(defn preds-finished? [ps sts j t]
  (every? (fn [i] (and (sts i) (<= (+ (sts i) ((:d ps) i)) t))) (preds (:E ps) j)))

(defn periods-active [ps t j]
  (set-range t (+ t (max 0 (dec ((:d ps) j))))))

(defn enough-capacity? [ps sts j t]
  (every? (fn [τ] (>= (residual-in-period ps sts τ) ((:k ps) j))) (periods-active ps t j)))

(defn st-feasible? [ps sts j t] (and (preds-finished? ps sts j t) (enough-capacity? ps sts j t)))

;=======================================================================================================================
; Additional capacities
;=======================================================================================================================
(defn remove-redundant-jump-in-t [[last-val oc-jumps] t]
  (if-let [val (get oc-jumps t)]
    (if (= val last-val) [val (dissoc oc-jumps t)] [val oc-jumps])
    [last-val oc-jumps]))

(defn remove-redundant-jumps [oc-jumps]
  (let [last-t (apply max (keys oc-jumps))
        first-t (apply min (keys oc-jumps))]
    (reduce remove-redundant-jump-in-t [(get oc-jumps first-t) oc-jumps] (set-range first-t last-t))))

(defn restrict-to-max-oc [ps oc-jumps]
  (map2 (fn [t oc] (min oc (:zmax ps))) oc-jumps))

(defn capacity-missing [ps sts j periods]
  (->> periods (map (partial residual-in-period ps sts)) (map #(- (get-in ps [:k j]) %))))

(defn period-to-missing [ps sts j periods]
  (->> (capacity-missing ps sts j periods) (map vector periods) (filter (comp pos? first)) (into {})))

(defn book-oc [ps sts t j]
  (let [misshash (period-to-missing ps sts j (periods-active ps t j))
        old-jumps (:oc-jumps ps)
        new-jumps (restrict-to-max-oc ps (map2 (fn [t miss] (if-let [oldval (get old-jumps t)] (+ oldval miss) miss)) misshash))]
    (assoc ps :ocjumps (remove-redundant-jumps (merge old-jumps new-jumps)))))

;=======================================================================================================================
; Serial schedule generation scheme
;=======================================================================================================================
(defn first-t-qualifying [pred] (first-where pred identity nat-nums))
(defn schedule-next [ps sts j] (assoc sts j (first-t-qualifying (partial st-feasible? ps sts j))))
(defn ssgs [ps λ] (reduce (partial schedule-next ps) {(first λ) 1} (rest λ)))

;=======================================================================================================================
; Parallel schedule generation scheme
;=======================================================================================================================
(defn eligible-set [ps sts t] (filter (fn [j] (preds-finished? ps sts j t)) (difference (:J ps) (keys sts))))
(defn eligible-and-feasible-set [ps sts t] (filter (fn [j] (enough-capacity? ps sts j t)) (eligible-set ps sts t)))
(defn uneligible-due-capacity [ps sts t] (difference (eligible-set ps sts t) (eligible-and-feasible-set ps sts t)))
(defn eligible-and-feas-with-oc [ps sts t]
  (filter (fn [j] (enough-capacity? (book-oc ps sts t j) sts j t)) (uneligible-due-capacity ps sts t)))

(defn next-dp [ps sts last-dp] (->> last-dp
                                    (active-in-period ps sts)
                                    (map (fn [i] (ft (:d ps) i (sts i))))
                                    (cons-if-empty last-dp)
                                    (apply min)))

(defn schedule-in-dp [λ ps sts dp]
  (loop [sts-acc sts]
    (let [eafs (sort-by (fn [j] (index-of λ j)) (eligible-and-feasible-set ps sts-acc dp))]
      (if (empty? eafs)
        sts-acc
        (recur (assoc sts-acc (first eafs) dp))))))

(defn psgs-step [λ ps [dp sts]]
  (let [new-sts (schedule-in-dp λ ps sts dp)]
    [(next-dp ps new-sts dp) new-sts]))

(defn psgs [ps λ]
  (second (loop [acc [1 {}]] (if (every? (acc 1) (:J ps)) acc (recur (psgs-step λ ps acc))))))

;=======================================================================================================================
; Fitness calculation
;=======================================================================================================================
(defn makespan-of-schedule [ps sts] (let [lj (last-job-sched sts)]
                                      (ft (:d ps) lj (sts lj))))
(defn periods-in-schedule [ps sts] (set-range 1 (makespan-of-schedule ps sts)))

(defn total-overtime-cost [ps sts]
  (sum (partial z (:oc-jumps ps)) (periods-in-schedule ps sts)))

(defn revenue [ps sts]
  (let [qlt (ps :qlt)]
    (->> (ps :qlevels)
         (map (fn [qlevel] (qlt [qlevel (makespan-of-schedule ps sts)])))
         (apply max))))

(defn fitness [ps sts] (- (revenue ps sts) (total-overtime-cost ps sts)))

;=======================================================================================================================
; Feasibility checks
;=======================================================================================================================
(defn res-usage-feasible? [ps sts] (every? (fn [t] (nneg? (residual-in-period ps sts t)))
                                           (periods-in-schedule ps sts)))

(defn precedence-adhered? [ps sts] (every? (fn [j] (preds-finished? ps sts j (sts j))) (keys sts)))

(defn feasible? [ps sts] (and (every? pos? (vals sts))
                              (res-usage-feasible? ps sts)
                              (precedence-adhered? ps sts)))

;=======================================================================================================================
; Left shifts
;=======================================================================================================================
(defn one-period-left-shift [sts j] (assoc sts j (dec (sts j))))

(defn n-period-left-shift [sts j δ]
  (if (zero? δ) sts (n-period-left-shift (one-period-left-shift sts j) j (dec δ))))

(defn local-ls? [ps sts j δ]
  (if (zero? δ) true (let [one-ls (one-period-left-shift sts j)]
                     (and (feasible? ps one-ls) (local-ls? ps one-ls j (dec δ))))))

(defn global-ls? [ps sts j δ]
  (and (feasible? ps (n-period-left-shift sts j δ)) (not (local-ls? ps sts j δ))))

(defn local-ls-feasible? [ps sts j]
  (feasible? ps (one-period-left-shift sts j)))

(defn global-ls-feasible? [ps sts j]
  (exists? (fn [δ] (global-ls? ps sts j δ)) (range 1 (sts j))))

(defn semi-active? [ps sts]
  (every? (comp not (partial local-ls-feasible? ps sts)) (keys sts)))

(defn active? [ps sts]
  (and (semi-active? ps sts)
       (every? (comp not (partial global-ls-feasible? ps sts)) (keys sts))))

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
