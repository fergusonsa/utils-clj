;; If connecting to a remote repl, you need to cut and paste the code in this file. 
;; lein repl :connect med02.cenx.localnet:4009
;;
;; OR
;;
;; if connecting to a local repl, you can run the following lines to load this file
;; $ cd ~/CENX/src/heimdallr
;; $ lein repl :connect `cat .nrepl-port`
;; user=> (ns cenx.heimdallr.customers.vzw-mpn.debug)
;; cenx.heimdallr.customers.vzw-mpn.debug=> (load-string (slurp "/Users/fergusonsa/CENX/utils/alarms-helpers.clj"))
;; 
(ns cenx.heimdallr.debug.fergusonsa)
(use 'cenx.heimdallr.customers.shared.vzw.debug)
(use 'cenx.heimdallr.debug)
(use 'clojure.pprint) 

;; for 6.x use this line:
;;(def send-alarm-msg send-vsm)
;; for 7.x use this line:
(def send-alarm-msg send-analytics)

(defn load-this-file [] (load-string (slurp "/Users/fergusonsa/CENX/utils/alarms-helpers.clj"))  )
 
(defonce submitted-alarms (atom #{}))
(def scotts-description "Sent by Scott F")
(defn format-date [dt] (.format (java.text.SimpleDateFormat. "yyyy-MM-dd HH:mm:ss") dt))

(defn display-num-alarms [] (println "\n# of alarms currently active:    " (count (get-all-active-alarms))) (println "\n# of MY alarms currently active: " (count @submitted-alarms)))

(defn show-alarm-info 
  ([] (show-alarm-info (get-all-active-alarms)))
  ([alrms]
    (println "Currently " (count alrms) " alarms!\n")
    (doseq [alarm-name (apply sorted-set alrms)]     
      (let [alarm-obj (get-object alarm-name)]
          (println (format "%-100s" alarm-name) " updated " (format-date (:fault/timestamp alarm-obj)) " text "  (:fault/details alarm-obj))))))

(defn is-my-alarm [alarm-name]
  (or (contains? @submitted-alarms alarm-name)   
    (let [alarm-obj (get-object alarm-name)]
      (if-let [txt (:fault/details alarm-obj)] (= txt scotts-description) false))))


(defn change-alarm
  ([alrm-name]
    (change-alarm alrm-name :Failed scotts-description))
  ([alrm-name state details]
    (if (= state :Ok)
      (swap! submitted-alarms disj alrm-name)
      (swap! submitted-alarms conj alrm-name))
    (println "setting alarm" alrm-name "to" state)
    (send-alarm-msg alrm-name state details)))

(defn cancel-all-alarms [] (doseq [alarm-name (get-all-active-alarms)] (change-alarm alarm-name :Ok scotts-description)))

(defn clear-alarm [alarm-name] (if (is-my-alarm alarm-name) (change-alarm alarm-name :Ok scotts-description)) (display-num-alarms))

(defn raise-random-alarms [num-to-submit]
  (let [init-subs (count @submitted-alarms)]
    (while (< (count @submitted-alarms) (+ init-subs num-to-submit))
      (let [alrm-name (rand-nth (get-alarm-names))]
        (if (and (not (nil? alrm-name))  (not (some #(=  alrm-name %) (get-all-active-alarms))))
          (change-alarm alrm-name :Failed scotts-description)))))
  (display-num-alarms)))
  
(defn cancel-all-my-alarms []
  (doseq [alarm-name (clojure.set/intersection @submitted-alarms (into #{} (get-all-active-alarms)))]     
    (change-alarm alarm-name :Ok scotts-description))
  (display-num-alarms))

(defn cancel-all-my-alarms2 [] (doseq [alarm-name (filter is-my-alarm (get-all-active-alarms))] (change-alarm alarm-name :Ok scotts-description)))

(defn show-active-daxData-alarms [] (pprint (apply sorted-set (filter (fn [x] (.contains x "daxDataViolationAlert")) (get-all-active-alarms)))))
(defn show-my-active-alarms [] (show-alarm-info @submitted-alarms))
(defn get-alarm-types [] (set (for [x (get-alarm-names)] (re-find #"^[a-zA-Z_-]*" x))))

(defn raise-alarm-of-type [alarm-type] (let [alrm-name (rand-nth (filter-alarm-names alarm-type))] (change-alarm alrm-name :Failed scotts-description)))    

(defn raise-one-alarm-of-each-type "Raise one alarm for all types available." [] (doseq [type (get-alarm-types)] (raise-alarm-of-type type)) (display-num-alarms))

(defn clear-random-alarms [num-to-clear]
  (let [init-subs (count @submitted-alarms)]
    (while (and (> (count @submitted-alarms) 0) (> (count @submitted-alarms) (- init-subs num-to-clear)) (> (count (get-all-active-alarms)) 0))
      (let [alarm-name (rand-nth (seq (clojure.set/intersection @submitted-alarms (into #{} (get-all-active-alarms)))))]
        (if (not (nil? alarm-name))
          (change-alarm alarm-name :Ok scotts-description)))))
  (display-num-alarms))

(defn find-my-alarms [] (reset! submitted-alarms (into #{} (filter is-my-alarm (get-all-active-alarms)))))


(defn get-display-name 
[id]
  (if (sequential? id)
    (map get-display-name id)
    (let [obj (get-object id)] (:eml3/displayName obj))))

(defn find-entites-for-alarms 
  ([] (apply find-entites-for-alarms @submitted-alarms))
  ([& alarm-names]
    (->> alarm-names
      (map #(let [e-id (get-entity-affected-by-alarm %)]
              (hash-map % (if (sequential? e-id)
                (map (fn [x] (hash-map :id x :name (get-display-name x))) e-id)
                {:id e-id :name (get-display-name e-id)}))))
      (into (sorted-map)))))
    
;; (send-alarm-msg  "daxDataViolationAlert:MPN01050:3205682:HA_MIP_Reg_Total"  :Failed scotts-description)

