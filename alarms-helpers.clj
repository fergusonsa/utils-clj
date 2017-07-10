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
(use 'cenx.heimdallr.customers.shared.vzw.debug) 
;;(use '[clj-time.core :exclude [second extend start]])


(defn load-this-file []
  (ns cenx.heimdallr.customers.vzw-mpn.debug)
  (load-string (slurp "/Users/fergusonsa/CENX/utils/alarms-helpers.clj"))
  )
 
;; (doseq [alrm (get-all-active-alarms)]
;;    (println alrm " is mine? " (is-my-alarm alrm))) 

(defonce submitted-alarms (atom #{}))

(def scotts-description "Sent by Scott F")

(defn format-date [dt]
    (.format (java.text.SimpleDateFormat. "yyyy-MM-dd HH:mm:ss") dt))


(defn show-alarm-info []
  (println "Currently " (count (get-all-active-alarms)) "active alarms!\n")
  (doseq [alarm-name (apply sorted-set (get-all-active-alarms))]     
    (let [alarm-obj (get-object alarm-name)]
        (println (format "%-100s" alarm-name) " updated " (format-date (:fault/timestamp alarm-obj)) " text "  (:vsm/text alarm-obj)))))


(defn is-my-alarm [alarm-name]
  (or (contains? @submitted-alarms alarm-name)   
    (let [alarm-obj (get-object alarm-name)]
      (if-let [txt (:vsm/text alarm-obj)] 
        (= txt scotts-description)
        false))))
 

(defn cancel-all-alarms []
  (doseq [alarm-name (get-all-active-alarms)] 
    (send-vsm alarm-name :Ok scotts-description)))

(defn clear-alarm [alarm-name]
  (if (is-my-alarm alarm-name)
    (do 
      (send-vsm alarm-name :Ok scotts-description)
      (swap! submitted-alarms disj alarm-name)))
  (println "\nNumber of alarms currently active: " (count (get-all-active-alarms)))
  (println "\nNumber of MY alarms currently active: " (count @submitted-alarms)))


(defn raise-random-alarms [num-to-submit]
  (let [initial-num-submitted (count @submitted-alarms)
        alarm-names (get-alarm-names)]
    (while (< (count @submitted-alarms) (+ initial-num-submitted num-to-submit))
      (let [alrm-name (rand-nth alarm-names)]
        (if (and (not (nil? alrm-name))  (not (some #(=  alrm-name %) (get-all-active-alarms))))
          (do 
            (println "submitting " alrm-name)
            (swap! submitted-alarms conj alrm-name)
            (send-vsm alrm-name :Failed scotts-description)))))
  (println "\nNumber of alarms currently active: " (count (get-all-active-alarms))))
  (println "\nNumber of MY alarms currently active: " (count @submitted-alarms)))


(defn cancel-all-my-alarms []
  (doseq [alarm-name (clojure.set/intersection @submitted-alarms (into #{} (get-all-active-alarms)))]     
    (do
      (println "Setting alarm " alarm-name " to :Ok")
      (send-vsm alarm-name :Ok scotts-description)
      (swap! submitted-alarms disj alarm-name)))
  (println "\nNumber of alarms currently active: " (count (get-all-active-alarms)))
  (println "\nNumber of MY alarms currently active: " (count @submitted-alarms)))

(defn cancel-all-my-alarms2 []
  (doseq [alarm-name (filter is-my-alarm (get-all-active-alarms))]     
    (do
      (println "Setting alarm " alarm-name " to :Ok")
      (send-vsm alarm-name :Ok scotts-description)
      (swap! submitted-alarms disj alarm-name))))


; Showing active alarms of type daxDataViolationAlert
(defn show-active-daxData-alarms []
    (pprint (apply sorted-set (filter (fn [x] (.contains x "daxDataViolationAlert")) (get-all-active-alarms)))))


(defn show-my-active-alarms []
    (pprint (apply sorted-set (filter is-my-alarm (get-all-active-alarms)))))

;; (send-vsm  "daxDataViolationAlert:MPN01050:3205682:HA_MIP_Reg_Total"  :Failed scotts-description)

(defn get-alarm-types []
  (set 
    (for [x (get-alarm-names)] 
      (re-find #"^[a-zA-Z_]*" x))))

(defn raise-alarm-of-type [type]
  (let [alrm-name (rand-nth (filter-alarm-names (str type ":")))]
    (println "submitting " alrm-name)
    (swap! submitted-alarms conj alrm-name)
    (send-vsm alrm-name :Failed scotts-description)))    

      
(defn raise-one-alarm-of-each-type []
  (dorun   
    (for [type (get-alarm-types)]
      (raise-alarm-of-type type)))  
  (println "\nNumber of alarms currently active: " (count (get-all-active-alarms)))
  (println "\nNumber of MY alarms currently active: " (count @submitted-alarms)))

(defn clear-random-alarms [num-to-clear]
  (let [initial-num-submitted (count @submitted-alarms)]
    (while (and (> (count @submitted-alarms) 0) (> (count @submitted-alarms) (- initial-num-submitted num-to-clear)) (> (count (get-all-active-alarms)) 0))
      (let [alarm-name (rand-nth (seq (clojure.set/intersection @submitted-alarms (into #{} (get-all-active-alarms)))))]
        (if (not (nil? alarm-name))
          (do
            (println "Setting alarm " alarm-name " to :Ok")
            (send-vsm alarm-name :Ok scotts-description)
            (swap! submitted-alarms disj alarm-name))))))
  (println "\nNumber of alarms currently active:    " (count (get-all-active-alarms)))
  (println "\nNumber of MY alarms currently active: " (count @submitted-alarms)))


(defn find-my-alarms []
  (reset! submitted-alarms (into #{} (filter is-my-alarm (get-all-active-alarms)))))
