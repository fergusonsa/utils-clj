(ns utils.core
  "Utility functions used by other namespaces in the utils project"
  (:require [utils.config :as config]
            [clojure.string :as string])
  (:use [clojure.pprint])
  (:gen-class))


(defn get-tag [repo-name version]
  (if (not (string? version))
    (println "!!!!!!!!!!!!!!!!!!!!!!!! version is not string! >" version "<"))
  (if (or (.startsWith version repo-name) (= version config/default-branch))
    version
    (str repo-name "-" version)))


(defn ignored? [classname]
  (let [ignored #{"callers" "dbg" "clojure.lang" "swank" "eval"}]
    (some #(re-find (re-pattern %) classname) ignored)))


(defn callers []
  (let [fns (map #(str (.getClassName %))
                 (-> (Throwable.) .fillInStackTrace .getStackTrace))]
    (vec (doall (remove ignored? fns)))))


(defn get-report-file-name-path [prefix & {:keys [create-parents path extension subdirectory]
                                           :or {create-parents true
                                                path config/reports-path
                                                extension ".txt"
                                                subdirectory nil}}]
   (let [dir-path (str (if (.endsWith path "/") path (str path "/")) (if (nil? subdirectory) "" (str subdirectory "/")))
         file-path (str dir-path prefix "-" (.format (java.text.SimpleDateFormat. "yyyyMMdd_HHmmss") (new java.util.Date)) extension)]
     (if create-parents (clojure.java.io/make-parents file-path))
     file-path))


(defn tests_calls []
  (let [todays-date (java.text.SimpleDateFormat. "yyyyMMdd_HH")]
    (assert (re-matches (re-pattern (str "/Users/fergusonsa/reports/PREFIX-" todays-date "[0-5][0-9][0-5][0-9].txt"))
                        (get-report-file-name-path "PREFIX" :create-parents false))
            "Testing (get-report-file-name-path \"PREFIX\" :create-parents false) failing to return expected result")
    (assert (re-matches (re-pattern (str "/Users/PREFIX2-" todays-date "[0-5][0-9][0-5][0-9].xxx"))
                        (get-report-file-name-path "PREFIX2" :path "/Users/" :extension ".xxx" :create-parents false))
            "Testing (get-report-file-name-path \"PREFIX2\" :path \"/Users/\" :extension \".xxx\" :create-parents false) failing to return expected result")))


(defn log-action [& args]
  (let [message (apply str args)
        calling-function (nth (callers) 3)
        log-file (str config/reports-path "/action-logs/action-log-" (.format (java.text.SimpleDateFormat. "yyyyMMdd") (new java.util.Date)) ".txt")
        timestamp-str (.format (java.text.SimpleDateFormat. "yyyy-MM-dd HH:mm:ss") (new java.util.Date))]
    (if (> (count message) 0)
      (spit log-file (format "%s %-40.40s %s\n" timestamp-str calling-function message) :append (.exists (clojure.java.io/file log-file))))))


(defn- print-function-help-info [fn-name var]
  (let [meta-info (meta var)]
    (print fn-name "")
    (if (:arglists meta-info)
      (pprint (:arglists meta-info))
      (println))
    (if (:doc meta-info)
      (println " " (string/trim (:doc meta-info))))
    (println)))


(defn help
  "Display the "
  ([]
   (help *ns*))
  ([name-space]
   (binding [*print-right-margin* 140]
      (let [nm-space (if (string? name-space) (symbol name-space) name-space)]
        (for [[fn-name var] (into (sorted-map) (ns-publics nm-space))]
          (print-function-help-info fn-name var)))))
  ([name-space fn-name]
   (binding [*print-right-margin* 140]
    (let [nm-space (if (string? name-space) (symbol name-space) name-space)
          fnc-name (if (string? fn-name) fn-name (name fn-name))
          var (ns-resolve nm-space (symbol fnc-name))]
      (print-function-help-info fnc-name var)))))
