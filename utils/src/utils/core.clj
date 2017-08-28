(ns utils.core
  "Utility functions used by other namespaces in the utils project"
  (:require [utils.constants :as constants]
            [clojure.string :as string])
  (:use [clojure.pprint])
  (:gen-class))


(defn get-tag
  "Returns the proper tag for a version of a repo, usually in the format of \"<repo-name>-<version>\"

  Arguments:
    repo-name - name of the app/module/repo.
    version - the version of the repo."
  [repo-name version]
  (if (not (string? version))
    (println "!!!!!!!!!!!!!!!!!!!!!!!! version is not string! >" version "<"))
  (if (or (= version constants/default-branch) (.startsWith version (str repo-name "-")))
    version
    (str repo-name "-" version)))


(defn check-optional-arguments-for-array
  "Guarantees to return a vector  "
  [args]
  (cond
    (nil? args)
    []

    (and (sequential? (first args)) (= (count args) 1))
    (first args)

    (and (sequential? args) (= (count args) 1) (nil? (first args)))
    []

    (sequential? args)
    args

    :else
    [args]))


(defn- ignored?
  ""
  [classname]
  (let [ignored #{"callers" "dbg" "clojure.lang" "swank" "eval"}]
    (some #(re-find (re-pattern %) classname) ignored)))


(defn- callers []
  (let [fns (map #(str (.getClassName %))
                 (-> (Throwable.) .fillInStackTrace .getStackTrace))]
    (vec (doall (remove ignored? fns)))))


(defn get-report-file-name-path
  "Returns a path for a report file with the format \"<:path>/<:subdirectory>/<prefix>-yyyyMMdd_HHmmss<:extenstion>\".

  Arguments:
    prefix - prefix for the report file name.
    :create-parents - Optional. Whether to create any missing parent directories. Defaults to true.
    :subdirectory - Optional. A string relative path of subdirectories. Defaults to nil.
    :path - Optional. A string absolute path to the root directory to place reports. Defaults to 'utils.constants/report-path.
    :extension - Optional. A string containing the file extension to use. Defaults to \".txt\"."
  [prefix & {:keys [create-parents path extension subdirectory]
             :or {create-parents true
                  path constants/reports-path
                  extension ".txt"
                  subdirectory nil}}]
   (let [dir-path (str (if (.endsWith path "/") path (str path "/")) (if (nil? subdirectory) "" (str subdirectory "/")))
         file-path (str dir-path prefix "-" (.format (java.text.SimpleDateFormat. "yyyyMMdd_HHmmss") (new java.util.Date)) extension)]
     (if create-parents (clojure.java.io/make-parents file-path))
     file-path))


(defn log-action
  "Write a log entry to the daily rotating log file with a timestamp, calling function name, and the string representation of the arguments passed in."
  [& args]
  (let [message (apply str args)
        calling-function (nth (callers) 3)
        log-file (str constants/reports-path "/action-logs/action-log-" (.format (java.text.SimpleDateFormat. "yyyyMMdd") (new java.util.Date)) ".txt")
        timestamp-str (.format (java.text.SimpleDateFormat. "yyyy-MM-dd HH:mm:ss") (new java.util.Date))]
    (if (> (count message) 0)
      (spit log-file (format "%s %-40.40s %s\n" timestamp-str calling-function message) :append (.exists (clojure.java.io/file log-file))))))


(defn- print-function-help-info
  [fn-mapentry desired-fn-name]
  (let [fn-name (key fn-mapentry)
        var (val fn-mapentry)
        meta-info (meta var)
        pattern (if (nil? desired-fn-name) #".*" (re-pattern desired-fn-name))
        instance (.get var)]
    (if (or (nil? desired-fn-name) (re-find pattern (name fn-name)))
      (do
        (if (clojure.test/function? instance)
          (do
            (print "Function" fn-name "")
            (if (:arglists meta-info)
              (pprint (:arglists meta-info))
              (println)))
          (println (type instance) fn-name ""))
        (if (:doc meta-info)
          (println " " (string/trim (:doc meta-info))))
        (println)))))


(defn help
  "Helper method in case someone uses \"help\" instead of \"utils-help\""
  [& args]
  (println "Should be using \"(utils.core/utils-help)\" instead of \"(utils.core/help)\"")
  (apply utils-help args))


(defn utils-help
  "Display help on the specified namespace, or the current one if not specified,

  Examples:
    (utils-help)
    (utils-help *ns*)
    (utils-help 'utils.constants)
    (utils-help 'utils.constants \"load-constants\")
    (utils-help 'utils.constants \"constants\")
  "
  ([]
   (utils-help *ns*))
  ([name-space & [desired-fn-name]]
   (binding [*print-right-margin* 140]
     (println)
     (let [nm-space (if (string? name-space) (symbol name-space) name-space)
           pattern (if (nil? desired-fn-name) nil (re-pattern desired-fn-name))]
       (->> (ns-publics nm-space)
            (into (sorted-map))
            (map #(print-function-help-info % desired-fn-name)))))))

