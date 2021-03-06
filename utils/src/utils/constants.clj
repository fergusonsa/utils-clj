(ns utils.constants
  "This namespae contains the constants and configurations that can change between users and projects.

  The default set of constants/configuration settings are:
    bitbucket-root-user - The bitbucket user that contains the git repositories under control.
    default-branch - The default master branch of the git repositories.
    library-namespace - The namespace containing all the controlled clojure libraries, such that <library-namespace>.<library-name> is the namespace for a library.
    nexus-url-base - The root url for checking for released versions of libraries and applications in a nexus repository
    reports-path - The root directory where reports are to be stored. Defaults to ~/reports
    available-src-roots - A vector containing directories containing various versions of sources
    src-root-dir - The root directory where local git repositories are located for each clojure module/application/library.
    central-config-root-dir - The root directory where the central config files for all applications are located.
    ssh-private-key-path - The path to the private rsa key file. Defaults to ~/.ssh/id_rsa
    ssh-public-key-path - The path to the public rsa key file. Defaults to ~/.ssh/id_rsa.pub
    user-root-path - The path to the user's root directory. Defaults to ~/
    workspace-root - The path to the root directory for the workspace.
    deployable-applications - A vector containing the names of applications that may be deployed in an environment.

  Checks for the file ~/.utils.constants/.utils.constants.clj and reads the map contained within and creates matching variables in the utils.constants namespace.

  The reasons for loading constants/configuration settings from a user specific file is to remove possibly sensitive information from the source code.
  "
  (:refer-clojure :exclude [help])
  (:require [clojure.java.io :as io]
            [clojure.test])
  (:use [clojure.pprint])
  (:import [java.nio.file Files CopyOption]))


(defn help
  []
  ((resolve 'utils.core/utils-help) 'utils.constants))


(def constants-file-path
  "The path for the file containing the customized configuration settings for the user."
  (str (System/getProperty "user.home") "/.utils.constants/.utils.constants.clj"))

(def defaults {"user-root-path" {:value (System/getProperty "user.home")
                                 :doc "The path to the user's root directory. Defaults to ~/"}
               "ssh-public-key-path" {:value (str (System/getProperty "user.home") "/.ssh/id_rsa.pub")
                                      :doc "The path to the public rsa key file. Defaults to ~/.ssh/id_rsa.pub"}
               "ssh-private-key-path" {:value (str (System/getProperty "user.home") "/.ssh/id_rsa")
                                       :doc "The root directory where local git repositories are located for each clojure module/application/library."}
               "workspace-root" {:value (str (System/getProperty "user.home") "/ROOT")
                                 :doc "The path to the root directory for the workspace."}
               "available-src-roots" {:doc "A vector containing directories containing various versions of sources."
                                          :value  ["/ROOT/src"]},
               "src-root-dir" {:value (str (System/getProperty "user.home") "/ROOT/src")
                               :doc "The root directory where local git repositories are located for each clojure module/application/library."}
               "central-config-root-dir" {:value "/opt/config/application"
                                          :doc "The root directory where the central config files for all applications are located."}
               "reports-path" {:value (str (System/getProperty "user.home") "/reports")
                               :doc "The root directory where reports are to be stored. Defaults to ~/reports"}
               "nexus-url-base" {:value "http://nexus.root.localnet:8081/nexus/content/groups/public/root/"
                                 :doc "The root url for checking for released versions of libraries and applications in a nexus repository"}
               "library-namespace" {:value "root"
                                    :doc "The namespace containing all the controlled clojure libraries, such that <library-namespace>.<library-name> is the namespace for a library."}
               "bitbucket-root-user" {:value "root-user"
                                      :doc "The bitbucket user that contains the git repositories under control."}
               "default-branch" {:value "integration"
                                 :doc "The default master branch of the git repositories."}
               "deployable-applications" {:doc "A vector containing the names of applications that may be deployed in an environment."
                                          :value  ["app1" "app2"]}})

(defn get-current-constants
  "Returns a map containing constants/configuration settings with the variable names as keys, and their values.
  Currently on returns string and long (integer) settings."
  []
  (->> (ns-publics 'utils.constants)
    (filter (fn [[k v]] (not (clojure.test/function? (.get v)))))
    (filter (fn [[k v]] (not (contains? #{'defaults 'constants-file-path} k))))
    (map (fn [[k v]] (hash-map (name k)
                               (hash-map :value (var-get v)
                                         :doc (get (meta v) :doc "")))))
    (into (sorted-map))))


(defn write-constants
  "Writes the provided map, or the current settings if a map is not provided, to the file path in 'utils.constants/config-file-path"
  ([]
   (->> (get-current-constants)
        (write-constants)))
  ([vals-map]
    (let [constants-file (io/file constants-file-path)
          backup-path (str constants-file-path "-" (.format (java.text.SimpleDateFormat. "yyyyMMdd_HHmmss") (new java.util.Date)))]
      (if (.exists constants-file)
        (Files/move (.toPath constants-file) (.toPath (io/file backup-path)) (into-array CopyOption {})))
      (spit constants-file-path (str "; auto generated config file settings. "
                           (.format (java.text.SimpleDateFormat. "yyyy-MM-dd HH:mm:ss") (new java.util.Date))
                           "\n" ))
      (spit constants-file-path (prn-str vals-map) :append true)

      (doto constants-file
          (.setReadable false false)
          (.setWritable false false)
          (.setReadable true)
          (.setWritable true)
          (.setExecutable false))
      nil)))


(defn load-constants
  "Loads the constants/configuration settings in the file path in 'utils.constants/constants-file-path
  If the file does not exist, it loads the default setting in 'utils.constants/defaults."
  [& args]
  (let [verbose (some #(= :verbose %) args)
        settings-map (if (.exists (io/file constants-file-path))
                       (clojure.edn/read-string (slurp constants-file-path))
                       (defaults))]
    (if verbose
     (println "Loading utils.constants settings from" (if (.exists (io/file constants-file-path)) constants-file-path "the defaults")))
    (doseq [[name-key value] settings-map]
       (if verbose (println name-key (type (:value value)) (:value value)))
       (eval `(def ~(symbol name-key) ~(get value :doc "") ~(:value value))))))


(defn show-constants
  "Displays the current constants/configuration settings."
  []
  (map (fn [[k v]]
         (println (str k " : \"" (:value v) "\""))
         (if (:doc v)
           (println (:doc v)))
         (println))
       (get-current-constants)))

;Load the configuration
(load-constants)


(defn- parse-int [s]
  (Integer/parseInt (re-find #"\A-?\d+" s)))


(defn set-src-root!
  "Sets the workspace source root directory for other util functions.
  If the 'new-root-path is not provided, the user is presented with a list of
  available source roots that are in the 'available-src-roots constant, and the
  user can enter the corresponding number to select a source root.

  Arguments:
    new-root-path - Optional - a directory path for the workspace root containing git repos."
  ([]
   (let [available-roots (zipmap (range 1 (+ (count available-src-roots) 1)) available-src-roots)]
     (binding [*print-right-margin* 30
              *print-miser-width* 12]
     (pprint available-src-roots))
     (println "Available source root directories:")
     (doseq [[k v] available-roots]
       (println "  " k ":" v))
     (print "Enter the number of the directory to use and hit <Enter>: ")
     (flush)
     (let [input (read-line)
           root-path (get available-roots (parse-int input))]
       (println "input: " input)
       (if root-path
         (set-src-root! root-path)
         ("Invalid character entered")))))
  ([new-root-path]
   (if (.isDirectory (io/file new-root-path))
     (alter-var-root #'src-root-dir (constantly new-root-path))
     (println new-root-path "is not a valid directory containing source."))))
