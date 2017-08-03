(ns utils.config
  "This namespae contains the constants and configurations that can change between users and projects.

  The default set of configuration settings are:
    bitbucket-root-user - The bitbucket user that contains the git repositories under control.
    default-branch - The default master branch of the git repositories.
    library-namespace - The namespace containing all the controlled clojure libraries, such that <library-namespace>.<library-name> is the namespace for a library.
    nexus-url-base - The root url for checking for released versions of libraries and applications in a nexus repository
    reports-path - The root directory where reports are to be stored. Defaults to ~/reports
    src-root-dir - The root directory where local git repositories are located for each clojure module/application/library.
    ssh-private-key-path - The path to the private rsa key file. Defaults to ~/.ssh/id_rsa
    ssh-public-key-path - The path to the public rsa key file. Defaults to ~/.ssh/id_rsa.pub
    user-root-path - The path to the user's root directory. Defaults to ~/
    workspace-root - The path to the root directory for the workspace.

  Checks for the file ~/.utils.config/.utils.config.clj and reads the map contained within and creates matching variables in the utils.config namespace.

  The reasons for loading configuration settings from a user specific file is to remove possibly sensitive information from the source code.
  "
  (:require [clojure.java.io :as io])
  (:use [clojure.pprint])
  (:import [java.nio.file Files CopyOption]))

(def config-file-path
  "The path for the file containing the customized configuration settings for the user."
  (str (System/getProperty "user.home") "/.utils.config/.utils.config.clj"))

(def defaults {"user-root-path" {:value (System/getProperty "user.home")
                                 :doc "The path to the user's root directory. Defaults to ~/"}
               "ssh-public-key-path" {:value (str (System/getProperty "user.home") "/.ssh/id_rsa.pub")
                                      :doc "The path to the public rsa key file. Defaults to ~/.ssh/id_rsa.pub"}
               "ssh-private-key-path" {:value (str (System/getProperty "user.home") "/.ssh/id_rsa")
                                       :doc "The root directory where local git repositories are located for each clojure module/application/library."}
               "workspace-root" {:value (str (System/getProperty "user.home") "/ROOT")
                                 :doc "The path to the root directory for the workspace."}
               "src-root-dir" {:value (str (System/getProperty "user.home") "/ROOT/src")
                               :doc "The root directory where local git repositories are located for each clojure module/application/library."}
               "reports-path" {:value (str (System/getProperty "user.home") "/reports")
                               :doc "The root directory where reports are to be stored. Defaults to ~/reports"}
               "nexus-url-base" {:value "http://nexus.root.localnet:8081/nexus/content/groups/public/root/"
                                 :doc "The root url for checking for released versions of libraries and applications in a nexus repository"}
               "library-namespace" {:value "root"
                                    :doc "The namespace containing all the controlled clojure libraries, such that <library-namespace>.<library-name> is the namespace for a library."}
               "bitbucket-root-user" {:value "root-user"
                                      :doc "The bitbucket user that contains the git repositories under control."}
               "default-branch" {:value "integration"
                                 :doc "The default master branch of the git repositories."}})

(defn get-current-config
  "Returns a map containing configuration settings with the variable names as keys, and their values.
  Currently on returns string and long (integer) settings."
  []
  (->> (ns-publics 'utils.config)
    (filter (fn [[k v]] (not (contains? #{'load-config 'write-config 'show-config 'defaults 'loaded 'config-file-path} k))))
    (filter (fn [[k v]] (contains? #{java.lang.String java.lang.Long} (type (var-get v)))))
    (map (fn [[k v]] (hash-map (name k)
                               (hash-map :value (var-get v)
                                         :doc (get (meta v) :doc "")))))
    (into (sorted-map))))


(defn write-config
  "Writes the provided map, or the current settings if a map is not provided, to the file path in 'utils.config/config-file-path"
  ([]
   (->> (get-current-config)
        (write-config)))
  ([vals-map]
    (let [config-file (io/file config-file-path)
          backup-path (str config-file-path "-" (.format (java.text.SimpleDateFormat. "yyyyMMdd_HHmmss") (new java.util.Date)))]
      (if (.exists config-file)
        (Files/move (.toPath config-file) (.toPath (io/file backup-path)) (into-array CopyOption {})))
      (spit config-file-path (str "; auto generated config file settings. "
                           (.format (java.text.SimpleDateFormat. "yyyy-MM-dd HH:mm:ss") (new java.util.Date))
                           "\n" ))
      (spit config-file-path (prn-str vals-map) :append true)

      (doto config-file
          (.setReadable false false)
          (.setWritable false false)
          (.setReadable true)
          (.setWritable true)
          (.setExecutable false))
      nil)))


(defn load-config
  "Loads the configuration settings in the file path in 'utils.config/config-file-path
  If the file does not exist, it loads the default setting in 'utils.config/defaults."
  []
  (let [settings-map (if (.exists (io/file config-file-path))
                       (clojure.edn/read-string (slurp config-file-path))
                       (defaults))]
    (println "Loading utils.config settings from" (if (.exists (io/file config-file-path)) config-file-path "the defaults"))
    (doseq [[name-key value] settings-map]
       (eval `(def ~(symbol name-key) ~(get value :doc "") ~(str (:value value)))))))


(defn show-config
  "Displays the current configuration settings."
  []
  (binding [*print-right-margin* 160]
    (-> (get-current-config)
        (pprint))))


;Load the configuration
(load-config)
