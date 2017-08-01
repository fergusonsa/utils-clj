(ns utils.config
  "This namespae contains the constants and configurations that can change between users and projects.

  The default set of configuration settings are:
    bitbucket-root-user - The bitbucket user that contains the git repositories under control.
    default-branch - The default master branch in the controlled git repositories.
    library-namespace - The namespace containing all the controlled clojure libraries, such that <library-namespace>.<library-name> is the namespace for a library.
    nexus-url-base - The root url for checking for released versions of libraries and applications in a nexus repository
    reports-path - The root directory where reports are to be stored. Defaults to ~/reports
    src-root-dir - The root directory where local git repositories are located for each clojure module/application/library.
    ssh-private-key-path - The path to the private rsa key file. Defaults to ~/.ssh/id_rsa
    ssh-public-key-path - The path to the public rsa key file. Defaults to ~/.ssh/id_rsa.pub
    user-root-path - The path to the user's root directory. Defaults to ~/
    workspace-root - The path to the root directory for the workspace.

  Checks for the file ~/.utils.config.clj and reads the map contained within and creates matching variables in the utils.config namespace.

  The reasons for loading configuration settings from a user specific file is to remove possibly sensitive information from the source code.
  "
  (:require [clojure.java.io :as io])
  (:use [clojure.pprint])
  (:import [java.nio.file Files CopyOption]))


(def defaults {"user-root-path" (System/getProperty "user.home")
               "ssh-public-key-path" (str (System/getProperty "user.home") "/.ssh/id_rsa.pub")
               "ssh-private-key-path" (str (System/getProperty "user.home") "/.ssh/id_rsa")
               "workspace-root" (str (System/getProperty "user.home") "/ROOT")
               "src-root-dir" (str (System/getProperty "user.home") "/ROOT/src")
               "reports-path" (str (System/getProperty "user.home") "/reports")
               "nexus-url-base" "http://nexus.root.localnet:8081/nexus/content/groups/public/root/"
               "library-namespace" "root"
               "bitbucket-root-user" "root-user"
               "default-branch" "integration"})


(defn write-config
  ([]
   (->> (ns-publics 'utils.config)
        (keys)
        (remove #{'load-config 'write-config 'show-config 'defaults 'loaded})
        (map #(hash-map (name %) (var-get (resolve %))))
        (into (sorted-map))
        (write-config)))
  ([vals-map]
    (let [file-path (str (System/getProperty "user.home") "/.utils.config.clj")
          config-file (io/file file-path)
          backup-path (str config-file "-" (.format (java.text.SimpleDateFormat. "yyyyMMdd_HHmmss") (new java.util.Date)))]
      (if (.exists config-file)
        (Files/move (.toPath (io/file file-path)) (.toPath (io/file backup-path)) (into-array CopyOption {})))
      (spit file-path (str "; auto generated config file settings. "
                           (.format (java.text.SimpleDateFormat. "yyyy-MM-dd HH:mm:ss") (new java.util.Date))
                           "\n" ))
      (spit file-path (prn-str vals-map) :append true)

      (doto config-file
          (.setReadable false false)
          (.setWritable false false)
          (.setReadable true)
          (.setWritable true)
          (.setExecutable false))
      nil)))


(defn load-config []
  (let [file-path (str (System/getProperty "user.home") "/.utils.config.clj")
        settings-map (if (.exists (io/file file-path))
                       (clojure.edn/read-string (slurp file-path))
                       (defaults))]
    (println "Loading utils.config settings from" file-path)
    (pprint settings-map)
    (doseq [[name-key value] settings-map]
       (println `(def ~(symbol name-key) ~(str "\"" value "\"")))
       (eval `(def ~(symbol name-key) ~(str value))))))

;;       (map #(do
;;              (println `(def ~(symbol (key %)) ~(str "\"" (val %) "\"")))
;;              (eval `(def ~(symbol (key %)) ~(str (val %))))) settings-map)))

;;     (->> (if (.exists (io/file file-path))
;;            (do
;;              (println "Loading utils.config settings from" file-path)
;;              (clojure.edn/read-string (slurp file-path)))
;;            (defaults))
;;          (map #(do
;;                  (println `(def ~(symbol (key %)) ~(str "\"" (val %) "\"")))
;;                  (eval `(def ~(symbol (key %)) ~(str "\"" (val %) "\""))))))))


(defn show-config []
   (->> (ns-publics 'utils.config)
        (keys)
        (remove #{'load-config 'write-config 'show-config 'defaults 'loaded})
        (filter #(= (type (resolve %)) clojure.lang.Var))
        (map #(hash-map (name %) (var-get (resolve %))))
        (into (sorted-map))
        (pprint)))

;Load the configuration
(def loaded (load-config))
