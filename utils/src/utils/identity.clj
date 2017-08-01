(ns utils.identity
  "Stores identity information that is saved in the file \"~/.ssh/utils.bitbucket.identity\",
  which should contain a clojure map containing the following:
    {:user \"<bitbucket-username>\", :password \"<password>\"}

  If the file does not exist, the username and password will default to \"USER\" and \"UNKNOWN-PWD\".

  The file can be written to using the write-identity function.

  The reason for loading the identity information is to keep possibly sensitive information
  out of the source code and to easily make it useable by other users without having to change code.
  "
  (:require [utils.config :as config]
            [clojure.java.io :as io])
  (:import [java.nio.file Files CopyOption]))


(defn write-identity [username password]
  (let [ident-file-path (str config/user-root-path "/.ssh/utils.bitbucket.identity")
        ident-file (io/file ident-file-path)
        backup-path (str ident-file (.format (java.text.SimpleDateFormat. "yyyyMMdd_HHmmss") (new java.util.Date)))]
    (if (.exists ident-file)
      (Files/move (.toPath (io/file ident-file-path)) (.toPath (io/file backup-path)) (into-array CopyOption {}))
      (.mkdirs ident-file))
    (spit ident-file-path (str "; identity file for clojure utilities.\n" (prn-str {:user username :password password})))
    (doto ident-file
        (.setReadable false false)
        (.setWritable false false)
        (.setReadable true)
        (.setWritable true)
        (.setExecutable false))
    nil))


(defn load-identity []
  (let [ident-file-path (str config/user-root-path "/.ssh/utils.bitbucket.identity")
        ident-file (io/file ident-file-path)]
    (if (not (.exists ident-file))
      (write-identity (. (. System getenv) get "USER") "UNKNOWN-PWD"))
    (load-file ident-file-path)))


(def identity-info (load-identity))
