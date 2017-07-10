(ns utils.identity
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
