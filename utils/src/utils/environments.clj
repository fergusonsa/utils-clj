(ns utils.environments
  (:require [clj-http.client :as client]
            [cheshire.core :as json]
            [clojure.string :as string]
            [clojure.java.io :as io]
            [yaml.core :as yaml]
            [utils.identity]
            [utils.core :as utils]
            [utils.config :as config])
  (:use [clojure.set :only [difference intersection]]
        [clojure.java.browse]
        [clojure.pprint]
        [slingshot.slingshot :only [try+]])
  (:import [java.io StringReader]
           [java.util Properties]))


(defonce connection-info (atom {:url "https://api.bitbucket.org/2.0/repositories/" config/bitbucket-root-user "/"}))


(defn get-env-app-info [env-url]
  (let [cloj-src (client/get env-url)
        mod-src (clojure.string/replace (:body cloj-src) #":connection ([a-zA-Z0-9.@]*)" ":connection \"$1\"")
        app-info (load-string mod-src)
        apps-list (string/split (:runtime-names app-info) #", ")]

    (assoc app-info :app-versions
      (into (sorted-map)
        (for [name apps-list]
          (if-let [ matcher (re-find #"([a-zA-Z-]*)(-(([0-9]+\.)+[0-9]+))?\.[jw]ar" name)]
            [(second matcher) (last (butlast matcher))]))))))


(defn get-env-app-versions [env-srvr]
  (let [env-url (str "http://" env-srvr "/api/status")
        env-info (get-env-app-info env-url)]
        (:app-versions env-info)))


(defn display-env-app-versions [env-srvr]
  (let [env-name (first (string/split env-srvr #"\." 2))
        env-app-versions (get-env-app-versions env-srvr)]
    (println "Application versions for" env-name)
    (pprint env-app-versions)
    (utils/log-action "(display-env-app-versions \"" env-srvr "\")")))


(defn compare-envs [env1-srvr env2-srvr]
  (let [env1-url (str "http://" env1-srvr "/api/status")
        env2-url (str "http://" env2-srvr "/api/status")
        env1-name (first (string/split env1-srvr #"\." 2))
        env2-name (first (string/split env2-srvr #"\." 2))
        env1-info (get-env-app-info env1-url)
        env2-info (get-env-app-info env2-url)
        env1-app-versions (:app-versions env1-info)
        env2-app-versions (:app-versions env2-info)
        k1 (set (keys env1-app-versions))
        k2 (set (keys env2-app-versions))
        missing-in-1 (difference k2 k1)
        missing-in-2 (difference k1 k2)
        present-but-different (filter #(not= (env1-app-versions %) (env2-app-versions %))
                                      (intersection k1 k2))
        map-diff (map #(list % (env1-app-versions %) (env2-app-versions %))
                    present-but-different)]
        [missing-in-1 missing-in-2 map-diff]))


(defn display-env-differences
  [env1-srvr env2-srvr]
  (let [env1-name (first (string/split env1-srvr #"\." 2))
        env2-name (first (string/split env2-srvr #"\." 2))
        compare-info (compare-envs env1-srvr env2-srvr)
        missing-in-1 (first compare-info)
        missing-in-2 (second compare-info)
        map-diff (last compare-info)]

    (println        (cl-format nil (str "~{App ~a is missing in " env1-name "~%~}~{App ~a is missing in "
                             env2-name "~%~}~{~{App ~a is different~%  "
                             env1-name " -> ~a~%  " env2-name " -> ~a~%~}~}")
                    missing-in-1
                    missing-in-2
                    map-diff))))


(defn open-app-version-diff [app-name version1 version2]
  (let [older-version (compare version1 version2)]
    (cond
      (> older-version 0) (let [url (str "https://bitbucket.org/" config/bitbucket-root-user "/" app-name "/compare/"
                                         app-name "-" version1 "%0D" app-name "-" version2 "#diff")]
        (println "Opening url " url)
        (browse-url url))
      (< older-version 0) (let [url (str "https://bitbucket.org/" config/bitbucket-root-user "/" app-name "/compare/"
                                         app-name "-" version2 "%0D" app-name "-" version1 "#diff")]
        (println "Opening url " url)
        (browse-url url)))))


(defn open-app-version-diffs [env1-srvr env2-srvr]
  (let [env1-name (first (string/split env1-srvr #"\." 2))
        env2-name (first (string/split env2-srvr #"\." 2))
        map-diff ((last compare-envs env1-srvr env2-srvr))]
    (doseq [[app-name version1 version2] map-diff]
      (open-app-version-diff app-name version1 version2))))


(defn get-app-diffs-between-tags [app-name version1 version2]
  (let [tag-results-1 (client/get (str "https://api.bitbucket.org/2.0/repositories/" config/bitbucket-root-user "/" app-name "/refs/tags/" app-name "-" version1)
            {:content-type :json :basic-auth [(:user utils.identity/identity-info) (:password utils.identity/identity-info)] :throw-exceptions false :as :json})
        tag-hash-1 (get-in tag-results-1 [:body :target :hash])
        tag-results-2 (client/get (str "https://api.bitbucket.org/2.0/repositories/" config/bitbucket-root-user "/" app-name "/refs/tags/" app-name "-" version2)
            {:content-type :json :basic-auth [(:user utils.identity/identity-info) (:password utils.identity/identity-info)] :throw-exceptions false :as :json})
        tag-hash-2 (get-in tag-results-2 [:body :target :hash])
        tag-diff-results (client/get (str "https://api.bitbucket.org/2.0/repositories/" config/bitbucket-root-user "/" app-name "/diff/" tag-hash-1 ".." tag-hash-2)
            { :basic-auth [(:user utils.identity/identity-info) (:password utils.identity/identity-info)] :throw-exceptions false })
        file-name (utils/get-report-file-name-path (str app-name "-diffs-between-" version1 "-" version2)) ]
;;     (spit file-name (:body tag-diff-results))
    (:body tag-diff-results)))


(defn download-app-war [app-name version]
  ;; http://nexus.cenx.localnet:8081/nexus/content/groups/public/cenx/apollo/3.3.16/apollo-3.3.16.war
  (let [war-filename (str app-name "-" version ".war")
        war-url (str config/nexus-url-base app-name "/" version "/" war-filename)
        war-dir (str config/workspace-root "/warfiles")
        app-war-dir (str war-dir "/" app-name)
        dest-path (str app-war-dir "/" war-filename)]
    (io/make-parents dest-path)
    (if-not (.exists (io/file dest-path))
      (do
        (println "Downloading " war-filename " for app " app-name)
        (try+
          (with-open [w (io/output-stream dest-path)]
            (.write w (:body (client/get war-url {:as :byte-array}))))
          (catch [:status 404] {}
            (println "The " war-filename "does not exist at" war-url))
          (catch Object _
            (println "++++++++++++++++++++++++++++++++++++++++++ ")
            (println "Exception trying to copy" war-filename "from" war-url "to" dest-path)
            (println "++++++++++++++++++++++++++++++++++++++++++ ")
            (println (:message &throw-context))
            (println "++++++++++++++++++++++++++++++++++++++++++ ")
            (println (:cause &throw-context))
            (println "++++++++++++++++++++++++++++++++++++++++++ "))))
      (println "war file " war-filename " already present for app " app-name))
      (utils/log-action "Downloaded war file" war-filename "to" dest-path)))


(defn download-wars-matching-environment [env-srvr]
  (let [env-url (str "http://" env-srvr "/api/status")
        env-info (get-env-app-info env-url)
        env-name (first (string/split env-srvr #"\." 2))
        env-app-versions (filter #(some? (second %)) (:app-versions env-info))]
    (pprint env-app-versions)
    (for [versions env-app-versions]
      (download-app-war (first versions) (second versions)))))


(defn deploy-wars-matching-environment [env-srvr]
  (let [env-url (str "http://" env-srvr "/api/status")
        env-info (get-env-app-info env-url)
        env-name (first (string/split env-srvr #"\." 2))
        env-app-versions (filter #(some? (second %)) (:app-versions env-info))
        url-base "http://admin:admin@192.168.99.100:9990/management"]
    (for [versions env-app-versions]
      (let [app-name (first versions)
            version (second versions)
            war-filename (str app-name "-" version ".war")
            war-dir (str config/workspace-root "/warfiles")
            app-war-dir (str war-dir "/" app-name)
            dest-path (str app-war-dir "/" war-filename)
            undeploy-url (str "curl -S -H \"content-Type: application/json\" -d '{\"operation\":\"undeploy\", \"address\":[{\"deployment\":\"" war-filename  "\"}]}' --digest" url-base)
            remove-url (str "curl -S -H \"content-Type: application/json\" -d '{\"operation\":\"remove\", \"address\":[{\"deployment\":\"" war-filename "\"}]}' --digest " url-base)
            upload-url (str "curl -F \"file=@" dest-path "\" --digest " url-base "/add-content")
            deploy-url (str "curl -S -H \"Content-Type: application/json\" -d '{\"content\":[{\"hash\": {\"BYTES_VALUE\" : \"...<BYTES_VALUE VALUE FROM RESPONSE TO UPLOAD URL>...\"}}], \"address\": [{\"deployment\":\"" war-filename "\"}], \"operation\":\"add\", \"enabled\":\"true\"}' --digest " url-base)]
        (println "For app " app-name)
        (println "undeploy url: " undeploy-url)
        (println "remove url  : " remove-url)
        (println "upload url  : " upload-url)
        (println "deploy url  : " deploy-url)
        (println)))))


(defn get-deployments-wildfly
  " NOT COMPLETED "
  [env-srv]
  (let [url-base "http://192.168.99.100:9990/management"
        srv-info-resp (client/get url-base {:basic-auth [(:user "admin") (:password "admin")] })]
        (true)
        ))


(defn update-docker-compose-app-version
  "
  "
  ([app-name version]
    (update-docker-compose-app-version "mpn" app-name version))
  ([project app-name version]
    (let [compose-yaml-file (str config/src-root-dir "/dev-env/projects/" project "/docker-compose.yml")
          compose-yaml (yaml/from-file compose-yaml-file)
          app-key (str (clojure.string/upper-case app-name) "-VERSION")
          existing-version (get-in compose-yaml ["services" "war-deployer" "environment" app-key])]
      (cond (nil? existing-version)
            (println "The app" app-name " is not deployed to wildfly via war-deployer. No change made.")

            (not= existing-version version)
            (do
              (.renameTo (io/file compose-yaml-file)
                         (io/file (str compose-yaml-file "." (.format (java.text.SimpleDateFormat. "yyyyMMdd_HHmmss")
                                                                      (new java.util.Date)))))
              (spit compose-yaml-file (yaml/generate-string
                                        (assoc-in compose-yaml
                                                  ["services" "war-deployer" "environment" app-key] version)
                                        :dumper-options {:flow-style :block}))
              (println "The app" app-name " version has been updated from" existing-version
                       "to" version ". dev-build will install the new version the next time it is run."))

            :else
             (println "The app" app-name " is alredy set to version" version ". No change made."))
      (utils/log-action "(update-docker-compose-app-version-for-env "\" app-name "\" \"" version "\") : updated " compose-yaml-file))))


(defn set-docker-compose-app-version-for-env
  "Gets the versions of apps currently runnning on the server env-srvr,
   Makes a backup of the existing dev-env/projects/mpn/docker-compose.yml file
   Updates the app versions in the docker-compose.yaml file to be the same as the server's"
  [env-srvr]
  (let [project-name "mpn"
        env-name (first (string/split env-srvr #"\." 2))
        env-app-versions (get-env-app-versions env-srvr)
        compose-yaml-file (str config/src-root-dir "/dev-env/projects/" project-name "/docker-compose.yml")
        compose-yaml-backup-file (str compose-yaml-file "-" (.format (java.text.SimpleDateFormat. "yyyyMMdd_HHmmss") (new java.util.Date)) ".txt")
        compose-yaml (yaml/from-file compose-yaml-file)
        app-keys (filter #(and (not (.startsWith % "HINTERLAND")) (.endsWith % "_VERSION")) (keys (get-in compose-yaml ["services" "war-deployer" "environment"])))
        app-versions (select-keys (get-in compose-yaml ["services" "war-deployer" "environment"]) app-keys)
        ren-env-app-versions (clojure.set/rename-keys env-app-versions (zipmap (keys env-app-versions) (map #(str (clojure.string/upper-case %) "_VERSION") (keys env-app-versions))))
        xx (select-keys ren-env-app-versions (keys app-versions))]
    (io/copy (io/file compose-yaml-file) (io/file compose-yaml-backup-file))
    (spit compose-yaml-file
          (yaml/generate-string
            (assoc-in compose-yaml
                    ["services" "war-deployer" "environment"]
                    (merge (get-in compose-yaml ["services" "war-deployer" "environment"])
                           xx))
            :dumper-options {:flow-style :block}))
    (utils/log-action "(set-docker-compose-app-version-for-env "\" env-srvr "\") : updated " compose-yaml-file " with versions " xx)))

(defn load-props
  "Given a path to a properties file, load it into a Java Properties object."
  [readable]
  (let [props (io/reader readable)]
    (doto (Properties.)
      (.load props))))


(defn get-app-versions-from-manifest-properties [branch]
  ; curl --user scott.ferguson@cenx.com:<pwd> https://bitbucket.org/cenx-cf/exanova/raw/906d95a8ffb5949adfabe32d6a1b1ff5fd6004f6/manifest.properties
;;   (doto (Properties.(
  (-> (str "https://bitbucket.org/" config/bitbucket-root-user "/exanova/raw/" branch "/manifest.properties")
      (client/get {:basic-auth [(:user utils.identity/identity-info) (:password utils.identity/identity-info)] })
      (:body)
      (.getBytes)
      (io/input-stream)
      (load-props)
      (select-keys ["apollo" "hinterland" "babelfish" "bifrost" "conduit" "delorean"
                    "hecate" "heimdallr" "icarus" "levski" "plataea" "naranathu"
                    "parker" "terminus" "tartarus"])))


(defn- example-function-calls []
  (let [env-srvr "med16.cenx.localnet:8080"
        env1-srvr "med02.cenx.localnet:8080"
        env2-srvr "localhost:8082"
        app-name "apollo"
        version1 "3.3.18"
        version2 "3.3.17"
        env-url ""]
    (get-env-app-versions env-srvr)
    (get-env-app-info env-url)
    (display-env-app-versions env-srvr)
    (compare-envs env1-srvr env2-srvr)
    (display-env-differences env1-srvr env2-srvr)
    (open-app-version-diff app-name version1 version2)
    (open-app-version-diffs env1-srvr env2-srvr)
    (get-app-diffs-between-tags app-name version1 version2)
    (download-app-war app-name version1)
    (download-wars-matching-environment env-srvr)
    (deploy-wars-matching-environment env-srvr)
    (update-docker-compose-app-version app-name version1)
    (update-docker-compose-app-version "mpn" app-name version1)
    (set-docker-compose-app-version-for-env env-srvr)
  ))
