(ns utils.environments
  (:require [clj-http.client :as client]
            [cheshire.core :as json]
            [clojure.string :as string]
            [clojure.java.io :as io]
            [yaml.core :as yaml]
            [utils.identity]
            [utils.core :as utils]
            [utils.constants :as constants]
            [utils.repositories :as repositories])
  (:use [clojure.set :only [difference intersection]]
        [clojure.java.browse]
        [clojure.pprint]
        [slingshot.slingshot :only [try+]])
  (:import [java.io StringReader]
           [java.util Properties]))


(defn get-server-status-api-url
  "Returns the proper url for the bifrost api status.
  Can handle included or missing protocol, port, and path portions of the url"
  [env-srvr]
  (str (if (not (.startsWith env-srvr "http")) "http://" "")
    env-srvr
    (if (and (not (re-find #":[0-9]+" env-srvr)) (not (.endsWith env-srvr "/api/status"))) ":8080" "")
    (if (not (.endsWith env-srvr "/api/status")) "/api/status" "")))


(defn get-env-app-info
  "Gets the versions of applications deployed to the specified server by calling the
  api/status interface and parsing the results to return a sorted map with application
  names as keys and versions as values."
  [env-srvr]
  (-> (get-server-status-api-url env-srvr)
      (client/get)
      (:body)
      (clojure.string/replace  #":connection ([a-zA-Z0-9.@]*)" ":connection \"$1\"")
      (load-string)
      (:runtime-names)
      (string/split  #", ")
      ((partial map (fn [x] (if-let [ matcher (re-find #"([a-zA-Z-]*)(?:-((?:[0-9.-]+)(?:-snapshot)?))?\.[jw]ar" x)]
                      [(second matcher) (last matcher)]
                      [x nil]))))
      (#(into (sorted-map) %))))


(defn get-env-versions
  "Gets the application versions for the specified environment. If the environment
  is not specified, then the versions of the local git repositories is used.

  Valid string formats for the environment id can be one of the following:
   - \"r/.*\" -- which is a release branch name.
   - nil -- which indicates to get the versions of the local git repositories.
   - \".*\" -- which should be a server name that hosts a bifrost status api.
               (see 'utils.environments/get-server-status-api-url)"
  [& [env-id]]
  (cond
    (nil? env-id) (-> (repositories/get-repo-src-versions)
                      (select-keys constants/deployable-applications))
    (string/starts-with? env-id "r/") (repositories/get-app-versions-from-manifest-properties env-id)
    :else (-> env-id
              (get-server-status-api-url)
              (get-env-app-info))))

(defn get-env-name
  "Gets a displayable name for the specified environment. If the environment
  is not specified, then it assumes that the desired environment is the local git repositories.

  Valid string formats for the environment id can be one of the following:
   - \"r/.*\" -- which is a release branch name.
   - nil -- which indicates to get the versions of the local git repositories.
   - \".*\" -- which should be a server name that hosts a bifrost status api.
               (see 'utils.environments/get-server-status-api-url)"
  [& [env-id]]
  (cond
    (nil? env-id) "Local git repositories"
    (string/starts-with? env-id "r/") (str "Release " env-id)
    :else (first (string/split env-id #"\." 2))))



(defn display-env-app-versions
  "Displays the application versions for the specified environment."
  [env-srvr]
  (println "Application versions for" (get-env-name env-srvr))
  (pprint (get-env-versions env-srvr))
  (utils/log-action "(display-env-app-versions \"" env-srvr "\")"))


(defn compare-envs
  "Returns a vector containing:
    - a set of applications names present in the second environment but missing
      from the first environment
    - a set of applications names present in the first environment but missing
      from the second environment
    - a lazy sequence containing lists
      which contain (\"<application-name>\" \"<env1-version>\" \"<env2-version>\")
    - a map with applications names as keys and versions as values for applications
      that are the same version in both environments
  "
  [env1-id & [env2-id]]
  (let [env1-app-versions (get-env-versions env1-id)
        env2-app-versions (get-env-versions env2-id)
        k1 (set (keys env1-app-versions))
        k2 (set (keys env2-app-versions))
        missing-in-1 (apply sorted-set (difference k2 k1))
        missing-in-2 (apply sorted-set (difference k1 k2))
        present-but-different (filter #(not= (env1-app-versions %) (env2-app-versions %))
                                      (intersection k1 k2))
        map-diff (sort-by
                   first
                   (map #(list % (env1-app-versions %) (env2-app-versions %))
                        present-but-different))
        same-versions (into (sorted-map)
                            (select-keys env1-app-versions
                                   (filter #(= (env1-app-versions %) (env2-app-versions %))
                                           (intersection k1 k2))))]
        [missing-in-1 missing-in-2 map-diff same-versions]))


(defn display-env-differences
  "Displays the application version differences between the first environment and the second environment.
  If the second environment is not specified, then the versions of the local git repositories is used.
  "
  [env1-id & [env2-id]]
  (let [env1-name (get-env-name env1-id)
        env2-name (get-env-name env2-id)
        [missing-in-1 missing-in-2 map-diff same-versions] (compare-envs env1-id env2-id)]
    (println "\nComparing applications deployed in the" env1-name "environment to those in the" env2-name "environment.\n")
    (println (cl-format nil (str "~{App ~a is missing in " env1-name "~%~}~{App ~a is missing in "
                                 env2-name "~%~}~{~{App ~a is different~%  "
                                 env1-name " -> ~a~%  " env2-name
                                 " -> ~a~%~}~}~{~{App ~a is the same version in both environments:  ~a~%~}~}")
                        missing-in-1
                        missing-in-2
                        map-diff
                        same-versions))))


(defn open-app-version-diff [app-name version1 version2]
  (let [older-version (compare version1 version2)]
    (cond
      (> older-version 0) (let [url (str "https://bitbucket.org/" constants/bitbucket-root-user "/" app-name "/compare/"
                                         app-name "-" version1 "%0D" app-name "-" version2 "#diff")]
        (println "Opening url " url)
        (browse-url url))
      (< older-version 0) (let [url (str "https://bitbucket.org/" constants/bitbucket-root-user "/" app-name "/compare/"
                                         app-name "-" version2 "%0D" app-name "-" version1 "#diff")]
        (println "Opening url " url)
        (browse-url url)))))


(defn open-app-version-diffs [env1-id env2-id]
  (let [[missing-in-1 missing-in-2 map-diff same-versions] (compare-envs env1-id env2-id)]
    (doseq [[app-name version1 version2] map-diff]
      (open-app-version-diff app-name version1 version2))))


(defn get-app-diffs-between-tags [app-name version1 version2]
  (let [tag-results-1 (client/get (str "https://api.bitbucket.org/2.0/repositories/" constants/bitbucket-root-user "/" app-name "/refs/tags/" app-name "-" version1)
            {:content-type :json :basic-auth [(:user utils.identity/identity-info) (:password utils.identity/identity-info)] :throw-exceptions false :as :json})
        tag-hash-1 (get-in tag-results-1 [:body :target :hash])
        tag-results-2 (client/get (str "https://api.bitbucket.org/2.0/repositories/" constants/bitbucket-root-user "/" app-name "/refs/tags/" app-name "-" version2)
            {:content-type :json :basic-auth [(:user utils.identity/identity-info) (:password utils.identity/identity-info)] :throw-exceptions false :as :json})
        tag-hash-2 (get-in tag-results-2 [:body :target :hash])
        tag-diff-results (client/get (str "https://api.bitbucket.org/2.0/repositories/" constants/bitbucket-root-user "/" app-name "/diff/" tag-hash-1 ".." tag-hash-2)
            { :basic-auth [(:user utils.identity/identity-info) (:password utils.identity/identity-info)] :throw-exceptions false })
        file-name (utils/get-report-file-name-path (str app-name "-diffs-between-" version1 "-" version2)) ]
;;     (spit file-name (:body tag-diff-results))
    (:body tag-diff-results)))


(defn download-app-war [app-name version]
  (let [war-filename (str app-name "-" version ".war")
        war-url (str constants/nexus-url-base app-name "/" version "/" war-filename)
        war-dir (str constants/workspace-root "/warfiles")
        app-war-dir (str war-dir "/" app-name)
        dest-path (str app-war-dir "/" war-filename)
        alt-war-path (str constants/user-root-path "/.m2/repository/" constants/library-namespace "/" app-name "/" version "/" war-filename)]
    (io/make-parents dest-path)
    (if-not (.exists (io/file dest-path))
      (if (.exists (io/file alt-war-path))
        (do
          (println "Copying" war-filename "for app" app-name "from" alt-war-path)
          (io/copy (io/file alt-war-path) (io/file dest-path)))
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
              (println "++++++++++++++++++++++++++++++++++++++++++ ")))))
      (println "war file " war-filename " already present for app " app-name))
      (utils/log-action "Downloaded war file" war-filename "to" dest-path)))


(defn download-wars-matching-environment [env-srvr]
  (let [env-app-versions (filter #(some? (second %)) (get-env-app-info env-srvr))]
    (pprint env-app-versions)
    (for [versions env-app-versions]
      (download-app-war (first versions) (second versions)))))


(defn deploy-wars-matching-environment [env-srvr]
  (let [env-app-versions (filter #(some? (second %)) (get-env-app-info env-srvr))
        url-base "http://admin:admin@192.168.99.100:9990/management"]
    (for [versions env-app-versions]
      (let [app-name (first versions)
            version (second versions)
            war-filename (str app-name "-" version ".war")
            war-dir (str constants/workspace-root "/warfiles")
            app-war-dir (str war-dir "/" app-name)
            dest-path (str app-war-dir "/" war-filename)]
        (println "For app " app-name)
        (println "undeploy url:" (str "curl -S -H \"content-Type: application/json\" -d '{\"operation\":\"undeploy\", \"address\":[{\"deployment\":\"" war-filename  "\"}]}' --digest" url-base))
        (println "remove url  :" (str "curl -S -H \"content-Type: application/json\" -d '{\"operation\":\"remove\", \"address\":[{\"deployment\":\"" war-filename "\"}]}' --digest " url-base))
        (println "upload url  :" (str "curl -F \"file=@" dest-path "\" --digest " url-base "/add-content"))
        (println "deploy url  :" (str "curl -S -H \"Content-Type: application/json\" -d '{\"content\":[{\"hash\": {\"BYTES_VALUE\" : \"...<BYTES_VALUE VALUE FROM RESPONSE TO UPLOAD URL>...\"}}], \"address\": [{\"deployment\":\"" war-filename "\"}], \"operation\":\"add\", \"enabled\":\"true\"}' --digest " url-base))
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
    (let [compose-yaml-file (str constants/src-root-dir "/dev-env/projects/" project "/docker-compose.yml")
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
        env-app-versions (get-env-app-info env-srvr)
        compose-yaml-file (str constants/src-root-dir "/dev-env/projects/" project-name "/docker-compose.yml")
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


