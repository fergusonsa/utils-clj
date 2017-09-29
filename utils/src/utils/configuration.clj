(ns utils.configuration
  (:refer-clojure :exclude [help])
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.stacktrace :refer [print-cause-trace]]
            [utils.environments :as environments]
            [utils.repositories :as repositories]
            [utils.constants :as constants]
            [clj-time.core :as time-core]
            [clj-time.format :as time-format]
            [utils.fake :as creds]
            [utils.core :as utils]
            [clojure.data :as data]
            [zookeeper :as zk]
            [zookeeper.data :as zkdata]
            [clojure.string :as str])
  (:use [clojure.pprint]
        [clojure.set :only [difference intersection]]
        [slingshot.slingshot :only [try+]])
  (:import (org.apache.zookeeper KeeperException$NoNodeException
                                 ZooKeeper)))

(defn help
  []
  (utils/utils-help 'utils.configuration))

(defonce ^:private zk-url (atom nil))
(defonce ^:private zktimeout (atom nil))
(defonce ^:private client (atom nil))
(defonce ^:private zk-client-state (atom {}))
(defonce ^:private reconnect-count (atom 0))


(def config-path-root "Root directory for central configuration files" constants/central-config-root-dir)
(def configs-to-check
  "Map containing app/module/repo names as keys with a sub-map containing 3 entries:
      :file-name - the config file name for the module in the central configuration location,
                   usually config.edn or config.clj.
      :source-config-path - the path to the config file relative to the root directory of the
                            source repository.
      :keys - a list of lists of keys for entries in the config files that have environment
              dependent values, these will be used in 'clojure.core/get-in calls to get the values."
  {
    "conduit" {:file-name "config.edn"
               :source-config-path "dev/resources/config.edn"
               :keys [[ "nested" :url]]}
    "heimdallr" {:file-name "config.edn"
                 :source-config-path "resources/heimdallr/config.edn"
                 :keys [[:full :elm-url]
                        [:full :zk :urls]
                        [:build :zk :urls]]}
    "naranathu" {:file-name "config.edn"
                 :source-config-path "resources/config.edn"
                 :keys [[:containers :ingest :host]]}
    "stentor" {:file-name "config.edn"
               :source-config-path ""
               :keys [[:host]]}
    "epiphany" {:file-name "config.edn"
                :source-config-path ""
                :keys [[:security :ldap :host]
                       [:rethinkdb :host]
                       [:solr-url]
                       [:inventory-collection]
                       [:inventory-collection-url]
                       [:alarms-collection-url]
                       [:events-collection-url]
                       [:analytics-api-url]]}
    "huginn" {:file-name "config.clj"
              :source-config-path ""
              :keys [[:solr-url]
                     [:zk-url]]}
    "parker" {:file-name "config.edn"
              :source-config-path "resources/config.edn"
              :keys [[:kafka :local "metadata.broker.list"]
                     [:solr :local :url]
                     [:solr :local :zookeeper-url]
                     [:analytics-index :local :url]
                     [:analytics-index :local :zookeeper-url]
                     [:event-service-config :solr-url]
                     [:event-service-config :zookeeper-url]
                     [:event-service-config :solr-zookeeper-url]]}
    "stores/apollo/resources" {:file-name "config.edn"
                               :source-config-path ""
                               :keys [[:inventory-websocket-endpoint]
                                      [:inventory-base-url]
                                      [:audit-base-url]
                                      [:presentation-base-url]
                                      [:search-service-base-url]
                                      [:conduit-url]
                                      [:webxng-url]
                                      [:ops-tracker-url]
                                      [:analytics-base-url]
                                      [:adhoc-base-url]]}
    "apollo" {:file-name "config.edn"
              :source-config-path "dev/resources/config.edn"
              :keys [[:presentation-base-url]
                     [:search-service-base-url]
                     [:analytics-base-url]
                     [:data-driven-ui]
                     [:audit-base-url]
                     [:conduit-url]
                     [:client-logging-service-url]]}
    "icarus" {:file-name "config.edn"
              :source-config-path "resources/config.edn"
              :keys [[:db-spec :subname]
                     [:change-control-solr :url]
                     [:trouble-ticket-solr :url]
                     [:parker-solr :url]]}
    "athena" {:file-name "config.edn"
              :source-config-path ""
              :keys [[:full :db-uri]
                     [:full :solr-url]
                     [:full :solr-zk-url]
                     [:full :es-kafka-url]
                     [:full :es-zk-url]
                     [:build :db-uri]
                     [:build :solr-url]
                     [:build :solr-zk-url]
                     [:build :es-kafka-url]
                     [:build :es-zk-url]]}
    "granitedb" {:file-name "config.clj"
                 :source-config-path ""
                 :keys [[:host]]}
    "ripper" {:file-name "db_config.clj"
              :source-config-path ""
              :keys [[:host]]}
    "terminus" {:file-name "config.clj"
                :source-config-path "resources/config.edn"
                :keys [[:client-config :solr-url]
                       [:client-config :kafka-url]
                       [:client-config :zookeeper-url]
                       [:data-sources :hippalectryon :solr-url]
                       [:data-sources :hippalectryon :solr-zookeeper-url]]}
    "bifrost" {:file-name "config.clj"
               :source-config-path "resources/bifrost/config.clj"
               :keys [[:inventory-solr-url]
                      [:event-service-solr-url]
                      [:reconciliation-solr-url]
                      [:solr-zookeeper-url]
                      [:fault-url]
                      [:domain-controller]
                      [:kafka-url]
                      [:zookeeper-url]
                      [:bpm :engine-url]
                      [:file-upload-targets :remote-host]
                      [:metrics-graphite :host]]}
    "hecate" {:file-name "config.edn"
              :source-config-path ""
              :keys [[:realtime :zk-url-list]
                     [:default :zk-url-list]
                     [:solr-query :uri]
                     [:event-sink :uri]
                     [:alarm-sink :uri]
                     [:kafka-output :zk-url-list]]}
    "moirai" {:file-name "config.edn"
              :source-config-path "resources/config.edn"
              :keys [[:zookeeper :urls]]}})


(def ValidIpAddressRegex #"(http[s]?:\/\/)?(([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])\.){3}(1[0-9]{2}|2[0-4][0-9]|25[0-5]|[1-9][0-9]|[0-9])(:[0-9]{1,6})?")


(def ValidHostnameRegex #"(http[s]?:\/\/)?(([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9\-]*[a-zA-Z0-9])\.){3,}([A-Za-z0-9][A-Za-z0-9\-]*[A-Za-z0-9]|[A-Za-z0-9])(:[0-9]{1,6})?")

(defn- tree-seq-filter
  "Returns a lazy sequence of the nodes in a tree, via a depth-first walk.
   branch? must be a fn of one arg that returns true if passed a node
   that can have children (but may not).  children must be a fn of one
   arg that returns a sequence of the children. Will only be called on
   nodes for which branch? returns true. Root is the root node of the
  tree."
   [branch? children filter? root]
   (let [walk (fn walk [node]
                (lazy-seq
                 (cons (if (filter? node) node nil)
                  (when (branch? node)
                    (mapcat walk (children node))))))]
     (walk root)))

(defn find-config-files
  "A tree seq on java.io.Files"
  [dir]
  (filter #(not (nil? %))
  (tree-seq-filter
      (fn [^java.io.File f] (and (. f (isDirectory)) (not (contains? #{"example" "test" "orca" "exanova" "orca-env" "dev-env"} (. f (getName))))))
      (fn [^java.io.File d] (seq (. d (listFiles))))
      (fn [^java.io.File f] (or (str/ends-with? (. f (getName)) "config.edn")
                                (str/ends-with? (. f (getName)) "_config.clj")))
     dir)))

(defn contains-hostname-ipaddress?
  ""
  [strng]
  (or (not (nil? (re-matches ValidHostnameRegex strng)))
      (not (nil? (re-matches ValidIpAddressRegex strng)))))

(defn flatten-vector-check
  [vect ]

)
; {
;  "x" "x" ;; string value
;  "y" 1   ;; integer value
;  "z" [ "x" "u"] ;; vector
;  "a" { "x" "u"} ;; map




(defn flatten-keys
  "based on https://stackoverflow.com/questions/32853004/iterate-over-all-keys-of-nested-map"
  [m]
  (if (not (map? m))
    {[] m}
    (into {}
          (for [[k v] m
                [ks v'] (flatten-keys v)]
            (do
              (println "k:" k " v:" v " ks:" ks " v':" v')
            (cond
              (string? v)
              (if (contains-hostname-ipaddress? v)
                [(cons k ks) v']
                nil)
              (or (number? v) (keyword? v) (symbol? v))
              nil

;;               (sequential? v)
;;               (filter #(

              :else [(cons k ks) v']))))))


(defn check-file
  [fl]
  (let [contents (clojure.edn/read-string (slurp fl))]
    (if (map? contents)
      ())))

(defn check-config-files
  ""
  [root-path]
  ())


(defn which-zookeeper
  "Show which zookeeper currently connected to"
  []
  (println "Current status" @zk-client-state "for zookeeper instance" @zk-url))


(defn find-nested
  "Helper function to find nested values.

  Arguments:
  m - map
  k - key to be searched for"
  [m k]
  (->> (tree-seq map? vals m)
       (filter map?)
       (some k)))


(defn create-client
  "Create a zookeeper client connected to zookeeper server, and return it.
  This is where the connection is actually made to ZooKeeper.
  Throws an exception if it fails to connect."
  []
  (zk/connect @zk-url :timeout-msec @zktimeout))


(defn get-reconnect-count
  "Get the number of times a reconnect to the zookeeper server has been attempted
  since the client started or since the last client reset, whichever is most recent."
  []
  @reconnect-count)


(defn- reconnect
  "Reconnect to zookeeper and renew the watch of registered nodes.
  This will close the old client and create a new client."
  []
  (locking client
    (swap! reconnect-count inc)
    (try
      (zk/close @client)
      (reset! client (create-client))
      (catch Exception e
        (print-cause-trace e)
        (throw (Exception. (str "failed to reconnect zookeeper server " @zk-url) e))))))


(defn get-client
  "Get a zookeeper client"
  []
  (when (= @zk-client-state :stopped)
    (throw (Exception. "Attempting to get zookeeper client while zk-client is down.")))
  (locking client
    (when-not (= (zk/state @client) :CONNECTED)
      (reconnect))
    @client))


(defn set-node-data!
  ([^String node data]
   (locking client
     (let [client (get-client)]
       (when-let [version (:version (zk/exists client node :watch? true))]
         (set-node-data! node data version)))))
  ([^String node data version]
   (locking client
     (let [client (get-client)]
       (zk/set-data client node (zkdata/to-bytes (pr-str data)) version)))))


(defn get-node-data
  "Returns the data associated with the given node if it exists."
  ([^String node]
   (locking client
     (let [client (get-client)]
       (if-let [raw-data (:data (zk/data client node :watch? true))]
         (let [return-data (edn/read-string (zkdata/to-string raw-data))]
           return-data))))))


(defn set-zk-config
  "Create a zookeeper client and store it and its config in atoms: url, timeout, and client."
  [^String host timeout]
  (do
    (reset! zktimeout timeout)
    (reset! zk-url host)
    (reset! client (create-client))))


(defn get-children
  "Returns the children of the given node or nil if none exist."
  ([^String node]
   (locking client
     (let [client (get-client)]
       (zk/children client node :watch? true)))))


(defn get-all-zookeeper-config
  "Retrieves all nested nodes in the zookeeper instance @zk-url, starting at the root config node, if a specific node is not specified.

    Arguments:
    node - Optional - The root zookeeper node to include all sub nodes. Defaults to the root config node."
  ([]
   (get-all-zookeeper-config (str "/" constants/library-namespace "/config")))
  ([node]
   (->> node
        (get-children)
        (map #(->> (str node "/" %)
                   (get-all-zookeeper-config)))
        (into {})
        (merge {node (get-node-data node)}))))


(defn save-zookeeper-config-to-file
  "Saves the zookeeper config to a report file.

  Arguments:
    :root-node - Optional - The root zookeeper node to include all sub nodes. Defaults to the root config node.
    :path - Optional - The path to the directory where the files will be placed. Defaults to the default report directory, 'utils.constants/reports-path.
    :file-name - Optional - The file name (without the extension) to save to. Defaults to \"zookeeper-<env-name>-<root-node>\".
    :extension - Optional - The extension for the file name to save to. Defaults to \".clj\".
    :full-path - Optional - The full path, including filename for the file to save to. Overrides :path, :file-name, and :extension arguments."
  [& {:keys [root-node path file-name extension full-path]
      :or {root-node (str "/" constants/library-namespace "/config")
           path constants/reports-path
           file-name (str "zookeeper-"
                          (environments/get-env-name @zk-url)
                          (clojure.string/replace root-node #"/" "-"))
           extenstion ".clj"}}]
  (let [file-path (if-not (nil? full-path)
                    full-path
                    (utils/get-report-file-name-path file-name :extension extension :path path))]
    (binding [*print-right-margin* 140
              *print-miser-width* 120]
      (with-open [fl (clojure.java.io/writer file-path)]
        (.write fl (str "; Zookeeper central configurations on server " @zk-url " \n"))
        (-> (get-all-zookeeper-config root-node)
            ((partial into (sorted-map)))
            (clojure.pprint/pprint fl)))
      (println "Wrote zookkeeper config data for" @zk-url "to file" file-path))

    (utils/log-action "saving zookeeper configs starting at node '" root-node "' to 1 file '" file-path "'.")))


(defn- save-zookeeper-config-to-individual-file
  "Saves the zookeeper config to a report file.

  Arguments:
    :root-node - Optional - The root zookeeper node to include all sub nodes. Defaults to the root config node.
    :path - Optional - The path to the directory where the files will be placed. Defaults to the default report directory, 'utils.constants/reports-path."
  [node-path config-map path]
  (let [path-depth (count (clojure.string/split node-path #"/"))
        ending (if (and (> 4 (count (clojure.string/split node-path #"/"))) (clojure.string/ends-with? node-path "config"))
                 ".clj"
                 "/config.clj")
        file-path (str path (subs node-path 12) ending)]
    (clojure.java.io/make-parents file-path)
    (binding [*print-right-margin* 140
              *print-miser-width* 120]
      (with-open [fl (clojure.java.io/writer file-path)]
        (.write fl (str "; Zookeeper central configurations on server " @zk-url
                        " - " (.format (java.text.SimpleDateFormat. "yyyyMMdd_HHmmss") (new java.util.Date))" \n"))
        (-> config-map
            ((partial into (sorted-map)))
            (clojure.pprint/pprint fl))))
    (println "Wrote zookkeeper config data for node" node-path "on server" @zk-url "to file" file-path)
    (utils/log-action "saving zookeeper configs starting at node '" node-path "' to 1 file '" file-path "'.")))


(defn save-zookeeper-configs-to-files
  "Saves each zookeeper config node to its own report file.

  Arguments:
    :root-node - Optional - The root zookeeper node to include all sub nodes. Defaults to the root config node.
    :path - Optional - The path to the directory where the files will be placed. Defaults to the default report directory, 'utils.constants/reports-path."
  [& {:keys [root-node path]
      :or {root-node (str "/" constants/library-namespace "/config")
           path (str constants/workspace-root "/zk-configs/" @zk-url "-" (.format (java.text.SimpleDateFormat. "yyyyMMdd_HHmmss") (new java.util.Date)))}}]
  (let [configs (get-all-zookeeper-config root-node)]
    (map #(if-not (nil? (second %)) (save-zookeeper-config-to-individual-file (first %) (second %) path)) configs)))


(defn start-zkclient
  "Start function for Leaven ZKClient component"
  ([]
   (if (or (nil? @zk-url)
           (nil? @zktimeout))
     (start-zkclient "127.0.0.1:2181" 60000)
     (start-zkclient @zk-url @zktimeout)))
  ([host]
   (if (nil? @zktimeout)
     (start-zkclient host 60000)
     (start-zkclient host @zktimeout)))
  ([host timeout]
   (if-not (= :started @zk-client-state)
     (do
       (utils/log-action "starting zookeeper client to '" host)
       (set-zk-config host timeout)
       (reset! zk-client-state :started))
     :already-started)))


(defn stop-zkclient
  "Stop function for Leaven ZKClient component"
  [host]
  (if (= :started @zk-client-state)
    (do
        (reset! zk-client-state :stopped)
        (locking client
          (zk/close @client)
          (reset! reconnect-count 0)))))


(defn switch-zkclient
  "Switch the zookeeper client to a new client host
  Arguments:

  "
  [host]
  (utils/log-action "Switching zookeeper client from '" @zk-url "' to '" host)
  (stop-zkclient @zk-url)
  (start-zkclient host))

(defn get-zookeeper-config
  ""
  [app-name & subnodes]
  (-> (str "/" constants/library-namespace "/config/" app-name (clojure.string/join subnodes))
    (get-node-data)))


(defn get-config-file-path
  "
  Arguments:
    location - Possible values: :source - get the config files in the local code repositories
                                :central - get the config files in the central config location
                                :orca-app - get the config files in the orca application configuration directories
                                :orca-zookeeper - get the config files in the orca zookeeper configuration directories
                                :zookeeper - Get the configuration
               Defaults to the central config location.
    app-name - name of the app/module/repo to find config files for.
    details - Optional, the entry from 'utils.core/configs-to-check for the specified app."
  ([location app-name]
   (get-config-file-path location app-name (get configs-to-check app-name)))
  ([location app-name details]
   (cond
     (= location :source)
     (str constants/workspace-root "/src/" app-name "/" (:source-config-path details))

     (= location :central)
     (str config-path-root "/" app-name "/" (if (nil? details) "config.edn" (:file-name details)))

     (= location :orca-app)
     (str constants/workspace-root "/src/orca-env/current-env/target/hosts/tools01/application/" app-name "/" (:file-name details))

     (= location :orca-zookeeper)
     (str constants/workspace-root "/src/orca-env/current-env/target/zookeeper/" app-name "/config.zk.edn")

     (= location :zookeeper)
     nil)))


(defn get-config
  "
  Arguments:
    location - Possible values: :source - get the config files in the local code repositories
                                :central - get the config files in the central config location
                                :orca-app - get the config files in the orca app configuration directories
                                :orca-zookeeper - get the config files in the orca zookeeper configuration directories
                                :zookeeper - Get the configuration
               Defaults to the central config location.
    app-name - name of the app/module/repo to find config files for.
    details - Optional, the entry from 'utils.core/configs-to-check for the specified app."

  ([location app-name]
   (get-config location app-name (get configs-to-check app-name)))
  ([location app-name details]
   (cond
     (contains? #{:source :central :orca-app :orca-zookeeper} location)
     (-> (get-config-file-path location app-name details)
         (load-file))

     (= location :zookeeper)
     (get-zookeeper-config app-name)

     :else
     nil)))


(defn get-config-file-paths
  "Returns a list containing all valid paths to config files for the desired app/module/repo.

  Arguments:
    app-name - name of the app/module/repo to find config files for.
    details - Optional, the entry from 'utils.core/configs-to-check for the specified app."
  ([app-name]
   (get-config-file-paths app-name (get configs-to-check app-name)))
  ([app-name details]
   (->> [:source :central :orca]
        (map get-config-file-path)
        (filter #(.exists (io/file %))))))


(defn save-zookeeper-config-to-orca
  ""
  [app-name]
  (let [pth (str constants/src-root-dir "/orca-env/current-env/target/zookeeper/" app-name "/config.zk.edn")
        dest-file (clojure.java.io/file pth)]
    (if (.exists dest-file)
      (clojure.java.io/copy dest-file (clojure.java.io/file (str pth "." (.format (java.text.SimpleDateFormat. "yyyyMMdd_HHmmss") (new java.util.Date)))))
      (clojure.java.io/make-parents pth))
    (save-zookeeper-config-to-file :root-node (str "/" constants/library-namespace "/config/" app-name)
                                   :full-path pth)))


(defn check-config-file
  "
  Arguments:
    app-name - name of the app/module/repo to find config files for.
    path -
    details - Optional, the entry from 'utils.core/configs-to-check for the specified app."
  [app-name path details]
  (println "Checking app" app-name "config in" path "\n")
  (if (.exists (io/file path))
    (try+
      (let [cfgs (load-file path)]
        (doseq [ky-to-check (:keys details)]
          (if (= "nested" (first ky-to-check))
            (println ky-to-check " == " (find-nested cfgs (second ky-to-check)))
            (println ky-to-check " == " (get-in cfgs ky-to-check "<not-present>")))))
      (catch Object _
        (println "***** Exception trying to load file" path)
        (println "*****" (:message &throw-context))))
    (println "***** Cannot find the expected file" path)))


(defn get-modules-to-check-config
  "
  Arguments:
    location - if this is :source then config files in the local repositories are modified,
               else the config files in the central config location are modified.
               Defaults to the central config location.
    cnfgs-names-to-check - Optional - list of 0 or more repository names to modify the configs for.
                           Defaults to the keys of 'utils.core/configs-to-check."
  [location & cnfgs-names-to-check]
  (let [cnfgs-submitted (set (utils/check-optional-arguments-for-array cnfgs-names-to-check))]
    (if (= :source  location)
      ;; Checking the source root directory for apps/modules to check
      ;; Get the list of apps/git repos in the source root and have
      ;; entries in the map of configuration setting to check
      (let [apps-in-source-root (intersection (set (repositories/get-repos-in-src-root))
                                              (set (filter #(not(empty? (get-in configs-to-check [% :source-config-path])))
                                                           (keys configs-to-check))))]
        (if (empty? cnfgs-submitted)
            apps-in-source-root
            (intersection apps-in-source-root (set cnfgs-submitted))))

      (if (= :central location)
        ;; Checking the central config directory for apps/modules to check
        ;; Get the list of apps with config directories in the central config directory and have
        ;; entries in the map of configuration setting to check
        (let [apps-in-config-path-root (intersection (set (keys configs-to-check))
                                                     (set (map #(.getName %)
                                                               (filter #(and (.isDirectory (clojure.java.io/file %))
                                                                             (.contains (keys configs-to-check) (.getName %)))
                                                                       (.listFiles (clojure.java.io/file config-path-root))))))]
          (if (empty? cnfgs-submitted)
            apps-in-config-path-root
            (intersection cnfgs-submitted apps-in-config-path-root)))))))


(defn check-config-settings
  "Displays the current settings for the apps/modules in the
  (utils.constants/central-config-root-dir) directory or in the source repositories.

  Arguments:
  location - if this is :source then config files in the local repositories are modified,
             else the config files in the central config location are modified.
  cnfgs-names-to-check - Optional - list of 0 or more repository names to modify the configs for.
                         Defaults to the keys of 'utils.core/configs-to-check."
  ([]
   (apply (partial check-config-settings :all) (keys configs-to-check)))
  ([location-arg & cnfgs-names-to-check]
   (let [locations (if (= location-arg :all) [:source :central] [location-arg])]
     (doseq [location locations]
       (if (= location :source)
         (println "\nConfig settings in source repositories - "
                  (time-format/unparse (time-format/formatters :date-hour-minute-second) (time-core/now)) "\n")
         (println "\nConfig settings in" config-path-root "- "
                  (time-format/unparse (time-format/formatters :date-hour-minute-second) (time-core/now)) "\n\n"))
       (let [apps-details (->> (get-modules-to-check-config location cnfgs-names-to-check)
                                       (select-keys configs-to-check)
                                       (into (sorted-map)))]
         (doseq [[app-name details] apps-details]
           (let [file-path (get-config-file-path location app-name details)]
              (check-config-file app-name file-path details)
              (println))))))))


(defn save-config-to-file
  [app-name location config]
  (spit (get-config-file-path location app-name) config))


(defn replace-config-setting
  "
  Arguments:
  locations - if this is :source then config files in the local repositories are modified,
             else the config files in the central config location are modified.
  old-value - string to be replaced
  new-value - replacement string
  cnfgs-names-to-check - Optional - list of 0 or more repository names to modify the configs for.
                         Defaults to the keys of 'utils.core/configs-to-check."
  ([old-value new-value]
   (replace-config-setting :central old-value new-value))
  ([location-arg old-value new-value & cnfgs-names-to-check]
   (let [locations (if (= location-arg :all) [:source :central] [location-arg])]
     (doseq [location locations]
       (doseq [[app-name details] (into (sorted-map) (select-keys configs-to-check (get-modules-to-check-config location cnfgs-names-to-check)))]
         (let [file-path (get-config-file-path location app-name details)]
           (if (.exists (io/file file-path))
             (let [file-contents (slurp file-path)
                   new-contents (.replace file-contents old-value new-value)]
               (if (not= file-contents new-contents)
                 (do
                   (println "Replacing config setting '" old-value"' with '" new-value "' in file " file-path)
                   (spit file-path new-contents)
                   (let [file-contents-list (clojure.string/split-lines file-contents)
                         new-contents-list (clojure.string/split-lines new-contents)
                         diffs (data/diff file-contents-list new-contents-list)]
                     (println "Changed lines:")
                     (clojure.pprint/pprint diffs)
                     (for [x (range (count (first diffs)))]
                       (if (not (nil? (nth (first diffs) x))) (println "Line" x ":" (nth (first diffs) x) "          became     " (nth (second diffs) x)))))
                   (utils/log-action "replaced config setting '" old-value"' with '" new-value "' in file " file-path))
                 (println "No changes in file " file-path))))))))))


(defn set-config-setting
  "
  Arguments:
    repo-name - name of the app/module/repo to set the config value for.
    keys-seq - list of keys to set the value in the config file.
    new-value - replacement string"
  [repo-name keys-seq new-value]
  (let [file-paths (get-config-file-paths repo-name)]
    (utils/log-action "setting config setting " keys-seq " with \"" new-value "\" for repo " repo-name " in file(s) " file-paths)
    (map #(-> %
              (load-file)
              (assoc-in keys-seq new-value)
              (clojure.pprint/pprint (clojure.java.io/writer %))
         file-paths))))


(defn set-zookeeper-config-from-file
  "
  Arguments:
    from-location - Possible values: :source - Get the config files in the local code repositories
                                     :central - Get the config files in the central config location
                                     :orca - Get the config files in the orca configuration directories
                                     :zookeeper - Get the configuration from zookeeper, but would not have any changes.
                                     :custom - Get the config file from the path specified in the :custom-path argument.
                    Defaults to the central config location.
    app-name - name of the app/module/repo to find config files for.
    :custom-path - Optional - path to the custom config edn text file to load into zookeeper."
  [from-location app-name & {:keys [custom-path]}]
  (->> (if (= from-location :custom)
         (load-file custom-path)
         (get-config from-location app-name))
       (set-node-data! (str "/" constants/library-namespace "/config/" app-name)))
  (utils/log-action "setting zookeeper config setting " (str "/" constants/library-namespace "/config/" app-name)
                    " with contents of file " from-location " " custom-path))


(defn compare-configs
  ([app-name location1 location2]
   (let [config-1 (get-config location1 app-name)
         config-2 (get-config location2 app-name)]
     (compare-configs config-1 config-2 location1 location2)))
  ([config1 config2 desc1 desc2]
   (let [diffs (data/diff config1 config2)
         descript1 (if (nil? desc1) "first" desc1)
         descript2 (if (nil? desc1) "second" desc2)]
     (binding [*print-right-margin* 140
              *print-miser-width* 120]
       (println "Portions of configs that are the same:")
       (clojure.pprint/pprint (into (sorted-map) (last diffs)))
       (println "\nPortions of" descript1 "config that is unique:")
       (clojure.pprint/pprint (into (sorted-map) (first diffs)))
       (println "\nPortions of" descript2 "config that is unique:")
       (clojure.pprint/pprint (into (sorted-map) (second diffs)))))))

(defn copy-config-from-to
  ""
  [app-name from-location to-location & {:keys [custom-path]}]
  (let [config-to-save (if (= from-location :custom)
                        (load-file custom-path)
                        (get-config from-location app-name))]
    (if (= to-location :zookeeper)
      (set-node-data! (str "/" constants/library-namespace "/config/" app-name) config-to-save)
      (spit (get-config-file-path to-location app-name) config-to-save)))
  (utils/log-action "copying app config for " app-name
                    " from " from-location " to " to-location " " custom-path))

