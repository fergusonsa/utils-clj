(ns utils.configuration
  (:require [clojure.java.io :as io]
            [utils.environments :as environments]
            [utils.repositories :as repositories]
            [utils.constants :as constants]
            [clj-time.core :as time-core]
            [clj-time.format :as time-format]
            [utils.fake :as creds]
            [utils.core :as utils]
            [clojure.data :as data])
  (:use [clojure.pprint]
        [clojure.set :only [difference intersection]]
        [slingshot.slingshot :only [try+]]))

(def config-path-root "/opt/cenx/application")
(def configs-to-check {
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
                                   :source-config-path ""
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

(defn find-nested [m k]
  (->> (tree-seq map? vals m)
       (filter map?)
       (some k)))

(defn check-config-file [app-name path details]
  (println "Checking app" app-name "config in" path)
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
  [location cnfgs-names-to-check]
  (let [cnfgs-submitted (set (if (seq? (first cnfgs-names-to-check))
                               (first cnfgs-names-to-check)
                               cnfgs-names-to-check))]
    (if (= location "source")
      ;; Checking the source root directory for apps/modules to check
      ;; Get the list of apps/git repos in the source root and have
      ;; entries in the map of configuration setting to check
      (let [apps-in-source-root (intersection (set (repositories/get-repos-in-src-root))
                                              (set (filter #(not(empty? (get-in configs-to-check [% :source-config-path])))
                                                           (keys configs-to-check))))]
        (if (empty? cnfgs-submitted)
          apps-in-source-root
          (intersection apps-in-source-root (set cnfgs-submitted))))
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
          (intersection cnfgs-submitted apps-in-config-path-root))))))

(defn get-config-file-path
  [location app-name details]
  (if (= location "source")
    (str constants/workspace-root "/src/" app-name "/" (:source-config-path details))
    (str config-path-root "/" app-name "/" (:file-name details))))


(defn check-config-settings
  "Displays the current settings for the apps/modules in the /opt/cenx/application directory or in the source repositories.

  Arguments:
  location - optional - if this is \"source\" then config files in the local repositories are modified,
             else the config files in the central config location are modified. Defaults to the central config location.
  cnfgs-names-to-check - optional list of 0 or more repository names to modify the configs for."
  ([]
   (check-config-settings "central" (keys configs-to-check)))
  ([location & cnfgs-names-to-check]
   (if (= location "source")
     (println "\nConfig settings in source repositories - " (time-format/unparse (time-format/formatters :date-hour-minute-second) (time-core/now)) "\n")
     (println "\nConfig settings in" config-path-root "- " (time-format/unparse (time-format/formatters :date-hour-minute-second) (time-core/now)) "\n"))

   (doseq [[app-name details] (into (sorted-map) (select-keys configs-to-check (get-modules-to-check-config location cnfgs-names-to-check)))]
     (let [file-path (get-config-file-path location app-name details)]
        (check-config-file app-name file-path details)
        (println)))))


(defn replace-config-setting
  "
  arguments:
  location - if this is \"source\" then config files in the local repositories are modified,
             else the config files in the central config location are modified.
  old-value - string to be replaced
  new-value - replacement string
  cnfgs-names-to-check - optional list of 0 or more repository names to modify the configs for."
  ([old-value new-value]
   (replace-config-setting "central" old-value new-value))
  ([location old-value new-value & cnfgs-names-to-check]
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
                 (for [x (range (count (first diffs)))]
                   (if (not (nil? (nth (first diffs) x))) (println "Line" x ":" (nth (first diffs) x) "          became     " (nth (second diffs) x)))))
               (utils/log-action "replaced config setting '" old-value"' with '" new-value "' in file " file-path))
             (println "No changes in file " file-path))))))))