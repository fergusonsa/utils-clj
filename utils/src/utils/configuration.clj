(ns utils.configuration
  (:require [clojure.java.io :as io]
            [utils.environments :as environments]
            [utils.repositories :as repositories]
            [utils.constants :as constants]
            [clj-time.core :as time-core]
            [clj-time.format :as time-format]
            [utils.fake :as creds]
            [utils.core :as utils])
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
   (doseq [[app-name details] (select-keys configs-to-check (if (seq? (first cnfgs-names-to-check)) (first cnfgs-names-to-check) cnfgs-names-to-check))]
     (let [file-path (if (= location "source")
                       (str constants/workspace-root "/src/" app-name "/" (:source-config-path details))
                       (str config-path-root "/" app-name "/" (:file-name details)))]
        (check-config-file app-name file-path details)
        (println)))))


(defn check-source-config-settings
  "Displays the current settings for the apps/modules in the source repositories."
  ([& cnfgs-names-to-check]
    (let [repos-to-check (if (> (count cnfgs-names-to-check) 0)
                           (intersection (set cnfgs-names-to-check) (set (repositories/get-repos-in-src-root)) (set (keys configs-to-check)))
                           (intersection (set (repositories/get-repos-in-src-root)) (set (keys configs-to-check))))]
      (println "\nConfig settings in source repositories - " (time-format/unparse (time-format/formatters :date-hour-minute-second) (time-core/now)) "\n")
      (doseq [[app-name details] (select-keys configs-to-check repos-to-check)]
        (if (and (not (nil? (:source-config-path details))) (not= "" (:source-config-path details)))
          (do
            (check-config-file app-name (str constants/workspace-root "/src/" app-name "/" (:source-config-path details)) details)
            (println)))))))

(defn replace-source-config-setting
  ""
  [old-value new-value & cnfgs-names-to-check]
    (let [repos-to-check (if (> (count cnfgs-names-to-check) 0)
                           (intersection (set cnfgs-names-to-check) (set (repositories/get-repos-in-src-root)) (set (keys configs-to-check)))
                           (intersection (set (repositories/get-repos-in-src-root)) (set (keys configs-to-check))))]
      (doseq [[app-name details] (select-keys configs-to-check repos-to-check)]
        (let [file-path (str constants/workspace-root "/src/" app-name "/" (:source-config-path details))]
        (if (and (not (nil? (:source-config-path details)))
                 (not= "" (:source-config-path details))
                 (.exists (io/file file-path)))
          (let [file-contents (slurp file-path)
                new-contents (.replace file-contents old-value new-value)]
;;             (println "***************" file-path)
;;             (println file-contents)
;;             (println "***************")
;;             (println file-contents)
;;             (println "***************")
            (if (not= file-contents new-contents)
              (do
                (spit file-path new-contents)
                (println "replaced config setting '" old-value"' with '" new-value "' in file " file-path)
                (utils/log-action "replaced config setting '" old-value"' with '" new-value "' in file " file-path))
              (println "No changes in file " file-path)))
          (println file-path "does not exist!"))))))


(defn replace-config-setting
  "
  arguments:
  location - if this is \"source\" then config files in the local repositories are modified,
             else the config files in the central config location are modified.
  old-value - string to be replaced
  new-value - replacement string
  cnfgs-names-to-check - optional list of 0 or more repository names to modify the configs for."
  [location old-value new-value & cnfgs-names-to-check]
    (let [repos-to-check (if (> (count cnfgs-names-to-check) 0)
                           (intersection (set cnfgs-names-to-check) (set (repositories/get-repos-in-src-root)) (set (keys configs-to-check)))
                           (intersection (set (repositories/get-repos-in-src-root)) (set (keys configs-to-check))))]
      (doseq [[app-name details] (select-keys configs-to-check repos-to-check)]
        (let [file-path (if (= location "source")
                          (str constants/workspace-root "/src/" app-name "/" (:source-config-path details))
                          (str config-path-root "/" app-name "/" (:file-name details)))]
        (if (and (not (nil? (:source-config-path details)))
                 (not= "" (:source-config-path details))
                 (.exists (io/file file-path)))
          (let [file-contents (slurp file-path)
                new-contents (.replace file-contents old-value new-value)]
            (if (not= file-contents new-contents)
              (do
                (spit file-path new-contents)
                (println (str "replaced config setting '" old-value"' with '" new-value "' in file " file-path))
                (utils/log-action "replaced config setting '" old-value"' with '" new-value "' in file " file-path))
              (println "No changes in file " file-path))))))))
