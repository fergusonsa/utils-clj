(ns utils.local-environment
  (:require [clojure.java.io :as io]
            [utils.environments :as environments]
            [utils.repositories :as repositories]
            [utils.config :as config]
            [utils.fake :as creds])
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
  ([]
   (check-config-settings (keys configs-to-check)))
  ([& cnfgs-names-to-check]
    (doseq [[app-name details] (select-keys configs-to-check (if (seq? (first cnfgs-names-to-check)) (first cnfgs-names-to-check) cnfgs-names-to-check))]
      (check-config-file app-name (str config-path-root "/" app-name "/" (:file-name details)) details)
      (println))))


(defn check-source-config-settings
  ([& cnfgs-names-to-check]
    (let [repos-to-check (if (> (count cnfgs-names-to-check) 0)
                           (intersection (set cnfgs-names-to-check) (set (repositories/get-repos-in-src-root)) (set (keys configs-to-check)))
                           (intersection (set (repositories/get-repos-in-src-root)) (set (keys configs-to-check))))]
      (doseq [[app-name details] (select-keys configs-to-check repos-to-check)]
        (if (and (not (nil? (:source-config-path details))) (not= "" (:source-config-path details)))
          (do
            (check-config-file app-name (str config/workspace-root "/src/" app-name "/" (:source-config-path details)) details)
            (println)))))))
