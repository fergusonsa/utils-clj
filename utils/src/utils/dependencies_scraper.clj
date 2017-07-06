(ns utils.dependencies-scraper
  (:require [clj-http.client :as client]
            [cheshire.core :as json]
            [clj-time.format :as time-format]
            [clojure.tools.reader.edn :as edn]
            [clojure.string]
            [utils.core :as utils]
            [utils.identity]
            [utils.config :as config])
  (:use clojure.pprint
        clojure.core
        [clojure.set :only [difference intersection]]
        [slingshot.slingshot :only [try+]]))

(def cenx-repo-root (str "https://api.bitbucket.org/2.0/repositories/" config/bitbucket-root-user "/"))

(defonce connection-info (atom {:url (str "https://api.bitbucket.org/2.0/repositories/" config/bitbucket-root-user "/")}))

(def library_info (atom {}))

(defn retrieve_library_info
  [lib_name]
  (let [lib_info (client/get (str "https://clojars.org/api/artifacts/" lib_name)
                                                                 {:as :json})]
    (swap! library_info assoc lib_name (:body lib_info))
    (:body lib_info)))

;;  repository-dependency-info should be storing stuff in this format:
;; {"library1-name": {"version1": {:dependencies {"library2-name": {:name "library2-name" :version "versionA"},
;;                                                "library3-name": {:name "library3-name" :version "versionB"},...}},
;;                    "version2": {:dependencies {"library2-name": {:name "library2-name" :version "versionX"},
;;                                                "library4-name": {:name "library4-name" :version "versionC},...}},...}}
(defonce repository-dependency-info (atom {}))

(defonce repository_info (atom {:dependencies (sorted-map) :libraries {} }))

(defn get_library_info [lib_name]
  (get @library_info lib_name (retrieve_library_info lib_name)))


(defn modify-project-clj-src [project_cli_src repo-name]
  (try+

    ;; Change the project.clj source from a (defproject... macro call to just a map
    (-> (str "{\n " (subs project_cli_src (clojure.string/index-of project_cli_src "\n") (- (count (clojure.string/trimr project_cli_src)) 1)) "\n}")
      ;; Remove the :cljfmt key and value as the value may have a regex pattern that may not be easily escaped!
      (clojure.string/replace  #":cljfmt\s+\{([^{}]*|\{([^{}]*|\{[^{}]*\})*\})*}" "")
      ;; Remove the :package key and value as the value may have a pattern that may not be easily escaped!
      (clojure.string/replace  #":package\s+\{([^{}]*|\{([^{}]*|\{[^{}]*\})*\})*}" "")
      ;; Remove the :minify-assets key and value as it is not required and may cause load-string to fail.
      (clojure.string/replace  #":minify-assets\s+\{([^{}]*|\{([^{}]*|\{[^{}]*\})*\})*}" "")
      ;; Remove the :description key and value as it may have \n which could mess up how later regex happen.
      (clojure.string/replace #":description\s+\"[^\"]*\"" "")
      ;; Remove any :injections key and value as they are not required and may cause load-string to fail.
      (clojure.string/replace #":injections\s+\[[^\[\]]*\]" "")
      ;; Remove the :repositories key and value as they are not required and may cause load-string to fail.
      (clojure.string/replace #":repositories\s+(\^:replace\s+)?\[([^\[\]]*|\[([^\[\]]*|\[[^\[\]]*\])*\])*\]" "")
      ;; The :uberjar-exclusions attribute  can have a regex pattern and since we do not need it, just remove it
      (clojure.string/replace #":uberjar-exclusions\s+\[([^\[\]]*|\[([^\[\]]*|\[[^\[\]]*\])*\])*\]" "")

      ;; TODO Need to handle multiple names inside [...] as in
      ;; :exclusions ["cenx/stentor" potemkin cenx/hades "org.clojure/clojure"]
      (clojure.string/replace #"\[[^\[\]]*\]" (fn [v] (let [parts (clojure.string/split v #" +")] (if (> (count parts) 2) (apply str (interpose "\n" parts)) v))))

      ;; Change any namespace or class names to strings since they will not be present in the repl
      (clojure.string/replace #"(\[)([a-zA-Z][a-zA-Z0-9.\/\-_]*)" "$1\"$2\"")
      (clojure.string/replace #" ([a-zA-Z][a-zA-Z0-9.\/\-_]*)(])" " \"$1\"$2")
      (clojure.string/replace #"(\n[\t ]*|:[a-zA-Z-]+ )([a-zA-Z][a-zA-Z0-9.\/\-_]*)" "$1\"$2\"")
      )

    (catch Object _
       (println "<><><><><><><><><><><><><><><><><><><><> ")
       (println "Exception trying to modify the project.clj file for repo " repo-name)
       (println "<><><><><><><><><><><><><><><><><><><><> ")
       (println (:message &throw-context))
       (println "<><  ><><><><><><><><><><><><><><><><><><> ")
       (println (:cause &throw-context))
       (println "<><><><><><><><><><><><><><><><><><><><> ")
       (println project_cli_src)
       (println "<><><><><><><><><><><><><><><><><><><><>")
       project_cli_src)))


(defn get-original-project-clj [repo-name branch]
  ; Get the source for the project.clj file
  ; curl --user scott.ferguson@cenx.com:<pwd> https://api.bitbucket.org/1.0/repositories/cenx-cf/ares/raw/integration/project.clj
  (clojure.string/trim (:body (client/get (str "https://api.bitbucket.org/1.0/repositories/" config/bitbucket-root-user "/" repo-name "/raw/" branch "/project.clj")
                                          {:basic-auth [(:user utils.identity/identity-info) (:password utils.identity/identity-info)] }))))

(defn get-project-dependencies [conn-info repo-name branch slug]
  (try+
    (let [project_cli_src (get-original-project-clj slug branch)
          modified_src (modify-project-clj-src project_cli_src repo-name)]
      (try+
        (if-let [project_clj (load-string modified_src)]
            (dissoc (apply merge-with (comp vec flatten vector) (map (partial apply hash-map) (:dependencies project_clj))) :exclusions :excludes :only)
          :error-loading-project-data)
        (catch Object _
          (println "<><><><><><><><><><><><><><><><><><><><> ")
          (println "Exception trying to load project.clj file for repo " repo-name)
          (println "<><><><><><><><><><><><><><><><><><><><> ")
          (println (:message &throw-context))
          (let [mtches (re-matches #".*\((.*):([0-9]*):([0-9]*)\)" (:message &throw-context))]
            (if (not (nil? mtches))
              (let [line-num  (Integer. (last (butlast mtches)))]
                (println "look at line " line-num ":")
                (println (nth (line-seq (java.io.BufferedReader. (java.io.StringReader. modified_src)))) line-num))))
          (if (.startsWith (:message &throw-context) "java.lang.RuntimeException: Unable to resolve symbol: ")
            (let [symbol-str (first (clojure.string/split (subs (:message &throw-context) 54 (clojure.string/index-of (:message &throw-context) " " 54)) #" "))
                  lines (re-seq (re-pattern (str ".*" symbol-str ".*\n")) (println-str modified_src))]
              (println "Possible issue lines:")
              (dorun (map #(println %) lines))))
          (println "<><><><><><><><><><><><><><><><><><><><> ")
          (println (:cause &throw-context))
          (println "<><><><><><><><><><><><><><><><><><><><> ")
          (println modified_src)
          (println "<><><><><><><><><><><><><><><><><><><><>")
          :exception-loading-project-data)))
      (catch [:status 404] {}
        (println "There is not a project.clj file for branch" branch " of repo" repo-name )
        :no-project-clj-file)
      (catch Object _
        (println "++++++++++++++++++++++++++++++++++++++++++ ")
        (println "Exception trying to get project.clj file for repo " repo-name (str "https://api.bitbucket.org/1.0/repositories/" config/bitbucket-root-user "/" slug "/raw/" branch "/project.clj"))
        (println "++++++++++++++++++++++++++++++++++++++++++ ")
        (println (:message &throw-context))
        (println "++++++++++++++++++++++++++++++++++++++++++ ")
        (println (:cause &throw-context))
        (println "++++++++++++++++++++++++++++++++++++++++++ ")
        :exception-getting-project-clj-file)))


(defn xxx [repo_info]
;;  (println (:name repo_info) "--" (:updated_on repo_info))
  { (:name repo_info) (if (> (compare (time-format/parse (time-format/formatter "yyyy-MM-dd'T'HH:mm:ss.SSSSSSZ")
                                     (:updated_on repo_info))
                  (time-format/parse (time-format/formatter "yyyy-MM-dd") "2016-01-01")) 0)
                      (get-project-dependencies @connection-info
                                     (:name repo_info)
                                     (get-in repo_info [:mainbranch :name])
                                     (:slug repo_info))
                        :skipped)})


(defn parse-repo-list-page []
  ; $ curl --user scott.ferguson@cenx.com:<pwd> https://api.bitbucket.org/2.0/repositories/cenx-cf/
   (println "loop for  ----------------- " (:url @connection-info) "====")
   (try+
     (let [results (client/get (:url @connection-info)
                              {:content-type :json
                               :basic-auth [(:user utils.identity/identity-info) (:password utils.identity/identity-info)]
                               :throw-exceptions false
                               :as :json})
          before_date (time-format/parse (time-format/formatter "yyyy-MM-dd") "2016-01-01")
          repo_list_values (:values (:body results))]
      (swap! connection-info assoc :url (get-in results [:body :next]))
;;    (swap! connection-info assoc :url nil)
    ;(println "Page " (get-in results [:body :page]) "Next " (get-in results [:body :next]))
;;    (pprint repo_list_values)
      (let [final-results (into {} (map xxx repo_list_values))]
        (println "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$")
        (println "Page results type: " (type final-results))
        (pprint final-results)
        (println "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$")
        final-results))
    (catch Object _
      ;; An error occurred trying to get and parse the current repo list page.
      ;; Report, set the next page to nil, and return an empty map.
      (println "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
      (println "An error occurred trying to get and parse the current repo list page.")
      (println "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
      (println (:message &throw-context))
      (println "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
      (println (:cause &throw-context))
      (println "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
      (swap! connection-info assoc :url nil)
      {})))


(defn -main
  ([]
    (println "Starting ----------------- " (:url @connection-info) "====")
    (while (not (nil? (:url @connection-info)))
      (let [page-results (parse-repo-list-page)]
        (swap! repository_info assoc :dependencies (into (:dependencies @repository_info) page-results)))
      (println "##########after page " (:url @connection-info) "#########")
      (pprint @repository_info)
      (println "###################"))
    (println "Finished getting dependencies----------------- ")
    (pprint @repository_info)
    (println "Finished ----------------- ")
    (let [log-file (utils/get-report-file-name-path  "bitbucket-dependency-scraper-logs")]
      (spit log-file (pr-str @repository_info))
      (println "Wrote to file " log-file " ----------------- ")))

  ([page]
    (if (integer? page)
      (swap! connection-info assoc :url (str "https://api.bitbucket.org/2.0/repositories/" config/bitbucket-root-user "/?page=" page)))
    (-main)))


(defn display-project-dependencies [& repos]
  (for [repo-name repos]
    (do
      (println "Getting dependency info for repo " repo-name)
      (let [deps (get-project-dependencies @connection-info repo-name config/default-branch repo-name)]
        (pprint (into (sorted-map) deps))
        (println ""))))
  (utils/log-action "(display-project-dependencies \"" (apply str (interpose "," repos)) "\")"))


(defn display-project-dependencies-versions
  [& args]
  (doseq [[repo-name version] (apply hash-map args)]
     (let [tag (utils/get-tag repo-name version)
           deps (get-project-dependencies @connection-info repo-name tag repo-name)]
       (println "Getting dependency info for repo " repo-name " for tag version " tag)
       (pprint (into (sorted-map) deps))
       (println ""))
        ))


(defn display-project-dependencies-diffs [repo-name v1 v2]
  (let [tag1 (utils/get-tag repo-name v1)
        deps1 (get-project-dependencies @connection-info repo-name tag1 repo-name)
        tag2 (utils/get-tag repo-name v2)
        deps2 (get-project-dependencies @connection-info repo-name tag2 repo-name)
        k1 (set (keys deps1))
        k2 (set (keys deps2))
        missing-in-1 (difference k2 k1)
        missing-in-2 (difference k1 k2)
        present-but-different (filter #(not= (deps1 %) (deps2 %))
                                      (intersection k1 k2))
        map-diff (map #(list % (deps1 %) (deps2 %)) present-but-different)]
    (println (cl-format nil (str "~{Dependency ~a is missing in " tag1 "~%~}~{Dependency ~a is missing in "
                             tag2 "~%~}~{~{Dependency ~a is different~%  "
                             tag1 " -> ~a~%  " tag2 " -> ~a~%~}~}")
                        missing-in-1
                        missing-in-2
                        map-diff))
    (utils/log-action "(display-project-dependencies-diffs \"" repo-name "\" \"" v1 "\" \"" v2"\")")))


(declare build-dependency-node)

(defn build-cenx-dependency-node [repo-name version]
  (let [tag (utils/get-tag repo-name version)
        deps (get-project-dependencies @connection-info repo-name tag repo-name)
        deps2 (if (keyword? deps) {} deps)

        node {:name repo-name :version version :dependencies (reduce (fn [m [k v]] (assoc m k (build-dependency-node k v))) {} deps2)}]
    (swap! repository-dependency-info assoc-in [repo-name version] node)
    node))


(defn build-external-dependency-node [module-name version]
  (let [node {:name module-name :version version}]
    (swap! repository-dependency-info assoc-in [module-name version] node)
    node))


(defn build-dependency-node [module-name version]
  (println "building node for \"" module-name "\" version \"" version "\"")
  (let [sub-name (if (.startsWith module-name (str config/library-namespace "/")) (subs module-name 5) module-name)]
    (if-let [already-saved-node (get-in @repository-dependency-info [sub-name version])]
      already-saved-node
      (if (.startsWith module-name (str config/library-namespace "/"))
        (build-cenx-dependency-node sub-name version)
        (build-external-dependency-node module-name version)))))


(defn node-has-dependencies? [tree]
  (and (contains? tree :dependencies) (> (count (:dependencies tree)) 0)))


(defn make-tree-readable
  ([tree]
    (make-tree-readable tree 0))
  ([tree indent]
    (let [indent-padding (str "\n" (apply str (repeat indent " ")))]
      (if (node-has-dependencies? tree)
        (str
          (if (= indent 0) "{" "")
          (format "%s%s {%s    :version %s," indent-padding (:name tree) indent-padding (:version tree))
          (format "%s    :dependencies {" indent-padding )
          (apply str (interpose "," (map #(make-tree-readable (second %) (+ indent 8)) (:dependencies tree))))
          "}}"
          (if (= indent 0) "}" ""))
        (format "%s%s { :version %s }" indent-padding (:name tree) (:version tree))))))


(defn check-version [repo-name version]
  (if (or (= version config/default-branch) (.startsWith version repo-name))
    version
    (str repo-name "-" version)))


(defn find-module-dependencies
  ([module-name]
    (find-module-dependencies module-name config/default-branch))
  ([module-name version]
    (let [proper-version (check-version module-name version)]
      (build-cenx-dependency-node module-name proper-version))))


(defn display-project-dependency-tree
  ([repo-name]
    (display-project-dependency-tree repo-name config/default-branch))
  ([repo-name version]
    (let [res (find-module-dependencies repo-name version)
          log-file (get-report-file-name-path (str repo-name "-" version "-dependency-tree") :subdirectory "dependency-trees")
          out-str (make-tree-readable res)]
      (println out-str)
      (spit log-file out-str)
      (println "Wrote dependency tree for " repo-name version "to" log-file)
      (utils/log-action "Wrote dependency tree for " repo-name version "to" log-file))))


(defn create-dependency-tree-reports [& args]
  (doseq [[repo-name version] (if (map? args) args (apply hash-map args))]
    (let [log-file (get-report-file-name-path (str repo-name "-" version "-dependency-tree") :subdirectory "dependency-trees")]
      (->> (find-module-dependencies repo-name version)
            (make-tree-readable)
            (spit log-file))
      (println "Wrote dependency tree for " repo-name version "to" log-file)
      (utils/log-action "Wrote dependency tree for " repo-name version "to" log-file))))
