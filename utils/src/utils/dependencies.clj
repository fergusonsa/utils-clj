(ns utils.dependencies
  ""
  (:refer-clojure :exclude [help])
  (:require [clj-http.client :as client]
            [cheshire.core :as json]
            [clj-time.format :as time-format]
            [clojure.tools.reader.edn :as edn]
            [clojure.string]
            [utils.core :as utils]
            [utils.identity]
            [utils.constants :as constants]
            [version-clj.core :as version])
  (:use clojure.pprint
        clojure.core
        [clojure.set :only [difference intersection]]
        [slingshot.slingshot :only [try+]])
  (:import [java.nio.file Files CopyOption]))

(declare build-dependency-node)


(def build-repo-dependency-node-repo-root (str "https://api.bitbucket.org/2.0/repositories/" constants/bitbucket-root-user "/"))


(defonce connection-info (atom {:url (str "https://api.bitbucket.org/2.0/repositories/" constants/bitbucket-root-user "/")}))


(defonce repository-info-file-path (str constants/user-root-path "/.clojure-utils/repository-info.clj"))

(defonce module-dependency-info-file-path (str constants/user-root-path "/.clojure-utils/module-dependency-info.clj"))

(defonce library-info-file-path (str constants/user-root-path "/.clojure-utils/library-info.clj"))


(defonce library-info (atom (utils/load-datastructure-from-file library-info-file-path)))


(defonce repository-info (atom (utils/load-datastructure-from-file repository-info-file-path)))


(defn save-library-info
  []
  (utils/save-datastructure-to-file @library-info library-info-file-path))

(defn save-repository-info
  []
  (utils/save-datastructure-to-file @repository-info repository-info-file-path))


(defn retrieve-library-info
  [lib-name]
  (let [lib-info (client/get (str "https://clojars.org/api/artifacts/" lib-name)
                                                                 {:as :json})]
    (swap! library-info assoc lib-name (:body lib-info))
    (:body lib-info)))




;;  module-dependency-info should be storing stuff in this format:
;; {"library1-name": {"version1": {:dependencies {"library2-name": {:name "library2-name" :version "versionA"},
;;                                                "library3-name": {:name "library3-name" :version "versionB"},...}},
;;                    "version2": {:dependencies {"library2-name": {:name "library2-name" :version "versionX"},
;;                                                "library4-name": {:name "library4-name" :version "versionC},...}},...}}
(defonce module-dependency-info (atom (utils/load-datastructure-from-file module-dependency-info-file-path)))


(defn get-library-info [lib-name]
  (get @library-info lib-name (retrieve-library-info lib-name)))


(defn save-module-dependency-info
  []
  (utils/save-datastructure-to-file @module-dependency-info module-dependency-info-file-path))


(defn get-module-dependency-info
  [module-name version]
  (if-not (get-in @module-dependency-info [module-name version])
    (build-dependency-node module-name version))
  (get-in @module-dependency-info [module-name version]))


(defn get-module-dependency-tree
  [module-name version]
  ())


(defn strip-name-from-version
  "Removes the module name and dash (-) from the beginning of a version that is applied to the application. An example is
    (strip-name-from-version \"apollo\" \"apollo-3.3.18\") => \"3.3.18\"
  Also will change a version that is a sequence into a string to prevent later issues. An example is
    (strip-name-from-version \"prismatic/schema\" [\"0.4.3\", \"0.4.2\"]) => \"[0.4.3, 0.4.2\"
  All other cases will return the original version
  "
  [module-name version]
  (if (and (string? version) (clojure.string/starts-with? version module-name))
    (clojure.string/replace-first version (re-pattern (str module-name "-")) "")
    (if (coll? version)
      (str "[" (apply str (interpose ", " version)) "]")
      version)))


(defn modify-project-clj-src [project_cli_src repo-name]
  (try+

    ;; Change the project.clj source from a (defproject... macro call to just a map
    (-> (str "{\n " (subs project_cli_src 0 (- (count (clojure.string/trimr project_cli_src)) 1)) "\n}")
        ;; Change the (defproject declaration to be :name and :version map keys
        (clojure.string/replace  #"\(\s*defproject\s+([a-zA-Z0-9-/]+)\s+" ":name \"$1\"\n:version ")

        ;; Remove the any comments
        (clojure.string/replace  #";.*\n" "")
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

        ;; Need to handle multiple names inside [...] as in
        ;; :exclusions ["lib-namespace/library1" "potemkin" "lib-namespace2/library2" "org.clojure/clojure"]
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
  (clojure.string/trim (:body (client/get (str "https://api.bitbucket.org/1.0/repositories/" constants/bitbucket-root-user "/" repo-name "/raw/" branch "/project.clj")
                                          {:basic-auth [(:user utils.identity/identity-info) (:password utils.identity/identity-info)] }))))

(defn get-project-dependencies [conn-info repo-name branch slug]
  (try+
    (let [project_cli_src (get-original-project-clj slug branch)
          modified_src (modify-project-clj-src project_cli_src repo-name)]
      (try+
        (if-let [project_clj (load-string modified_src)]
            (dissoc
              (apply merge-with
                     (comp vec flatten vector)
                     (map (partial apply hash-map) (:dependencies project_clj)))
              :scope :inclusions :exclusions :excludes :only)
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
        (println "Exception trying to get project.clj file for repo " repo-name (str "https://api.bitbucket.org/1.0/repositories/" constants/bitbucket-root-user "/" slug "/raw/" branch "/project.clj"))
        (println "++++++++++++++++++++++++++++++++++++++++++ ")
        (println (:message &throw-context))
        (println "++++++++++++++++++++++++++++++++++++++++++ ")
        (println (:cause &throw-context))
        (println "++++++++++++++++++++++++++++++++++++++++++ ")
        :exception-getting-project-clj-file)))


(defn get-repo-dependency-details [repo_info]
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
      (let [final-results (into {} (map get-repo-dependency-details repo_list_values))]
;;         (println "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$")
;;         (println "Page results type: " (type final-results))
;;         (pprint final-results)
;;         (println "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$")
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


(defn find-dependencies-all-bitbucket
  ([]
    (println "Starting ----------------- " (:url @connection-info) "====")
    (while (not (nil? (:url @connection-info)))
      (let [page-results (parse-repo-list-page)]
;;         (swap! module-dependency-info assoc-in [repo-name version] node)
        (swap! repository-info assoc :dependencies (into (:dependencies @repository-info) page-results)))
      (println "##########after page " (:url @connection-info) "#########")
      (pprint @repository-info)
      (println "###################"))
    (println "Finished getting dependencies----------------- ")
    (pprint @repository-info)
    (println "Finished ----------------- ")
    (let [log-file (utils/get-report-file-name-path  "bitbucket-dependency-scraper-logs" :extension ".clj")]
      (spit log-file (pr-str @repository-info))
      (println "Wrote to file " log-file " ----------------- ")))

  ([page]
    (if (integer? page)
      (swap! connection-info assoc :url (str "https://api.bitbucket.org/2.0/repositories/" constants/bitbucket-root-user "/?page=" page)))
    (find-dependencies-all-bitbucket)))


(defn display-project-dependencies [& repos]
  (for [repo-name repos]
    (do
      (println "Getting dependency info for repo " repo-name)
      (let [deps (get-project-dependencies @connection-info repo-name constants/default-branch repo-name)]
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



(defn build-repo-dependency-node [module-name version & {:keys [depth force-build] :or {depth 1
                                                                                      force-build false}}]
  (println (str "building repo-node for \"" module-name "\" version \"" version "\" and depth" depth "and force-build" force-build))
  (let [repo-name (if (.startsWith module-name (str constants/library-namespace "/")) (subs module-name (count (str constants/library-namespace "/"))) module-name)
        tag (utils/get-tag repo-name version)
        deps (get-project-dependencies @connection-info repo-name tag repo-name)
        deps2 (if (keyword? deps) {} deps)
        node {:name repo-name
              :full-name module-name
              :version (utils/strip-version repo-name version)
              :dependencies (reduce
                              (fn [m [k v]] (assoc m k (build-dependency-node k v :depth depth :force-build force-build)))
                              {}
                              deps2)}]
    (swap! module-dependency-info assoc-in [repo-name version] node)
    node))


(defn build-external-dependency-node [module-name version]
  (let [node {:name module-name :version version}]
    (swap! module-dependency-info assoc-in [module-name version] node)
    node))


(defn build-dependency-node [module-name version & {:keys [depth force-build] :or {depth 1
                                                                                   force-build false}}]
;;   (println "building node for \"" module-name "\" version \"" version "\" and depth" depth "and force-build" force-build)
  (let [sub-name (if (.startsWith module-name (str constants/library-namespace "/")) (subs module-name (count (str constants/library-namespace "/"))) module-name)]
    (let [already-saved-node (get-in @module-dependency-info [sub-name version])]
      (if (and already-saved-node (not force-build))
        already-saved-node
        (if (.startsWith module-name (str constants/library-namespace "/"))
          (if (pos? depth) (build-repo-dependency-node module-name version :depth (dec depth) :force-build force-build))
          (build-external-dependency-node module-name version))))))


(defn node-has-dependencies? [tree]
  (and (contains? tree :dependencies) (pos? (count (:dependencies tree)))))


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


(defn find-module-dependencies
  ([module-name]
    (find-module-dependencies module-name constants/default-branch))
  ([module-name version & {:keys [depth force-build] :or {depth 1
                                                          force-build false}}]
    (let [proper-version (utils/get-tag module-name version)]
      (build-repo-dependency-node module-name proper-version :depth depth :force-build force-build))))


(defn display-project-dependency-tree
  "Creates a text file report containing a readable report showing the dependency tree
  information for the specified repo/module name and its corresponding version.

  Arguments:
  repo-name - Desired repo/module name
  version (optional) - The version of the repo to get dependency information for.
                       If not supplied, the default branch is used.
                       (See 'utils.constants/default-branch)"
  ([repo-name]
    (display-project-dependency-tree repo-name constants/default-branch))
  ([repo-name version]
    (let [res (find-module-dependencies repo-name version)
          log-file (utils/get-report-file-name-path (str repo-name "-" version "-dependency-tree")
                                                    :subdirectory "dependency-trees")
          out-str (make-tree-readable res)]
      (println out-str)
      (spit log-file out-str)
      (println "Wrote dependency tree for " repo-name version "to" log-file)
      (utils/log-action "Wrote dependency tree for " repo-name version "to" log-file))))


(defn create-dependency-tree-reports
  "Creates text file reports containing a readable report showing the dependency tree
  information for the specified repo/module names and their corresponding versions.

  Arguments:
  either a map containing a repo/module name as keys and corresponding version as values,
      or one or more pairs of strings a repo/module name and corresponding version.

  (create-dependency-tree-reports {\"repo1\" \"1.0.0\", \"repo2\" \"1.0.1\", \"repo3\" \"1.1.0\", ...})
  or
  (create-dependency-tree-reports \"repo1\" \"1.0.0\" \"repo2\" \"1.0.1\" \"repo3\" \"1.1.0\")"
  [& args]
  (doseq [[repo-name version] (if (map? args) args (apply hash-map args))]
    (let [log-file (utils/get-report-file-name-path (str repo-name "-" version "-dependency-tree")
                                                    :subdirectory "dependency-trees"
                                                    :extension ".clj")]
      (->> (find-module-dependencies repo-name version)
            (make-tree-readable)
            (spit log-file))
      (println "Wrote dependency tree for " repo-name version "to" log-file)
      (utils/log-action "Wrote dependency tree for " repo-name version "to" log-file))))


(defn coallate-dependencies-versions
  "

  Arguements:
   deps - module dependenct tree containing the depenencies of the top level module.

  Returns a hash-map containing the names of all of the libraries present in the dependency tree as keys, and the "
  [deps]
  (let [deps-act (cond
                   (map? deps)
                   deps

                   (string? deps)
                   ())]
    (if (get deps-act :name)
      (try+
        (let [base-map {(:name deps-act) (sorted-set-by version/version-compare (strip-name-from-version (:name deps-act) (:version deps-act)))}
              sub-deps (get deps-act :dependencies)]
          (merge-with into (hash-map)
                      base-map
                      (if (nil? sub-deps)
                        (hash-map)
                        (apply merge-with into (hash-map) (map coallate-dependencies-versions (vals sub-deps))))))
        (catch Object _
          (println "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
          (pprint deps-act)
          (println (:object &throw-context))
          (println (:message &throw-context))
          (println (:cause &throw-context))
          (println (:stack-trace &throw-context))
          (println "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")))
      (println ""))))


(defn coallate-dependencies-versions-sorted
  "Helper function that wraps 'coallate-dependencies-versions to return a sorted-map of the results."
  [deps]
  (into (sorted-map) (coallate-dependencies-versions deps)))


(defn find-modules-using-library
  ""
  [module-name]
  (let [lib-name (if (.startsWith module-name (str constants/library-namespace "/")) module-name (str constants/library-namespace "/" module-name))]
;;     (->> @repository-info
;;         (:dependencies)
;;         (filter (fn [[k v]] (and (map? v) (some #(= % lib-name) (keys v))))))))
    (filter (fn [[k v]] (and (map? v) (some #(= % lib-name) (keys v)))) (:dependencies @repository-info))))

(defn find-modules-using-library2
  ""
  [module-name]
  (let [lib-name (if (.startsWith module-name (str constants/library-namespace "/")) module-name (str constants/library-namespace "/" module-name))]
;;     (->> @repository-info
;;         (:dependencies)
;;         (filter (fn [[k v]] (and (map? v) (some #(= % lib-name) (keys v))))))))
    (filter (fn [[k1 v1]]
              (pos? (count (disj (reduce conj #{} (map (fn [[k2 v2]] (get-in v2 [:dependencies module-name])) v1)) nil))))
            @module-dependency-info)))
