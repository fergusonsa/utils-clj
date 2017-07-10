(ns utils.repositories
  (:require [clj-jgit.porcelain]
            [utils.dependencies-scraper]
            [clojure.java.io :as io]
            [utils.core :as utils]
            [utils.environments :as environments]
            [utils.config :as config])
  (:use [clojure.set :only [difference intersection]]
        [clojure.java.browse]
        [clojure.pprint]
        [slingshot.slingshot :only [try+]])
  (:import [org.eclipse.jgit.api.errors RefNotFoundException
                                        StashApplyFailureException]
           [java.nio.file Files
                          Paths]
           [java.nio.file.attribute FileAttribute]))


(defn get-repos-in-src-root []
  (let [directory (clojure.java.io/file config/src-root-dir)]
    (map #(.getName %) (filter #(and (.isDirectory (clojure.java.io/file %))
                                     (.isDirectory (clojure.java.io/file (str % "/.git")))
                                     (.isFile (clojure.java.io/file (str % "/project.clj"))))
                                (.listFiles directory)))))


(defn create-symlink
  "creates a symlink to another file

   based on https://crossclj.info/ns/im.chit/hara.io.file/2.5.6/hara.io.file.html#_create-symlink
   "
  ([path link-to]
   (io/make-parents link-to)
   (Files/createSymbolicLink (Paths/get path (make-array String 0))
                             (Paths/get link-to (make-array String 0))
                              (into-array FileAttribute []))
   (utils/log-action "Created symlink" link-to "to" path)))


(defn get-version-from-repo [repo]
  (if (clj-jgit.porcelain/git-branch-attached? repo)
    (clj-jgit.porcelain/git-branch-current repo)
    (.call (.setTarget (.describe repo) (clj-jgit.porcelain/git-branch-current repo)))))


(defn get-repo-version [repo-name]
  (clj-jgit.porcelain/with-repo (str config/src-root-dir "/" repo-name)
    (get-version-from-repo repo)))


(defn show-src-version [& repo-names]
    (let [repos-to-check (if (> (count repo-names) 0)
                           (intersection (set repo-names) (set (get-repos-in-src-root)))
                           (get-repos-in-src-root))]
      (pprint (map #(list % (get-repo-version %)) repos-to-check))))


(defn set-repo-version
  ([repo-name version]
   (if (.isDirectory (io/file (str config/src-root-dir "/" repo-name)))
     (clj-jgit.porcelain/with-repo (str config/src-root-dir "/" repo-name)
       (set-repo-version repo repo-name version))
     (println "There is not a local repo for" repo-name)))

  ([repo repo-name version]
   (let [proper-version (utils/get-tag repo-name version)]
    (if (not= proper-version (get-version-from-repo repo))
      (let [orig-status (clj-jgit.porcelain/git-status repo)
            orig-status-count (apply + (map #(count (second %))
                                            (select-keys orig-status
                                                         [:added :changed :missing :modified :removed])))]
;;         (pprint orig-status)
;;         (println "orig-status-count: " orig-status-count)
        (if (> orig-status-count 0)
          (try+
            (clj-jgit.porcelain/git-create-stash repo)
            (catch StashApplyFailureException e#
              (println "\n** Exception trying to stash existing changes in the repo" repo-name)
              (println "** Check for existing conflicts in" (str config/src-root-dir "/" repo-name)))))
        (try+
          (clj-jgit.porcelain/git-checkout repo (str "tags/" proper-version))
          (println "Repo" repo-name "now set to version" version)
          (utils/log-action "set local repo" (str config/src-root-dir "/" repo-name) "to version" version)
          (catch RefNotFoundException e#
            (println "\n** Exception trying to checkout version" version "for repo " repo-name)
            (println "** Need to manually perform \"git fetch\" in " (str config/src-root-dir "/" repo-name) "\n")))
        (if (> orig-status-count 0)
          (try+
            (clj-jgit.porcelain/git-pop-stash repo)
            (catch StashApplyFailureException e#
              (println "\n** Applying stashed changes resulted in a conflict in the repo" repo-name)
              (println "** Check the conflicts in" (str config/src-root-dir "/" repo-name))))))
      (println "Repo" repo-name "is already set to" version))
    (let [new-status (clj-jgit.porcelain/git-status repo)
          new-status-count (apply + (map #(count (second %))
                                     (select-keys new-status
                                                  [:added :changed :missing :modified :removed])))]
      (if (> new-status-count 0)
        (do
          (println "  Repo" repo-name "has the following changed files due to stash applied changes:")
          (doseq [[k v] new-status]
            (if (> (count v) 0)
              (do
                (println "    " (name k) "files:")
                (run! #(println "        " %) v))))))))))


(defn set-repo-versions [& args]
  (doseq [[repo-name version] (if (map? (first args)) (first args) (apply hash-map args))]
    (set-repo-version repo-name version)))


(defn set-repo-version-same-as-env [env-srvr]
  (let [env-versions (environments/get-env-app-versions env-srvr)
        repos-to-set (intersection (set (keys env-versions)) (set (get-repos-in-src-root)))]
    (doseq [repo-name repos-to-set]
      (set-repo-version repo-name (str repo-name "-" (get env-versions repo-name))))))


(defn set-repo-version-same-as-manifest [branch]
  (-> (environments/get-app-versions-from-manifest-properties branch)
      (set-repo-versions)))


(defn get-library-repos-for-module
" Currently limited to direct dependency libraries, NOT nested libraries"
  ([repo-name]
    (get-library-repos-for-module repo-name config/default-branch))
  ([repo-name version library-name]
    (let [deps (utils.dependencies-scraper/find-module-dependencies repo-name version)
          dest-dir (str config/src-root-dir "/" library-name)
          source-url (str "git@bitbucket.org:" config/bitbucket-root-user"/" library-name ".git")
          lib-version (str library-name "-" (get-in deps [:dependencies (str config/library-namespace "/" library-name) :version]))
          repo-dir (str config/src-root-dir "/" repo-name)
          checkouts-dir (str repo-dir "/" library-name)
          checkouts-lib-symlink (str checkouts-dir "/" library-name)]
      (if (.isDirectory (clojure.java.io/file dest-dir))
        (do
          (println "Library repo" library-name "already exists in" dest-dir)
          (set-repo-version library-name lib-version))
        (do
          (io/make-parents dest-dir)
          (let [repo (clj-jgit.porcelain/git-clone-full source-url dest-dir)]
            (set-repo-version repo library-name lib-version))))
      (if (not (.exists  (io/file checkouts-lib-symlink)))
          (create-symlink dest-dir checkouts-lib-symlink)))))


(defn example_function_calls []
;;   (set-repo-version-same-as-env "med16.cenx.localnet:8080")
;;   (utils.environments/get-env-app-versions "med16.cenx.localnet:8080")
  )
