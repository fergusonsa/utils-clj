(ns utils.repositories
  (:require [clj-jgit.porcelain]
            [utils.dependencies-scraper :as dependencies]
            [clojure.java.io :as io]
            [clj-time.core :as time-core]
            [clj-time.coerce :as time-coerce]
            [utils.core :as utils]
            [utils.environments :as environments]
            [utils.config :as config]
            [utils.identity]
            [clj-http.client :as client])
  (:use [clojure.set :only [difference intersection]]
        [clojure.java.browse]
        [clojure.pprint]
        [slingshot.slingshot :only [try+]]
        [clojure.tools.nrepl :as repl])
  (:import [org.eclipse.jgit.api.errors RefNotFoundException
                                        StashApplyFailureException
                                        CheckoutConflictException]
           [java.nio.file Files
                          Paths]
           [java.nio.file.attribute FileAttribute]))


(defn is-cenx-module? [module-name]
  (if-let [dep-info (get @dependencies/repository-dependency-info module-name)]
    (if-let [full-name (get (second (first dep-info)) :full-name)]
      (.startsWith full-name (str config/library-namespace "/"))
      false)
    false))


(defn get-repo [repo-name]
   (if (.isDirectory (io/file (str config/src-root-dir "/" repo-name)))
     (clj-jgit.porcelain/with-repo (str config/src-root-dir "/" repo-name)
       repo)
     nil))


(defn get-release-branches
  "Returns a sequence of maps containing
        {:name \"<branch-name>\" :target {:date \"YYYY-mm-ddTHH:MM:SS+00:00\"}}
   for all branches that start with \"r/\" in the manifest repo."
  [& {:keys [url]
      :or {url (str "https://api.bitbucket.org/2.0/repositories/"
                    config/bitbucket-root-user
                    "/exanova/refs/branches?q=name+%7E+%22r/%22&fields=-values.links,-values.type,-values.target.hash,-values.target.repository,-values.target.author,-values.target.parents,-values.target.links,-values.target.type,-values.target.message")}}]
   (let [results (:body (client/get url
                            {:content-type :json :basic-auth [(:user utils.identity/identity-info) (:password utils.identity/identity-info)] :throw-exceptions false :as :json}))]
      (concat (:values results)
            (if (get results :next)
              (get-release-branches :url (get results :next))))))

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
  [link-path target-path]
  (if (.isDirectory (io/file target-path))
    (do
      (io/make-parents link-path)
      (Files/createSymbolicLink (Paths/get link-path (make-array String 0))
                                (Paths/get target-path (make-array String 0))
                                (into-array FileAttribute []))
      (println "Created symlink" link-path "to" target-path)
      (utils/log-action "Created symlink" link-path "to" target-path))
    (println "The target path for the symlink" target-path "does not exist")))


(defn get-version-from-repo [repo]
  (if (clj-jgit.porcelain/git-branch-attached? repo)
    (clj-jgit.porcelain/git-branch-current repo)
    (.call (.setTarget (.describe repo) (clj-jgit.porcelain/git-branch-current repo)))))


(defn get-repo-version [repo-name]
  (if (.isDirectory (io/file (str config/src-root-dir "/" repo-name "/.git")))
    (clj-jgit.porcelain/with-repo (str config/src-root-dir "/" repo-name)
      (get-version-from-repo repo))))


(defn get-repo-version-map [repo-name]
  { repo-name (get-repo-version repo-name)})


(defn show-src-version [& repo-names]
;;   (let [repos-to-check (if (> (count repo-names) 0)
;;                          (intersection (set repo-names) (set (get-repos-in-src-root)))
;;                          (get-repos-in-src-root))]
;;     (map #(list % (dependencies/strip-name-from-version % (get-repo-version %))) repos-to-check)))
  (->> (if (> (count repo-names) 0)
         (intersection (set repo-names) (set (get-repos-in-src-root)))
         (get-repos-in-src-root))
       (map #(hash-map % (dependencies/strip-name-from-version % (get-repo-version %))))
       (into (sorted-map))
       ))

(defn check-for-checkouts-directory
  "Checks to see if there is a checkouts directory in the repo-path and returns a sequence of the files contained there."
  [repo-path]
  (let [checkouts-dir (io/file (str repo-path "/checkouts"))]
    (if (.isDirectory checkouts-dir)
      (vec (for [file (.listFiles checkouts-dir)] (.getName file)))
      ())))


(defn add-checkouts-link
  ""
  [repo-name library-name]
  (-> (str config/src-root-dir "/" repo-name "/checkouts/" library-name)
       (create-symlink (str config/src-root-dir "/" library-name))))


(defn remove-checkouts-link
  "Deletes the specified library symlinks in the repo's checkouts directory.
  If no library names are passed, then it will delete them all."
  [repo-name & library-names]
  (->> (if (and (not (nil? library-names))
               (> (count library-names) 0))
         library-names
         (check-for-checkouts-directory (str config/src-root-dir "/" repo-name)))
       (map #(if (.delete (io/file (str config/src-root-dir "/" repo-name "/checkouts/" %)))
               (do
                 (println "Deleted checkouts link for" % "in repo" repo-name)
                 (utils/log-action "Deleted checkouts link for" % "in repo" repo-name))
               (println "Failed attempt to delete checkouts link for" % "in repo" repo-name)))))


(defn get-linked-repo-versions
  "Returns a map contianing the names of modules linked to in the checkouts directory
  as keys and their corresponding versions as values."
  [repo-name]
  (->> (str config/src-root-dir "/" repo-name)
       (check-for-checkouts-directory)
       (map get-repo-version-map)
       (into {})))


(defn check-for-running-repl?
  "Checks to see if there is a file named .nrepl-port in the root of the repo's path
  to see if there is a repl running currently for this repo"
  [repo-path]
  (if (.isFile (io/file (str repo-path "/.nrepl-port")))
    (try+
      (with-open[conn (repl/connect :port (load-file (str repo-path "/.nrepl-port")))]
        true)
      (catch Object _
        false))
    false))


(defn get-unmerged-paths
  "Returns a set of the paths to any/all unmerged files in the repo."
  ([repo]
    (get-unmerged-paths repo ""))
  ([repo path]
    (-> (.getRepository repo)
        (.readDirCache)
        (.getEntriesWithin path)
        ((partial filter #(> (.getStage %) 0)))
        ((partial map #(.getPathString %)))
        (set))))


(defn checkout-version-from-repo
  "Performs a git checkout on the specified repository for the specified tag version.
  If the verson is not found and a git fetch has not been performed in the past hour,
  performs a git fetch and reties the git checkout."
  ([repo-name version]
    (checkout-version-from-repo repo-name version (utils/get-tag repo-name version)))
  ([repo-name version proper-version]
    (clj-jgit.porcelain/with-repo (str config/src-root-dir "/" repo-name)
      (checkout-version-from-repo repo repo-name version proper-version)))
  ([repo repo-name version proper-version]
    (try+
      (clj-jgit.porcelain/git-checkout repo (str "tags/" proper-version))
      (println "Repo" repo-name "now set to version" version)
      (if (check-for-running-repl? (str config/src-root-dir "/" repo-name))
        (println "  - There is currently a repl running for this repo. It should be restarted to load changes."))
      (let [linked-repo-versions (get-linked-repo-versions repo-name)]
        (if (> (count linked-repo-versions) 0)
          (do
            (println "  - checkouts directory present with links to the following modules:")
            (pprint linked-repo-versions))))
      (utils/log-action "set local repo" (str config/src-root-dir "/" repo-name) "to version" version)
      (catch RefNotFoundException e#
        ;; Check to see if a fetch has been done in the past hour. If it has not, perform fetch and try to checkout again
        (if (time-core/before?
              (time-coerce/from-long (.lastModified (io/file (str config/src-root-dir "/apollo/.git/FETCH_HEAD"))))
              (time-core/minus (time-core/now) (time-core/hours 1)))
          (do
            (clj-jgit.porcelain/with-identity {
                :private (slurp config/ssh-private-key-path)
                :public (slurp config/ssh-public-key-path)
                :passphrase (:password utils.identity/identity-info)
                :exclusive true}
                (clj-jgit.porcelain/git-fetch-all repo))
            (checkout-version-from-repo repo repo-name version proper-version))
          (do
            (println "\n** Could not checkout/find the version" version "for repo" repo-name)
            (println "** Need to manually perform \"git fetch\" in " (str config/src-root-dir "/" repo-name) "\n")))))))


(defn set-repo-version
  "Performs git checkout of the desired version of the repo.
  Performs the following steps to try to reduce manual actions:
  1. Get current status.
  2. If changes present in starting status, perform git stash save to save changes for later application.
  3. Perform git checkout of desired version.
  4. Check to see if there is repl running for this repo and print message if there is.
  5. Check for linked modules in the checkouts directory and print message if there is.
  6. If changes were present in starting status, perform git stash pop to apply changes to new version of source.
  7. Reset any files that the stash apply staged.
  7. Get new current status and print appropriate messages.
  "
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
        (if (> orig-status-count 0)
          (try+
            (clj-jgit.porcelain/git-create-stash repo)
            (catch StashApplyFailureException e#
              (println "\n** Exception trying to stash existing changes in the repo" repo-name)
              (println "** Check for existing conflicts in" (str config/src-root-dir "/" repo-name)))))
        (try+
          (checkout-version-from-repo repo repo-name version proper-version)
          (catch RefNotFoundException e#
            (println "\n** Could not checkout/find the version" version "for repo" repo-name)
            (println "** Need to manually perform \"git fetch\" in " (str config/src-root-dir "/" repo-name) "\n"))
          (catch CheckoutConflictException e#
            (println "\n** Could not checkout version" version "for repo " repo-name "due to conflicts.")
            (println (:message &throw-context) "\n")))
        (if (> orig-status-count 0)
          (try+
            (clj-jgit.porcelain/git-pop-stash repo)
            (let [new-status (clj-jgit.porcelain/git-status repo)
                  unmerged-paths (get-unmerged-paths repo)
                  changed-files (get new-status :changed)]
              (if (and changed-files (> (count changed-files) 0))
                (do
                  (println "  Attempting to reset changed files " changed-files)
                  (map (partial clj-jgit.porcelain/git-reset repo "HEAD" nil) changed-files)))
              (if (> (count unmerged-paths) 0)
                (do
                  (println "  Attempting to reset unmerged files " unmerged-paths)
                  (map (partial clj-jgit.porcelain/git-reset repo "HEAD" nil) unmerged-paths))))
            (catch StashApplyFailureException e#
              (println "\n** Applying stashed changes resulted in a conflict in the repo" repo-name)
              (println "** Check the conflicts in" (str config/src-root-dir "/" repo-name) "\n")))))
      (println "Repo" repo-name "is already set to" version))
    (let [new-status (clj-jgit.porcelain/git-status repo)
          new-status-count (apply + (map #(count (second %))
                                     (select-keys new-status
                                                  [:added :changed :missing :modified :removed])))]
      (if (> new-status-count 0)
        (do
          (println "  - Repo" repo-name "has the following changed files due to stash applied changes:")
          (doseq [[k v] new-status]
            (if (> (count v) 0)
              (do
                (println "    " (name k) "files:")
                (run! #(println "        " %) v))))))))))


(defn set-repo-versions
  "Convienence function for applying set-repo-version function to multiple repos.
  Accepts either a map containing module-names as keys and corresponding versions as values,
      (set-repo-versions {\"apollo\" \"3.3.18\" \"bifrost\" \"1.9.6\"})
  or multiple arguments of module-name version.
      (set-repo-versions \"apollo\" \"3.3.18\" \"bifrost\" \"1.9.6\")"
  [& args]
  (doseq [[repo-name version] (if (map? (first args)) (first args) (apply hash-map args))]
    (set-repo-version repo-name version)))


(defn set-repo-version-same-as-env [env-srvr]
  (let [env-versions (environments/get-env-app-info env-srvr)
        repos-to-set (intersection (set (keys env-versions)) (set (get-repos-in-src-root)))]
    (doseq [repo-name repos-to-set]
      (set-repo-version repo-name (str repo-name "-" (get env-versions repo-name))))))


(defn set-repo-version-same-as-manifest
  "In all the local repos in the src directory to the versions of applications listed
  in the manifest.properties in the specified branch.

  For list of available release branches, run the function (see 'utils.repositories/get-release-branches)"
  [branch]
  (-> (environments/get-app-versions-from-manifest-properties branch)
      (set-repo-versions)))


(defn set-linked-repo-versions [repo-name]
  (let [lib-versions (get-linked-repo-versions repo-name)
        deps (dependencies/find-module-dependencies repo-name (get-repo-version repo-name) :depth 1)]
    (->> (select-keys (:dependencies deps) (filter is-cenx-module? (keys (:dependencies deps))))
         (vals)
         (map (fn [m] {(:name m) (:version m)}))
         (apply merge-with into (hash-map))
         (filter #(and (.contains (keys lib-versions) (key %)) (not= (val %) (get lib-versions %))))
         (into {})
         (set-repo-versions))))


(defn get-library-repos-for-module
"Currently limited to direct dependency libraries, NOT nested libraries"
  ([repo-name]
    (clj-jgit.porcelain/with-repo (str config/src-root-dir "/" repo-name)
      (get-library-repos-for-module repo-name (clj-jgit.porcelain/git-branch-current repo))))
  ([repo-name version]
    (let [deps (dependencies/find-module-dependencies repo-name version)
          coll-deps (dependencies/coallate-dependencies-versions deps)
          modules-to-check (select-keys coll-deps (filter is-cenx-module? (keys coll-deps)))
          modules-versions (into {} (map (fn [m] {(key m) (last (val m))}) modules-to-check))]
      (clj-jgit.porcelain/with-repo (str config/src-root-dir "/" repo-name)
        (map #(get-library-repos-for-module repo-name version (key %) (val %)) modules-versions))))
  ([repo-name version library-name library-version]
    (let [dest-dir (str config/src-root-dir "/" library-name)
          source-url (str "git@bitbucket.org:" config/bitbucket-root-user"/" library-name ".git")
          repo-dir (str config/src-root-dir "/" repo-name)
          checkouts-dir (str repo-dir "/checkouts")
          checkouts-lib-symlink (str checkouts-dir "/" library-name)]
      (if (.isDirectory (clojure.java.io/file dest-dir))
        (do ;; A local git repo for library module already exists. Just checkout the desired version.
          (println "Library repo" library-name "already exists in" dest-dir)
          (set-repo-version library-name library-version))
        (do ;; There is not an existing git repo for this library. Clone it and checkout the desired version.
          (println "Library repo" library-name "does not exists in" dest-dir "and will be cloned from bitbucket")
          (io/make-parents dest-dir)
          (clj-jgit.porcelain/with-identity {:private (slurp config/ssh-private-key-path)
                                             :public (slurp config/ssh-public-key-path)
                                             :passphrase (:password utils.identity/identity-info)
                                             :exclusive true}
            (let [repo (:repo (clj-jgit.porcelain/git-clone-full source-url dest-dir))]
              (set-repo-version repo library-name library-version)))))
;;       (if (not (.exists  (io/file checkouts-lib-symlink)))
;;           (create-symlink dest-dir checkouts-lib-symlink))
      )))


(defn example_function_calls []
;;   (set-repo-version-same-as-env "med16.cenx.localnet:8080")
  )
