(ns utils.repositories
  "Helper utilities for dealing with local git repositories of clojure code.

  Uses third party libraries clj-jgit, clj-http, clj-time, slingshot.slingshot, clojure.tools.nrepl
  "
  (:refer-clojure :exclude [help])
  (:require [clojure.java.io :as io]
            [utils.dependencies :as dependencies]
            [utils.core :as utils]
            [utils.constants :as constants]
            [utils.identity]
            [clj-time.core :as time-core]
            [clj-time.format :as time-format]
            [clj-time.coerce :as time-coerce]
            [clj-jgit.porcelain]
            [clj-http.client :as client]
            [clojure.tools.nrepl :as repl]
            [version-clj.core :as version])
  (:use [clojure.set :only [difference intersection]]
        [clojure.java.browse]
        [clojure.pprint]
        [slingshot.slingshot :only [try+]])
  (:import [org.eclipse.jgit.api.errors RefNotFoundException
                                        StashApplyFailureException
                                        CheckoutConflictException]
           [java.nio.file Files
                          Paths]
           [java.nio.file.attribute FileAttribute]
           [java.util Properties]))

(defn help
  []
  (utils/utils-help 'utils.repositories))

(defn is-controlled-module?
  "Checks to see if the module is a controlled module using the following rules:
  - if the module is in the utils.dependencies-scraper/repository-dependency-info,
    and the :full-name that starts with the utils.constants/library-namespace setting."
  [module-name]
  (if-let [dep-info (get @dependencies/repository-dependency-info module-name)]
    (if-let [full-name (get (second (first dep-info)) :full-name)]
      (.startsWith full-name (str constants/library-namespace "/"))
      false)
    false))


(defn get-repo
  "Helper method for getting a clj-jgit Git instance for the local git
  repository with the specified repo-name.
  If the root-path is not provided, uses the constants/src-root-dir setting as
  the directory where the repository is localed."
  ([repo-name]
   (get-repo constants/src-root-dir repo-name))
  ([root-path repo-name]
   (if (.isFile (io/file (str root-path "/" repo-name "/.git")))
     (clj-jgit.porcelain/with-repo (str root-path "/" repo-name)
       repo)
     nil)))


(defn get-release-branches
  "Returns a sequence of maps containing
        {:name \"<branch-name>\" :target {:date \"YYYY-mm-ddTHH:MM:SS+00:00\"}}
   for all branches that start with \"r/\" in the manifest repo."
  [& {:keys [url]
      :or {url (str "https://api.bitbucket.org/2.0/repositories/"
                    constants/bitbucket-root-user
                    "/exanova/refs/branches?q=name+%7E+%22r/%22"
                    "&fields=-values.links,-values.type,-values.target.hash,"
                    "-values.target.repository,-values.target.author,-values.target.parents,"
                    "-values.target.links,-values.target.type,-values.target.message")}}]
   (let [results (:body (client/get url
                            {:content-type :json
                             :basic-auth [(:user utils.identity/identity-info)
                                          (:password utils.identity/identity-info)]
                             :throw-exceptions false
                             :as :json}))]
      (into (sorted-map)
            (concat (map #(sorted-map (get-in % [:target :date]) (:name %)) (:values results))
                    (if (get results :next)
                      (get-release-branches :url (get results :next))
                      {})))))


(defn get-repo-refs
  "Returns a sequence of maps containing
        {:name \"<branch-name>\" :target {:date \"YYYY-mm-ddTHH:MM:SS+00:00\"}}
   for all branches that start with \"r/\" in the manifest repo.

  Arguments:
    repo-name - The name of the repo/application.
    ref-type -
    :url-arg <value> - Optional -
    :qualifiers <value> - Optional -
  "
  [repo-name ref-type & {:keys [url-arg qualifiers]
      :or {qualifiers ""}}]
    (let [url (if (nil? url-arg)
               (str "https://api.bitbucket.org/2.0/repositories/"
                    constants/bitbucket-root-user "/" repo-name "/refs/" ref-type "?" qualifiers
                    "fields=-values.links,-values.type,-values.target.hash,"
                    "-values.target.repository,-values.target.author,-values.target.parents,"
                    "-values.target.links,-values.target.type,-values.target.message")
               url-arg)
         results (:body
                   (client/get url
                               {:content-type :json
                                :basic-auth [(:user utils.identity/identity-info)
                                             (:password utils.identity/identity-info)]
                                :throw-exceptions false
                                :as :json}))]
     (into (sorted-map)
           (concat (map #(hash-map (:name %) (get-in % [:target :date])) (:values results))
                   (if-let [next-url (get results :next)]
                     (get-repo-refs repo-name ref-type :url-arg next-url)
                     {})))))


(defn get-release-docker-images []
  (let [url "https://ship.cenx.com/#browse/search/docker=attributes.docker.imageName%3Dexanova"
        results (:body (client/get url
                            {:basic-auth ["scott.ferguson"
                                          (:password utils.identity/identity-info)]
                             :throw-exceptions false}))]
    results))

(defn get-repo-branches
  "
  Arguments:
    repo-name - The name of the repo/application.
    :url <value> - Optional -
  "
  [repo-name & {:keys [url]
      :or {url (str "https://api.bitbucket.org/2.0/repositories/"
                    constants/bitbucket-root-user
                    "/" repo-name
                    "/refs/branches"
                    "?q=name+%7E+%22r/%22"
                    "&fields=-values.links,-values.type,-values.target.hash,"
                    "-values.target.repository,-values.target.author,-values.target.parents,"
                    "-values.target.links,-values.target.type,-values.target.message")}}]
  (println url)
   (let [results (:body (client/get url
                            {:content-type :json
                             :basic-auth [(:user utils.identity/identity-info)
                                          (:password utils.identity/identity-info)]
                             :throw-exceptions false
                             :as :json}))]
      (into (sorted-map)
            (concat (map #(hash-map (:name %) (get-in % [:target :date])) (:values results))
                    (if (get results :next)
                      (get-repo-branches :url (get results :next))
                      {})))))


;; (defn get-repo-tags-recursive
;;   "
;;   "
;;   [repo-name & {:keys [url]
;;                 :or {url (str "https://api.bitbucket.org/2.0/repositories/"
;;                               constants/bitbucket-root-user "/" repo-name "/refs/tags"
;;                               "?fields=-values.links,-values.type,-values.target.hash,"
;;                               "-values.target.repository,-values.target.author,-values.target.parents,"
;;                               "-values.target.links,-values.target.type,-values.target.message")}}]
;;   (try+
;;     (let [results (:body (client/get url
;;                               {:content-type :json
;;                              :basic-auth [(:user utils.identity/identity-info)
;;                                           (:password utils.identity/identity-info)]
;;                              :throw-exceptions false
;;                              :as :json}))]
;;       (into (sorted-map)
;;             (concat (map #(hash-map (:name %) (get-in % [:target :date])) (:values results))
;;                     (if (get results :next)
;;                       (get-repo-tags-recursive :url (get results :next))
;;                       {})))))

;; (defn get-repo-tags
;;   "
;;   "
;;   [repo-name]
;;   (let [url (str "https://api.bitbucket.org/2.0/repositories/"
;;                  constants/bitbucket-root-user
;;                  "/" repo-name "/refs/tags"
;;                  "?fields=-values.links,-values.type,-values.target.hash,"
;;                  "-values.target.repository,-values.target.author,-values.target.parents,"
;;                  "-values.target.links,-values.target.type,-values.target.message")]
;;     (->> (get-repo-tags-recursive repo-name :url url)
;;          (map #(hash-map (:name %) (get-in % [:target :date])))
;;          (into (sorted-map)))))



(defn get-repos-in-src-root
  "Returns a lazy sequence of directory names (not paths) in the utils.constants/src-root-dir directory that
  meet the following rules:
  - Contains a .git sub-directory.
  - Contains a project.clj file."
  []
  (let [directory (clojure.java.io/file constants/src-root-dir)]
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


(defn get-version-from-repo
  "Returns the git branch or tag for the git repository."
  [repo]
  (if (clj-jgit.porcelain/git-branch-attached? repo)
    (clj-jgit.porcelain/git-branch-current repo)
    (.call (.setTarget (.describe repo) (clj-jgit.porcelain/git-branch-current repo)))))


(defn get-repo-version
  "Returns the git branch or tag for the specified local git repository.

  Arguments:
    repo-name - The name of the repo/application.
  "
  [repo-name]
  (if (.isDirectory (io/file (str constants/src-root-dir "/" repo-name "/.git")))
    (clj-jgit.porcelain/with-repo (str constants/src-root-dir "/" repo-name)
      (get-version-from-repo repo))))


(defn get-repo-version-map
  "Returns a map with the git branch or tag for the specified local git repository.

  Arguments:
    repo-name - The name of the repo/application.
  "
  [repo-name]
  { repo-name (get-repo-version repo-name)})


(defn get-repo-src-versions
  "Gets the git branches or tags for the specified (or all) local git repositories in the source root directory."
  [& repo-names]
  (->> (if (> (count repo-names) 0)
         (intersection (set repo-names) (set (get-repos-in-src-root)))
         (get-repos-in-src-root))
       (map #(hash-map % (dependencies/strip-name-from-version % (get-repo-version %))))
       (into (sorted-map))))


(defn check-for-checkouts-directory
  "Checks to see if there is a checkouts directory in the repo-path and returns
  a sequence of the files contained there."
  [repo-path]
  (let [checkouts-dir (io/file (str repo-path "/checkouts"))]
    (if (.isDirectory checkouts-dir)
      (vec (for [file (.listFiles checkouts-dir)] (.getName file)))
      ())))


(defn add-checkouts-link
  "Creates a symlink in the repo's checkouts directory to the library's local git repo.

  Arguments:
    repo-name - The name of the repo/application.
    library-name -
  "
  [repo-name library-name]
  (-> (str constants/src-root-dir "/" repo-name "/checkouts/" library-name)
       (create-symlink (str constants/src-root-dir "/" library-name))))


(defn remove-checkouts-link
  "Deletes the specified library symlinks in the repo's checkouts directory.
  If no library names are passed, then it will delete them all.

  Arguments:
    repo-name - The name of the repo/application.
    library-names - Optional -
  "
  [repo-name & library-names]
  (->> (if (and (not (nil? library-names))
               (> (count library-names) 0))
         library-names
         (check-for-checkouts-directory (str constants/src-root-dir "/" repo-name)))
       (map #(if (.delete (io/file (str constants/src-root-dir "/" repo-name "/checkouts/" %)))
               (do
                 (println "Deleted checkouts link for" % "in repo" repo-name)
                 (utils/log-action "Deleted checkouts link for" % "in repo" repo-name))
               (println "Failed attempt to delete checkouts link for" % "in repo" repo-name)))))


(defn get-linked-repo-versions
  "Returns a map contianing the names of modules linked to in the checkouts directory
  as keys and their corresponding versions as values.

  Arguments:
    repo-name - The name of the repo/application.
  "
  [repo-name]
  (->> (str constants/src-root-dir "/" repo-name)
       (check-for-checkouts-directory)
       (map get-repo-version-map)
       (into {})))


(defn show-repo-versions
  "Displays the git branches or tags for the specified (or all) local git repositories in the
  source root directory, including any linked repos in checkouts subdirectories."
  [& repo-names]
  (->> (if (> (count repo-names) 0)
         (intersection (set repo-names) (set (get-repos-in-src-root)))
         (get-repos-in-src-root))
       (map #(hash-map % {:version (dependencies/strip-name-from-version % (get-repo-version %))
                          :linked-repos (get-linked-repo-versions %)}))
       (into (sorted-map))
       (pprint)))


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
    (clj-jgit.porcelain/with-repo (str constants/src-root-dir "/" repo-name)
      (checkout-version-from-repo repo repo-name version proper-version)))
  ([repo repo-name version proper-version]
    (try+
      (clj-jgit.porcelain/git-checkout repo (str "tags/" proper-version))
      (println "Repo" repo-name "now set to version" version)
      (if (check-for-running-repl? (str constants/src-root-dir "/" repo-name))
        (println "  - There is currently a repl running for this repo. It should be restarted to load changes."))
      (let [linked-repo-versions (get-linked-repo-versions repo-name)]
        (if (> (count linked-repo-versions) 0)
          (do
            (println "  - checkouts directory present with links to the following modules:")
            (pprint linked-repo-versions))))
      (utils/log-action "set local repo" (str constants/src-root-dir "/" repo-name) "to version" version)
      (catch RefNotFoundException e#
        ;; Check to see if a fetch has been done in the past hour. If it has not, perform fetch and try to checkout again
        (if (time-core/before?
              (time-coerce/from-long (.lastModified (io/file (str constants/src-root-dir "/apollo/.git/FETCH_HEAD"))))
              (time-core/minus (time-core/now) (time-core/hours 1)))
          (do
            (clj-jgit.porcelain/with-identity {
                :private (slurp constants/ssh-private-key-path)
                :public (slurp constants/ssh-public-key-path)
                :passphrase (:password utils.identity/identity-info)
                :exclusive true}
                (clj-jgit.porcelain/git-fetch-all repo))
            (checkout-version-from-repo repo repo-name version proper-version))
          (do
            (println "\n** Could not checkout/find the version" version "for repo" repo-name)
            (println "** Need to manually perform \"git fetch\" in " (str constants/src-root-dir "/" repo-name) "\n")))))))


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

  Arguments:
    repo - Optional -
    repo-name - The name of the repo/application.
    version - Version of the repo/application to find releases for.
  "
  ([repo-name version]
   (if (.isDirectory (io/file (str constants/src-root-dir "/" repo-name)))
     (clj-jgit.porcelain/with-repo (str constants/src-root-dir "/" repo-name)
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
              (println "** Check for existing conflicts in" (str constants/src-root-dir "/" repo-name)))))
        (try+
          (checkout-version-from-repo repo repo-name version proper-version)
          (catch RefNotFoundException e#
            (println "\n** Could not checkout/find the version" version "for repo" repo-name)
            (println "** Need to manually perform \"git fetch\" in " (str constants/src-root-dir "/" repo-name) "\n"))
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
              (println "** Check the conflicts in" (str constants/src-root-dir "/" repo-name) "\n")))))
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
  (->> (if (map? (first args))
        (first args)
        (apply hash-map args))
      (map #(set-repo-version (key %) (val %)))))


(defn- get-env-versions [env-id]
  ((resolve 'utils.environments/get-env-versions) env-id))


(defn set-local-repo-versions-same-as
  "In all the local repos in the src directory to the versions of applications in the specified environment.

    Valid string formats for the environment id can be one of the following:
   - \"r/.*\" -- which is a release branch name.
   - nil -- which indicates to get the versions of the local git repositories.
   - \".*\" -- which should be a server name that hosts a bifrost status api.
               (see 'utils.environments/get-server-status-api-url)"
  [env-id]
  (-> (get-env-versions env-id)
      (select-keys (get-repos-in-src-root))
      (set-repo-versions)))


(defn set-linked-repo-versions
  "
  Arguments:
    repo-name - The name of the repo/application."
  [repo-name]
  (let [lib-versions (get-linked-repo-versions repo-name)
        deps (dependencies/find-module-dependencies repo-name (get-repo-version repo-name) :depth 1)]
    (->> (select-keys (:dependencies deps) (filter is-controlled-module? (keys (:dependencies deps))))
         (vals)
         (map (fn [m] {(:name m) (:version m)}))
         (apply merge-with into (hash-map))
         (filter #(and (.contains (keys lib-versions) (key %))
                       (not= (val %) (get lib-versions %))))
         (into {})
         (set-repo-versions))))


(defn show-library-versions-for-module
  "
  Arguments:
    repo-name - The name of the repo/application."
  ([repo-name]
    (clj-jgit.porcelain/with-repo (str constants/src-root-dir "/" repo-name)
      (println repo-name (get-version-from-repo repo))
      (show-library-versions-for-module repo-name (get-version-from-repo repo))))
  ([repo-name version]
   (let [deps (into (sorted-map) (:dependencies (dependencies/find-module-dependencies repo-name version)))
         modules-to-check (select-keys deps (filter is-controlled-module? (keys deps)))]
     (println (count deps))
     (if (> (count deps) 0)
       (do
         (println "Non-third party library requirements for" repo-name "\n")
         (doseq [[module-name module-info] deps]
           (println (format "%-60s %s" module-name (:version module-info)))))
       (println "No Non-third party library requirements for" repo-name "\n")))))




(defn checkout-library-repos-for-module
  "Currently limited to direct dependency libraries, NOT nested libraries

  Arguments:
    repo-name - The name of the repo/application."
  ([repo-name]
    (clj-jgit.porcelain/with-repo (str constants/src-root-dir "/" repo-name)
      (checkout-library-repos-for-module repo-name (get-version-from-repo repo))))
  ([repo-name version]
    (let [deps (dependencies/find-module-dependencies repo-name version)
          coll-deps (dependencies/coallate-dependencies-versions deps)
          modules-to-check (select-keys coll-deps (filter is-controlled-module? (keys coll-deps)))
          modules-versions (into {} (map (fn [m] {(key m) (last (val m))}) modules-to-check))]
      (clj-jgit.porcelain/with-repo (str constants/src-root-dir "/" repo-name)
        (map #(checkout-library-repos-for-module repo-name version (key %) (val %)) modules-versions))))
  ([repo-name version library-name library-version]
    (let [dest-dir (str constants/src-root-dir "/" library-name)
          source-url (str "git@bitbucket.org:" constants/bitbucket-root-user"/" library-name ".git")
          repo-dir (str constants/src-root-dir "/" repo-name)
          checkouts-dir (str repo-dir "/checkouts")]
      (if (.isDirectory (clojure.java.io/file dest-dir))
        (do ;; A local git repo for library module already exists. Just checkout the desired version.
          (println "\nLibrary repo" library-name "already exists in" dest-dir)
          (set-repo-version library-name library-version))
        (do ;; There is not an existing git repo for this library. Clone it and checkout the desired version.
          (println "\nLibrary repo" library-name "does not exist in" dest-dir "and will be cloned from bitbucket")
          (io/make-parents dest-dir)
          (clj-jgit.porcelain/with-identity {:private (slurp constants/ssh-private-key-path)
                                             :public (slurp constants/ssh-public-key-path)
                                             :passphrase (:password utils.identity/identity-info)
                                             :exclusive true}
            (let [repo (:repo (clj-jgit.porcelain/git-clone-full source-url dest-dir))]
              (set-repo-version repo library-name library-version))))))))


(defn load-props
  "Given a path to a properties file, load it into a Java Properties object."
  [readable]
  (let [props (io/reader readable)]
    (doto (Properties.)
      (.load props))))



(defn strip-hash-comments-map-values
  "Returns a map containing the same keys as the map submitted but with the string
  values having been trimmed of any comments started with #."
  [m]
  (into (sorted-map)
        (for [[k v] m]
          [k (-> v
                 (clojure.string/split #"#" 2)
                 (first)
                 (clojure.string/trim ))])))


(defn get-app-versions-from-manifest-properties
  "Return a map containing the application versions contianed in the desired branch version of manifest.properties.

  For list of available release branches, run the function (see 'utils.repositories/get-release-branches)"
  [branch]
  (try+
    (-> (str "https://bitbucket.org/" constants/bitbucket-root-user "/exanova/raw/" branch "/manifest.properties")
        (client/get {:basic-auth [(:user utils.identity/identity-info) (:password utils.identity/identity-info)] })
        (:body)
        (.getBytes)
        (io/input-stream)
        (load-props)
        (select-keys constants/deployable-applications)
        (strip-hash-comments-map-values))
    (catch [:status 404] {}
      (println "The" branch "does not exist")
      {})
    (catch Object _
      (println "Exception trying to get app versions from manifest.properties for branch" branch)
      (println (:message &throw-context))
      {})))


(defn find-manifest-containing-app-version
  "

  repo-name - The name of the repo/application.
  version - Version of the repo/application to find releases for.

  Returns a sorted map containing the release branch names as keys and the version of
  the specified repo/application name's version in that release as value.

  Arguments:
    repo-name - The name of the repo/application.
    version - Version of the repo/application to find releases for.
  "
  [repo-name version]
  ; Get the time the tag for that version was created.
   (let [formatter (time-format/formatter "yyyy-MM-dd'T'HH:mm:ss+00:00")
         tag-date (-> (str "https://api.bitbucket.org/2.0/repositories/"
                           constants/bitbucket-root-user
                           "/" repo-name
                           "/refs/tags/" (utils/get-tag repo-name version)
                           "?fields=-links,-type,-target.hash,"
                           "-target.repository,-target.author,-target.parents,"
                           "-target.links,-target.type,-target.message")
                      (client/get {:content-type :json
                                   :basic-auth [(:user utils.identity/identity-info)
                                                (:password utils.identity/identity-info)]
                                   :throw-exceptions false
                                   :as :json})
                      (get-in [:body :target :date])
                      ((partial time-format/parse formatter)))
         releases-after-tag-date (->> (get-release-branches)
                                      (map (fn [[k v]] (if (time-core/after? (time-format/parse formatter v)
                                                                             tag-date)
                                                         {v k}
                                                         {})))
                                      (into (sorted-map)))]
;;      (println "******")
;;      (println tag-date)
;;      (println "******")
;;      (pprint releases-after-tag-date)
;;      (println "******")
     (into (sorted-map)
           (map (fn [[rel-date release-name]]
            (let [rel-version (get (get-app-versions-from-manifest-properties release-name) repo-name "not-included")]
              (if (<= (version/version-compare version rel-version) 0)
                { release-name  rel-version}
                {})))
            releases-after-tag-date))))


