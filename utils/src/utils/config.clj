(ns utils.config
  "This namespae contains the constants and configurations that can change between users and projects.")

(def user-root-path "/Users/fergusonsa")

(def workspace-root (str user-root-path "/CENX"))

(def src-root-dir (str workspace-root "/src"))

(def reports-path (str user-root-path "/reports"))

(def nexus-url-base "http://nexus.cenx.localnet:8081/nexus/content/groups/public/cenx/")

(def library-namespace "cenx")

(def bitbucket-root-user "cenx-cf")

(def default-branch "integration")
