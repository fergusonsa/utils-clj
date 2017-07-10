(ns utils.fake
  "This namespace contains faked functions that may be required by config files so that
  they can be loaded using 'clojure.core/load-string.
  To use, include a function of the same signature as being called in this namespace
  and then include the following in the :requires references in 'utils.local_environment:
     [utils.fake :as <original-namespace>]
  (refer 'utils.local_environment) ")

(defn hash-bcrypt [& args]
  args)
