(ns utils.main
  ""
  (:require [clj-http.client :as client]
            [cheshire.core :as json]
            [clj-time.format :as time-format]
            [clojure.tools.reader.edn :as edn]
            [clojure.string]
            [utils.identity]
            [utils.constants :as constants]
            [utils.environments :as environments]
            [utils.configuration :as configuration]
            [utils.dependencies :as dependencies]
            [utils.repositories :as repositories]
            [utils.core :as utils]
            [version-clj.core :as version])
  (:use clojure.pprint
        clojure.core
        [clojure.set :only [difference intersection]]
        [slingshot.slingshot :only [try+]]))
