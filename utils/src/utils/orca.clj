(ns utils.orca
  ""
(:require [k2nr.docker.core :as docker.core]
          [k2nr.docker.image :as docker.image]))


(defn get-docker-client []
  (docker.core/make-client "127.0.0.1"))


(defn search-image
  ""
  [client term]
  (docker.image/search client term))

