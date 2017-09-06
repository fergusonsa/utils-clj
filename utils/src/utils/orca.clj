(ns utils.orca
  ""
(:require [k2nr.docker.core :as docker.core]
          [k2nr.docker.image :as docker.image]))


(defn get-docker-client
  ([]
    (get-docker-client "127.0.0.1:4243"))
  ([host]
   (docker.core/make-client host)))


(defn search-image
  ""
  ([term]
   (search-image (get-docker-client) term))
  ([client term]
   (docker.image/search client term)))

