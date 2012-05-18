(ns ease.test.core
  (:use [ease.core]
        [ease.mongo]
        [ease.song]
        [ease.zmq]
        [midje.sweet]))

(fact (+ 2 2) => 5)