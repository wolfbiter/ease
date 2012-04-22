;; Core ZMQ functionality methods

(ns ease.zmq
  (:use [org.zeromq.clojure])
  (:import (org.zeromq ZMQ ZMsg)))
  
  
(def connection "tcp://*:5555")

(defn send-message [sock msg]
  (. msg send sock))

(defn recv-message [sock]
  (ZMsg/recvMsg sock))

(defn request-reply [sock msg]
  (do (send-message sock msg)
      (recv-message sock)))

(defn reply-request [sock handler]
  (send-message sock (handler (recv-message sock))))

(defn make-node [type connection]
  (let [ctx (make-context 1)
        sock (make-socket ctx type)]
    (do (connect sock connection)
        [ctx sock])))

(def make-client (partial make-node +req+))
(def make-server (partial make-node +rep+))

(defn make-message
  "Returns a Clojure-wrapped ZMQ multipart message of the given strings"
  [& parts]
  (let [msg (new ZMsg)]
    (do (doseq [part parts] (. msg addString part))
        msg)))

(defn message-str
  "Reads a ZMQ multi-part message into a single string. Destroys message."
  ([msg] (message-str msg ""))
  ([msg sofar]
    (let [nxt (. msg popString)]
      (if nxt
        (recur msg (str sofar " " nxt))
        sofar))))

(def sock (make-client connection))