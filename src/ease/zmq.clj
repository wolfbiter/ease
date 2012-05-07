;; Core ease.zmq functionality methods

(ns ease.zmq
  (:import (org.zeromq ZMQ ZMsg)))

(def +req+ 3)
(def +rep+ 4)

;; JZMQ Interop

(defn make-context [io-threads]
  (ZMQ/context io-threads))

(defn make-socket [context socket-type]
  (.socket context socket-type))

(defn set-socket-option [socket option value]
  (.setsockopt socket option value))

(defn bind [socket address]
  (.bind socket address))

(defn connect [socket address]
  (.connect socket address))

(defn send-msg [[ctx sock :as node] msg]
  (. msg send sock))

(defn recv-msg [[ctx sock :as node]]
  (ZMsg/recvMsg sock))


;; Messaging

(defn request-reply [node msg]
  (do (send-msg node msg)
      (recv-msg node)))

(defn reply-request [node handler]
  (send-msg node (handler (recv-msg node))))

(defn strs->msg
  "Returns a Clojure-wrapped ZMQ multipart message of the given strings"
  ([] (new ZMsg))
  ([& parts]
    (let [msg (new ZMsg)]
      (do (doseq [part parts]
            (. msg addString part))
          msg))))

(defn msg->strs
  "Reads a ZMQ multi-part message into a vector of strings. Destroys message."
  ([msg] (msg->strs msg []))
  ([msg sofar]
    (let [nxt (. msg popString)]
      (if nxt
        (recur msg (conj sofar nxt))
        sofar))))

;; Node Creation

(defn make-ctx
  "Calls make-context with number of io threads. Defaults to 1."
  ([] (make-context 1))
  ([n] (make-context n)))

(defn make-node [type to ctx addr]
  (let [sock (make-socket ctx type)]
    (do (to sock addr)
        [ctx sock])))

(defn get-ctx [[ctx sock]]
  ctx)

(defn get-sock [[ctx sock]]
  sock)

(def make-client (partial make-node +req+ connect))
(def make-server (partial make-node +rep+ bind))

(defn echo-handler [msg]
  (let [strs (msg->strs msg)]
    (do (apply println strs))
    (apply strs->msg strs)))

;; Node Application

(defn connect-client
  "Connects and returns a new server of given address."
  ([addr] (connect-client addr 1))
  ([addr n]
     (ref (make-client (make-ctx n) addr))))

(defn connect-server
  "Connects and returns a new server of given address."
  ([addr] (connect-server addr 1))
  ([addr n]
  (ref (make-server (make-ctx n) addr))))

(defn activate [node]
  (if (delay? @node)
    (dosync
     (let [activated-node (force @node)]
       (ref-set node activated-node)))
    node))

(defn automate-server [handler addr]
  (let [server (connect-server addr)]
    (loop []
        (do (reply-request @server handler)
            (recur)))))