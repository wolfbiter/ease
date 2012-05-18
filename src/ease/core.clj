(ns ease.core
  (:require [noir.server :as server])
  (:use [ease.song]
        [ease.zmq]
        [ease.mongo]
        [clojure.java.shell :only [sh]]
        [clojure.pprint :only [pp pprint]]
        [clojure.string :only [replace lower-case] :rename {replace replace-str}]))

;; For ease

(activate-db "ease")     ; mongo

(def local-addr "tcp://*:5555")             ; zmq
(def mikey-addr "tcp://192.168.1.35:5555")  ; zmq
(def client (connect-client mikey-addr))    ; zmq
(def mikey-test "file:///storage/test.mp3") ; zmq

(server/load-views "src/ease/views/")                            ; server
(defn -main                                                      ; server
  "Must be called with lein trampoline run"                      ; server
  [& m]                                                          ; server
  (do (let [mode (keyword (or (first m) :dev))                   ; server
            port (Integer. (get (System/getenv) "PORT" "8080"))] ; server
        (server/start port {:mode mode :ns 'ease}))              ; server
      (clojure.main/repl :init #(ns ease.core))))                ; server

(def ex "Test/Remember The Name 1.mp3")
(def test-folder "Best/4.5Stars")
(def main-folder "Best")
(def main-music (lazy-seq (make-playlist main-folder)))
(def test-music (lazy-seq (make-playlist test-folder)))

(def best (partial retrieve "best"))
(def attr add-attribute)
(def rm-attr remove-attribute)

(def five (lazy-seq (make-playlist "Best/5Stars")))
(def four-half (lazy-seq (make-playlist "Best/4.5Stars")))
(def four (lazy-seq (make-playlist "Best/4Stars")))
(def three (lazy-seq (make-playlist "Best/3Stars")))
(def three-half (lazy-seq (make-playlist "Best/3.5Stars")))
(def o-source (lazy-seq (make-playlist "Best/OpenSource")))
(def best-music (list five four-half four three-half three o-source))

;; ZMQ Interface Function

(defn send-command
  "Sends a ZMQ multi-message to @addr with the given string arguments"
  [& parts]
  (let [msg (apply strs->msg parts)]
    (msg->strs (request-reply @client msg))))

(defn print-command [& parts]
  (do (println "Sent command made with args below:")
      (print (first parts))
      (doall (map #(print (str " " %)) (rest parts)))
      (println)))

;; Library Methods

(defn eq
  ([mode] (eq "0" mode))
  ([channel mode] (send-command "eq" channel mode)))

(defn vol
  ([lvl] (vol "0" lvl))
  ([channel lvl] (send-command "vol" channel lvl)))

(defn status
  ([] (status "0"))
  ([channel] (send-command "status" channel)))

(defn skip
  ([] (skip "0"))
  ([channel] (send-command "skip" channel)))

(defn length
  ([] (length "0"))
  ([channel] (send-command "length" channel)))

(defn pause
  "Pauses the channel"
  ([] (pause "0"))
  ([channel] (send-command "pause" channel)))

(defn stop
  "Pauses the channel and removes all queued songs."
  ([] (stop "0"))
  ([channel] (send-command "stop" channel)))

(defn queue
  "Queues given song-map, uri, or playlist into given channel (default 0)."
  ([input] (queue "0" input))
  ([channel input]
     (cond (map? input) 
           (send-command "queue" channel (:uri input))
           (string? input)
           (send-command "queue" channel input)
           (coll? input)
           (doall (map #(send-command "queue" channel (:uri %)) input)))))

(defn play
  "Plays given input on given channel. Defaults to shorthand for playing shuffled
   best on channel 0."
  ([] (do (queue "0" (shuffle (retrieve "best" :favorite)))
          (play "0")))
  ([input] (cond (string? input)
                 (send-command "play" input)
                 (or (map? input) (coll? input))
                 (play "0" input)))
  ([channel input]
     (cond (map? input)
           (do (send-command "push" channel (:uri input))
               (play channel))
           (coll? input)
           (do (send-command "push" channel (:uri (first input)))
               (play channel)
               (doall (map #(queue channel %) (rest input)))))))

(comment 
  (defn -main 
    "Allows this program to be called by 'lein trampoline run'"
    [& args]
    (pprint "Entering -main")
    (loop [input (read-line)]
      (if (= input "q")
        (pprint "Exiting -main")
        (do (pprint
             (try (eval (read-string input))
                  (catch Exception e (.pprintStackTrace e))))
            (recur (read-line)))))))

;; Auxiliary
