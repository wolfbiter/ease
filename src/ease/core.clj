(ns ease.core
  (:require [noir.server :as server])
  (:use [ease.song]
        [ease.zmq]
        [ease.mongo]
        [ease.dupes]
        [ease.debug]
        [clojure.java.shell :only [sh]]
        [clojure.pprint :only [pp pprint]]
        [clojure.string :only [lower-case]]))

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
;(def get-attr get-attribute)

(def five (lazy-seq (make-playlist "Best/5Stars")))
(def four-half (lazy-seq (make-playlist "Best/4.5Stars")))
(def four (lazy-seq (make-playlist "Best/4Stars")))
(def three (lazy-seq (make-playlist "Best/3Stars")))
(def three-half (lazy-seq (make-playlist "Best/3.5Stars")))
(def o-source (lazy-seq (make-playlist "Best/OpenSource")))
(def best-music (list five four-half four three-half three o-source))

;; ZMQ Interface Functions

(defn send-command
  "Sends a ZMQ multi-message to @addr with the given string arguments"
  [& parts]
  (let [msg (apply strs->msg parts)]
    (msg->strs (request-reply @client msg))))

(defn get-readable-name [string]
  (.replaceAll (re-find #"\$.*\@" string) "[@$]" ""))

(defmacro new-command
  "Creates a generic command that defaults selection of channel to 0.
   num-extra-inputs refers to the desired number of inputs for the new command,
   DISCLUDING the assumed channel."
  [command-name num-extra-inputs]
    `(defn ~command-name
       [& inputs#]
       (let [name# (get-readable-name (str ~command-name))
             count# (count inputs#)]
         (cond (= count# ~num-extra-inputs)
               (apply send-command (flatten (list name# "0" inputs#)))
               (= count# (+ 1 ~num-extra-inputs))
               (apply send-command (flatten (list name# inputs#)))
               :else
               (println "No dice; I expected optional [channel] followed by"
                        ~num-extra-inputs "inputs.")))))

(defn print-command [& parts]
  (do (println "Sent command made with args below:")
      (print (first parts))
      (doall (map #(print (str " " %)) (rest parts)))
      (println)))

;; Library Methods

(new-command vol 1)     ; Changes the volume of channel to given double in range 0-1

(new-command status 0)  ; Returns gstreamer status of channel

(new-command skip 0)    ; Skips to next song in queue

(new-command length 0)  ; Returns current queue length

(new-command pause 0)    ; Pauses the channel

(new-command stop 0)    ; Pauses the channel and removes all songs in that queue

(new-command now-playing 0)    ; Returns uri of what's playing in channel

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
  ([] (do (if (= 0 (length))
            (queue "0" (shuffle (retrieve "best" :favorite))))
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

;; Auxiliary

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
