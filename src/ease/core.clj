(ns ease.core
  (:use org.zeromq.clojure)
  (:import (org.zeromq ZMQ ZMsg))
  (:import (org.jaudiotagger.audio AudioFileIO AudioFile))
  (:import (org.jaudiotagger.audio.mp3 MP3AudioHeader MP3File))
  (:import (org.jaudiotagger.tag FieldKey))
  (:import (org.jaudiotagger.audio.generic AudioFileWriter))
  (:import (java.io File BufferedReader)))

;; Core metadata functions. Uri is given as a string.
        
(defn getbitrate [uri]
  (-> (AudioFileIO/read (new File uri))
      .getAudioHeader
      .getBitRate))
    
(defn getfile [uri]
  (-> (AudioFileIO/read (new File uri))))

(defn gettag [uri]
  (.getTag (getfile uri)))

(defn getartist [uri]
      (.getFirst (gettag uri) FieldKey/ARTIST))

(defn getalbum [uri]
  (.getFirst (gettag uri) FieldKey/ALBUM))

(defn getname [uri]
      (.getFirst (gettag uri) FieldKey/TRACK))

(defn getbpm [uri]
      (.getFirst (gettag uri) FieldKey/BPM))

(defn setartist [uri text]
  (doall (.setField (gettag uri) FieldKey/ARTIST text)
         (.commit (getfile uri))))

(defn setalbum [uri text]
  (doall (.setField (gettag uri) FieldKey/ALBUM text)
         (.commit (getfile uri))))

(defn setbpm [uri text]
  (let [f (getfile uri)]
    (.setField (.getTag f) FieldKey/BPM text)
    (.commit f)))


;; Core functionality methods

(def connection "tcp://192.168.1.35:5555")

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

(defn make-message [& parts]
  (let [msg (new ZMsg)]
    (do (doseq [part parts] (. msg addString parts))
        msg)))

(defn message-str
  "Concatenates multi-part message to a single string"
  ([msg] (message-str msg ""))
  ([msg sofar]
    (let [nxt (. msg popString)]
      (if nxt
        (recur msg (str sofar " " nxt))
        sofar))))

(defn play [string])
  


(defn add [uri]
  (let [artist (getartist uri)
        album (getalbum uri)
        bpm (getbpm uri)
        name (getname uri)])
  ;; take the file, copy it over as we want, etc
  )
        

;; Allows this program to be called by "lein trampoline run"

(defn -main [& args]
  (println "Entering -main")
  (loop [input (read-line)]
    (if (= input "q")
      (println "Exiting -main")
      (do (println
           (try (eval (read-string input))
                (catch Exception e (.printStackTrace e))))
          (recur (read-line))))))