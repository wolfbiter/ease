(ns ease.core
  (:use [monger.core :only [connect! set-db! get-db]]
        [monger.collection
         :only [insert insert-batch find-maps remove save any? count]
         :rename {remove del count cnt}]
        [clojure.set :only [difference intersection]])
  (:import (org.jaudiotagger.audio AudioFileIO AudioFile))
  (:import (org.jaudiotagger.audio.mp3 MP3AudioHeader MP3File))
  (:import (org.jaudiotagger.tag FieldKey))
  (:import (org.jaudiotagger.audio.generic AudioFileWriter))
  (:import (java.io File BufferedReader)))

;; For ease

(connect!)
(set-db! (get-db "ease"))
(def ex "musics/Guilt.mp3")

;; ZMQ Ghost functions

(def connection "tcp://*:5555")

(def sock :sock)

(defn send-command [& parts]
  (do (println "Sent command made with args below:")
      (print (str "     " (first parts)))
      (doall (map #(print (str " " %)) (rest parts)))
      (println)))

;; Clojure wrappers for jaudiotagger methods

(defn get-file [uri]
  (-> (AudioFileIO/read (new File uri))))

(defn get-tag [input]
  (if (string? input)
    (.getTag (get-file input))
    (.getTag input)))

(defn get-k [k tag]
  (.getFirst tag k))

(defn get-artist [tag]
  (get-k FieldKey/ARTIST tag))

(defn get-album [tag]
  (get-k FieldKey/ALBUM tag))

(defn get-title [tag]
  (get-k FieldKey/TITLE tag))

(defn get-track [tag]
  (get-k FieldKey/TRACK tag))

(defn get-bpm [tag]
  (get-k FieldKey/BPM tag))

(defn get-bitrate [input]
  (if (string? input)
  (-> (get-file input)
      .getAudioHeader
      .getBitRate)
  (.getBitRate input)))

(defn get-length [input]
  (if (string? input)
  (-> (get-file input)
      .getAudioHeader
      .getTrackLength)
  (.getTrackLength input)))

(defn get-time []
  (.format
   (java.text.DateFormat/getInstance)
   (. (java.util.Calendar/getInstance) getTime)))

(defn setartist [song text]
  (doall (.setField (get-tag song) FieldKey/ARTIST text)
         (.commit (get-file song))))

(defn setalbum [song text]
  (doall (.setField (get-tag song) FieldKey/ALBUM text)
         (.commit (get-file song))))

(defn setbpm [song text]
  (let [f (get-file song)]
    (.setField (.get-tag f) FieldKey/BPM text)
    (.commit f)))
     
;; Library Methods

(def main-folder "musics")

(defn song-map [uri]
  (let [tag (get-tag uri)
        title (.trim (get-title tag))
        header (.getAudioHeader (get-file uri))]
    {:title title
     :artist (get-artist tag)
     :mod ""
     :album (get-album tag)
     :length (get-length header)
     :track (get-track tag)
     :bpm (get-bpm tag)
     :bitrate (get-bitrate header)
     :uri uri
     :attributes #{}
     :chop '()
     :added (get-time)}))

(defn song-difference [song1 song2]
  (let [vals1 (set (vals song1))
        vals2 (set (vals song2))
        vals1-vals2 (difference vals1 vals2)
        vals2-vals1 (difference vals2 vals1)]
    (list (list vals1-vals2) (list vals2-vals1))))

(defn make-playlist
  "Creates a (play)list of song-maps from the given folder."
  [folder]
  (let [uris (.list (new File folder))
        path (str folder "/")]
    (for [uri uris]
      (song-map (str path uri)))))

(def main-music (make-playlist main-folder))

(defn eq
  ([mode] (eq mode 0))
  ([mode channel] (send-command "eq" mode channel)))

(defn vol
  ([lvl] (vol lvl 0))
  ([lvl channel] (send-command "vol" lvl channel)))

(defn play
  ([input] (play input 0))
  ([input channel]
     (cond (map? input) 
           (send-command "play" (:uri input) channel)
           (string? input)
           (send-command "play" input channel)
           :else
           (doall (map #(send-command "play" (:uri %) channel) input)))))

(defn -main 
  "Allows this program to be called by 'lein trampoline run'"
  [& args]
  (println "Entering -main")
  (loop [input (read-line)]
    (if (= input "q")
      (println "Exiting -main")
      (do (println
           (try (eval (read-string input))
                (catch Exception e (.printStackTrace e))))
          (recur (read-line))))))


;; MongerDB interop

(defn check-dupes [doc-name input]
  (cond (seq? input)
        (doall (map (partial check-dupes doc-name) input))
        (map? input)
        (if (any? doc-name {:title (:title input)})
          (println "Saved, but you just duplicated title: " (:title input))
          (println "Saved: " (:title input)))))

(defn store
  "Stores the given playlist/song into document 'doc-name'"
  [doc-name input]
  (cond (map? input)
        (do (check-dupes doc-name input)
            (save doc-name input))
        (string? input)
        (let [song (song-map input)]
          (do (check-dupes doc-name song)
              (save doc-name song)))
        :else
        (do (check-dupes doc-name input)
            (insert-batch doc-name (vec input)))))

(defn retrieve
  "Returns a (play)list of songs in given doc-name, the requested song, or all songs
   in doc-name matching given criteria in the form of {:key1 value1 :key2 value2...}"
  ([doc-name] (find-maps doc-name))
  ([doc-name criteria]
     (if (string? criteria)
       (find-maps doc-name {:title criteria})
       (find-maps doc-name criteria))))

(defn delete
  "Removes A song, a LIST of songs, ALL songs, or simply those matching criteria"
  ([doc-name] (del doc-name))
  ([doc-name input]
     (cond (seq? input)
           (doall (map (partial delete doc-name) input))
           (string? input)
           (del doc-name (song-map input))
           (map? input)
           (del doc-name input))))

(defn count-list
  "Counts the number of entries in the given doc, overloaded for criteria"
  ([doc-name] (cnt doc-name))
  ([doc-name criteria] (cnt doc-name criteria)))