(ns ease.core
  (:use [monger.core :only [connect! set-db! get-db]]
        [monger.collection
         :only [insert insert-batch find-maps remove save any? count]
         :rename {remove del count cnt}]
        [clojure.set :only [difference intersection]]
        [clojure.pprint :only [pprint]])
  (:import (org.jaudiotagger.audio AudioFileIO AudioFile))
  (:import (org.jaudiotagger.audio.mp3 MP3AudioHeader MP3File))
  (:import (org.jaudiotagger.tag FieldKey))
  (:import (org.jaudiotagger.audio.generic AudioFileWriter))
  (:import (java.io File BufferedReader)))

;; For ease
 
(connect!)
(set-db! (get-db "ease"))
(def ex "main-music/mp3/Guilt.mp3")
(def exo "main-music/ogg/Guilt.ogg")
(def prob "main-music/mp3/OpenYourEyes.mp3")
(def probo "main-music/ogg/OpenYourEyes.ogg")
(def main-folder "main-music/mp3")
(def main-foldero "main-music/ogg")

;; ZMQ Ghost Function

(defn send-command [& parts]
  (do (println "Sent command made with args below:")
      (print (first parts))
      (doall (map #(print (str " " %)) (rest parts)))
      (println)))

;; Clojure Jaudiotagger Interop

(defn get-tag [uri])

(defn get-time []
  (.format
   (java.text.DateFormat/getInstance)
   (. (java.util.Calendar/getInstance) getTime)))

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

;; Library Methods

(defn song-diff [song1 song2]
  (let [vals1 (set (vals song1))
        vals2 (set (vals song2))
        vals1-vals2 (difference vals1 vals2)
        vals2-vals1 (difference vals2 vals1)]
    (list vals1-vals2 vals2-vals1)))

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
  "Plays given song, uri, or playlist."
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
  (pprint "Entering -main")
  (loop [input (read-line)]
    (if (= input "q")
      (pprint "Exiting -main")
      (do (pprint
           (try (eval (read-string input))
                (catch Exception e (.pprintStackTrace e))))
          (recur (read-line))))))

;; MongerDB interop

(defn print-store [doc-name input]
  (cond (seq? input)
        (doall (map (partial print-store doc-name) input))
        (map? input)
        (if (any? doc-name {:title (:title input)})
          (pprint (str "Saved, but you just duplicated title: " (:title input)))
          (pprint (str "Saved: " (:title input))))))

(defn store
  "Stores the given playlist/song into document 'doc-name'. Takes a song-map,
   uri, or list of song-maps. Returns WriteResults for each stored song."
  [doc-name input]
  (cond (map? input)
        (do (print-store doc-name input)
            (save doc-name input))
        (string? input)
        (store doc-name (song-map input))
        (seq? input)
        (do (print-store doc-name input)
            (insert-batch doc-name (vec input)))))

(defn retrieve
  "Returns a (play)list of songs in given doc-name, the requested song, or all songs
   in doc-name matching given criteria in the form of {:key1 value1 :key2 value2...}"
  ([doc-name] (find-maps doc-name))
  ([doc-name criteria]
     (if (string? criteria)
       (find-maps doc-name {:title criteria})
       (find-maps doc-name criteria))))

(defn print-delete [doc-name input]
  (do (pprint (str "Deleted: " (count-db doc-name input) " copies matching:"))
      (pprint input)))
        
(defn delete
  "Removes A song, a LIST of songs, ALL songs, or simply those matching criteria.
   Returns the WriteResult(s) in a list."
  ([doc-name] (del doc-name))
  ([doc-name input]
     (cond (seq? input)
           (doall (map (partial delete doc-name) input))
           (string? input)
           (delete doc-name {:title input})
           (map? input)
           (do (print-delete doc-name input)
               (del doc-name input)))))

(defn count-db
  "Counts the number of entries in the given doc, overloaded for criteria"
  ([doc-name] (cnt doc-name))
  ([doc-name criteria] (cnt doc-name criteria)))