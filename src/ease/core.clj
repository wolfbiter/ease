(ns ease.core
  (:use [monger.core :only [connect! set-db! get-db]]
        [monger.collection
         :only [insert insert-batch find-maps remove save any? count]
         :rename {remove del count cnt}]
        [clojure.set :only [difference intersection]]
        [clojure.pprint :only [pp pprint]]
        [clojure.string :only [replace lower-case] :rename {replace replace-str}])
  (:import (org.jaudiotagger.audio AudioFileIO AudioFile)
           (org.jaudiotagger.audio.mp3 MP3AudioHeader MP3File)
           (org.jaudiotagger.tag FieldKey)
           (org.jaudiotagger.audio.generic AudioFileWriter)
           (java.io File BufferedReader)))

;; For ease
 
(connect!)
(set-db! (get-db "ease"))
(def ex "main-music/ogg/Guilt.ogg")
(def main-folder "main-music/ogg")

;; ZMQ Ghost Function

(defn send-command [& parts]
  (do (println "Sent command made with args below:")
      (print (first parts))
      (doall (map #(print (str " " %)) (rest parts)))
      (println)))

;; Clojure Jaudiotagger Interop, with some spice.

(defn get-file [uri]
  (if (string? uri)
    (AudioFileIO/read (new File uri))
    (AudioFileIO/read uri)))

(defn get-tag [uri]
  (.getTag (get-file uri)))

(defn get-header [uri]
    (.getAudioHeader (get-file uri)))

(defn get-k [k tag]
  (.getFirst tag k))

(defn get-title [tag]
  (get-k FieldKey/TITLE tag))

(defn get-artist [tag]
  (get-k FieldKey/ARTIST tag))

(defn get-album [tag]
  (get-k FieldKey/ALBUM tag))

(defn get-track [tag]
  (get-k FieldKey/TRACK tag))

(defn get-bpm [tag]
  (int (Double/valueOf (get-k FieldKey/BPM tag))))

(defn get-genre [tag]
  (get-k FieldKey/GENRE tag))

(defn get-bitrate [header]
    (.getBitRate header))

(defn get-length [header]
    (.getTrackLength header))

(defn get-uri [uri]
  (if (string? uri)
    uri
    (.getPath uri)))

(defn get-time []
  (.format
   (java.text.DateFormat/getInstance)
   (. (java.util.Calendar/getInstance) getTime)))

(def viable-mods '("mix" "edit" "original" "extended" "rework" "bootleg"
                   "feat" "with" "vocal" "instrumental"))

(defn viable-mod? [string]
  (if (nil? string)
    false
    (let [lower (.toLowerCase string)]
      (not-every? false? (doall (map #(.contains lower %) viable-mods))))))

(defn get-mod-title [tag]
  (re-find #"\(.*\)" (get-title tag)))

(defn get-mod-artist [tag]
  (re-find #"\(.*\)" (get-artist tag)))

(defn get-mod [tag]
  (let [mod-title (get-mod-title tag)
        mod-artist (get-mod-artist tag)]
    (cond (viable-mod? mod-title) (.replaceAll mod-title "[()]" "")
          (viable-mod? mod-artist) (.replaceAll mod-artist "[()]" "")
          :else "")))

(defn choose-title [tag]
  (let [title (get-title tag)
        mod-title (get-mod-title tag)]
    (if (= mod-title nil)
      title
      (replace-str title mod-title ""))))

(defn choose-artist [tag]
  (let [artist (get-artist tag)
        mod-artist (get-mod-artist tag)]
    (if (= mod-artist nil)
      artist
      (replace-str artist mod-artist ""))))

(defn song-map [uri]
  (let [tag (get-tag uri)
        header (get-header uri)
        mod (get-mod tag)]
    {:title (.trim (choose-title tag))
     :artist (get-artist tag)
     :mod (get-mod tag)
     :plays 0
     :album (get-album tag)
     :length (get-length header)
     :genre (get-genre tag)
     :track (get-track tag)
     :bpm (get-bpm tag)
     :bitrate (get-bitrate header)
     :uri (get-uri uri)
     :attributes #{}
     :chop '()
     :added (get-time)}))

;; Library Methods

(defn make-playlist
  "Creates a (play)list of song-maps from the given folder and all subdirectories."
  [folder]
  (cond (string? folder)
        (make-playlist (new File folder))
        (= File (class folder))
        (if (.isDirectory folder)
          (let [uris (.list folder)
                path (str folder "/")]
            (flatten (map #(make-playlist (str path %)) uris)))
          (song-map folder))))
        

(def main-music (make-playlist main-folder))

(defn song-diff [song1 song2]
  (let [vals1 (set (vals song1))
        vals2 (set (vals song2))
        vals1-vals2 (difference vals1 vals2)
        vals2-vals1 (difference vals2 vals1)]
    (list vals1-vals2 vals2-vals1)))

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

(defn count-db
  "Counts the number of entries in the given doc, overloaded for criteria"
  ([doc-name] (cnt doc-name))
  ([doc-name criteria] (cnt doc-name criteria)))

(defn print-store
  "Simple print statement that lists deletes."
  [doc-name input]
  (cond (seq? input)
        (doall (map (partial print-store doc-name) input))
        (map? input)
        (if (any? doc-name {:title (:title input)})
          (pprint (str "Stored, but you just duplicated title: " (:title input)))
          (pprint (str "Stored: " (:title input))))))

(defn store
  "Stores the given playlist/song into document 'doc-name'. Takes a song-map,
   uri, or seq of song-maps. Returns WriteResults for each stored song."
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
  "Returns a (play)list of song(s) in given doc-name, the requested song, or all
   songs in doc-name matching given criteria in the form of
   {:key1 val1 :key2 val2...}"
  ([doc-name] (find-maps doc-name))
  ([doc-name criteria]
     (if (string? criteria)
       (find-maps doc-name {:title criteria})
       (find-maps doc-name criteria))))

(defn safe-delete
  "Prompts the user to make sure they REALLY want to do it...
   'd' 'y' '<enter>' are shortcuts."
  [doc-name input]
  (do (println "Really delete"
               (count-db doc-name input)
               "song(s)? 'yes' 'details'")
      (loop [response (lower-case (read-line))]
        (cond (.contains response "n") nil
              (or (= "" response) (.contains response "y"))
              (do (println "Deleted all matches to:")
                  (pprint input)
                  (del doc-name input))
              (.contains response "d")
              (do (doall (map #(println %) (retrieve doc-name input)))
                  (recur (lower-case (read-line))))))))
        
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
           (safe-delete doc-name input))))
