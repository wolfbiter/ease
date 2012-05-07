(ns ease.core
  (:require [noir.server :as server])
  (:use [monger.core :only [connect! set-db! get-db]]
        [monger.collection
         :only [insert insert-batch find-maps remove save any? count]
         :rename {remove del count cnt}]
        [ease.zmq]
        [clojure.set :only [difference intersection]]
        [clojure.pprint :only [pp pprint]]
        [clojure.string :only [replace lower-case] :rename {replace replace-str}])
  (:import (org.jaudiotagger.audio AudioFileIO AudioFile)
           (org.jaudiotagger.audio.mp3 MP3AudioHeader MP3File)
           (org.jaudiotagger.tag FieldKey)
           (org.jaudiotagger.audio.generic AudioFileWriter)
           (java.io File BufferedReader)))

;; For ease

(connect!)                                  ; monger
(set-db! (get-db "ease"))                   ; monger

(def local-addr "tcp://*:5555")             ; zmq
(def mikey-addr "tcp://192.168.1.35:5555")  ; zmq
(def client (connect-client local-addr))    ; zmq

(server/load-views "src/ease/views/")                            ; server
(defn  -main                                                     ; server
  "Must be called with lein trampline run"                       ; server
  [& m]                                                          ; server
  (do (let [mode (keyword (or (first m) :dev))                   ; server
            port (Integer. (get (System/getenv) "PORT" "8080"))] ; server
        (server/start port {:mode mode :ns 'ease}))              ; server
      (clojure.main/repl :init #(ns ease.core))))                ; server

(def ex "main-music/ogg/Derezzed.ogg")
(def ex2 "TestBest/Blaumance.mp3")
(def main-folder "main-music/ogg")
(def main-foldero "TestBest")

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
  (clojure.string/replace (get-k FieldKey/TITLE tag) #"www.*\.[^\s]*" ""))

(defn get-artist [tag]
  (get-k FieldKey/ARTIST tag))

(defn get-album [tag]
  (get-k FieldKey/ALBUM tag))

(defn get-track [tag]
  (try (int (Double/valueOf (get-k FieldKey/TRACK tag)))
       (catch Exception e -1)))

(defn get-bpm [tag]
  (let [bpm (get-k FieldKey/BPM tag)]
    (if (= "" bpm)
      -1
      (int (Double/valueOf bpm)))))

(defn get-genre [tag]
  (get-k FieldKey/GENRE tag))

(defn get-bitrate [header]
  (let [br (.getBitRate header)]
    (if (= "" br)
      -1
      (int (Double/valueOf br)))))

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
                   "vocal" "instrumental"))

(defn viable-mod? [string]
  (if (nil? string)
    false
    (let [lower (.toLowerCase string)]
      (not-every? false? (doall (map #(.contains lower %) viable-mods))))))

(defn mod-title-matches [tag]
  (let [a (re-find #"\(.*\)" (get-title tag))
        b (re-find #" - .+"  (get-title tag))]
    (if (= a nil) b a)))

(defn mod-artist-matches [tag]
  (let [a (re-find #"\(.*\)" (get-artist tag))
        b (re-find #" - .+"  (get-artist tag))]
    (if (= a nil) b a)))

(defn get-mod [tag]
  (let [mod-title (mod-title-matches tag)
        mod-artist (mod-artist-matches tag)]
    (cond (viable-mod? mod-title) (.trim (.replaceAll mod-title "[()-]" ""))
          (viable-mod? mod-artist) (.trim (.replaceAll mod-artist "[()-]" ""))
          :else "")))

(defn choose-title [tag]
  (let [title (get-title tag)
        mod-title (mod-title-matches tag)]
    (if (= mod-title nil)
      title
      (replace-str title mod-title ""))))

(defn choose-artist [tag]
  (let [artist (get-artist tag)
        mod-artist (mod-artist-matches tag)]
    (if (= mod-artist nil)
      artist
      (replace-str artist mod-artist ""))))

(defn song-map [uri]
  (let [tag (get-tag uri)
        header (get-header uri)]
    {:title (choose-title tag)
     :artist (choose-artist tag)
     :mod (get-mod tag)
     :play-history '()
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
        

(def main-music (delay (make-playlist main-folder)))
(def main-musico (delay (make-playlist main-foldero)))

(defn song-diff [song1 song2]
  (let [vals1 (set (vals song1))
        vals2 (set (vals song2))
        vals1-vals2 (difference vals1 vals2)
        vals2-vals1 (difference vals2 vals1)]
    (list vals1-vals2 vals2-vals1)))

(defn eq
  ([mode] (eq "0" mode))
  ([channel mode] (send-command "eq" channel mode)))

(defn vol
  ([lvl] (vol "0" lvl))
  ([channel lvl] (send-command "vol" channel lvl)))

(defn play
  "Plays given song-map, uri, or playlist."
  ([input] (play input "0"))
  ([input channel]
     (cond (map? input) 
           (send-command "play" (:uri input) channel)
           (string? input)
           (send-command "play" input channel)
           :else
           (doall (map #(send-command "play" channel (:uri %)) input)))))

;(defn -main 
;  "Allows this program to be called by 'lein trampoline run'"
;  [& args]
;  (pprint "Entering -main")
;  (loop [input (read-line)]
;    (if (= input "q")
;      (pprint "Exiting -main")
;      (do (pprint
;           (try (eval (read-string input))
;                (catch Exception e (.pprintStackTrace e))))
;          (recur (read-line))))))

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
  "Returns a (play)list of song(s) in given doc-name, the requested song,
   or all songs in doc-name matching given criteria in the form of
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
  (let [n (count-db doc-name input)]
    (if (not (= 0 n))
      (do (println "Really delete" n "song(s)? 'yes' 'details'")
          (loop [response (lower-case (read-line))]
            (cond (.contains response "n")
                  (println "Cancelled")
                  (or (= "" response) (.contains response "y"))
                  (do (println "Deleted all " doc-name "'s matches to:")
                      (pprint (if (= {} input) "Everything" input))
                      (del doc-name input))
                  (.contains response "d")
                  (do (doall (map #(pprint %) (retrieve doc-name input)))
                      (recur (lower-case (read-line))))))))
    (println "No matches to delete.")))

(defn delete
  "Removes A song, a LIST of songs, ALL songs, or simply those matching
   criteria. Returns the WriteResult(s) in a list."
  ([doc-name] (safe-delete doc-name {}))
  ([doc-name input]
     (cond (seq? input)
           (doall (map (partial delete doc-name) input))
           (string? input)
           (delete doc-name {:title input})
           (map? input)
           (safe-delete doc-name input))))

;; Auxiliary