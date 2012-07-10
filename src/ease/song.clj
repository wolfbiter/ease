;; Clojure Jaudiotagger Interop, with some spice.

; Methods of interest: song-map, make-playlist, song-diff, viable-mods

(ns ease.song
  (:require ease.config)
  (:use [clojure.string :only [replace lower-case] :rename {replace replace-str}]
        [clojure.set :only [difference intersection]]
        [ease.debug])
  (:import (org.jaudiotagger.audio AudioFileIO AudioFile)
           (org.jaudiotagger.audio.mp3 MP3AudioHeader MP3File)
           (org.jaudiotagger.tag FieldKey)
           (org.jaudiotagger.audio.generic AudioFileWriter)
           (org.jaudiotagger.tag.datatype Artwork)
           (java.io File BufferedReader)))

(def viable-mods '("remix" "mix" "edit" "original" "extended" "rework" "bootleg"
                   "vocal" "instrumental"))

(defn remove-mod [mod]
  (let [edited (atom mod)]
    (doall (map #(swap! edited replace-str % "") viable-mods))
    @edited))

(defn normalize
  "Filters noise from given string so as to make more relevant matches."
  [string]
  (lower-case (.replace string " " "")))

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

(defn remove-ads [tag key]
  (clojure.string/replace (get-k key tag) #"www.*\.[^\s]*" ""))

(defn get-title [tag]
  (remove-ads tag FieldKey/TITLE))

(defn get-artist [tag]
  (remove-ads tag FieldKey/ARTIST))

(defn get-album [tag]
  (remove-ads tag FieldKey/ALBUM))

(defn get-art [tag]
  (if-let [art (.getFirstArtwork tag)]
    (.getImage art)))

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
  (let [part (if (string? uri)
               uri
               (.getPath uri))]
        (str "file://" ease.config/data-dir part)))

(defn get-time []
  (.format
   (java.text.DateFormat/getInstance)
   (. (java.util.Calendar/getInstance) getTime)))

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
          :else "Original Mix")))

(defn choose-title [tag]
  (let [title (get-title tag)
        mod-title (mod-title-matches tag)]
    (if (= mod-title nil)
      title
      (.trim (replace-str title mod-title "")))))

(defn choose-artist [tag]
  (let [artist (get-artist tag)
        mod-artist (mod-artist-matches tag)]
    (if (= mod-artist nil)
      artist
      (.trim (replace-str artist mod-artist "")))))

(defn song-map [uri]
  (let [tag (get-tag uri)
        header (get-header uri)
        time (get-time)
        title (choose-title tag)
        artist (choose-artist tag)
        mod (get-mod tag)]
    {:title title
     :match [(normalize title) (remove-mod (normalize mod)) (normalize artist)]
     :artist artist
     :mod mod
     :plays '[]
     :album (get-album tag)
     :length (get-length header)
     :genre (get-genre tag)
     :track (get-track tag)
     :bpm (get-bpm tag)
     :bitrate (get-bitrate header)
     :attributes []
     :uri (get-uri uri)
     :chop '[]
     :date-added time
     :edits []
     :art "art!"})) ;(get-art tag)}))

(defn make-playlist
  "Creates a (play)list of song-maps from the given folder and all
   subdirectories."
  [folder]
  (cond (string? folder)
        (make-playlist (new File folder))
        (= File (class folder))
        (if (.isDirectory folder)
          (let [uris (.list folder)
                path (str folder "/")]
            (flatten (map #(make-playlist (str path %)) uris)))
          (song-map folder))))

(defn song-diff [song1 song2]
  (let [vals1 (set (vals song1))
        vals2 (set (vals song2))
        vals1-vals2 (difference vals1 vals2)
        vals2-vals1 (difference vals2 vals1)]
    (list vals1-vals2 vals2-vals1)))