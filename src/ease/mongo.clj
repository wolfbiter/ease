;; MongerDB interop

; Entry methods: activate-db, count-db, ADD, RETRIEVE, DELETE, SEARCH
;                get-info, delete-dupes, any?, EDIT, add-attribute, remove-attribute

(ns ease.mongo
  (:use
   [ease.debug]
   [ease.song]
   [clojure.pprint :only [pp pprint]]
   [clojure.string :only [replace lower-case] :rename {replace replace-str}]
   [monger.core :only [connect! set-db! get-db]]
   [monger.collection
    :only [update insert insert-batch find-maps remove save any? count]
    :rename {remove del count cnt any? any}]))

(defn conj-val
  "Conjoins given input to the value corresponding to given keyword in given map.
   Assumes value in map is a vector, as this is what mongerDB uses.
   ex: (conj-val {:hi ['test']} :hi 'two') returns {:hi ['test 'two']}."
  [mp kw input]
  {kw (conj (kw mp) input)})
  
(defn any?
  "Returns true if given playlist contains anything, overloaded for criteria."
  ([playlist] (any playlist))
  ([playlist criteria] (any playlist criteria)))

(defn count-db
  "Counts the number of entries in the given playlist, overloaded for criteria"
  ([playlist] (cnt playlist))
  ([playlist criteria] (cnt playlist criteria)))

(defn print-add
  "Simple print statement that lists deletes. Takes a song or playlist."
  [playlist input]
  (cond (map? input)
        (do (pprint (str "Added: " (:title input)))
            (if (any? playlist {:title (:title input)})
              (pprint (str "You just duplicated title: " (:title input)))))
        (coll? input)
        (doall (map (partial print-add playlist) input))))

(defn add
  "Stores the given playlist/song into document 'playlist'. Takes a song-map,
   uri*, or seq of song-maps or uris, OR a seq of SEQS of song-maps or uris.
   Returns WriteResults for each stored song. *note, uri must be .mp3 .ogg or .flac"
  [playlist input]
  (cond (map? input)
        (do (print-add playlist input)
            (save playlist input))
        (string? input)
        (add playlist (song-map input))
        (coll? input)
        (try
          ; success for flat colls
          (do (print-add playlist input)
              (insert-batch playlist (vec input)))
          ; retry for nested colls
          (catch Exception e (doall (map #(add playlist %) input))))))

(defn full-search
  "Returns all songs that have any field containing given query. Slow but thorough."
  ([query] (full-search "best" query))
  ([playlist query] "WRITE THIS"))

(defn search
  "Returns all songs in playlist that have given query in match. Defaults to best."
  ([query] (search "best" query))
  ([playlist query]
     (find-maps playlist {:match {"$in" [query]}})))
                             

(defn retrieve
  "Returns a (play)list of song(s) in given stored playlist, the requested song,
   or all songs in playlist matching given criteria in the form of
   {:key1 val1 :key2 val2...}. If no matches, will try a search."
  ([playlist] (find-maps playlist))
  ([playlist criteria]
     (if-let [response (try (find-maps playlist criteria)
                            (catch Exception e))]
       response
       (search playlist (if (map? criteria)
                          (:match criteria)
                          (normalize criteria))))))

(defn edit
  "Updates given song(s) in stored playlist with the map of criteria presented.
   Can take a title string to represent a song already in given playlist.
   Defaults to writing to playlist 'best'. Note: updates edit-history.
   ex: (edit 'best' 'ecstasy' {:bpm 200 :title 'wired'})."
  ([input criteria] (edit "best" input criteria))
  ([playlist input criteria]
     (cond (map? input)
           (update playlist
                   {:_id (:_id input)}
                   {"$set" (conj criteria
                                 (conj-val input :edits (str criteria)))})
           (string? input)
           (edit playlist (retrieve playlist input) criteria)
           (coll? input)
           (doall (map #(edit playlist % criteria) input)))))

(defn add-attribute
  "Adds given element as an attribute to given song(s) in given stored playlist.
   Can take a title string to represent a song already in given playlist.
   ex: (add-attribute 'jazz' (retrieve 'jazz' 'Saxy') 'groovy'). Defaults to best."
  ([input attr] (add-attribute "best" input attr))
  ([playlist input attr]
     (cond (map? input)
           (edit playlist input {:attributes (conj (dprint :attributes input) attr)})
           (string? input)
           (add-attribute playlist (retrieve playlist input))
           (coll? input)
           (doall (map #(add-attribute playlist % attr) input)))))

(defn remove-attribute
  "Removes given attribute from given song(s) in stored playlist. Defaults to best.
   ex: (remove-attribute 'best' five-stars 'boring'). "
  ([input attr] (remove-attribute "best" input attr))
  ([playlist input attr]
     (cond (map? input)
           (edit playlist input {:attributes (disj (set (:attributes input)) attr)})
           (string? input)
           (remove-attribute playlist (retrieve playlist input) attr)
           (coll? input)
           (doall (map #(remove-attribute playlist % attr) input)))))

(defn safe-delete
  "Prompts the user to make sure they REALLY want to do it...
   'd' 'y' '<enter>' are shortcuts."
  [playlist input]
  (let [n (count-db playlist input)]
    (if (not (= 0 n))
      (do (println "Really delete" n "song(s)? 'yes' 'details'")
          ; Loop to allow for asking details
          (loop [response (lower-case (read-line))]
            (cond (.contains response "n")
                  (println "Cancelled")
                  (or (= "" response) (.contains response "y"))
                  (do (print "Deleted all matches matches in" playlist "to: ")
                      (println (if (= {} input) "Everything." input))
                      (del playlist input))
                  (.contains response "d")
                  (do (doall (map #(pprint %) (retrieve playlist input)))
                      (recur (lower-case (read-line)))))))
      (println "No matches to delete."))))

(defn delete
  "Removes A song-map, a LIST of song-maps, ALL song-maps, or simply those matching
   criteria. Returns the WriteResult(s) in a list."
  ([playlist] (safe-delete playlist {}))
  ([playlist input]
     (cond 
      (map? input)
      (safe-delete playlist input)
      (string? input)
      (delete playlist {:title input})
      (coll? input)
      (doall (map (partial delete playlist) input)))))

(defn get-info
  "Returns a list of all matches to the given key from all songs in given playlist"
  [playlist k]
  (cond (string? playlist)
        (doall (for [song (retrieve playlist)] (k song)))
        (coll? playlist)
        (doall (for [song playlist] (k song)))))

(defn titles [playlist]
  (get-info playlist :title))

(defn activate-db [name]
  (do (connect!)
      (set-db! (get-db name))))