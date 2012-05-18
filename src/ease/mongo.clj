;; MongerDB interop

; Entry methods: activate-db, count-db, ADD, RETRIEVE, DELETE,
;                get-info, delete-dupes, any?, EDIT, add-attribute, remove-attribute

(ns ease.mongo
  (:use
   [ease.song]
   [clojure.pprint :only [pp pprint]]
   [clojure.string :only [replace lower-case] :rename {replace replace-str}]
   [monger.core :only [connect! set-db! get-db]]
   [monger.collection
    :only [update insert insert-batch find-maps remove save any? count]
    :rename {remove del count cnt any? any}]))

(defn activate-db [name]
  (do (connect!)
      (set-db! (get-db name))))

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
   Returns WriteResults for each stored song. *note, uri must be .mp3 or .ogg"
  [playlist input]
  (cond (map? input)
        (do (print-add playlist input)
            (save playlist input))
        (string? input)
        (add playlist (song-map input))
        (coll? input)
        (try
          ; success for flat colls
          (do (insert-batch playlist (vec input))
              (print-add playlist input))
          ; retry for nested colls
          (catch Exception e (doall (map #(add playlist %) input))))))

(defn retrieve
  "Returns a (play)list of song(s) in given stored playlist, the requested song,
   or all songs in playlist matching given criteria in the form of
   {:key1 val1 :key2 val2...}. Overloaded to accept attributes and title-strings."
  ([playlist] (find-maps playlist))
  ([playlist criteria]
     (cond (string? criteria)
           (find-maps playlist {:title criteria})
           (map? criteria)
           (find-maps playlist criteria)
           (keyword? criteria)
           (find-maps playlist {criteria ""})))
  ([playlist attr val]
     (find-maps playlist {attr val})))

(defn edit
  "Updates given song(s) in stored playlist with the map of criteria presented.
   Can take a title string to represent a song already in given playlist."
  [playlist input criteria]
  (cond (map? input)
        (monger.collection/update playlist
                                  {:_id (:_id input)}
                                  {"$set" criteria})
        (string? input)
        (edit playlist (retrieve playlist input) criteria)
        (coll? input)
        (doall (map #(edit playlist % criteria) input))))

(defn add-attribute
  "Adds given keyword as an attribute to given song(s) in given stored playlist.
   Can take a val - the attribute's corresponding value - as an optional arg."
  ([input attr] (add-attribute "best" input attr))
  ([playlist input attr] (add-attribute playlist input attr ""))
  ([playlist input attr val]
     (edit playlist input {attr val})))

(defn remove-attribute
  "Removes given attribute (as a keyword) from given song(s) in stored playlist.
   Requires val if val was created with the given attribute."
  ([playlist input attr] (remove-attribute playlist input attr ""))
  ([playlist input attr val]
  (cond (map? input)
        (monger.collection/update playlist
                                  {:_id (:_id input)}
                                  {"$unset" {attr val}})
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

;; Several methods culminating in "delete-dupes"

(defn titles [playlist]
  (get-info playlist :title))

(defn normalize [string]
  (lower-case (.replace string " " "")))

(defn check-song
  "Returns true iff this song's artist and mod match at least one other
   pair from the given map."
  [mod-artists song]
  (let [others (dissoc mod-artists (:_id song))]
    (not-every? #(= (first %) (first (rest %))) others)))
    

(defn similar-fields
  "Checks the given songs for similar artists and mods. Returns all songs which
   match at least one other song."
  [songs]
  (let [mod-artists
        (into {} (map #(vec
                        (list (:_id %)
                              (list (normalize (:mod %))
                                    (normalize (:artist %)))))
                      songs))]
    (filter (partial check-song mod-artists) songs)))
    
(defn real-dupes
  "Helper method that returns true for each song that is close to the match.
   Seems ripe for recognition algorithm improvements."
  [playlist [match [& ids]]]
  ; First ensure more than one entry for each match - else it's not a dupe
  (if (> (count ids) 1)
       ; Then check that the songs have matching fields - those that do are dupes
       (let [dupes (flatten (map #(retrieve playlist {:_id %}) ids))]
         (similar-fields dupes))))
         
(defn get-dupes
  "Retrieves a map containing nested lists of duplicate _ids.
   ex: ( (_id1-1 _id1-2 _id1-3) (_id2-1 _id2-2) ... )"
  [playlist]
  (loop [met #{}
         title_ids (map #(list (:title %) (:_id %)) (retrieve playlist))
         dupes {}]
    (if-let [[title _id] (first title_ids)]
      
      ;(comment Recursion: if this title has been met, add it to the list of other
      ;         matches. if it has not been met, create a new match and place itself
      ;         there.)

      ; This method filters candidates first by title, and then checks other fields.
      
      (let [match (normalize title)]
        (if (contains? met match)
          (recur met
                 (rest title_ids)
                 (assoc dupes match (apply list _id (get dupes match))))
          (recur (conj met match)
                 (rest title_ids)
                 (assoc dupes match (apply list _id (get dupes match))))))
      ; Base case, retain only the duplicates and return.
      (doall (remove nil? (map (partial real-dupes playlist) dupes))))))

(defn select-which-dupes-to-delete
  "Handles the actual user i/o for the delete-dupes method."
  [playlist songs]
    (do (println)
        ; First print out all the duplicate songs
        (doall (for [n (range (count songs))]
                 (do (print n ": " )
                     ; Ignore uri
                     (doall (pprint (dissoc (nth songs n) :uri)))
                     (println))))
        ; Then interpret input
        (let [input (read-line)]
          (doall (for
                     [n (range (count songs))]
                   (if (.contains input (str n))
                     (println (str "Saved: " n))
                     (do (println "Deleted:" n)
                         (del playlist
                              {:_id (:_id (nth songs n))}))))))))

(defn delete-dupes
  "Prompts user for removal of every duplicate song found in given playlist.
   Returns a list of lists of WriteResults corresponding to user decisions."
  [playlist]
  (if-let [dupes (get-dupes playlist)]
    (do
      (println "Found" (count dupes) "potential sets.")
      (println)
      (println "I will now present you with matches. For each set, you must decide which (if any) to KEEP. ex: 0 1 123")
      (doall (map (partial select-which-dupes-to-delete playlist) dupes)))
    (println "No duplicates found.")))