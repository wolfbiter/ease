;; Several methods culminating in "delete-dupes" and "get-dupes"

(ns ease.dupes
  (:use
   [ease.song]
   [ease.mongo]
   [ease.debug]
   [clojure.pprint :only [pp pprint]]
   [clojure.string :only [replace lower-case] :rename {replace replace-str}]
   [monger.collection
    :only [update insert insert-batch find-maps remove save any? count]
    :rename {remove del count cnt any? any}]))

(defn check-song
  "Returns true iff this song's artist and mod match at least one other
   pair from the given mod-artist map, as in {_id1 (mod1 art1) _id2 (mod2 art2)}."
  [mod-artists song]
  (let [others (dissoc mod-artists (:_id song))]
    (not (not-any? #(and (= (first (nth % 1))
                            (normalize (:mod song)))
                         (= (nth (nth % 1) 1)
                            (normalize (:artist song))))
                   others))))

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
      
     ; (comment Recursion: if this title has been met, add it to the list of other
     ;          matches. if it has not been met, create a new match and place itself
     ;          there.)

     ;This method filters candidates first by title, and then checks other fields.
      
      (let [match (normalize title)]
        (if (contains? met match)
          (recur met
                 (rest title_ids)
                 (assoc dupes match (apply list _id (get dupes match))))
          (recur (conj met match)
                 (rest title_ids)
                 (assoc dupes match (apply list _id (get dupes match))))))
      ; Base case, retain only the duplicates and return.
      (doall (remove #(or (nil? %) (= '() %))
                     (map (partial real-dupes playlist) dupes))))))

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
  (let [dupes (get-dupes playlist)]
    (if (not (= dupes '()))
      (do
        (println "Found" (count dupes) "potential set(s).")
        (println)
        (println "I will now present you with matches. For each set, you must decide which (if any) to KEEP. ex: 0 1 123")
        (doall (map (partial select-which-dupes-to-delete playlist) dupes)))
      (println "No duplicates found."))))