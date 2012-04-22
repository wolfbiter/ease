(ns ease.example
  (:use org.zeromq.clojure))
   
(defn- string-to-bytes [s] (.getBytes s))
(defn- bytes-to-string [b] (String. b))

(defn handle [socket query] 
  (let [query_ (bytes-to-string query)
        resultset (str "Received query: " query_)]
    (send- socket (string-to-bytes resultset))))

(defn- on-thread [f]
  (doto (Thread. #^Runnable f) 
    (.start)))

(defn start-server []
  (let [ctx (make-context 1 1)
        socket (make-socket ctx +rep+)]
    
    ; Create separate 'dispatcher' thread
    (on-thread 
       #(do 
           (bind socket "tcp://lo:5555")
           (while true
             (let [query (recv socket)]
               (handle socket query)))))))

(defn send-to-server [query] 
  (with-context [ctx 1 1]
    (with-socket [socket ctx +req+]
      (connect socket "tcp://localhost:5555")
      (send- socket (string-to-bytes query))
      (println (str "Received response: " (bytes-to-string (recv socket)))))))