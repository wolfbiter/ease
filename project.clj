(defproject ease "1.0.0-SNAPSHOT"
  :description "FIXME: write description"
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [overtone "0.6.0"]
                 [org/jaudiotagger "2.0.1"]
                 [com.leadtune/clojure-zmq "2.1.0"]
                 ;[congomongo "0.1.8"]
                 [com.novemberain/monger "1.0.0-SNAPSHOT"]
                 [noir "1.2.1"]]
  :native-path "/usr/local/lib"
  :dev-dependencies [[swank-clojure "1.3.3"]]
  :main ease.core)
