(ns ease.views.welcome
  (:require [ease.views.common :as common]
            [noir.content.getting-started])
  (:use [clojure.string :only [split-lines]]
        [noir.core :only [defpage defpartial]]
        [hiccup.core :only [html]]))

(defpage "/" []
  (common/layout
   [:div.sixteen.columns
    [:h1.remove-bottom {:style "text-align:center;"} "ease"]]
   [:div.one-third.column
    [:h3 "about ease"]
    [:p "ease is an awesome music program"]]
   [:div.one-third.column
    [:h3 "using ease"]
    [:ol
     [:li "start ease"]
     [:li "???"]
     [:li "PROFIT!"]]]
   [:div.one-third.column
    [:h3 "docs and support"]
    [:p "none at this time"]]))
