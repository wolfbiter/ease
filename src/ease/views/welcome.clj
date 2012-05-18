(ns ease.views.welcome
  (:require [ease.views.common :as common]
            [noir.content.getting-started])
  (:use [clojure.string :only [split-lines]]
        [noir.core :only [defpage defpartial]]
        [hiccup.core :only [html]]))

(defpage "/" []
  (common/layout
   [:div.sixteen.columns
    [:h1.remove-bottom {:style "text-align:center;"} "Ease"]]
   [:div.one-third.column
    [:h3.fixie]
    [:blockquote
     [:p.fixie]]]
   [:div.one-third.column
    [:h3.fixie]
    [:ol 
     [:li.fixie]
     [:li.fixie]
     [:li.fixie]]]
   [:div.one-third.column
    [:h3.fixie]
    [:p.fixie]]))
