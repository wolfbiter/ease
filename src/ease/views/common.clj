(ns ease.views.common
  (:use [noir.core :only [defpartial]]
        [hiccup.page-helpers :only [include-css html5]]))

(defpartial layout [& content]
  (html5
   [:head
    [:title "ease"]
    (include-css "/css/reset.css")
    (include-css "/css/base.css")
    (include-css "/css/skeleton.css")
    (include-css "/css/layout.css")
    (include-css "/css/style.css")]
   [:body
    [:div.container
     content]
    [:script {:src "/js/jquery-1.7.2.js"}]
    [:script {:src "/js/tabs.js"}]
    [:script {:src "/js/fixie_min.js"}]]))