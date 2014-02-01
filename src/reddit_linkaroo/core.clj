(ns reddit-linkaroo.core
  (:require 
    [clojure.string :as str]
    [net.cgrand.enlive-html :as html]))

(defn only [s]
  (when (= 1 (count s)) s))

(defn inc? [n test]
  (if test
    (inc n)
    n))

(defn text [link]
  (first (:content link)))

(defn url [link]
  (:href (:attrs link)))

(defn fetch [url]
  (html/html-resource (java.net.URL. url)))

(defn match-pattern [pattern string]
  (re-find pattern (str/lower-case string)))

(defn calc-score [link]
  (-> 0
    (inc? (match-pattern #"^[a]h.*ol.*oo" (text link)))
    (inc? (match-pattern #"ah, the old reddit" (text link)))
    (inc? (match-pattern #"switcharoo" (text link)))
    (inc? (match-pattern #"switch" (text link)))
    (inc? (match-pattern #"roo" (text link)))
    (inc? (match-pattern #"old.*switch" (text link)))
    (inc? (match-pattern #"oooo" (text link)))
    (inc? (match-pattern #"\?context\=" (url link)))))

(defn match-aroo [link aroo-set]
  (let [text (text link)
        url (url link)] 
    (and
      (try (java.net.URL. url) (catch Exception e))
      (string? text)
      (not (aroo-set url))
      (re-find #"reddit.*comments" url)
      (calc-score link))))

(defn select-link [html aroo-set]
    (let [highlighted-link (only (html/select html [:.usertext.border :div.usertext-body :div.md :a]))
          links (or (seq highlighted-link) (html/select html [:div.usertext-body :div.md :a]))
          [score link] (first (sort-by first > (filter (comp number? first) (map (juxt #(match-aroo % aroo-set) identity) 
                                                                                 links))))]
      (when (and score (< 0 score))
        (url link))))

(defn down-the-rabbit-hole [start-url]
  (loop [url start-url aroos []]
    (let [aroos (conj aroos url)
          next-link (select-link (fetch url) (set aroos))]
      (prn "next-link" next-link)
      (if (and next-link (not ((set aroos) next-link)))
        (recur next-link aroos)
        aroos))))

(defn get-linked-trail 
  "Returns a reddit comment-friendly trail of links. Run it through (print ...) before posting to get rid of escapes in the string"
  [urls]
  (let [escaped-urls (map #(str/escape % {\( "\\(" \) "\\)"}) urls)]
    (str/join " -> " (map #(str "[" %1 "](" %2 ")") (range) escaped-urls))))

;; Example usage:
;; (print (get-linked-trail (down-the-rabbit-hole "http://www.reddit.com/r/aww/comments/1trpuw/husband_in_pug_shirt_pug_in_husband_shirt/ceb3qpt?context=3")))