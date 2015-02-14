;;
;; Kimsufi.com bot, http://twitter.com/KimsufiBot
;;
;; Written by antekone
;; http://anadoxin.org/blog
;;
;; vim:tw=0:et:

;;
;; I was using this to learn Clojure, beware of dragons in this bad code!
;;
(ns bot.core
    (:gen-class)
    (:require [clojure.string :as string])
    (:require [clojure.java.jdbc :as sql])
    (:require [clojure.data.json :as json])
    (:use [twitter.oauth] [twitter.callbacks] [twitter.callbacks.handlers] [twitter.api.restful])
    (:import (twitter.callbacks.protocols SyncSingleCallback)))

;;
;; Parameters (in milliseconds)
;;
(def kimsufi-crawl-interval 50000) ;; every 50 seconds
(def twitter-update-interval 10000) ;; every 10 seconds, but touches twitter only if there is any data to be written

;;
;; Load login/password information from `db.txt` file.
;;
(defn get-db-txt [] (zipmap [:user :pass] (map string/trim (string/split (slurp "db.txt") #","))))

;; Accessors for get-db-txt's structure
(defn db-get-username [] ((get-db-txt) :user))
(defn db-get-password [] ((get-db-txt) :pass))

(def mysql-db {:subprotocol "mysql" :subname "//127.0.0.1/kimsufi" :user (db-get-username) :password (db-get-password)})

;;
;; Load OAuth keys from `oauth.txt` file.
;;
(defn oauth-get-keys [] (zipmap [:key :secret :ukey :usecret] (map string/trim (string/split (slurp "oauth.txt") #","))))

;; Accessors for oauth-get-key's structure
(defn oauth-get-key [] ((oauth-get-keys) :key))
(defn oauth-get-secret [] ((oauth-get-keys) :secret))
(defn oauth-get-user-key [] ((oauth-get-keys) :ukey))
(defn oauth-get-user-secret [] ((oauth-get-keys) :usecret))

(def oauth-creds (make-oauth-creds (oauth-get-key) (oauth-get-secret) (oauth-get-user-key) (oauth-get-user-secret)))

;; (defn get-default-input [] (slurp "data.txt"))
(defn get-default-input [] (slurp "https://ws.ovh.com/dedicated/r2/ws.dispatcher/getAvailability2"))

(defn sql-get-now [] (java.sql.Timestamp. (.getTime (java.util.Date.))))
(defn get-zones-array [json-data]       (map (fn [entry] (entry "zones")) json-data))
(defn parse-zones [zones-entry]         (map (fn [zones-set] { :zone (zones-set "zone") :avail (zones-set "availability") }) zones-entry))
(defn get-zone-availability [json-data] (map (fn [entry] (parse-zones entry)) (get-zones-array json-data)))

(defn db-get-last-avail-for [plan-name zone-name]
    (let [last-entry (sql/query mysql-db ["select * from states where plan=? and zone=? order by now desc limit 1" plan-name zone-name])]
        (if (not= (count last-entry) 0)
            ((first last-entry) :avail)
            true)))

(defn db-set-tweettime-on [id]
    (sql/update! mysql-db :states { :tweettime (sql-get-now) } ["id=?" id]))

(defn update-db-helper [plan-name zone-name avail]
    (sql/insert! mysql-db :states { :plan plan-name :zone zone-name :avail avail :now (sql-get-now) :tweettime nil }))

(defn update-db [idata]
    (dorun
        (for [avail-data idata]
            (let [plan-name (avail-data 0) zone-name (avail-data 1) available (avail-data 2)]
                (let [prev-available (db-get-last-avail-for plan-name zone-name)]
                    (when (not= prev-available available)
                        (update-db-helper plan-name zone-name available)))))))

(defn get-availability-data [content]
    (let [in-json (json/read-str content)]
        (if (and (contains? in-json "version") (contains? in-json "answer"))
            (if (= (in-json "version") "1.0")
                (let [answer-data (in-json "answer")]
                    (if (contains? answer-data "availability")
                        (answer-data "availability")
                        []))
                [])
            [])))

(defn filter-references [content refnames]
    (let [avail-data (get-availability-data content)]
        (filter (fn [entry]
                    (let [reference-str (entry "reference")]
                        (some #(= reference-str %) refnames)))
                avail-data)))

(defn translate-json [content refnames]
    (let [zone-avails (get-zone-availability (filter-references content refnames))]
        (zipmap refnames zone-avails)))

(defn get-avail-by-zonename [zone-dict zone-name]
    (if (= 0 (count zone-dict))
        nil
        (let [x (first zone-dict)]
            (if (= (x :zone) zone-name)
                (not= "unknown" (x :avail))
                (recur (rest zone-dict) zone-name)))))

(defn is-ref-zone-available [translated-data refname zonename]
    (let [zone-dict (translated-data refname)]
        (get-avail-by-zonename zone-dict zonename)))

(defn zone-name-to-str [zonename]
    (cond
        (= zonename "bhs") "Beauharnois, Canada (NA)"
        (= zonename "gra") "Gravelines, France (West EU)"
        (= zonename "rbx") "Roubaix, France (West EU)"
        (= zonename "sbg") "Strasbourg, France (Central EU)"
        (= zonename "par") "Paris, France (Central EU)"
        :else              "Unknown location"))

(defn plan-name-to-str [planname]
    (cond
        (= planname "150sk10") "KS-1"
        (= planname "150sk20") "KS-2a"
        (= planname "150sk21") "KS-2b"
        (= planname "150sk22") "KS-2c"
        (= planname "150sk30") "KS-3"
        (= planname "150sk31") "KS-3a"
        (= planname "150sk40") "KS-4"
        (= planname "150sk41") "KS-4a"
        (= planname "150sk42") "KS-4b"
        (= planname "150sk50") "KS-5"
        (= planname "150sk60") "KS-6"
        :else                  "Unknown plan name"))

(defn check-availability [content]
    (let [refnames ["150sk10" "150sk20" "150sk21" "150sk30" "150sk31" "150sk40" "150sk41" "150sk42" "150sk50" "150sk60"]]
        (let [idata (translate-json content refnames)]
            (for [zone ["rbx" "gra" "bhs" "sbg"] plan refnames]
                [plan zone (is-ref-zone-available idata plan zone)]))))

(defn console-output [idata]
    (println "Output:")
    (dorun
        (for [avail-data idata]
                (let [plan-name (avail-data 0) zone-name (avail-data 1) available (avail-data 2)]
                    (println (format "%s: %s in %s" (if available "yes" "no ") (plan-name-to-str plan-name) (zone-name-to-str zone-name)))))))

(defn run-every [func ms]
    (do
        (try
            (func)
        (catch Exception e
            (println "Exception occured in a thread: " e)))

        (Thread/sleep ms)
        (recur func ms)))

(defn kimsufi-main []
    (let [idata (check-availability (get-default-input))]
        (dorun
            (try
                (update-db idata)
            (catch Exception e
                (println "Can't update the database -- skipping this step!")))

            (console-output idata))))

(defn get-twitter-date-str [date-object]
    (let [df (java.text.SimpleDateFormat. "dd/MM/yyyy HH:mm")]
        (.format df date-object)))

(defn tweet [text] (statuses-update :oauth-creds oauth-creds :params { :status text }))

(defn twitter-update-helper [item]
    (let [event-id (item :id) event-time (get-twitter-date-str (item :now)) event-plan (item :plan) event-zone (item :zone) event-avail (item :avail)]
        (let [tweet-text (format "%s %s in %s (%s) #kimsufi"
                                 (if (true? event-avail) "Available! -" "Not available anymore:")
                                 (plan-name-to-str event-plan)
                                 (zone-name-to-str event-zone)
                                 event-time)]
            (println (format "Tweeting: %s" tweet-text))
            (tweet tweet-text)
            (db-set-tweettime-on event-id))))

(defn twitter-update-main []
    (let [item (first (sql/query mysql-db ["select id,plan,zone,avail,now from states where tweettime is null order by id asc limit 1"]))]
        (cond (= nil item) true
              :else (twitter-update-helper item))))

(defn -main [& args]
    (future (run-every twitter-update-main twitter-update-interval))
    (future (run-every kimsufi-main kimsufi-crawl-interval)))

;;
;; REPL stuff
;;
(defn r [] (use 'bot.core :reload))
