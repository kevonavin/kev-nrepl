(ns kev-nrepl.nrepl
  (:require [babashka.fs :as fs]
            [clojure.core.async :as async]
            [clojure.edn :as edn]
            [clojure.tools.namespace.find]
            [juxt.dirwatch :as dirwatch]
            [kev-nrepl.var-index :as index]
            [nrepl.middleware :as middleware]
            [nrepl.transport :as transport]
            [nrepl.misc :refer [response-for]]
            [nrepl.middleware.print :as print]))

(defn make-updater!
  "expects `f` to return an async channel but only to indicate it's completion.
  `f` is called for it's side effects.

  returns a channel and calls `f` with the latest input to that channel.
  at most one invocation of `f` outstanding at any given time.

  so if you call `f` 5 times sequentially before the first invocation's channel puts
  a value, only the first and the last will invoke"
  [f]
  (let [c (async/chan 5)]
    (async/go
      (try
        (letfn [(call-next [fchan item]
                  (async/go
                    (if fchan
                      (async/alt!
                        fchan ([_] (call-next (when item (f item)) nil)) ;processing done, restart if able
                        c     ([item] (if fchan
                                        (call-next fchan item) ; processing, replace item
                                        (call-next (f item) nil)))) ; got item, start
                      (call-next (f (async/<! c)) nil))))] ; waiting
          (async/go
            (call-next nil nil)))
        (catch Exception e
          (tap> e))))
    c))

(defonce global-index
  (let [index (atom nil)
        kondo-update-chan (make-updater! (fn [& args]
                                           (async/thread
                                             (println {"updating var index!" args})
                                             (reset! index (index/kondo->index (index/get-kondo)))
                                             (println "updated!"))))]
    (async/>!! kondo-update-chan "start!")
    (apply dirwatch/watch-dir (cons (fn [& args]
                                      (prn ["dir change:" args])
                                      (async/>!! kondo-update-chan "something"))
                                    (index/get-paths)))
    index))

(defn root-relative-path [absolute-path]
  (str (fs/relativize (fs/absolutize (fs/path ".")) absolute-path)))

(defn actions [] {:find-var (fn [[file line col]]
                              (let [var (index/point->var @global-index [(root-relative-path file) line col])]
                                (tap> {::change! var})
                                {:name (::index/fullname var)
                                 :result
                                 (if (= ::index/var-defn (::index/var-type var)) ;; it's def, return usages
                                   (map index/var->loc (some-> @global-index ::index/vars
                                                               (get (::index/fullname var)) ::index/var-usages))
                                   (map index/var->loc (some-> @global-index ::index/vars
                                                               (get (::index/fullname var)) ::index/var-defns)))}))
                  :nothing #(str "nothing-" %)})

(defn kevin-handler [{t :transport :as msg}]
  (let [[command arg] (edn/read-string (:req msg))]
    (transport/send t (response-for msg {:req   [command arg]
                                         :out  ((command (actions)) arg)
                                         :status #{:done}}))))

(defn middleware [handler]
  (fn [{:keys [op] :as msg}]
    (-> msg
        (update :transport
                (fn [t]
                  (reify transport/Transport
                    (recv [_] (transport/recv t))
                    (recv [_ timeout]
                      (transport/recv t timeout))
                    (send [this reply]
       ;;               (tap> {::request (dissoc msg :session) ::reply reply})
                      (transport/send t reply)
                      this))))
        ((if (= op "kevin")
           kevin-handler
           handler)))))

(middleware/set-descriptor! #'middleware
                            {:requires #{#'print/wrap-print}
                             :expects #{"eval" "find-symbol"}
                             :handles {"kevin" {:doc "this is just something I threw together"
                                                :returns {} :requires {}}}})
