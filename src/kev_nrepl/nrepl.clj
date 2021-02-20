(ns kev-nrepl.nrepl
  (:require [babashka.fs :as fs]
            [clojure.core.async :as async]
            [clojure.edn :as edn]
            [clojure.tools.deps.alpha.repl :as deps.repl]
            [clojure.tools.namespace.find]
            [clojure.tools.namespace.dir :as n.dir]
            [clojure.tools.namespace.track :as n.track]
            [juxt.dirwatch :as dirwatch]
            [kev-nrepl.var-index :as index]
            [nrepl.middleware :as middleware]
            [nrepl.transport :as transport]
            [nrepl.misc :refer [response-for]]
            [nrepl.middleware.print :as print]))

;;most adapted from https://github.com/clj-kondo/clj-kondo/blob/master/analysis/src/clj_kondo/tools/find_var.clj

(defn make-updater!
  "expects `f` to return an async channel but only to indicate it's completion.
  This is used for side effects. returns a channel and calls `f` with
  the latest thing passed to that channel. at most one invocation of `f`
  happening at any given time."
  [f]
  (let [c (async/chan)]
    (async/go-loop [f-chan nil
                    input nil]
      (if (and f-chan (async/poll! f-chan))
        (if input
          (recur (f input) nil)
          (recur nil nil))
        (recur f-chan (async/<! c))))
    c))

(defonce global-index
  (let [index (atom nil)
        kondo-update-chan (make-updater! (fn []
                                           (tap> "updating var index!")
                                           (reset! index (index/kondo->index (index/get-kondo)))
                                           (tap> "updated!")))]
    (async/go (async/>! kondo-update-chan "start!"))
    (apply dirwatch/watch-dir (cons (fn [& args]
                                      (async/go (async/>! kondo-update-chan args)))
                                    (index/get-paths)))
    index))

(defn root-relative-path [absolute-path]
  (str (fs/relativize (fs/absolutize (fs/path ".")) absolute-path)))

(defn point->var
  "looks up code point in the analysis var database to see if it's on a variable
  usage or def and returns it if so"
  [file line col]
  (let [analysis (:analysis (index/get-kondo))]
    (letfn [(var-matches [{:keys [filename name-col name-row name-end-col] :as match}]
              (and (= file filename)
                   (= name-row line)
                   (>= (inc col) name-col)
                   (< col name-end-col)
                   match))]
      (or (some var-matches
                (:var-usages analysis))
          (some var-matches
                (:var-definitions analysis))))))

;; TODO factor all the bs into an emacs package so others can use it
;; TODO goto java source? - will be more intense than kondo
;; - use clojure.tools.namespace.find with java.classpath
;;   as stated here: https://github.com/clojure/tools.namespace
;; TODO navigation stack. need to "go back" when you go to java source

;; inversion of control on the elisp side
;; use tools.deps to get the merged deps.edn bc src paths may be added with
;; aliases
;; maybe implement formatting too since that sucks ass
;; guava classpath might be the move
;; https://stackoverflow.com/questions/15720822/how-to-get-names-of-classes-inside-a-jar-file
;; for clojure sources though, tools.namespace should be fine
;;  - see if you can just add this to :lint when passing to analysis
(def actions {:find-var-old (fn [[file line col]]
                          (tap> {:point->var (point->var (root-relative-path file) line col)})
                          (let [{find-ns :ns find-to :to find-name :name} (point->var (root-relative-path file) line col)]
                            (if find-ns ;; then its a def, find usages
                              (let [fullname (symbol (str find-ns) (str find-name))]
                                {:name fullname
                                 :result (->> (get-kondo) :analysis :var-usages
                                              (keep (fn [{:keys [:to :name :filename :row :col]}]
                                                      (when (and (= find-name name)
                                                                 (= find-ns to))
                                                        (str filename ":" row ":" col)))))})
                              (let [fullname (symbol (str find-to) (str find-name))]
                                {:name fullname
                                 :result (->> (get-kondo) :analysis :var-definitions
                                              (keep (fn [{:keys [:ns :name :filename :row :col]}]
                                                      (when (and (= find-name name)
                                                                 (= find-to ns))
                                                        (str filename ":" row ":" col)))))}))))
              :find-var (fn [[file line col]]
                          (let [var (index/point->var @global-index [(root-relative-path file) line col])]
                            (tap> {::found-var var})
                            (if (= ::index/var-defn (::index/var-type)) ;; it's def, return usages
                              (map index/var->loc (some-> @global-index ::index/vars
                                                          (get (::index/fullname var)) ::index/var-usages))
                              (map index/var->loc (some-> @global-index ::index/vars
                                                          (get (::index/fullname var)) ::index/var-defns)))))
              :nothing #(str "nothing-" %)})

(defn kevin-handler [{t :transport :as msg}]
  (let [[command arg] (edn/read-string (:req msg))]
    (transport/send t (response-for msg {:req   [command arg]
                                         :out  ((command actions) arg)
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
