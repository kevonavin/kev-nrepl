(ns kev-nrepl.index
  (:require [babashka.fs :as fs]
            [clj-kondo.core :as clj-kondo]
            [clojure.core.async :as async]
            [clojure.java.classpath :as cp]
            [clojure.spec.alpha :as s]
            [com.wsscode.pathom3.connect.operation :as pco]
            [clojure.string :as str]
            [juxt.dirwatch :as dirwatch]
            [integrant.core :as ig]
            )
  (:import (com.github.javaparser StaticJavaParser)
           (java.util.jar JarFile)))

(comment

  (apply require '(
                   [clojure.tools.deps.alpha.repl :as deps.repl]
                   [clojure.tools.deps.alpha.extensions :as deps.ext]
                   [clojure.tools.deps.alpha.util.maven :as deps.util.maven]
                   [clojure.tools.deps.alpha.util.session :as deps.util.session]
                   [clojure.tools.deps.alpha.extensions.maven :as deps.ext.maven]
                   [clojure.tools.namespace.find :as ns.find]
                   [clojure.java.classpath :as cp]
                   [orchard.info :as o.info]
                   ))

  (loaded-libs)
  ;; java resolution path
  ;; fullname -> clojure lib -> add "$sources" and call deps.ext/coord-paths -> parse source

  ;; give a dep, download just that dep (not transitive - that's handled by the core algorithm)
  (deps.ext/coord-paths 'com.github.javaparser/javaparser-core$sources {:mvn/version "3.19.0"} :mvn {:mvn/repos deps.util.maven/standard-repos})

  (deps.repl/add-libs '{com.github.javaparser/javaparser-core$sources {:mvn/version "3.19.0"}})
  ;; find java src
  (->> (cp/system-classpath)
       (filter cp/jar-file?)
       #_(some #(and (re-find #"clojure/clojure"
                            (.getName %)) %))
       (map #(JarFile. %))
       (mapcat cp/filenames-in-jar)
       ;;(map #(.getName %))
       (filter (partial re-find #"\.java$"))
       )

  (->> (cp/classpath-jarfiles)
      (map #(.getName %)))

  (cp/filenames-in-jar)
  (re-find #"github/javaparser" "kevinonavin/github/javaparser/rare")

  (deps.repl/add-libs '{clj-http/clj-http {:mvn/version "3.12.1"}
                        honeysql/honeysql {:mvn/version "1.0.444"}
                        expound/expound {:mvn/version "0.8.9"}
                        org.apache.commons/commons-pool2 {:mvn/version "2.8.0"}
                        com.github.javaparser/javaparser-core {:mvn/version "3.19.0"}})


  ,,,)

(defn get-paths []
  (filter fs/relative? (cp/system-classpath)))

(defn get-kondo [] (clj-kondo/run! {:config {:output {:analysis true}}
                                    :lint (get-paths)}))

(def indexmerge
  (partial merge-with
           (fn [l r]
             (cond
               (and (map? l) (map? r)) (indexmerge l r)
               (or (string? l) (string? r)) l
               (and (seqable? l)
                    (seqable? r)) (concat l r)
               :else l))))

(def category-map {:namespace-definitions ::ns-def
                   :var-definitions       ::var-defn
                   :var-usages            ::var-usage})

(defn kondo->index
  "converts clj-kondo analysis into a more efficient data structure for lookups"
  [kondo]
  (->> kondo :analysis
       (filter #(contains? category-map (first %)))
       (mapcat
        (fn [[category varlist]]
          (mapcat
           (fn [{:keys [to ns name name-row filename name-col name-end-col]}]
             (let [fullname (if (or to ns) (symbol (str (or to ns)) (str name)) name)
                   var-type (category-map category)
                   var-info {::var-type var-type
                             ::line name-row
                             ::ns (or to ns)
                             ::name name
                             ::fullname fullname
                             ::filename filename
                             ::name-col-start name-col
                             ::name-col-end name-end-col}]
               [{::files {filename {name-row [var-info]}}}
                {::vars {fullname {(keyword (str/replace (str var-type "s") #":" ""))
                                   [var-info]}}}]))
           varlist)))
       (reduce indexmerge {})))

;; distinct from var-info stuff bc that relates to a var so is different. This
;; relates specifically to a source code point and uses an *absolute* path (consider renaming).
;; basically this can be in middle of symbols. Maybe they should just be combined? probs not
(s/def ::src-location
  (s/keys :req-un [::filename ::line ::col]))

(def var-info-keys [::name ::fullname ::filename ::line ::ns
               ::var-type ::name-col-start ::name-col-end])

(s/def ::var-info
  (s/keys :req [::name ::fullname ::filename ::line
                ::var-type ::name-col-start ::name-col-end]
          :opt [::ns ;;not there for namespace, as that's just ::name
                ]))

(defn mapspec->keys [mspec]
  (->> mspec s/get-spec s/describe rest (partition 2) (mapcat second) vec))

(pco/defresolver var->defns [{::keys [kondo-index]} {::keys [fullname] :as info}]
  {::defns (-> @kondo-index ::vars fullname ::var-defns)})

(pco/defresolver var->usages [{::keys [kondo-index]} {::keys [fullname] :as info}]
  {::usages (-> @kondo-index ::vars fullname ::var-usages)})

(defn root-relative-path [path]
  (str (fs/relativize (fs/absolutize (fs/path ".")) path)))

(defn absolute-path [path]
  (str (fs/absolutize path)))

(pco/defresolver src-location->var [{::keys [kondo-index]} {{:keys [filename line col]} ::src-location}]
  {::pco/input [::src-location]
   ::pco/output (mapspec->keys ::var-info)}
  (tap> {'finding [filename line col]})
  (let [relative-path (root-relative-path filename)]
    (some
     (fn [{::keys [name-col-start name-col-end] :as var}]
       (and (>= (inc col) name-col-start)
            (< (inc col) name-col-end)
            var))
     (some-> @kondo-index ::files (get relative-path) (get line)))))

(pco/defresolver var->src-location [{::keys [filename line name-col-start]}]
  {::src-location {:filename (absolute-path filename) :line line :col name-col-start}})

(pco/defresolver src-location->string [{{:keys [filename line col]} ::src-location}]
  {::src-location-string (str filename ":" line ":" col)})

(defn resolvers [] [var->src-location
                    var->usages
                    var->defns
                    src-location->string
                    src-location->var])

(defn make-updater
  "expects `f` to return an async channel but only to indicate it's completion.
  `f` is called for it's side effects.

  returns a channel and calls `f` with the latest input to that channel.
  at most one invocation of `f` outstanding at any given time.

  so if you call `f` 5 times sequentially before the first invocation's channel puts
  a value, only the first and the last will invoke.

  Stops if you close the channel."
  [f]
  (let [c (async/chan 5)]
    (async/go
      (try
        (letfn [(call-next [fchan item]
                  (async/go
                    (if fchan
                      (async/alt!
                        fchan ([_] (call-next (when item (f item)) nil)) ;processing done, restart if able
                        c     ([item] (when item ;; if this thing's closed, stop
                                        (if fchan
                                         (call-next fchan item) ; processing, replace item
                                         (call-next (f item) nil))))) ; got item, start
                      (call-next (f (async/<! c)) nil))))] ; waiting
          (async/go
            (call-next nil nil)))
        (catch Exception e
          (println "got updater exception " e)
          (.printStackTrace e)
          (tap> e))))
    c))

(defmethod ig/init-key ::dir-watcher [_ {::keys [index-update-chan]}]
  (apply dirwatch/watch-dir
         (cons (fn [& args]
                 (prn ["dir change:" args])
                 (async/>!! index-update-chan "something"))
               (get-paths))))

(defmethod ig/halt-key! ::dir-watcher [_ watcher]
  (dirwatch/close-watcher watcher))

;; send it something on start too
(defmethod ig/init-key ::index-update-chan [_ {::keys [kondo-index]}]
  (doto (make-updater (fn [& args]
                        (async/thread
                          (tap> {'index-update-args args})
                          ;; in future, will inspect file updated in args and incrementally update index
                          (reset! kondo-index (kondo->index (get-kondo)))
                          (tap> 'indexing-completed))))
    (async/>!! "start!")))

(defmethod ig/halt-key! ::index-update-chan [_ c]
  (async/close! c))

(defmethod ig/init-key ::kondo-index [_ _]
  (atom nil))

(comment

  (require '[integrant.repl :as ir])

  (def kondo (get-kondo))

  (def index (kondo->index kondo))

  (point->var index ["src/ekata/lis/cass.clj" 34 10])


  ;; lookup a var in the ::vars index
  (-> index ::vars
      (get (::fullname (point->var index ["src/ekata/lis/cass.clj" 34 10]))))

  ,,,)
