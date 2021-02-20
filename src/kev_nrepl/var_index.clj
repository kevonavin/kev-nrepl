(ns kev-nrepl.var-index
  (:require [babashka.fs :as fs]
            [clj-kondo.core :as clj-kondo]
            [clojure.java.classpath :as cp]
            [clojure.string :as str]))

;; public API

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

(defn kondo->index [kondo]
  (->> kondo :analysis
       (filter #(contains? category-map (first %)))
       (mapcat
        (fn [[category varlist]]
          (mapcat
           (fn [{:keys [to ns name name-row filename name-col name-end-col] :as var}]
             (let [fullname (if (or to ns) (symbol (str (or to ns)) (str name)) name)
                   var-type (category-map category)
                   var-dict {::var-type var-type
                             ::line name-row
                             ::ns (or to ns)
                             ::fullname fullname
                             ::filename filename
                             ::name-col-start name-col
                             ::name-col-end name-end-col}]
               [{::files {filename {name-row [var-dict]}}}
                {::vars {fullname {(keyword (str/replace (str var-type "s") #":" ""))
                                   [var-dict]}}}]))
           varlist)))
       (reduce indexmerge {})))

(defn point->var [index [filename line col]]
  (some
   (fn [{::keys [name-col-start name-col-end] :as var}]
     (and (>= (inc col) name-col-start)
          (< (inc col) name-col-end)
          var))
   (some-> index ::files (get filename) (get line))))

(defn var->loc [{::keys [filename line name-col-start]}]
  (str filename ":" line ":" name-col-start))

(comment

  (def kondo (get-kondo))

  (def index (kondo->index kondo))
  (point->var index ["src/ekata/lis/cass.clj" 34 10])

  (-> index ::vars
      (get (::fullname (point->var index ["src/ekata/lis/cass.clj" 34 10]))))

  ,,,)
