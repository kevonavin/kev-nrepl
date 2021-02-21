(ns kev-nrepl.nrepl
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.tools.namespace.find]
            [com.wsscode.pathom3.interface.smart-map :as psm]
            [integrant.core :as ig]
            [kev-nrepl.index :as index]
            [kev-nrepl.pathom :as kev.pathom]
            [nrepl.middleware :as middleware]
            [nrepl.transport :as transport]
            [nrepl.misc :refer [response-for]]))

(def config
  {::index/kondo-index nil
   ;; it's a little simpler wraping them in maps I think
   ::index/index-update-chan {::index/kondo-index #ig/ref ::index/kondo-index}
   ::index/dir-watcher {::index/index-update-chan #ig/ref ::index/index-update-chan}
   ::kev.pathom/env {::index/kondo-index #ig/ref ::index/kondo-index}})

(defonce sys (atom (-> config ig/prep ig/init)))
(defn system [] @sys)
(defn reset [] (swap! sys (fn [old-system]
                            (ig/halt! old-system)
                            (-> config ig/prep ig/init))))

(defn actions
  "the interface of this middleware. takes integrant system as soul param returns a map
  of action names to functions to be invoked on those actions.
  in elisp youd send :op = 'kevin', :req '[:action-key action params]'.
  So the req is an edn string"
  [{:keys [::kev.pathom/env]}]
  {:find-var (fn [[file line col]]
               (let [var (psm/smart-map env
                                        {::index/src-location {:filename file
                                                               :line line
                                                               :col col}})]
                 {:name (::index/fullname var)
                  :result
                  (if (= ::index/var-defn (::index/var-type var)) ;; it's def, return usages
                    (->> var ::index/usages (map ::index/src-location-string))
                    (->> var ::index/defns (map ::index/src-location-string)))}))
   :nothing #(str "nothing-" %)})

(defn kevin-handler [{t :transport :as msg}]
  (let [[command arg] (edn/read-string (:req msg))]
    (transport/send t (response-for msg {:req   [command arg]
                                         :out  ((command (actions (system))) arg)
                                         :status #{:done}}))))

(defn middleware [handler]
  (fn [{:keys [op] :as msg}]
    (-> msg
        #_(update :transport
                  (fn [t]
                    (reify transport/Transport
                      (recv [_] (transport/recv t))
                      (recv [_ timeout]
                        (transport/recv t timeout))
                      (send [this reply]
;;                      (tap> {::request (dissoc msg :session) ::reply reply})
                        (transport/send t reply)
                        this))))
        ((if (= op "kevin")
           kevin-handler
           handler)))))

(defn middleware-0 [& args]
  (apply middleware args))

(middleware/set-descriptor! #'middleware-0
                            {:handles {"kevin" {:doc "this is just something I threw together"
                                                :returns {} :requires {}}}})
