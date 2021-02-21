(ns kev-nrepl.pathom
  (:require
   [com.wsscode.pathom3.connect.indexes :as pci]
   [com.wsscode.pathom3.interface.smart-map :as psm]
   [integrant.core :as ig]
   [kev-nrepl.index :as index]
   ))

(defmethod ig/init-key ::env [_ addons]
  (merge (-> (index/resolvers)
             (pci/register)
             (psm/with-wrap-nested? true)
             (psm/with-error-mode ::psm/error-mode-loud))
         addons))

(comment
  (apply require '(
                   [integrant.core :as ig]
                   [kev-nrepl.nrepl :as nrepl]
             ))

  (tap> (psm/smart-map (-> (nrepl/system) ::env)
                       {::index/src-location {:filename "/Users/kkrausse/Documents/ekata/services/lis-clj/src/ekata/lis/cass.clj"
                                              :line 34 :col 11}}))
  (->> (psm/smart-map (-> (nrepl/system) ::env)
                      {::index/src-location {:filename "/Users/kkrausse/Documents/ekata/services/lis-clj/src/ekata/lis/cass.clj"
                                             :line 34 :col 11}})
      ::index/defns
      (map ::index/src-location-string))

  (do
    (nrepl/reset)
    nil
    )


  (psm/with)
,,,)


