(ns c3.core
  (:require [clj-yaml.core :as yaml]
            [k8s.core :as k8s]
            [clojure.string :as str]))

(defn debug [x]
  (spit "/tmp/result.yaml"
        (yaml/generate-string x :dumper-options {:flow-style :block})))

(comment
  ;; look at the root of api
  (->
   (k8s/curl "/")
   cheshire.core/parse-string
   debug)

  ;; v1 api
  (-> (k8s/curl "/api/v1")
      cheshire.core/parse-string
      debug)

  ;; in swagger format
  (->
   (k8s/curl "/swagger.json")
   cheshire.core/parse-string
   debug)

  ;; get all pods
  (-> (k8s/curl "/api/v1/pods")
      cheshire.core/parse-string
      (get "items")
      (->> (map #(get-in % ["metadata" "name"])))
      debug)

  (-> (k8s/curl "/api/v1/namespaces/default/pods")
      cheshire.core/parse-string
      debug)

  )

;; we need to create table for custom resource
(def app-definition
  {:apiVersion "apiextensions.k8s.io/v1beta1"
   :kind "CustomResourceDefinition"
   :metadata {:name "apps.health-samurai.io"}
   :spec {:group "health-samurai.io"
          :version "v1"
          :names {:kind "App", :plural "apps"}
          :scope "Namespaced"}})

(defn init []
  (k8s/patch app-definition))

(comment

  ;; cleanup
  (-> (k8s/delete app-definition)
      (debug))

  ;; see app-definition
  (debug app-definition)

  ;; kubectl apply crd.yaml
  (-> (init)
      debug)

  ;; kubectl get crd
  (->
   (k8s/query app-definition)
   debug)

  )


;; sample of declaration
(def test-app
  {:kind "App"
   :ns "default"
   :apiVersion "health-samurai.io/v1"
   :metadata {:name "myservice"}
   :spec {:domain "devoops.local"
          :image "gcr.io/google_containers/echoserver"
          :version 1.4
          :replicas 1}})

(defn change-name [x nm]
  (assoc-in x [:metadata :name] nm))

(comment
  ;; kubectl get apps

  (-> test-app debug)

  ;; list apps
  (-> (k8s/query test-app)
      debug)

  ;; kubectl get apps
  (-> (k8s/create test-app)
      debug)

  (-> test-app
      (change-name "yaservice")
      (k8s/create)
      (debug))

  (-> test-app
      (change-name "yaservice")
      (k8s/delete)
      (debug))

  ;; list apps
  (->> (k8s/query test-app)
       :items
       (map (fn [x] [(get-in x [:metadata :name]) (:spec x)]))
       debug)


  ;; to delete
  (-> (k8s/delete test-app)
      (debug))

  ;; to delete all
  (-> (k8s/delete-collection
       (assoc test-app :labelSelector "app=myservice"))
      (debug))


  )

(defn deployment
  [{{n :name} :metadata
    {img :image
     v :version
     d :domain :as spec} :spec
    :as res}]

  (let [template
        {:metadata {:labels {:app n}}
         :spec {:containers
                [{:name "app"
                  :image (str img ":" v)
                  :ports [{:containerPort 8080}]}]}}]
    {:apiVersion "apps/v1beta1"
     :kind "Deployment"
     :metadata {:name n
                :labels {:app n
                         :system "c3"
                         :domain d}}
     :spec {:replicas (or (:replicas spec) 1)
            :template template}}))

(comment
  ;; generate deployment
  (-> test-app
      deployment
      debug)

  (def mydepl
    (-> test-app
        deployment))

  ;; create deployment
  (-> (assoc mydepl :ns "default")
      (k8s/create)
      (debug))

  ;; delete deployment cascade
  (-> (assoc mydepl :ns "default")
      (k8s/delete {:kind "DeleteOptions"
                   :apiVersion "v1"
                   :propagationPolicy "Background"})
      (debug))

  )

(defn service

  [{{n :name} :metadata
    {img :image
     v :version
     d :domain :as spec} :spec
    :as res}]

  {:apiVersion "v1"
   :kind "Service"
   :metadata {:name n
              :labels {:app n
                       :system "c3"
                       :domain d}}
   :spec {:selector {:app n}
          :ports [{:protocol "TCP"
                   :port 80
                   :targetPort 8080}]}})

(comment
  (-> test-app
      service
      debug)

  )

(defn ingress-line [{{n :name} :metadata
                     {d :domain :as spec} :spec :as res}]
  {:host (str n "." d)
   :http {:paths [{:path "/"
                   :backend {:serviceName n
                             :servicePort 80}}]}})


(comment
  (-> test-app
      ingress-line 
      debug)
  )


(defn get-deployments []
  (mapv #(merge % {:kind "Deployment" :apiVersion "apps/v1beta1"})
        (:items (k8s/query {:kind "Deployment"
                            :apiVersion "apps/v1beta1"
                            :ns "default"}
                           {:labelSelector "system in (c3)"}))))

(comment
  (-> (get-deployments)
      (debug))

  )

(defn get-services []
  (mapv #(merge % {:kind "Service" :apiVersion "v1"})
        (:items (k8s/query {:kind "Service"
                            :apiVersion "v1"
                            :ns "default"} {:labelSelector "system in (c3)"}))))

(comment
  (-> (get-services)
      (debug))

  )


(defn get-apps []
  (:items (k8s/query {:kind "App"
                      :apiVersion "health-samurai.io/v1"
                      :ns "default"})))

(comment
  (-> (get-apps)
      (debug))

  )


(def ingress {:kind "Ingress"
              :apiVersion "extensions/v1beta1"
              :metadata {:name "c3"
                         :namespace "default"}})


(defn get-ingresses []
  (if-let [ing (k8s/do-find {:kind "Ingress"
                          :apiVersion "extensions/v1beta1"
                          :id "c3"
                          :ns "default"})]
    [ing]
    []))

(comment
  (-> (get-ingresses)
      (debug))
  )

(defn expected-resources [apps]
  (when-not (empty? apps)
    (let [resources  (->> apps
                          (mapcat (fn [a] [(deployment a) (service a)])))
          ing (assoc-in ingress [:spec :rules]
                        (sort-by :host (mapv ingress-line apps)))]
      (conj resources ing))))

(comment
  (-> 
   (expected-resources [test-app])
   debug)

  )

(defn actual-resources []
  (concat (get-deployments)
          (get-services)
          (get-ingresses)))


(comment
  (-> 
   (actual-resources)
   debug)

  )

(defn index-resources [ress]
  (->> ress
       (reduce
        (fn [acc res]
          (assoc-in acc [(:kind res) (get-in res [:metadata :name])] res))
        {})))

(comment
  (-> (get-apps)
      expected-resources
      index-resources
      debug)

  (-> (actual-resources)
      index-resources
      debug)
  )

(defn map-index [f idx]
  (->> idx
       (reduce
        (fn [acc [tp ress]]
          (->> ress
               (reduce
                (fn [acc [id res]]
                  (f acc [tp id] res))
                acc))
          ) [])))

(defn compare-recur [res pth spec real]
  (cond
    (and (map? spec) (map? real)) (reduce (fn [res [k v]]
                                            (compare-recur res (conj pth k) v (get real k))
                                            ) res spec)
    (and (vector? spec) (vector? real)) (reduce (fn [res [i x]]
                                                  (compare-recur res (conj pth i) x (nth real i nil)))
                                                res (map-indexed (fn [i x] [i x]) spec))
    (= spec real) res
    :else (conj (or res []) {:path pth :spec spec :real real})))

(defn do-compare [spec res]
  (compare-recur nil [] spec res))

(comment
  (debug
   (compare-recur nil [] {:a 1} {:a 2}))

  (debug
   (compare-recur nil [] {:a {:b [{:c 1 :d 3} {:c 1 :d 3} ]}}
                  {:a {:b [{:c 1 :d 2} {:c 2 :d 3}]}}))

  (debug
   (compare-recur nil []
                  {:a {:b [{:c 1 :d 3} {:c 1 :d 3} ]}}
                  {:a {:b [{:c 1 :d 3} {:c 1 :d 3} ]}}))
  )

(defn *reconcile [expected actual]
  (let [expected  (index-resources expected)
        actual    (index-resources actual)
        to-update (->> expected
                       (map-index (fn [acc pth res]
                                    (if-let [act (get-in actual pth)]
                                      (if-let [diff (do-compare res act)]
                                        (conj acc {:action :update :resource res :diff diff})
                                        acc)
                                      (conj acc {:action :create :resource res})))))
        to-delete (->> actual
                       (map-index (fn [acc pth act]
                                    (if (get-in expected pth)
                                      acc
                                      (conj acc {:action :delete
                                                 :resource act
                                                 :options (when (contains? #{"Deployment"} (:kind act))
                                                            {:kind "DeleteOptions"
                                                             :apiVersion "v1"
                                                             :propagationPolicy "Background"})})))))]
    (into to-update to-delete)))

(defn reconcile-actions []
  (*reconcile
   (expected-resources (get-apps))
   (actual-resources)))

(defn reconcile [actions]
  (doseq [{res :resource a :action opt :options :as act} actions]
    (let [res (assoc res :ns "default")]
      (cond
        (= a :create) (println (k8s/create res))
        (= a :update) (println (k8s/patch res)) 
        (= a :delete) (println (k8s/delete res opt))
        (= a :delete-collection) (println (k8s/delete-collection res)))))
  actions)

(comment

  ;; to delete all
  (-> (k8s/delete-collection
       test-app)
      (debug))

  ;; list apps
  (-> (k8s/query test-app)
      debug)

  ;; create app
  (-> (k8s/create test-app)
      debug)

  ;; create another app 
  (-> test-app
      (change-name "yaservice")
      (k8s/create)
      (debug))

  ;; dry run
  (->> (reconcile-actions) 
       debug)

  ;; apply
  (-> (reconcile-actions)
      reconcile
      debug)

  (-> test-app
      (k8s/query)
      debug)

  (-> test-app
      (k8s/delete)
      (debug))

  ;; delete another app 
  (-> test-app
      (change-name "yaservice")
      (k8s/delete)
      (debug))

  )

(defonce server (atom nil))
(defn stop []
  (when-let [thr @server]
    (.interrupt thr)
    (reset! server nil)))

(defn start []
  (stop)
  (let [thr (Thread.
             (fn []
               (println "Start")
               (try
                 (while (not (Thread/interrupted))
                   (reconcile (reconcile-actions))
                   (Thread/sleep 5000))
                 (catch java.lang.InterruptedException e
                   (println "Bay, bay")))))]
    (reset! server thr)
    (.start thr)))

(comment

  ;; run in background
  (start)

  (stop)

  ;; just pack to docker
  ;; and deploy to cluster

  )
