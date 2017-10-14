(ns c3.core
  (:require [clj-yaml.core :as yaml]
            [k8s.core :as k8s]
            [clojure.string :as str]))


(defn debug [x]
  (spit "/tmp/result.yaml"
        (yaml/generate-string x  :flow-style :block)))

;; table metaphor
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
  (-> app-definition debug)

  (->
   (k8s/curl "/")
   cheshire.core/parse-string
   debug)

  ;; kubectl apply crd.yaml
  (->
   (init)
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
   :spec {:domain "devoops.logal"
          :image "gcr.io/google_containers/echoserver"
          :version 1.4
          :replicas 1}})

(comment
  ;; kubectl get apps

  (-> (k8s/query test-app)
      (debug))

  ;; kubectl get apps
  (-> (k8s/create test-app)
      (debug))

  (-> (k8s/create
       (assoc-in test-app [:metadata :name] "myweb"))
      (debug))

  (-> (k8s/delete (assoc-in test-app [:metadata :name] "myweb"))
      (debug))

  (-> (k8s/delete test-app)
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
  (-> test-app
      deployment
      debug)
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
                                      (conj acc {:action :delete :resource act})))))]
    (into to-update to-delete)))

(defn reconcile []
  (let [actions (*reconcile
                 (expected-resources (get-apps))
                 (actual-resources))]
    (doseq [{res :resource a :action :as act} actions]
      (let [res (assoc res :ns "default")]
        (println (str/upper-case (name (:action act)))   res)
        (cond
          (= a :create) (println (k8s/create res))
          (= a :update) (println (k8s/patch res)) 
          (= a :delete) (println (k8s/delete res)))))
    actions))

(comment
  (-> (reconcile)
      debug)

  (-> (k8s/delete
       (assoc-in test-app [:metadata :name] "awesome"))
      (debug))

  )

(defonce server (atom nil))
(defn stop []
  (when-let [thr @server]
    (.interrupt thr)
    (reset! server nil)))

(defn start []
  (stop)
  (let [thr (Thread. (fn []
                       (println "Start")
                       (try
                         (while (not (Thread/interrupted))
                           (reconcile)
                           (Thread/sleep 5000))
                         (catch java.lang.InterruptedException e
                           (println "Bay, bay")))))]
    (reset! server thr)
    (.start thr)))


(comment

  (stop)

  (start)

  )
