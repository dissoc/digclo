(ns digclo.core
  (:require
   [clojure.string :refer [ends-with?]]
   [digclo.utils :refer [record-type-kw->id
                         record-type-id->kw]])
  (:import
   (java.net Inet4Address Inet6Address InetAddress)
   (java.util.function BiConsumer)
   (org.xbill.DNS DClass Message Name Record Section SimpleResolver)))

;; TODO re-wite for async operation if this is used
(defn basic-name->ip
  [name & {:keys [cn? ipv6?]
           :or   {cn?   false
                  ipv6? true}}]
  (loop [result []
         ips    (InetAddress/getAllByName name)]
    (let [obj     (first ips)
          version (cond
                    (instance? java.net.Inet6Address obj) :ipv6
                    (instance? java.net.Inet4Address obj) :ip4)
          ip-map  (cond-> {:ip (.getHostAddress obj)
                           :version version}
                    cn? (assoc :cn (.getCanonicalHostName obj)))]
      (if (next ips)
        (recur (conj result ip-map)
               (next ips))
        (conj result ip-map)))))

(defn format-a-record [a-record]
  {:address         (-> a-record
                        .getAddress
                        .getHostAddress)
   :ttl             (-> a-record
                        .getTTL)
   :record-type     (-> a-record
                        .getType
                        record-type-id->kw)
   :additional-name (-> a-record
                        .getAdditionalName)})

(defn format-question [question]
  {:name        (-> question .getName str)
   :record-type (-> question
                    .getType
                    record-type-id->kw)
   :ttl         (-> question .getTTL)})

(defn format-response [response]
  {:question   (map format-question
                    (.getSectionArray response Section/QUESTION))
   :answer     (map format-a-record
                    (.getSectionArray response Section/ANSWER))
   :additional (.getSectionRRsets response Section/ADDITIONAL)
   :zone       (.getSectionRRsets response Section/ZONE)})

(defn lookup [name & {:keys [server record-type on-answer]
                      :or   {server      "8.8.8.8"
                             record-type :a}}]
  (let [n             (Name/fromString (if (ends-with? name ".")
                                         name
                                         (str name ".")))
        r             (new SimpleResolver server)
        query-record  (Record/newRecord n
                                        (record-type-kw->id :a)
                                        DClass/IN)
        query-message (Message/newQuery query-record)
        answer        (.sendAsync r query-message)]
    (if on-answer
      (.whenComplete answer (reify
                              BiConsumer
                              (accept [_ m e]
                                (on-answer m))))
      (format-response @answer))))
