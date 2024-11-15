(ns digclo.core
  (:require
   [clojure.java.data :as j]
   [clojure.string :refer [ends-with?]]
   [digclo.utils :refer [record-type-id->kw record-type-kw->id]])
  (:import
   (java.net Inet4Address Inet6Address InetAddress)
   (java.util.function BiConsumer)
   (org.xbill.DNS ARecord DClass EmptyRecord Message Name OPTRecord Record Section SimpleResolver SOARecord TXTRecord)))

;; useful links
;; https://www.ietf.org/rfc/rfc1034.txt

;; TODO re-wite for async operation if this is used
(defn ^:deprecated basic-name->ip
  "pure java lookup"
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

(defn format-record [obj]
  {:name            (-> obj .getName str)
   :type            (-> obj .getType record-type-id->kw)
   :ttl             (-> obj .getTTL)
   :rr-set-type     (-> obj .getRRsetType)
   :additional-name (-> obj .getAdditionalName)})

(defmethod j/from-java OPTRecord [obj]
  (merge (format-record obj)
         {:payload-size    (.getPayloadSize obj)
          :extended-r-code (-> obj .getExtendedRcode)
          :options         (-> obj .getOptions)
          :version         (-> obj .getVersion)
          :flags           (-> obj .getFlags)}))

(defmethod j/from-java ARecord [obj]
  (merge (format-record obj)
         {:address (-> obj .getAddress .getHostAddress)}))

(defmethod j/from-java EmptyRecord [obj]
  (format-record obj))

(defmethod j/from-java SOARecord [obj]
  (merge (format-record obj)
         {:host    (-> obj .getHost j/from-java)
          :admin   (-> obj .getAdmin j/from-java)
          :serial  (-> obj .getSerial)
          :refresh (-> obj .getRefresh)
          :retry   (-> obj .getRetry)
          :expire  (-> obj .getExpire)
          :minimum (-> obj .getMinimum)}))

(defmethod j/from-java TXTRecord [obj]
  (merge (format-record obj)
         {:strings (-> obj .getStrings)}))

(defmethod j/from-java Name [obj]
  (str obj))

(defmethod j/from-java Message [message]
  {:question   (mapv j/from-java
                     (.getSectionArray message Section/QUESTION))
   :answer     (mapv j/from-java
                     (.getSectionArray message Section/ANSWER))
   :authority  (mapv j/from-java
                     (.getSectionArray message Section/AUTHORITY))
   :additional (mapv j/from-java
                     (.getSectionArray message Section/ADDITIONAL))})

(defn lookup [name & {:keys [server record-type on-answer raw?]
                      :or   {server      "8.8.8.8"
                             raw?        false
                             record-type :a}}]
  (let [n             (Name/fromString (if (ends-with? name ".")
                                         name
                                         (str name ".")))
        r             (new SimpleResolver server)
        query-record  (Record/newRecord n
                                        (record-type-kw->id record-type)
                                        DClass/IN)
        query-message (Message/newQuery query-record)
        answer        (.sendAsync r query-message)]
    (if on-answer
      (.whenComplete answer (reify
                              BiConsumer
                              (accept [_ m e]
                                (on-answer (if raw?
                                             (str m)
                                             (j/from-java m))))))
      (if raw?
        (str @answer)
        (j/from-java @answer)))))
