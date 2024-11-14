(ns digclo.core
  (:import [java.net Inet6Address Inet4Address InetAddress]))

;; TODO re-wite for async operation if this is used
(defn name->ip
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
