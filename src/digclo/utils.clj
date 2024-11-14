(ns digclo.utils
  (:import [org.xbill.DNS Type]))

(defn record-type-kw->id [kw]
  (case kw
    :a        Type/A
    :ns       Type/NS
    :md       Type/MD
    :mf       Type/MF
    :cname    Type/CNAME
    :soa      Type/SOA
    :mb       Type/MB
    :mg       Type/MG
    :null     Type/NULL
    :wks      Type/WKS
    :ptr      Type/PTR
    :hinfo    Type/HINFO
    :minfo    Type/MINFO
    :mx       Type/MX
    :txt      Type/TXT
    :rp       Type/RP
    :afsdb    Type/AFSDB
    :x25      Type/X25
    :isdn     Type/ISDN
    :rt       Type/RT
    :nsap     Type/NSAP
    :nsap-ptr Type/NSAP_PTR
    :sig      Type/SIG
    :key      Type/KEY
    :px       Type/PX
    :gpos     Type/GPOS
    :aaaa     Type/AAAA
    :loc      Type/LOC
    :nxt      Type/NXT))

(defn record-type-id->kw [id]
  (-> id
      Type/string
      clojure.string/lower-case
      keyword))
