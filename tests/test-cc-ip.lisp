(in-package :cc-tests)

(def-suite ip
  :description "test cc-ip package")

(in-suite ip)

(test ip-test
      (is (cc-ip:ip-equal (cc-ip:ip-from-string "1.2.3.4")
			  (cc-ip:ip-from-string "1.2.3.4")))
      (is (if (cc-ip:ip-address-p "1.2.3.4") t))
      (is (if (cc-ip:ip-address-p "2001:0db8:85a3:0000:0000:8a2e:0370:7334") t))
      (is (equal "1.2.3.4" (cc-ip:ip-to-string (cc-ip:ip-from-string "1.2.3.4"))))
      (is (if (cc-ip:ipv4-address-p "1.2.3.4") t))
      (is (not (cc-ip:ipv4-address-p "1.2.3.4.5")))
      (is (if (cc-ip:ipv6-address-p "2001:0db8:85a3:0000:0000:8a2e:0370:7334") t))
      (is (cc-ip:ip-equal (cc-ip:ip-from-string "2001:0db8:85a3:0000:0000:8a2e:0370:7334")
			  (cc-ip:ip-from-string "2001:0db8:85a3:0000:0000:8a2e:0370:7334")))
      (is (= 4 (cc-ip:ip-len (cc-ip:ip-from-string "1.2.3.4"))))
      (is (= 16 (cc-ip:ip-len (cc-ip:ip-from-string "2001:0db8:85a3:0000:0000:8a2e:0370:7334"))))    
      )



