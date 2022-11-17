(in-package :cc-tests)

(def-suite ip
  :description "test cc-net package")

(in-suite ip)

(test ip-test
      (is (cc-net:ip-equal (cc-net:ip-from-string "1.2.3.4")
			  (cc-net:ip-from-string "1.2.3.4")))
      (is (if (cc-net:ip-address-p "1.2.3.4") t))
      (is (if (cc-net:ip-address-p "2001:0db8:85a3:0000:0000:8a2e:0370:7334") t))
      (is (equal "1.2.3.4" (cc-net:ip-to-string (cc-net:ip-from-string "1.2.3.4"))))
      (is (if (cc-net:ipv4-address-p "1.2.3.4") t))
      (is (not (cc-net:ipv4-address-p "1.2.3.4.5")))
      (is (if (cc-net:ipv6-address-p "2001:0db8:85a3:0000:0000:8a2e:0370:7334") t))
      (is (cc-net:ip-equal (cc-net:ip-from-string "2001:0db8:85a3:0000:0000:8a2e:0370:7334")
			  (cc-net:ip-from-string "2001:0db8:85a3:0000:0000:8a2e:0370:7334")))
      (is (= 4 (cc-net:ip-len (cc-net:ip-from-string "1.2.3.4"))))
      (is (= 16 (cc-net:ip-len (cc-net:ip-from-string "2001:0db8:85a3:0000:0000:8a2e:0370:7334"))))    
      )



