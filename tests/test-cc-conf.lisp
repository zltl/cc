
(in-package :cc-tests)

(setf (cc-conf:try-files cc-conf:*default-conf*)
      (list "./tests/test-config.yaml"
	    "no-exists-file-path-xxxxx")
      (cc-conf:name cc-conf:*default-conf*) "TEST"
      (cc-conf:from-env cc-conf:*default-conf*) t
      (cc-conf:env-prefix cc-conf:*default-conf*) "CC_TEST")

(cc-conf:+load-yaml)

(test test-conf
  (not (null (cc-conf:+load-yaml)))
  (is (null (cc-conf:+get-value "not.exists.name.x.y.z.kj")))
  (is (= 213 (cc-conf:+get-value "A")))
  (not (null (cc-conf:+get-value "B.d")))
  (is (string= (cc-conf:+get-value "B.d.exc") "this isstring"))
  (is (= 233.123 (cc-conf:+get-value "B.d.flv")))
  (is (= 5040 (cc-conf:parse-duration (cc-conf:+get-value "B.d.dura"))))
  (is (= 56789
	 (progn
	   (setf (uiop:getenv "CC_TEST_XYZ_IJK") "56789")
	   (cc-conf:+get-value "xYz.Ijk"))))
  (is (= 5.6789
	 (progn
	   (setf (uiop:getenv "CC_TEST_XYZ_OPQ") "5.6789")
	   (cc-conf:+get-value "xYz.opQ")))))
