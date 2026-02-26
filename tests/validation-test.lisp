;;;; tests/validation-test.lisp
;;;; Tests for input validation utilities

(in-package :cl-tron-mcp/tests)

(deftest test-validate-required
  (testing "validate-required accepts non-nil values"
    (ok (eq (validate-required "test" "value") "value"))
    (ok (eq (validate-required "test" 123) 123))
    (ok (eq (validate-required "test" t) t)))

(deftest test-validate-required-fails
  (testing "validate-required rejects nil values"
    (signals validation-error
      (validate-required "test" nil))))

(deftest test-validate-string
  (testing "validate-string accepts valid strings"
    (ok (stringp (validate-string "test" "hello")))
    (ok (stringp (validate-string "test" "hello" :min-length 3)))
    (ok (stringp (validate-string "test" "hi" :max-length 10)))))

(deftest test-validate-string-fails
  (testing "validate-string rejects invalid strings"
    (signals validation-error
      (validate-string "test" 123))
    (signals validation-error
      (validate-string "test" "hi" :min-length 3))
    (signals validation-error
      (validate-string "test" "hello world" :max-length 5)))))

(deftest test-validate-integer
  (testing "validate-integer accepts valid integers"
    (ok (integerp (validate-integer "test" 42)))
    (ok (integerp (validate-integer "test" 10 :min 5)))
    (ok (integerp (validate-integer "test" 5 :max 10)))))

(deftest test-validate-integer-fails
  (testing "validate-integer rejects invalid integers"
    (signals validation-error
      (validate-integer "test" "not an integer"))
    (signals validation-error
      (validate-integer "test" 3 :min 5))
    (signals validation-error
      (validate-integer "test" 15 :max 10))))

(deftest test-validate-boolean
  (testing "validate-boolean accepts valid booleans"
    (ok (eq (validate-boolean "test" t) t))
    (ok (eq (validate-boolean "test" nil) nil))))

(deftest test-validate-boolean-fails
  (testing "validate-boolean rejects invalid booleans"
    (signals validation-error
      (validate-boolean "test" "true"))
    (signals validation-error
      (validate-boolean "test" 1))))

(deftest test-validate-choice
  (testing "validate-choice accepts valid choices"
    (ok (stringp (validate-choice "test" "flat" '("flat" "graph" "cumulative"))))
    (ok (stringp (validate-choice "test" "graph" '("flat" "graph" "cumulative"))))))

(deftest test-validate-choice-fails
  (testing "validate-choice rejects invalid choices"
    (signals validation-error
      (validate-choice "test" "invalid" '("flat" "graph" "cumulative")))))

(deftest test-validate-object-id
  (testing "validate-object-id accepts valid object IDs"
    (ok (stringp (validate-object-id "test" "123")))
    (ok (integerp (validate-object-id "test" 123)))))

(deftest test-validate-object-id-fails
  (testing "validate-object-id rejects invalid object IDs"
    (signals validation-error
      (validate-object-id "test" '(1 2 3)))))

(deftest test-validate-symbol-name
  (testing "validate-symbol-name accepts valid symbol names"
    (ok (stringp (validate-symbol-name "test" "my-function")))
    (ok (stringp (validate-symbol-name "test" "*global-var*")))
    (ok (stringp (validate-symbol-name "test" "my-package:symbol")))))

(deftest test-validate-symbol-name-fails
  (testing "validate-symbol-name rejects invalid symbol names"
    (signals validation-error
      (validate-symbol-name "test" "invalid symbol"))
    (signals validation-error
      (validate-symbol-name "test" "symbol@"))))

(deftest test-validate-package-name
  (testing "validate-package-name accepts valid package names"
    (ok (stringp (validate-package-name "test" "my-package")))
    (ok (stringp (validate-package-name "test" "CL-USER")))))

(deftest test-validate-package-name-fails
  (testing "validate-package-name rejects invalid package names"
    (signals validation-error
      (validate-package-name "test" "invalid package"))
    (signals validation-error
      (validate-package-name "test" "package@"))))

(deftest test-with-validation
  (testing "with-validation macro works correctly"
    (ok (with-validation
           ((validate-string "test" "hello"))
           :success))
    (ok (eq (with-validation
               ((validate-integer "test" 42 :min 0 :max 100))
               :success)
             :success))))

(deftest test-with-validation-fails
  (testing "with-validation macro returns error on validation failure"
    (let ((result (with-validation
                    ((validate-integer "test" 150 :max 100))
                    :success)))
      (ok (getf result :error))
      (ok (stringp (getf result :message)))
      (ok (string= (getf result :parameter) "test")))))