(uiop:define-package print-as-pseudocode/print-as-pseudocode
  (:nicknames print-as-pseudocode)
  (:export print-as-pseudocode translate-file)
  (:import-from fiveam
                def-test def-suite is is-true is-false)
  (:import-from alexandria
                once-only curry rcurry)
  (:import-from gefjon-utils
                define-class with-slot-accessors)
  (:use cl iterate))
(in-package print-as-pseudocode/print-as-pseudocode)

(declaim (optimize (debug 3)))

(defgeneric write-as-pseudocode (object stream))

(defun print-as-pseudocode (object &optional (stream *standard-output*))
  (fresh-line stream)
  (pprint-logical-block (stream nil)
    (write-as-pseudocode object stream)
    (pprint-newline :mandatory stream)))

(defun translate-file (source-file destination-file)
  (with-open-file (out destination-file
                       :direction :output
                       :if-exists :supersede)
    (iter (for form in-file source-file)
      (unless (first-time-p)
        (terpri out))
      (print-as-pseudocode form out))))

(defmethod write-as-pseudocode ((char character) stream)
  (write-char #\' stream)
  (write-char char stream)
  (write-char #\' stream)
  (values))

(defmethod write-as-pseudocode ((true (eql 't)) stream)
  (declare (ignorable true))
  (write-string "True" stream)
  (values))

(defmethod write-as-pseudocode ((null null) stream)
  (declare (ignorable null))
  (write-string "Nil" stream)
  (values))

(defmethod write-as-pseudocode ((number number) stream)
  (write number :stream stream :pretty t))

(defmethod write-as-pseudocode ((- (eql '-)) stream)
  (write-char #\- stream)
  (values))

(defmethod write-as-pseudocode ((symbol symbol) stream)
  (write-string (substitute #\_ #\- (string-downcase (symbol-name symbol)))
                stream)
  (values))

(defgeneric write-form (stream head &rest tail))

(defgeneric write-maybe-parenthesized (expr stream)
  (:method ((symbol symbol) stream)
    (write-as-pseudocode symbol stream)
    (values))
  (:method ((string string) stream)
    (write-as-pseudocode string stream)
    (values))
  (:method ((number number) stream)
    (write-as-pseudocode number stream)
    (values))
  (:method (obj stream)
    (write-char #\( stream)
    (write-as-pseudocode obj stream)
    (write-char #\) stream)
    (values)))

(defun write-one-optional-arg (stream arg)
  (destructuring-bind (name default)
      (etypecase arg
        (symbol (list arg nil))
        (list arg))
    (write-as-pseudocode name stream)
    (write-string " = " stream)
    (write-as-pseudocode default stream)))

(defun write-arglist (arglist stream
                      &key (open "(")
                        (close ")")
                        (separator ", ")
                        before-last
                        (recurse #'write-as-pseudocode))
  (write-string open stream)
  (pprint-logical-block (stream nil)
    (iter (for (arg . more) on arglist)
      (unless (first-time-p)
        (write-string separator stream)
        (pprint-newline :linear stream))
      (unless more
        (when before-last
          (write-string before-last stream)))
      (funcall recurse arg stream)))
  (write-string close stream))

(defun write-lambda-list (arglist stream
                          &aux first-arg-done-p)
  (labels ((maybe-comma ()
             (if first-arg-done-p
                 (progn
                   (write-string ", " stream)
                   (pprint-newline :linear stream))
                 (setf first-arg-done-p t)))
           (print-normal-arg (&optional arg &rest more-args)
             (when arg
               (case arg
                 (&optional (apply #'print-optional-arg more-args))
                 (&rest (apply #'print-rest-arg more-args))
                 (&key (apply #'print-key-arg more-args))
                 (&aux (apply #'print-aux-arg more-args))
                 (otherwise
                  (maybe-comma)
                  (write-as-pseudocode arg stream)
                  (apply #'print-normal-arg more-args)))))
           (print-optional-arg (&optional arg &rest more-args)
             (when arg
               (case arg
                 (&rest (apply #'print-rest-arg more-args))
                 (&key (apply #'print-key-arg more-args))
                 (&aux (apply #'print-aux-arg more-args))
                 (otherwise
                  (maybe-comma)
                  (write-one-optional-arg stream arg)
                  (apply #'print-optional-arg more-args)))))
           (print-rest-arg (&optional arg &rest more-args)
             (when arg
               (case arg
                 (&aux (apply #'print-aux-arg more-args))
                 (&key (apply #'print-key-arg more-args))
                 (otherwise
                  (maybe-comma)
                  (write-string "..." stream)
                  (write-as-pseudocode arg stream)
                  (apply #'print-rest-arg more-args)))))
           (print-key-arg (&optional arg &rest more-args)
             (when arg
               (case arg
                 (&aux (apply #'print-aux-arg more-args))
                 (&allow-other-keys (apply #'print-key-arg more-args))
                 (otherwise
                  (maybe-comma)
                  (write-one-optional-arg stream arg)
                  (apply #'print-key-arg more-args)))))
           (print-aux-arg (&optional arg &rest more-args)
             (when arg
               (maybe-comma)
               (write-one-optional-arg stream arg)
               (apply #'print-aux-arg more-args))))
    (write-char #\( stream)
    (pprint-logical-block (stream arglist)
      (apply #'print-normal-arg arglist)))
  (write-char #\) stream)
  (values))

(defun write-as-nested-lists (seq stream)
  (etypecase seq
    ((or symbol number) (write-as-pseudocode seq stream))
    (sequence 
     (write-char #\[ stream)
     (pprint-logical-block (stream nil)
       (iter (for elt in-sequence seq)
         (unless (first-time-p)
           (write-string ", " stream)
           (pprint-newline :fill stream))
         (write-as-nested-lists elt stream)))
     (write-char #\] stream)))
  (values))

(defmethod write-as-pseudocode ((vector vector) stream)
  (write-as-nested-lists vector stream)
  (values))

(defun write-as-funcall (stream operator &rest args)
  (write-maybe-parenthesized operator stream)
  (write-arglist args stream)
  (values))

(defmethod write-form (stream (head symbol) &rest args)
  (apply #'write-as-funcall stream head args)
  (values))

(defmethod write-as-pseudocode ((string string) stream)
  (write string :stream stream
                :escape t))

(defmethod write-as-pseudocode ((list cons) stream)
  (apply #'write-form stream list)
  (values))

(defmacro define-form ((name &rest args) (stream) &body body)
  `(defmethod write-form (,stream (head (eql ',name)) &rest args)
     (declare (ignorable head))
     (destructuring-bind ,args args
       ,@body)
     (values)))

(defun list-of-progn-p (list)
  (and (null (rest list))
       (consp (first list))
       (eq (first (first list)) 'progn)))

(defmacro with-block (stream &body body)
  (check-type stream symbol)
  `(progn
     (write-char #\{ ,stream)
     (pprint-newline :mandatory ,stream)
     (write-string "  " ,stream)
     (pprint-logical-block (,stream nil)
       ,@body)
     (pprint-newline :mandatory ,stream)
     (write-char #\} ,stream)))

(defun write-progn (exprs stream)
  (when (list-of-progn-p exprs)
    (return-from write-progn (write-progn (rest (first exprs)) stream)))

  (with-block stream
    (iter (for expr in exprs)
      (unless (first-time-p)
        (pprint-newline :mandatory stream))
      (write-as-pseudocode expr stream))))

(define-form (progn &rest exprs) (stream)
  (write-progn exprs stream))

(define-form (lambda arglist &body body) (stream)
  (write-string "function" stream)
  (write-lambda-list arglist stream)
  (write-char #\space stream)
  (apply #'write-form stream 'progn body))

(defun write-local-bindings (stream bind-fn bindings body)
  (iter (for binding in bindings)
    (funcall bind-fn stream binding)
    (pprint-newline :mandatory stream))
  (iter (for body-form in body)
    (unless (first-time-p)
      (pprint-newline :mandatory stream))
    (write-as-pseudocode body-form stream))
  (values))

(define-form (setf place value &rest others) (stream)
  (write-string "set " stream)
  (write-as-pseudocode place stream)
  (write-string " := " stream)
  (write-as-pseudocode value stream)
  (when others
    (pprint-newline :mandatory stream)
    (apply #'write-form stream 'setf others)))

(defun write-defun (stream fn)
  (destructuring-bind (name arglist &body body) fn
    (write-string "function " stream)
    (write-as-pseudocode name stream)
    (write-lambda-list arglist stream)
    (write-char #\space stream)
    (write-progn body stream)))

(define-form (defun &rest fn) (stream)
  (write-defun stream fn))

(defun write-let-binding (stream binding
                          &key (kind "let")
                            (write-binding #'write-as-pseudocode))
  (destructuring-bind (name initform)
      (etypecase binding
        (list binding)
        (symbol (list binding nil)))
    (write-string kind stream)
    (write-char #\space stream)
    (funcall write-binding name stream)
    (write-string " = " stream)
    (write-as-pseudocode initform stream))
  (values))

(define-form (defparameter name value) (stream)
  (write-let-binding stream (list name value)))

(define-form (let* bindings &body body) (stream)
  (write-local-bindings stream #'write-let-binding bindings body)
  (values))

(defun write-infix (stream operator operands)
  (pprint-logical-block (stream nil)
    (iter (for operand in operands)
      (unless (first-time-p)
        (pprint-newline :fill stream)
        (write-char #\space stream)
        (write-as-pseudocode operator stream)
        (write-char #\space stream))
      (write-maybe-parenthesized operand stream))))

(defun write-prefix (stream operator operand)
  (write-as-pseudocode operator stream)
  (write-maybe-parenthesized operand stream))

(define-form (+ &rest operands) (stream)
  (write-infix stream '+ operands))

(define-form (- &rest operands) (stream)
  (if (= (length operands) 1)
      (write-prefix stream '- (first operands))
      (write-infix stream '- operands)))

(define-form (* &rest operands) (stream)
  (write-infix stream '* operands))

(define-form (/ &rest operands) (stream)
  (if (= (length operands) 1)
      (write-infix stream '/ (cons 1 operands))
      (write-infix stream '/ operands)))

(define-form (not thing) (stream)
  (write-prefix stream '! thing))

(define-form (and &rest operands) (stream)
  (write-infix stream '&& operands))

(define-form (or &rest operands) (stream)
  (write-infix stream '\|\| operands))

(define-form (flet functions &body body) (stream)
  (write-local-bindings stream #'write-defun functions body))

(define-form (labels functions &body body) (stream)
  (write-local-bindings stream #'write-defun functions body))

(define-form #'function (stream)
  (write-as-pseudocode function stream))

(define-form 'thing (stream)
  (etypecase thing
    (symbol (write-as-pseudocode (string-downcase (symbol-name thing)) stream))
    (list (write-as-nested-lists thing stream))))

(define-form (list &rest stuff) (stream)
  (write-arglist stuff stream :open "[" :close "]"))

(define-form (vector &rest stuff) (stream)
  (write-arglist stuff stream :open "[" :close "]"))

(define-form (funcall fn &rest args) (stream)
  (apply #'write-as-funcall stream fn args))

(define-form (apply fn &rest args) (stream)
  (write-maybe-parenthesized fn stream)
  (write-arglist args stream :before-last "..."))

(define-form (iter &body body) (stream)
  (write-string "iterate " stream)
  (write-progn body stream))

(defun write-iter-form (stream head thing other-things)
  (write-as-pseudocode head stream)
  (write-char #\space stream)
  (write-as-pseudocode thing stream)
  (write-char #\space stream)
  (pprint-logical-block (stream nil)
    (iter (for (preposition value) on other-things by #'cddr)
      (unless (first-time-p)
        (write-char #\space stream)
        (pprint-newline :fill stream))
      (write-as-pseudocode preposition stream)
      (write-char #\space stream)
      (write-as-pseudocode value stream)))
  (values))

(defmacro define-iter-forms (&rest heads)
  (cons 'progn
        (iter (for head in heads)
          (collect `(define-form (,head thing &rest other-things) (stream)
                      (write-iter-form stream ',head thing other-things))))))

(define-iter-forms
  ;; variable bindings
  for with
  ;; reductions
  sum summing
  multiply multiplying
  counting
  maximize maximizing
  minimize minimizing
  reducing
  ;; accumulations
  collect collecting
  adjoining
  appending
  nconcing
  unioning
  nunioning
  accumulate accumulating
  ;; finders
  finding
  ;; tests
  always never thereis
  ;; control flow
  finish leave while until
  ;; code placement
  initially
  after-each
  else)

(define-form (finally &body body) (stream)
  (write-string "finally " stream)
  (write-progn body stream))

(define-form (if test then else) (stream)
  (write-string "if " stream)
  (write-maybe-parenthesized test stream)
  (write-char #\space stream)
  (write-progn (list then) stream)
  (write-string " else " stream)
  (write-progn (list else) stream))

(define-form (when test &body body) (stream)
  (write-string "if " stream)
  (write-maybe-parenthesized test stream)
  (write-char #\space stream)
  (write-progn body stream))

(define-form (unless test &body body) (stream)
  (write-string "if " stream)
  (write-prefix stream '! test)
  (write-char #\space stream)
  (write-progn body stream))

(define-form (return thing) (stream)
  (write-string "return " stream)
  (write-as-pseudocode thing stream))

(define-form (return-from block-name value) (stream)
  (write-string "return " stream)
  (write-as-pseudocode value stream)
  (write-string " from " stream)
  (write-as-pseudocode block-name stream))

(define-form (defmacro name arglist &body body) (stream)
  (write-string "macro " stream)
  (write-as-pseudocode name stream)
  (write-lambda-list arglist stream)
  (write-char #\space stream)
  (write-progn body stream))

(define-form (#+sbcl sb-int:quasiquote thing) (stream)
  (write-string "syntax template: " stream)
  (write thing :stream stream))

(define-form (define-class name slots &key superclasses &allow-other-keys)
    (stream)
  (write-string "class " stream)
  (write-as-pseudocode name stream)
  (when superclasses
    (write-string " : " stream)
    (write-arglist superclasses stream :open "" :close ""))
  (write-char #\space stream)
  (with-block stream
    (pprint-logical-block (stream nil)
      (iter (for slot in slots)
        (unless (first-time-p)
          (pprint-newline :mandatory stream))
        (destructuring-bind (name type &key (initform nil init-supplied-p) &allow-other-keys)
            slot
          (write-as-pseudocode name stream)
          (write-string ": " stream)
          (write-as-pseudocode type stream)
          (when init-supplied-p
            (write-string " = " stream)
            (write-as-pseudocode initform stream)))))))

(defun write-specializer (specializer stream)
  (etypecase specializer
    (symbol (write-as-pseudocode specializer stream))
    (list (destructuring-bind (name type) specializer
            (write-as-pseudocode name stream)
            (write-string ": " stream)
            (write-as-pseudocode type stream)))))

(define-form (defmethod name arglist &body body) (stream)
  (write-string "method " stream)
  (write-as-pseudocode name stream)
  ;; FIXME: rewrite in terms of WRITE-LAMBDA-LIST
  (write-arglist arglist stream :recurse #'write-specializer)
  (write-char #\space stream)
  (write-progn body stream))

(define-form (defgeneric name arglist &rest stuff) (stream)
  (declare (ignore stuff))
  (write-string "generic " stream)
  (write-as-pseudocode name stream)
  (write-lambda-list arglist stream))

(defun write-case (stream term &rest clauses)
  (write-string "switch " stream)
  (write-maybe-parenthesized term stream)
  (write-char #\space stream)
  (with-block stream
    (iter (for (thing . body) in clauses)
      (unless (first-time-p)
        (pprint-newline :mandatory stream))
      (write-as-pseudocode thing stream)
      (write-string " -> " stream)
      (write-progn body stream))))

(define-form (etypecase term &body clauses) (stream)
  (apply #'write-case stream `(type-of ,term) clauses))

(define-form (ecase term &body clauses) (stream)
  (apply #'write-case stream term clauses))

(define-form (cond &body clauses) (stream)
  (iter (for (test . then) in clauses)
    (unless (first-time-p)
      (write-string " else " stream))
    (write-string "if " stream)
    (write-maybe-parenthesized test stream)
    (write-char #\space stream)
    (write-progn then stream)))

(define-form (make-instance class &rest pairs) (stream)
  (write-string "construct " stream)
  (write-as-pseudocode class stream)
  (write-char #\space stream)
  (with-block stream
    (iter (for (initarg initform) on pairs by #'cddr)
      (unless (first-time-p)
        (pprint-newline :mandatory stream))
      (write-as-pseudocode initarg stream)
      (write-string ": " stream)
      (write-as-pseudocode initform stream))))

(define-form (aref arr &rest indices) (stream)
  (write-as-pseudocode arr stream)
  (write-arglist indices stream :open "[" :close "]"))

(define-form (1+ thing) (stream)
  (apply #'write-form stream '+ thing (list 1)))

(define-form (1- thing) (stream)
  (apply #'write-form stream '- thing (list 1)))

(define-form (values &rest values) (stream)
  (if values
      (write-arglist values stream)
      (write-as-pseudocode nil stream)))

(define-form (def-suite name) (stream)
  (write-string "define test suite " stream)
  (write-as-pseudocode name stream))

(define-form (def-test name (&key suite &allow-other-keys) &body body) (stream)
  (write-string "test " stream)
  (write-as-pseudocode name stream)
  (write-char #\space stream)
  (when suite
    (write-string "in suite " stream)
    (write-as-pseudocode suite stream)
    (write-char #\space stream))
  (write-progn body stream))

(define-form (map result-type function &rest sequences) (stream)
  (declare (ignore result-type))
  (write-string "map " stream)
  (write-as-pseudocode function stream)
  (write-string " across " stream)
  (write-arglist sequences stream :open "" :close ""))

(define-form (uiop:define-package name &rest stuff) (stream)
  (declare (ignore stuff))
  (write-string "define namespace " stream)
  (write-as-pseudocode name stream)
  (write-string " { contents elided }" stream))

(define-form (in-package name) (stream)
  (write-string "in namespace " stream)
  (write-as-pseudocode name stream))

(defmacro define-equality (&rest operators)
  (cons 'progn
        (iter (for op in operators)
          (collect `(define-form (,op &rest stuff) (stream)
                      (write-infix stream '== stuff))))))

(define-equality eq eql equal equalp = char= string=)

(define-form (is test &rest stuff) (stream)
  (declare (ignore stuff))
  (write-string "test for " stream)
  (write-as-pseudocode test stream))

(define-form (is-false test &rest stuff) (stream)
  (declare (ignore stuff))
  (write-string "test not " stream)
  (write-as-pseudocode test stream))

(define-form (is-true test &rest stuff) (stream)
  (declare (ignore stuff))
  (write-string "test for " stream)
  (write-as-pseudocode test stream))

(define-form (destructuring-bind pattern term &body body) (stream)
  (write-local-bindings stream
                        (rcurry #'write-let-binding
                                :write-binding #'write-lambda-list)
                        `((,pattern ,term))
                        body))

(define-form (typep thing type) (stream)
  (write-as-pseudocode thing stream)
  (write-string " is of type " stream)
  (write-as-pseudocode 
   (if (and (consp type) (eq (first type) 'quote))
       (second type)
       type)
   stream))

(defmacro define-orders (&rest order-ops)
  (cons 'progn
        (iter (for op in order-ops)
          (collect `(define-form (,op &rest numbers) (stream)
                      (write-infix stream ',op numbers))))))

(define-orders < <= > >=)

(define-form (with-slot-accessors slots instance &body body) (stream)
  (write-local-bindings stream
                        (rcurry #'write-let-binding
                                :kind "unpack"
                                :write-binding #'write-progn)
                        `((,slots ,instance))
                        body))

(define-form (assert condition &rest stuff) (stream)
  (declare (ignore stuff))
  (write-string "assert " stream)
  (write-as-pseudocode condition stream))

(define-form (with-output-to-string stuff &body body) (stream)
  (declare (ignore stuff))
  (write-string "print_to_string " stream)
  (write-progn body stream))

(define-form (dotimes (var count) &body body) (stream)
  (write-string "for " stream)
  (write-as-pseudocode var stream)
  (write-string " from 0 below " stream)
  (write-as-pseudocode count stream)
  (write-char #\space stream)
  (write-progn body stream))
