;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;; Utilities dealing with strings and characters.
;;; The symbols in this package are designed to be used the package prefix.
;;; Use the ACE.CORE namespace for simple syntax.
;;;

;;; TODO(czak): Unify the libraries across google3/lisp and travel/qpx.

(defpackage #:ace.core.string
  (:use #:common-lisp
        #:ace.core.defun
        #:ace.core.once-only)
  (:import-from #:ace.core.check #:dcheck #:check #:expect)
  (:import-from #:ace.core.macro
                #:with-gensyms
                #:gensymp
                #:without-code-deletion-notes
                #:eval*)
  (:import-from #:ace.core.vector #:index #:size #:with-vector)
  (:import-from #:ace.core.collect #:with-collected-values)
  (:export
   #:designator
   #:cat
   #:prefixp
   #:suffixp
   #:split
   #:tokenize
   #:delimit
   #:do-tokens
   #:search-replace
   #:join
   #:len
   #:emptyp
   #:index
   #:clear
   #:copy
   #:hash ; TODO(czak): Implement.
   #:read-as-keyword
   #:read-as-keyword-error
   ;; Also exported from the ACE.CORE namespace.
   #:end-of-token-p
   #:base-char-p
   #:whitespacep))

(in-package #:ace.core.string)

(deftype designator () "A type that can be passed to STRING." '(or string symbol character))

(deftype index? () "NULL or INDEX." '(or null index))

(defun check-string (string start end)
  "Checks if START and END forms are within the STRING if they all are constant forms."
  (cond ((not (stringp string)))
        ((and (integerp start) (integerp end))
         (expect (<= 0 start end (length string))))
        ((integerp start)
         (expect (<= 0 start (length string))))
        ((integerp end)
         (expect (<= 0 end (length string))))))

(defun* prefixp (prefix string &key (start1 0) end1 (start2 0) end2 ignore-case)
  (declare (self (string string &key index index? index index? t) boolean))
  "True if STRING starts with the PREFIX.
 Both strings can be restricted in their range using the START1, END1, START2, or END2 parameters.
 It is an error if START1, END1, START2, or END2 are outside of the corresponding string boundaries.
 It is an error for START1 > END1 or for START2 > END2.
 The errors are not necessarily signaled in optimized builds.
 Arguments:
  PREFIX - the supposed prefix of STRING.
  STRING - the string that is tested for the prefix.
  START1 - position on PREFIX to start comparison from.
  END1   - final exclusive position on PREFIX to end the comparison at.
  START2 - position on STRING to start comparison from.
  END2   - final exclusive position on the STRING to end the comparison at.
  IGNORE-CASE - if non-nil, the test will be case insensitive.
 Related:
  cl:string=
  cl:search
  string:suffixp
  sequence:prefixp
  alexandria:starts-with-subseq
  emacs:string-prefix-p
"
  #-opt
  (progn
    (dcheck (<= 0 start1 (length prefix)))
    (when end1 (dcheck (<= start1 end1 (length prefix))))
    (dcheck (<= 0 start2 (length string)))
    (when end2 (dcheck (<= start2 end2 (length string)))))

  (let* ((length (- (or end1 (length prefix)) start1))
         (end    (+ length start2)))
    (declare (type index end length))

    (and (<= end (or end2 (length string)))
         (if ignore-case
             (string-equal prefix string
                           :start1 start1 :end1 end1
                           :start2 start2 :end2 end)
             (string= prefix string
                      :start1 start1 :end1 end1
                      :start2 start2 :end2 end)))))

(define-compiler-macro* prefixp (&whole whole prefix string
                                 &key (start1 `(or ,start1 0)) end1
                                      (start2 `(or ,start2 0)) end2
                                      ignore-case)
  (declare (string prefix string)
           (index start1 start2)
           (index? end1 end2))
  (check-string prefix start1 end1)
  (check-string string start2 end2)
  (typecase ignore-case
    (boolean
     ;; TODO(czak): Make ONCE-ONLY* evaluate each init-form in succession.
     ;;  This cascade of ONCE-ONLY is needed because it evaluates all
     ;;  init-forms in parallel and we need PREFIX and STRING as a value.
     (once-only ((end1 `(or ,end1 (length ,prefix)))
                 (end2 `(or ,end2 (length ,string))))
       (declare (index end1 end2))
       (once-only ((end `(+ (- ,end1 ,start1) ,start2)))
         (declare (index end))
         `(and (<= ,end ,end2)
               (,(if ignore-case 'string-equal 'string=)
                ,prefix ,string
                :start1 ,start1 :end1 ,end1
                :start2 ,start2 :end2 ,end)))))
    (t (return whole))))

(defun* suffixp (suffix string &key (start1 0) end1 (start2 0) end2 ignore-case)
  (declare (self (string string &key index index? index index? t) boolean))
  "True if STRING ends with SUFFIX.
 Both strings can be restricted in their range using the START1, END1, START2, or END2 parameters.
 It is an error if START1, END1, START2, or END2 are outside of the corresponding string boundaries.
 It is an error for START1 > END1 or for START2 > END2.
 The errors are not necessarily signaled in optimized builds.
 Arguments:
  SUFFIX - the supposed suffix of STRING.
  STRING - the string that is tested for the suffix.
  START1 - position on SUFFIX to start comparison from.
  END1   - final exclusive position on SUFFIX to end the comparison at.
  START2 - position on the STRING to start comparison from.
  END2   - final and exclusive position on the STRING to end the comparison at.
  IGNORE-CASE - if non-nil, the test will be case insensitive.
 Related:
  cl:string=
  cl:search
  string:prefixp
  sequence:suffixp
  alexandria:string-ends-with-subseq
  emacs:string-suffix-p
"
  #-opt
  (progn
    (dcheck (<= start1 (length suffix)))
    (when end1 (dcheck (<= start1 end1 (length suffix))))
    (dcheck (<= start2 (length string)))
    (when end2 (dcheck (<= start2 end2 (length string)))))

  (let* ((length (- (or end1 (length suffix)) start1))
         (start  (- (or end2 (length string)) length)))
    ;; START maybe negative.
    (declare (type index length) (fixnum start))

    (and (>= start start2)
         (if ignore-case
             (string-equal suffix string
                           :start1 start1 :end1 end1
                           :start2 start :end2 end2)
             (string= suffix string
                      :start1 start1 :end1 end1
                      :start2 start :end2 end2)))))

(define-compiler-macro* suffixp (&whole whole suffix string
                                 &key (start1 `(or ,start1 0)) end1
                                      (start2 `(or ,start2 0)) end2
                                      ignore-case)
  (declare (string prefix string)
           (index start1 start2)
           (index? end1 end2))
  (check-string suffix start1 end1)
  (check-string string start2 end2)
  (typecase ignore-case
    (boolean
     (once-only ((end1 `(or ,end1 (length ,suffix)))
                 (end2 `(or ,end2 (length ,string))))
       (declare (index end1 end2))
       (once-only ((start `(- ,end2 (- ,end1 ,start1))))
         (declare (fixnum start))
         `(and (>= ,start ,start2)
               (,(if ignore-case 'string-equal 'string=)
                ,suffix ,string
                :start1 ,start1 :end1 ,end1
                :start2 ,start :end2 ,end2)))))
    (t (return whole))))

(defun* base-char-p (character)
  (declare (self inline foldable (character) boolean))
  "True if the CHARACTER is a base-char."
  (typep character 'base-char))

(defun* whitespacep (character)
  (declare (self inline foldable (character) (or null character)))
  "True if the CHARACTER is a whitespace character."
  (when (base-char-p character)
    (find
     (the base-char character)
     #.(coerce '(#\Space #\Backspace #\Tab #\Newline #\Linefeed #\Page #\Return)
               'simple-base-string)
     :test #'char=)))

(defun* end-of-token-p (char)
  (declare (self ((or null character)) boolean))
  "True if the CHAR terminates a token. I.e. it is whitespace, a terminating char, or NIL."
  (and (or (whitespacep char)
           (member char '(nil  #\( #\) #\" #\' #\` #\, #\;))) t))

;;;
;;; Tokenization - an exercise in optimization...
;;;

(deftype char-selector ()
  "A character, character sequence, or a function used to select characters."
  '(or (function (character) t) character sequence symbol))

(deftype char-selector? () "NULL or CHAR-SELECTOR" '(or null char-selector))

;;;
;;; Delimit ...
;;;

(defun* %%delimit-by-fn
    (call-back string by start end count keep-rest keep-empty)
  ;; This is a helper function for DELIMIT and TOKENIZE.
  ;; It uses a function to find the separators.
  ;; It is tempting to make this function inline. But it is 6 KB when optimized.
  (declare
   (self (function simple-string function index index index? t t) index))
  (macrolet
      ((%tok (&key count keep-empty)
         `(loop ,@(when count
                    '(:for token-count :of-type index :from count :above 0))
                ,@(if keep-empty
                      '(:for %token-start :of-type index = start
                        :then (1+ %token-end))
                      '(:for %token-start :of-type index?
                        = (position-if-not by string :start start :end end)
                        :then (position-if-not by string
                               :start (1+ %token-end) :end end)
                        :while %token-start))
                :for %token-end :of-type index?
                  = (position-if by string :start %token-start :end end)
                :do (funcall call-back %token-start (or %token-end end))
                :while %token-end
                :finally
                ,@(when count
                    `((when (and keep-rest %token-end
                                 (if keep-empty
                                     (< %token-end end)
                                     (< (1+ %token-end) end)))
                        (funcall call-back (1+ %token-end) end))))
                  (return (1+ (or %token-end end)))))
       (tok ()
         `(cond ((null count)
                 (if keep-empty (%tok :keep-empty t) (%tok)))
                ((zerop count)
                 (when (and keep-rest (or keep-empty (< start end)))
                   (funcall call-back start end))
                 start)
                (keep-empty    (%tok :count t :keep-empty t))
                (t             (%tok :count t)))))
    (declare (function by))
    (etypecase string
      (base-string (tok))
      ((array character) (tok)))))

(defun* %%delimit-by-char
    (call-back string char start end count keep-rest keep-empty)
  ;; This is a helper function for DELIMIT and TOKENIZE.
  ;; It uses a character as a separator.
  ;; It is tempting to make this function inline. But it is 6 KB when optimized.
  (declare
   (self (function simple-string character index index index? t t) index))
  (macrolet
      ((%tok (&key count keep-empty)
         `(loop ,@(when count
                    '(:for token-count :of-type index :from count :above 0))
                ,@(if keep-empty
                      '(:for %token-start :of-type index = start
                        :then (1+ %token-end))
                      '(:for %token-start :of-type index?
                        = (position char string
                           :start start :end end
                           :test-not #'char=)
                        :then (position char string
                               :start (1+ %token-end) :end end
                               :test-not #'char=)
                        :while %token-start))
                :for %token-end :of-type index?
                  = (position char string
                              :start %token-start :end end
                              :test #'char=)
                :do (funcall call-back %token-start (or %token-end end))
                :while %token-end
                :finally
                ,@(when count
                    `((when (and keep-rest %token-end
                                 (if keep-empty
                                     (< %token-end end)
                                     (< (1+ %token-end) end)))
                        (funcall call-back (1+ %token-end) end))))
                  (return (1+ (or %token-end end)))))
       (tok ()
         `(cond ((null count)
                 (if keep-empty (%tok :keep-empty t) (%tok)))
                ((zerop count)
                 (when (and keep-rest (or keep-empty (< start end)))
                   (funcall call-back start end))
                 start)
                (keep-empty    (%tok :count t :keep-empty t))
                (t             (%tok :count t)))))
    (etypecase string
      (base-string (tok))
      ((array character) (tok)))))

(defun* %%delimit (call-back string by start end count keep-rest keep-empty)
  ;; This function is a wrapper around %%delimit-by-fn and %%delimit-by-char.
  ;; It adds dynamic scope functions used the wrapped functions.
  (declare (self (function simple-string char-selector? index index index? t t)
                 index))
  (flet ((use (fn)
           (%%delimit-by-fn
            call-back string fn start end count keep-rest keep-empty))
         (lst (c)
           (find (the character c) (the list by) :test #'char=))
         (simple-str (c)
           (find (the character c)
                 (the (and simple-string (array character)) by)
                 :test #'char=))
         (str (c)
           (find (the character c) (the string by) :test #'char=))
         (array (c)
           (find (the character c) (the (array character) by) :test #'char=))
         (seq (c)
           (find (the character c) by :test #'char=)))
    (declare (dynamic-extent #'use #'simple-str #'str #'array #'seq))
    (etypecase by
      ((or null character)
       (%%delimit-by-char
        call-back string (or by #\Space) start end count keep-rest keep-empty))
      (function  (use by))
      (symbol    (use (symbol-function by)))
      (list      (use #'lst))
      ((and simple-string (array character))
       (use #'simple-str))
      (string    (use #'str))
      ((array character) (use #'array))
      (sequence  (use #'seq)))))

(define-compiler-macro %%delimit
    (&whole whole call-back string by start end count keep-rest keep-empty
            &environment env)
  (once-only (call-back string by)
    (declare (dynamic-extent call-back by) (inline call-back)
             (type index start) (type index? end count))
    (cond ((typep by '(cons (member function lambda)) env)
           `(%%delimit-by-fn ,call-back ,string ,by
                             ,start ,end ,count ,keep-rest ,keep-empty))
          ((constantp by env)
           (let ((by! (eval* by env)))
             (etypecase by!
               ((or null character)
                `(%%delimit-by-char ,call-back ,string ,(or by! #\Space)
                                    ,start ,end ,count ,keep-rest ,keep-empty))
               (list
                `(flet ((by (char) (find char (the list ,by) :test #'char=)))
                   (declare (dynamic-extent #'by))
                   (%%delimit-by-fn ,call-back ,string #'by
                                    ,start ,end ,count ,keep-rest ,keep-empty)))
               (sequence
                `(flet ((by (char) (find char ,by :test #'char=)))
                   (declare (dynamic-extent #'by))
                   (%%delimit-by-fn ,call-back ,string #'by
                                    ,start ,end ,count ,keep-rest ,keep-empty)))
               (symbol
                `(%%delimit-by-fn ,call-back ,string (coerce ,by 'function)
                                  ,start ,end ,count ,keep-rest ,keep-empty)))))
           (t
            (return whole)))))

(defun* delimit (call-back string
                           &key (by #\Space) (start 0) end
                           count keep-rest keep-empty)
  "Tokenize the STRING, splitting it at characters selected using BY.

Returns one plus the last scanned index while tokenizing.

Parameters:
  CALL-BACK  - a function receives each token as a pair of delimiting indexes.
  BY         - a predicate that returns true for each delimiter.
  START      - is the starting position on the string. Default: 0.
  END        - is the end position on the string. Default: nil.
  COUNT      - the maximal count of tokens to tokenize.
  KEEP-REST  - if true, the rest of the string is returned as the last token.
  KEEP-EMPTY - if true, empty tokens delimited using BY will be returned.
"
  (declare (self ((function (index index) *)
                  (or null string)
                  &key
                  (:by char-selector)
                  (:start index)
                  (:end index?)
                  (:count index?)
                  (:keep-rest t)
                  (:keep-empty t))
                 index))
  (etypecase string
    (null start)
    (simple-string
     (%%delimit call-back string by
                start (or end (length string))
                count keep-rest keep-empty))
    (string
     (with-vector ((%string string) (%start start) (%end end) :force-inline t)
       (declare (optimize #+sbcl (sb-c::insert-array-bounds-checks 0)))
       (let ((offset (- %start start)))
         (declare (index offset))
         (if (zerop offset)
             (%%delimit call-back %string by
                        %start %end count keep-rest keep-empty)
             (- (flet ((%call-back (%token-start %token-end)
                         (declare (index %token-start %token-end))
                         (funcall call-back
                                  (- %token-start offset)
                                  (- %token-end offset))))
                  (declare (inline %call-back) (dynamic-extent #'%call-back))
                  (%%delimit #'%call-back %string by
                             %start %end count keep-rest keep-empty))
                offset)))))))

(define-compiler-macro* delimit
    (call-back string
               &key by (start `(or ,start 0)) end count keep-rest keep-empty)
  (declare (function call-back) (inline call-back)
           (dynamic-extent call-back by)
           (type index start) (type index? end count))
  `(without-code-deletion-notes ;; make the compiler pay for it.
     (etypecase ,string
       (null ,start)
       (simple-string
        (%%delimit ,call-back ,string ,by
                   ,start (or ,end (length ,string))
                   ,count ,keep-rest ,keep-empty))
       (string
        (with-vector ((%string ,string) (%start ,start) (%end ,end)
                      :force-inline t)
          (declare (optimize #+sbcl (sb-c::insert-array-bounds-checks 0)))
          (let ((offset (- %start ,start)))
            (declare (index offset))
            (if (zerop offset)
                (%%delimit ,call-back %string ,by %start %end
                           ,count ,keep-rest ,keep-empty)
                (- (flet ((%call-back (%token-start %token-end)
                            (declare (index %token-start %token-end))
                            (funcall ,call-back
                                     (- %token-start offset)
                                     (- %token-end offset))))
                     (declare (inline %call-back)
                              (dynamic-extent #'%call-back))
                     (%%delimit #'%call-back %string ,by
                                %start %end ,count ,keep-rest ,keep-empty))
                   offset))))))))

;;;
;;; Tokenize ...
;;;

(defun* tokenize
    (call-back string
               &key (by #\Space) (start 0) end count keep-rest keep-empty)
  "Tokenize the STRING, splitting it at characters using the BY char selector.

Returns one plus the last scanned index while tokenizing.

Parameters:
  CALL-BACK  - a function receives each token as a simple-string.
  BY         - a null or a character-selector.
  START      - is the starting position on the string. Default: 0.
  END        - is the end position on the string. Default: nil.
  COUNT      - the maximal count of tokens to tokenize.
  KEEP-REST  - if true, the rest of the string is returned as the last token.
  KEEP-EMPTY - if true, empty tokens delimited using BY will be returned.
"
  (declare (self ((function (string) *)
                  (or null string)
                  &key
                  (:by char-selector)
                  (:start index)
                  (:end index?)
                  (:count index?)
                  (:keep-rest t)
                  (:keep-empty t))
                 index))
  (if string
      (with-vector ((%string string) (%start start) (%end end) :force-inline t)
        (declare (optimize #+sbcl (sb-c::insert-array-bounds-checks 0)))
        (let ((offset (- %start start)))
          (declare (index offset))
          (flet ((token (%token-start %token-end)
                   (declare (index %token-start %token-end))
                   (funcall call-back (subseq (the string %string)
                                              %token-start %token-end))))
            (declare (inline token) (dynamic-extent #'token))
            (- (%%delimit #'token %string by
                          %start %end count keep-rest keep-empty)
               offset))))
      start))

(define-compiler-macro* tokenize
    (call-back string
               &key by (start `(or ,start 0)) end count keep-rest keep-empty)
  (declare (function call-back) (inline call-back)
           (dynamic-extent call-back by)
           (type index start) (type index? end count))
  `(without-code-deletion-notes ;; make the compiler pay for it.
     (if ,string
         (with-vector ((%string ,string) (%start ,start) (%end ,end)
                       :force-inline t)
           (declare (optimize #+sbcl (sb-c::insert-array-bounds-checks 0)))
           (flet ((%token (%token-start %token-end)
                    (declare (index %token-start %token-end))
                    (funcall ,call-back
                             (subseq %string %token-start %token-end))))
             (declare (inline %token) (dynamic-extent #'%token))
             (- (%%delimit #'%token %string ,by
                           %start %end ,count ,keep-rest ,keep-empty)
                (the index (- %start ,start)))))
         ,start)))

;;;
;;; DO-TOKENS
;;;

(defmacro do-tokens ((token string
                      &key (by #\Space) (start 0) end
                      count keep-empty keep-rest)
                     &body body)
  "Tokenize the STRING using a char selector BY as a separator.

Returns one plus the last scanned index while tokenizing.

Parameters:
  TOKEN - an atom or a list: (start end) that is bound to the token or to
    the starting and ending indexes on the string respectively.
    E.g. to collect the tokens use:
      (do-tokens (token string) (push token tokens))
    E.g. to capitalize every word between hyphens:
      (do-tokens ((start-var end-var) string :by #\-)
         (nstring-capitalize string :start start-var :end end-var))

  STRING - is the input string to be split into tokens.
  BY     - a function, a character, or a multiset sequence of characters.
  START  - the start position on the STRING. Default: 0.
  END    - the end position on the STRING. NIL means the end of the STRING.
  COUNT  - the maximal count of tokens to tokenize.
  KEEP-REST  - if true, the rest of the string is returned as the last token.
  KEEP-EMPTY - if true, empty tokens delimited using BY will be returned.
"
  (check-type token (or symbol (cons symbol (cons symbol null))))
  `(block nil
     (flet ((&body ,(if (atom token) (list token) token) ,@body))
       (declare (dynamic-extent #'&body) (inline &body))
       (,(if (atom token) 'tokenize 'delimit)
        #'&body ,string
        ,@(unless (eql by #\Space) `(:by ,by))
        ,@(unless (eql start 0)    `(:start (or ,start 0)))
        ,@(when end `(:end ,end))
        ,@(when count `(:count ,count))
        ,@(when keep-empty `(:keep-empty ,keep-empty))
        ,@(when keep-rest `(:keep-rest ,keep-rest))))))

;;;
;;; SPLIT
;;;

(defun* split (string &key (by #\Space) (start 0) end
                      count keep-rest keep-empty)
  "Return tokens from the STRING by splitting it using the BY delimiter.

The string is split at the CHAR characters or at ones that pass the TEST.
START and END demarcate the starting and final position on the string.
Empty strings are not returned in the list unless SKIP-EMPTY is NIL.
STRING can be NIL which results in NIL.
It is an error for START or END to be outside the STRING boundaries.
It is an error for START > END.
The errors are not necessarily signaled in optimized builds.

Parameters:
  STRING - the string to be split. STRING = nil returns nil.
  BY     - a function, a character, or a multiset sequence of characters.
  START  - the first postion on the string to return values for.
  END    - the final exclusive position on the string to return values for.
  COUNT  - the maximal count of tokens to return (plus maybe the rest).
  KEEP-REST  - if true, the rest of the string is returned as the last token.
  KEEP-EMPTY - if true, empty tokens delimited using BY will be returned.
"
  (declare (self ((or null string)
                  &key
                  (:by char-selector)
                  (:start index)
                  (:end index?)
                  (:count index?)
                  (:keep-rest t)
                  (:keep-empty t))
                 list))
  (with-collected-values (collect)
    (declare (dynamic-extent #'collect))
    (tokenize #'collect string
              :by by :start start :end end
              :count count :keep-rest keep-rest :keep-empty keep-empty)))

(define-compiler-macro* split
    (string &key by start end count keep-rest keep-empty)
  (declare (type (or null string) string) (dynamic-extent by))
  `(when ,string
     (with-collected-values (collect)
       (declare (dynamic-extent #'collect))
       (tokenize #'collect ,string
                 ,@(unless (eql by #\Space) `(:by ,by))
                 ,@(unless (eql start 0)    `(:start (or ,start 0)))
                 ,@(when end `(:end ,end))
                 ,@(when count `(:count ,count))
                 ,@(when keep-rest `(:keep-rest ,keep-rest))
                 ,@(when keep-empty `(:keep-empty ,keep-empty))))))

;;;
;;; Etc.
;;;

(defun* cat (&rest things)
  "Concatenate the THINGS as a string."
  (declare (self foldable (&rest t) simple-string) (dynamic-extent things))
  (apply #'concatenate 'simple-string (mapcar #'string things)))
(define-compiler-macro cat (&rest things)
  `(concatenate 'simple-string ,@(mapcar (lambda (x) `(string ,x)) things)))

(defun* join (separator strings)
  "Concatenate STRINGS using the SEPARATOR which can be a string, a char, or a symbol."
  (declare (self foldable inline (designator list) simple-string))
  (if strings
      (let ((separator (string separator)))
        (apply #'concatenate 'simple-string (pop strings)
               (loop for part in strings
                     collect separator
                     collect part)))
      ""))

(defun* len (string)
  "Return STRING length."
  (declare (self foldable inline (string) size))
  (length string))

(defun* emptyp (string)
  "True if STRING is empty."
  (declare (self foldable inline (string) boolean))
  (zerop (the size (length string))))

(defun* search-replace (old new string &key (start 0) (end nil) (test #'char=))
  "Searches for the occurrences of OLD and replaces them with NEW in the STRING.
  The search takes place between START and END. The characters are compared using TEST.
  If OLD is not found in STRING, the STRING is returned.
  Otherwise, a new copy of STRING is returned where OLD is replaced with NEW."
  (declare (self foldable (string string string &key index index? function) simple-string))
  (with-vector ((old old) (start1 0) (end1 nil))
    (with-vector ((string string) (start2 start) (end2 end))
      (let ((index0 (search old string :test test
                                       :start1 start1 :end1 end1
                                       :start2 start2 :end2 end2)))
        (declare (type index? index0))
        (if index0
            (apply
             ;; CONCATENATE seems faster than WITH-OUTPUT-TO-STREAM or FORMAT.
             #'concatenate 'string
             (subseq string start index0)
             new
             (loop with skip of-type index = (length old)
                   for start2 of-type index = (+ index0 skip) then (+ index skip)
                   for index = (search old string :test test
                                                  :start1 start1 :end1 end1
                                                  :start2 start2 :end2 end2)
                   collect (subseq string start2 index)
                   when index
                     collect new
                   while index))
            string)))))

;;;
;;; TODO(czak): Remove READ-AS-KEYWORD.
;;;

(define-condition read-as-keyword-error (error)
  ((string :initarg :string :reader read-as-keyword-error-string))
  (:report (lambda (error out)
             (format out "Cannot convert ~S to a keyword!"
                     (read-as-keyword-error-string error))))
  (:documentation
   "An error that signals that a STRING could not be converted to a keyword."))

(defun* read-as-keyword (string)
  "Accepts a STRING and returns a keyword with the STRING read in the default reader case.
 The string should not have a colon character #\: at any other than the first position or
 two colon characters '::' at any other than the first two positions.
 The string should not have a sharp sign character #\# at the first position nor
 it should have any whitespace nor it should contain any token terminating characters.
 In case there are misplaced colon or sharp sign characters no value is read or interned.
 Also the string should read to a keyword as the function uses READ-FROM-STRING.
 Argument:
  string - a string or character for the string of the keyword.
 Signals:
  string:read-as-keyword-error - in case the STRING cannot be read to a keyword.
 Examples:
  (string:read-as-keyword \"tEsT\") => :TEST
 Related:
  alexandria:make-keyword
  iterate:keywordize
  asdf:keywordize
  cl-protobufs:keywordify
  qpx:keywordify"
  (declare (self (string) keyword))
  (let ((pos (position #\: string :from-end t :test #'char=)))
    (when (or (and pos (or (> pos 1) (char/= #\: (char string 0))))
              (and (plusp (length string)) (char= (char string 0) #\#))
              (position-if #'end-of-token-p string))
      (error 'read-as-keyword-error :string string)))
  (let* ((*package* (find-package "KEYWORD"))
         (result (read-from-string string)))
    (unless (typep result 'keyword)
      (error 'read-as-keyword-error :string string))
    result))

(defun* clear (string &key purge)
  "Clears a STRING destructively. Returns the CLEARED string or the empty string with PURGE."
  (declare (self inline (string &key boolean) string))
  (if (or purge (zerop (length string)))
      ""
      (adjust-array string 0)))

(defun* copy (string &key into)
  "Copies the STRING. If destination INTO is provided,
 it is attempted to copy the string into the destination.
 Returns the INTO destination or a new string."
  ;; The idea here is to reuse as much of the destinations's memory as possible before creating
  ;; a copy of the string in a new place.
  (declare (self (string &key (or null string)) string))
  (cond ((or (not into)
             (not (subtypep (array-element-type into) (array-element-type string))))
         (copy-seq string))
        ((eq string into)
         string)
        ((= (length string) (length into))
         (replace into string))
        ((and (array-has-fill-pointer-p into)
              (<= (length string) (array-dimension into 0)))
         (setf (fill-pointer into) (length string))
         (replace into string))
        (t
         (copy-seq string))))
