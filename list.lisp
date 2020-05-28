;;; Copyright 2020 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;; Utilities related to lists and conses.
;;; The package is designed to be used by other packages.
;;;

(ace.core:defpackage* #:ace.core.list
  (:use #:common-lisp
        #:ace.core.once-only
        #:ace.core.defun
        #:ace.core.collect)
  (:import-from #:ace.core.macro
                ace.core.macro:find-type-declaration
                ace.core.macro:with-gensyms
                ace.core.macro:eval*
                ace.core.macro:lexenv
                ace.core.macro:gensym*
                ace.core.macro:gensymp
                ace.core.macro:eval-always)
  (:shadow cl:find
           cl:find-if
           cl:find-if-not
           cl:position
           cl:position-if
           cl:position-if-not
           cl:remove
           cl:remove-if
           cl:remove-if-not
           cl:delete
           cl:delete-if
           cl:delete-if-not
           cl:length)
  (:export
   #:length
   #:length=
   #:elements-eq
   #:match
   #:copy-if #:copy-if-not
   #:find #:find-if #:find-if-not
   #:position #:position-if #:position-if-not
   #:remove #:remove-if #:remove-if-not
   #:delete #:delete-if #:delete-if-not
   #:delete-adjacent

   #:removef
   #:removef-if
   #:removef-if-not
   #:deletef
   #:deletef-if
   #:deletef-if-not

   #:partition
   #:npartition
   #:dolist*
   #:car-or-atom
   #:mappend
   #:intersectp
   #:push*
   #:npush*
   #:appendf
   #:nconcf
   #:nreversef
   #:unionf
   #:intersectionf))

(cl:in-package #:ace.core.list)

(deftype position () "Position on the list." `(integer 0 ,array-dimension-limit))
(deftype position* () "Position on the list or NIL." '(or position null))
(deftype item-count () "Count of elements." `(integer 0 ,array-dimension-limit))
(deftype item-count* () "Count of elements or NIL." '(or item-count null))

(deftype function$ () "Function or symbol" '(or function symbol))

(defun* length (list)
  "Returns the length of the LIST.
No precautions are made as to if the LIST is circular."
  (declare (self inline foldable (list) position))
  (cl:length list))

(defun* length= (list1 list2)
  "Return true if the lengths of LIST1 and LIST2 are equal."
  (declare (self inline (list list) boolean))
  (do ((l1 list1 (cdr l1))
       (l2 list2 (cdr l2)))
      ((not (and l1 l2))
       (not (or l1 l2)))))

(defun* elements-eq (list1 list2)
  "True if LIST1 and LIST2 are equal lists - with elements compared using EQ.
The lists need to be in equal length for the function to return true.
The function will also return true if the lists or their sub-lists compare EQ."
  (declare (self inline (list list) boolean))
  (do ((l1 list1 (cdr l1))
       (l2 list2 (cdr l2)))
      ((eq l1 l2) t)
    (unless (eq (car l1) (car l2))
      (return nil))))

(defun* match (list1 list2 &key
                           (test #'eql) (key #'identity)
                           (start1 0) (start2 0))
"Compares elements of LIST1 and LIST2 using TEST function over KEY values.

Parameters START1 and START2 determine where the comparison starts on each of
the lists, respectively.

True if there is a match between the lists.
The second and third value are sub-list at position where the
mismatch (or match) was determined.

The LIST:MATCH will short the comparison if the lists or their sub-list are EQ.
In this case (values T <sub-LIST1> <sub-LIST2>) are returned.
"
  (declare (self inline (list list &key function function) boolean t t))
  (do ((l1 (nthcdr start1 list1) (cdr l1))
       (l2 (nthcdr start2 list2) (cdr l2)))
      ((eq l1 l2) (values t l1 l2))
    (unless (and l1 l2 (funcall test
                                (funcall key (car l1))
                                (funcall key (car l2))))
      (return (values nil l1 l2)))))

(define-modify-macro appendf (&rest more-lists) append
  "Modify macro that appends the first argument (PLACE) and MORE-LISTS together
 and assigns the result to PLACE.")
(define-modify-macro nconcf (&rest more-lists) nconc
  "Modify macro that concatenates the first argument (PLACE) and MORE-LISTS together
 and assigns the result to PLACE. The operation may be destructive for all arguments.")
(define-modify-macro nreversef () nreverse
  "Modify macro that reverses the first argument (PLACE) and assigns the result to it.")

(define-modify-macro intersectionf (list1 list2 &rest args) union
  "Modify macro that creates an intersection of LIST1 and LIST2 and sets it in place of LIST1.")
(define-modify-macro unionf (list1 list2 &rest args) union
  "Modify macro that creates a union of LIST1 and LIST2 and sets it in place of LIST1.")

;;;
;;; FIND(-*) and POSITION(-*) functions with implicit LIST type declaration.
;;;

(defconstant +find-constant-list-unfold-length+ 7
  "Constant list up to this length are unfolded instead of creating a LOOP.")

(eval-always
(defun %find-if-form (whole predicate list &rest args &key start end from-end key)
  (once-only ((predicate `(coerce ,predicate 'function))
              list
              &rest args &key start end from-end
              (key `(coerce (or ,key #'identity) 'function)))
    (declare (list list) (position start) (position* end)
             (function predicate key)
             (inline predicate key)
             (dynamic-extent predicate key))
    ;; Use a LOOP
    (let* ((when/unless (ecase (first whole)
                          ((find position find-if position-if) 'when)
                          ((find-if-not position-if-not) 'unless)))
           (posp (cl:find (first whole) '(position position-if position-if-not) :test #'eq))
           (%pos (and (or posp end) (gensym* :pos)))
           (%item (gensym "ITEM-"))
           ;; just for aesthetics.
           (key (unless (eq key '#'identity) key))
           (%result (and from-end (gensym* :result)))
           (env (lexenv))
           (unfold
            (and (constantp list env)
                 (constantp start env)
                 (constantp end env)
                 (not from-end)))
           (start-value (and unfold (if start (eval* start) 0)))
           (end-value (and unfold end (eval* end)))
           (list-items (and unfold list (eval* list)))
           (list-length (and unfold (length list-items)))
           (block (and unfold (gensym* :block))))
      (if (and list-length (<= list-length +find-constant-list-unfold-length+))
          ;; Unfold the list.
          `(block ,block
             ,@(loop
                 :for item :in (nthcdr start-value list-items)
                 :for pos :from (or start-value 0)
                 :while (or (null end-value) (< pos end-value))
                 :collect
                 `(,when/unless
                   (funcall ,predicate
                            ,(if key `(funcall ,key ',item) `(quote ,item)))
                   (return-from ,block ,(if posp pos `(quote ,item))))))
          ;; Use LOOP
          `(loop
             ,@(when %result `(:with ,%result = nil))
             ,@(when %pos `(:for ,%pos :of-type position :from ,(or start 0)))
             ,@(cond ((gensymp end) `(:while (or (null ,end) (< ,%pos ,end))))
                     (end `(:while (< ,%pos ,end))))
             :for ,%item :in ,(if start `(nthcdr ,start ,list) list)
             :do
             (,when/unless
              (funcall ,predicate ,(if key `(funcall ,key ,%item) %item))
              ,@(when %result `((setf ,%result ,(if posp %pos %item))))
              ,@(case from-end
                  ((t)   nil)
                  ((nil) `((return ,(or %result (if posp %pos %item)))))
                  (t     `((unless ,from-end (return ,%result))))))
             ;; FINALLY
             ,@(when %result `(:finally (return ,%result)))))))))

(eval-always
(defun %remove-if-form (whole predicate list &rest args &key start end from-end count key)
  (once-only ((predicate `(coerce ,predicate 'function))
              list
              &rest args &key start end from-end count
              (key `(coerce (or ,key #'identity) 'function)))
    (declare (list list) (position start) (position* end)
             (type item-count* count)
             (function predicate key)
             (inline predicate key)
             (dynamic-extent predicate key))
    ;; Use a LOOP
    (let* ((count-function
            (ecase (first whole)
              ((delete delete-if delete-if-not) 'delete-count)
              ((remove remove-if remove-if-not) 'remove-count)
              ((copy-if copy-if-not)            'copy-if-count)))
           (simple-function
            (ecase (first whole)
              ((delete delete-if delete-if-not) '%%delete)
              ((remove remove-if remove-if-not) '%%remove)
              ((copy-if copy-if-not)            '%%copy-if)))
           (%item (gensym* :item))
           ;; just for aesthetics.
           (key (unless (eq key '#'identity) key))
           (env (lexenv))
           (pred/key
            (cond
              ((member (first whole) '(remove-if-not delete-if-not copy-if-not))
               `(lambda (,%item)
                  (not (funcall
                        ,predicate ,(if key `(funcall ,key ,%item) %item)))))
              (key
               `(lambda (,%item)
                  (funcall ,predicate (funcall ,key ,%item))))
              (t
               predicate))))
      ;; Need a dynamic extent FLET here.
      (once-only (pred/key)
        (declare (function pred/key)
                 (inline pred/key)
                 (dynamic-extent pred/key))
        (if (and count (or (not (constantp count env)) (eval* count env)))
            `(,count-function ,pred/key ,list ,start ,end ,count ,from-end)
            `(,simple-function ,pred/key ,list (or ,start 0) ,end)))))))

(eval-always
(defun %find/remove-form (whole item list &rest args
                          &key start end from-end key test test-not count)
  (once-only (item list
              &rest args
              &key start end from-end key test test-not count)
    (declare (type (or function null) key test test-not)
             (inline key test test-not)
             (dynamic-extent key test test-not))
    (let* ((x item)
           (y (gensym "Y-"))
           (predicate
            (cond ((and test test-not (not (gensymp test)) (not (gensymp test-not)))
                   (warn "Both TEST and TEST-NOT specified in ~A." whole) ; NOLINT
                   `(error "Both TEST and TEST-NOT used in ~A." ',(first whole)))
                  ((or (gensymp test) (gensymp test-not))
                   (let ((%test (if (gensymp test) test (and test t)))
                         (%test-not (if (gensymp test-not) test-not (and test-not t))))
                     `(cond ((and ,%test ,%test-not)
                             (error "Both TEST and TEST-NOT used in ~A." ',(first whole)))
                            (,%test     (lambda (,y) (funcall ,test ,x ,y)))
                            (,%test-not (lambda (,y) (not (funcall ,test-not ,x ,y))))
                            (t          (lambda (,y) (eql ,x ,y))))))
                  (test-not `(lambda (,y) (not (funcall ,test-not ,x ,y))))
                  (test     `(lambda (,y) (funcall ,test ,x ,y)))
                  (t        `(lambda (,y) (eql ,x ,y))))))
      (cond ((member (car whole) '(delete remove))
             (%remove-if-form
              whole predicate list
              :start start :end end
              :from-end from-end :key key :count count))
            ((member (car whole) '(find position))
             (when count
               (warn "~A does not accept a COUNT argument." (car whole))) ; NOLINT
             (%find-if-form
              whole predicate list
              :start start :end end
              :from-end from-end :key key)))))))

(defun* find (item list &key from-end (start 0) end key test test-not)
  "Find ITEM in a LIST. This is only marginally faster than CL:FIND
because it assumes that the sequence is a LIST and KEY, TEST, TEST-NOT are functions.
Parameters: FROM-END, START, END, KEY, TEST, TEST-NOT - as in CL:FIND."
  (declare (self (t list &key
                    (:key function$)
                    (:test function$)
                    (:test-not function$))
                 t))
  (cl:find item (the list list)
           :from-end from-end :start start :end end :key key
           :test test :test-not test-not))
(define-compiler-macro find (&whole whole &rest args)
  (apply #'%find/remove-form whole args))

(defun* find-if (predicate list &key from-end (start 0) end key)
  "Find an item in a LIST that matches the PREDICATE.
This is only marginally faster than CL:FIND-IF
because it assumes that PREDICATE is a function and the sequence is a LIST.

Parameters: FROM-END, START, END, KEY - as in CL:FIND-IF."
  (declare (self (function$ list &key (:key function$)) t))
  (cl:find-if
   predicate (the list list) :from-end from-end :start start :end end :key key))
(define-compiler-macro find-if (&whole whole &rest args)
  (apply #'%find-if-form whole args))

(defun* find-if-not (predicate list &key from-end (start 0) end key)
  "Find an item in a LIST that matches the PREDICATE.
This is only marginally faster than CL:FIND-IF-NOT
because it assumes that PREDICATE is a function and the sequence is a LIST.

Parameters: FROM-END, START, END, KEY - as in CL:FIND-IF-NOT."
  (declare (self (function$ list &key (:key function$)) t))
  (cl:find-if-not
   predicate (the list list) :from-end from-end :start start :end end :key key))
(define-compiler-macro find-if-not (&whole whole &rest args)
  (apply #'%find-if-form whole args))

(defun* position (item list &key from-end (start 0) end key test test-not)
  "Find the position of the ITEM in a LIST. This is only marginally faster than CL:POSITION
because it assumes that the sequence is a LIST and KEY, TEST, TEST-NOT are functions.
Parameters: FROM-END, START, END, KEY, TEST, TEST-NOT - as in CL:POSITION."
  (declare (self (t list &key
                    (:key function$)
                    (:test function$)
                    (:test-not function$))
                 position*))
  (cl:position item (the list list)
               :from-end from-end :start start :end end
               :key key :test test :test-not test-not))
(define-compiler-macro position (&whole whole &rest args)
  (apply #'%find/remove-form whole args))

(defun* position-if (predicate list &key from-end (start 0) end key)
  "Find the position of an ITEM in a LIST for which the PREDICATE is true.
This is only marginally faster than CL:POSITION-IF because it assumes
that the sequence is a LIST and KEY, TEST, TEST-NOT are functions.
Parameters: FROM-END, START, END, KEY - as in CL:POSITION-IF."
  (declare (self (function$ list &key (:key function$)) position*))
  (cl:position-if predicate (the list list)
                  :from-end from-end :start start :end end :key key))
(define-compiler-macro position-if (&whole whole &rest args)
  (apply #'%find-if-form whole args))

(defun* position-if-not (predicate list &key from-end (start 0) end key)
  "Find the position of an ITEM in a LIST for which the PREDICATE is NIL.
This is only marginally faster than CL:POSITION-IF because it assumes
that the sequence is a LIST and KEY, TEST, TEST-NOT are functions.
Parameters: FROM-END, START, END, KEY - as in CL:POSITION-IF."
  (declare (self (function$ list &key (:key function$)) position*))
  (cl:position-if-not
   predicate (the list list) :from-end from-end :start start :end end :key key))
(define-compiler-macro position-if-not (&whole whole &rest args)
  (apply #'%find-if-form whole args))


;;;
;;; REMOVE(-*) and DELETE(-*) functions with implicit LIST type declaration.
;;;
;;; (cl:remove item sequence
;;;  &key from-end (test #'eql) test-not (start 0) end count key)
;;;
;;; (cl:remove-if predicate sequence &key from-end (start 0) end count key)
;;;
;;; (cl:delete item sequence
;;;  &key from-end (test #'eql) test-not (start 0) end count key)
;;;
;;; (cl:delete-if predicate sequence &key from-end (start 0) end count key)

(defun* %%delete (pred list start end)
  (declare (self (function list position position*) list))
  (cond
    ((null end)
     (let ((handle (cons nil list)))
       (declare (dynamic-extent handle))
       (do* ((previous (nthcdr start handle))
             (current (cdr previous) (cdr current)))
            ((null current)
             (cdr handle))
         (if (funcall pred (car current))
             (rplacd previous (cdr current))
             (pop previous)))))
    ((< start end)
     (let ((handle (cons nil list)))
       (declare (dynamic-extent handle))
       (do* ((previous (nthcdr start handle))
             (current (cdr previous) (cdr current))
             (index start (1+ index)))
            ((or (null current) (= index end))
             (cdr handle))
         (declare (position index))
         (if (funcall pred (car current))
             (rplacd previous (cdr current))
             (pop previous)))))
    (list)))

;; TODO(czak, dougk): Make SBCL inline stuff properly.
(define-compiler-macro %%delete (&whole w pred list start end &environment env)
  (if (and (constantp end env) (null (eval* end env)))
      ;; inline
      (with-gensyms (%handle %prev %cur)
        (once-only (pred start)
          `(let ((,%handle (cons nil ,list)))
             (declare (dynamic-extent ,%handle))
             (do* ((,%prev ,(if (member start '(nil 0))
                                %handle
                                `(nthcdr ,start ,%handle)))
                   (,%cur (cdr ,%prev) (cdr ,%cur)))
                  ((null ,%cur)
                   (cdr ,%handle))
               (if (funcall ,pred (car ,%cur))
                   (rplacd ,%prev (cdr ,%cur))
                   (pop ,%prev))))))
      w))

(defun* %%delete-count (pred list start end count)
  (declare (self inline (function list position position item-count) list))
  (let ((handle (cons nil list)))
    (declare (position start end) (dynamic-extent handle))
    (do* ((previous (nthcdr start handle))
          (current (cdr previous) (cdr current))
          (index start (1+ index)))
         ;; Assume (<= end len) ->
         ;;   no need to check for (null current) here.
         ((or (= index end) (zerop count))
          (cdr handle))
      (declare (position index))
      (cond ((funcall pred (car current))
             (rplacd previous (cdr current))
             (decf count))
            (t
             (pop previous))))))

(defun* delete-count (predicate list start end count &optional from-end)
  "Delete items matching PREDICATE from the LIST.

The LIST will be modified destructively.

Parameters:
 PREDICATE - a boolean function.
 LIST - the original list that will be modified.
 START - index on the list where deleting starts (default 0).
 END - index on the list before which the deleting stops (default end of list).
 COUNT - number of items to delete.
 FROM-END - if true the list will be reversed twice.
"
  (declare
   (self (function list position* position* item-count* &optional t) list))
  (cond
    ((null list) nil)
    ((null count)
     (%%delete predicate list (or start 0) end))
    ((zerop count) list)
    ((let* ((start (or start 0))
            (len (length list))
            (end (if end (min end len) len)))
       (declare (position start end) (item-count len))
       (cond
         ((>= start end) list)
         ((>= count (- end start))
          ;; Just simply call %%remove
          (%%delete predicate list start end))
         ((not from-end)
          (%%delete-count predicate list start end count))
         (t
           ;; Need to reverse the list twice.
          (nreverse
            (%%delete-count
             predicate (nreverse list)
             (- len end) (- len start) count))))))))

;; remove

(defun* %%remove (pred list start end)
  (declare (self (function list position position*) list))
  (cond ((null end)
         (loop
           :for items. :on (nthcdr start list)
           :when (funcall pred (car items.))
             :do (return
                   (with-collected-values ((collect :mode :nconc))
                     (collect (ldiff list items.)) ; copy
                     (loop
                       :with rem. = (cdr items.)
                       :for items. :on rem.
                       :do (when (funcall pred (car items.))
                             (collect (ldiff rem. items.)) ; copy
                             (setf rem. (cdr items.)))
                       :finally (collect rem.)))) ; tail
           :finally (return list)))
        ((< start end)
         (loop
           :for items. :on (nthcdr start list)
           :for i :of-type position :from start :below end
           :when (funcall pred (car items.))
             :do (return
                   (with-collected-values ((collect :mode :nconc))
                     (collect (subseq list 0 i)) ; copy
                     (loop
                       :with rem. = (cdr items.)
                       :for items. :on rem.
                       :for j :of-type position :from (1+ i) :below end
                       :do (when (funcall pred (car items.))
                             (collect (ldiff rem. items.)) ; copy
                             (setf rem. (cdr items.)))
                       :finally (collect rem.)))) ; tail
           :finally (return list)))
        (list)))

;; TODO(czak, dougk): Make SBCL inline stuff properly.
(define-compiler-macro %%remove (&whole w pred list start end &environment env)
  (if (and (constantp end env) (null (eval* end env)))
      ;; inline
      (with-gensyms (%items. %rem. %collect)
        (once-only (pred list start)
          `(loop
             :for ,%items. :on ,(if (member start '(nil 0))
                                    list
                                    `(nthcdr ,start ,list))
             :when (funcall ,pred (car ,%items.))
             :do (return
                   (with-collected-values ((,%collect :mode :nconc))
                     (,%collect (ldiff ,list ,%items.)) ; copy
                     (loop
                       :with ,%rem. = (cdr ,%items.)
                       :for ,%items. :on ,%rem.
                       :do (when (funcall ,pred (car ,%items.))
                             (,%collect (ldiff ,%rem. ,%items.)) ; copy
                             (setf ,%rem. (cdr ,%items.)))
                       :finally (,%collect ,%rem.)))) ; tail
           :finally (return ,list))))
      w))

(defun* %%remove-count (pred list start end count)
  (declare (self inline (function list position position item-count) list))
  (loop
    :for items. :on (nthcdr start list)
    :for i :of-type position :from start :below end
    :when (funcall pred (car items.))
      :do (decf count)
          (return
            (with-collected-values ((collect :mode :nconc))
              (collect (subseq list 0 i)) ; copy
              (loop
                :with rem. = (cdr items.)
                :while (plusp count)
                :for items. :on rem.
                :for j :of-type position :from (1+ i) :below end
                :do (when (funcall pred (car items.))
                      (collect (ldiff rem. items.)) ; copy
                      (setf rem. (cdr items.))
                      (decf count))
                :finally (collect rem.)))) ; tail
    :finally (return list)))

(defun* remove-count (predicate list start end count &optional from-end)
  "Remove items matching PREDICATE from the LIST.

The LIST will not be modified, but its tail maybe reused in the new list.

Parameters:
 PREDICATE - a boolean function.
 LIST - the original list that will be modified.
 START - index on the list where removing starts (default 0).
 END - index on the list before which the removing stops (default end of list).
 COUNT - number of items to remove.
 FROM-END - if true the list will be reversed twice.
"
  (declare
   (self (function list position* position* item-count* &optional t) list))
  (cond
    ((null list) nil)
    ((null count)
     (%%remove predicate list (or start 0) end))
    ((zerop count) list)
    ((let* ((start (or start 0))
            (len (length list))
            (end (if end (min end len) len)))
       (declare (position start end) (item-count len))
       (cond
         ((>= start end) list)
         ((>= count (- end start))
          ;; Just simply call %%remove
          (%%remove predicate list start end))
         ((not from-end)
          (%%remove-count predicate list start end count))
         (t
           ;; Need to reverse the list twice.
           (nreverse
            (%%delete-count
             predicate (reverse list) ; copy
             (- len end) (- len start) count))))))))

(defun* remove-if (predicate list &key from-end (start 0) end key count)
  "Remove any item in the LIST that matches the PREDICATE.

This operation is not destructive to the LIST.
Unlike CL:REMOVE-IF, this will return the original LIST unless something was
actually removed from it. An unchanged tail of the LIST, maybe also be returned.

Parameters: FROM-END, START, END, KEY, COUNT - as in CL:REMOVE-IF.
"
  (declare (self (function$ list &key (:key function$)) t))
  (let ((predicate (coerce predicate 'function))
        (key (and key (coerce key 'function))))
    (labels ((pred (x) (funcall predicate x))
             (pred/key (x) (funcall #'pred (funcall key x))))
      (declare (dynamic-extent #'pred #'pred/key) (inline pred pred/key))
      (remove-count (if key #'pred/key #'pred) list start end count from-end))))
(define-compiler-macro remove-if (&whole whole &rest args)
  (apply #'%remove-if-form whole args))

(defun* remove-if-not (predicate list &key from-end (start 0) end key count)
  "Remove any item in the LIST that does not match the PREDICATE.

This operation is not destructive to the LIST.
Unlike CL:REMOVE-IF-NOT, this will return the original LIST unless something was
actually removed from it. An unchanged tail of the LIST, maybe also be returned.

Parameters: FROM-END, START, END, KEY, COUNT - as in CL:REMOVE-IF-NOT.
"
  (declare (self (function$ list &key (:key function$)) t))
  (let ((predicate (coerce predicate 'function))
        (key (and key (coerce key 'function))))
    (flet ((predicate (x) (not (funcall predicate x))))
      (declare (inline predicate) (dynamic-extent #'predicate))
      (remove-if
       #'predicate list
       :from-end from-end :start start :end end :key key :count count))))
(define-compiler-macro remove-if-not (&whole whole &rest args)
  (apply #'%remove-if-form whole args))

(defun* remove (item list &key from-end (start 0) end key test test-not count)
  "Remove any item in the LIST that match the ITEM using KEY, TEST, and TEST-NOT
functions for comparison. This operation is not destructive to the LIST.

Unlike CL:REMOVE, this will return the original LIST unless something was
actually removed from it. An unchanged tail of the LIST, maybe also be returned.

Parameters: FROM-END, START, END, TEST, TEST-NOT, KEY, COUNT - as in CL:REMOVE.
"
  (declare (self (t list &key
                    (:key function$)
                    (:test function$)
                    (:test-not function$))
                 t))
  (let ((key (and key (coerce key 'function)))
        (test (and test (coerce test 'function)))
        (test-not (and test-not (coerce test-not 'function))))
    (flet ((p1 (y) (not (funcall test-not item y)))
           (p2 (y) (funcall test item y))
           (p3 (y) (eql item y)))
      (declare (inline p1 p2 p3) (dynamic-extent #'p1 #'p2 #'p3))
      (remove-if
       (cond (test-not
              (when test (error "Both TEST and TEST-NOT used in ~A." 'remove))
              #'p1)
             (test #'p2)
             (t #'p3))
       list
       :from-end from-end :start start :end end
       :key key :count count))))
(define-compiler-macro remove (&whole whole &rest args)
  (apply #'%find/remove-form whole args))

;; delete

(defun* delete-if (predicate list &key from-end (start 0) end key count)
  "Delete any item in the LIST that matches the PREDICATE.

This operation is destructive to the LIST unless nothing was deleted.

Parameters: FROM-END, START, END, KEY, COUNT - as in CL:DELETE-IF.
"
  (declare (self (function$ list &key (:key function$)) t))
  (let ((predicate (coerce predicate 'function))
        (key (and key (coerce key 'function))))
    (labels ((pred (x) (funcall predicate x))
             (pred/key (x) (funcall #'pred (funcall key x))))
      (declare (dynamic-extent #'pred #'pred/key) (inline pred pred/key))
      (delete-count (if key #'pred/key #'pred) list start end count from-end))))
(define-compiler-macro delete-if (&whole whole &rest args)
  (apply #'%remove-if-form whole args))

(defun* delete-if-not (predicate list &key from-end (start 0) end key count)
  "Delete any item in the LIST that does not match the PREDICATE.

This operation is destructive to the LIST unless nothing was deleted.

Parameters: FROM-END, START, END, KEY, COUNT - as in CL:DELETE-IF-NOT.
"
  (declare (self (function$ list &key (:key function$)) t))
  (let ((predicate (coerce predicate 'function))
        (key (and key (coerce key 'function))))
    (flet ((predicate (x) (not (funcall predicate x))))
      (declare (inline predicate) (dynamic-extent #'predicate))
      (delete-if
       #'predicate list
       :from-end from-end :start start :end end :key key :count count))))
(define-compiler-macro delete-if-not (&whole whole &rest args)
  (apply #'%remove-if-form whole args))

(defun* delete (item list &key from-end (start 0) end key test test-not count)
  "Delete any item in the LIST that match the ITEM using KEY, TEST, and TEST-NOT
functions for comparison.

This operation is destructive to the LIST unless nothing was deleted.

Parameters: FROM-END, START, END, TEST, TEST-NOT, KEY, COUNT - as in CL:REMOVE.
"
  (declare (self (t list &key
                    (:key function$)
                    (:test function$)
                    (:test-not function$))
                 t))
  (let ((key (and key (coerce key 'function)))
        (test (and test (coerce test 'function)))
        (test-not (and test-not (coerce test-not 'function))))
    (flet ((p1 (y) (not (funcall test-not item y)))
           (p2 (y) (funcall test item y))
           (p3 (y) (eql item y)))
      (declare (inline p1 p2 p3) (dynamic-extent #'p1 #'p2 #'p3))
      (delete-if
       (cond (test-not
              (when test (error "Both TEST and TEST-NOT used in ~A." 'delete))
              #'p1)
             (test #'p2)
             (t #'p3))
       list
       :from-end from-end :start start :end end
       :key key :count count))))
(define-compiler-macro delete (&whole whole &rest args)
  (apply #'%find/remove-form whole args))

;;; DELETE-ADJACENT

(defun* %%delete-adjacent (list test start)
  (declare (self inline (list function position) list))
  (loop
    :with rest = (nthcdr start list)
    :while (cdr rest)
    :do
    (if (funcall test (first rest) (second rest))
        (pop (cdr rest))
        (pop rest)))
  list)
#-opt (declaim (notinline %%delete-adjacent))

(defun* %%delete-adjacent-end (list test start end)
  (declare (self (list function position position) list))
  (loop
    :with rest = (nthcdr start list)
    :with end-1 fixnum = (1- end)
    :while (and (cdr rest) (< start end-1))
    :do
    (if (funcall test (first rest) (second rest))
        (pop (cdr rest))
        (pop rest))
    (incf start))
  list)

(defun* %%delete-adjacent-keyed (list test key start end)
  (declare (self (list function function position position*) list))
  (cond
    (end
     (loop
       :with rest = (nthcdr start list)
       :with end-1 fixnum = (1- end)
       :while (and (cdr rest) (< start end-1))
       :with head = (funcall key (first rest))
       :do
       (if (funcall test head (setf head (funcall key (second rest))))
           (pop (cdr rest))
           (pop rest))
       (incf start)))
    (t
     (loop
       :with rest = (nthcdr start list)
       :while (cdr rest)
       :with head = (funcall key (first rest))
       :do
       (if (funcall test head (setf head (funcall key (second rest))))
           (pop (cdr rest))
           (pop rest)))))
  list)

(defun* delete-adjacent (list &key (test #'eql) key (start 0) end from-end)
  "Delete adjacent items from the LIST passing the TEST predicate.

The list will be modified destructively.
If FROM-END is NIL, the first of the duplicated items is retained.
Otherwise, the last of the duplicates in the sequence is retained.

Only elements within (start, end) range can be deleted, if FROM-END is NIL.
If FROM-END is true, elements within [start, end-1) are conditionally deleted.

KEY, if present, will be applied only to elements within the [start, end) range.

Parameters:
 LIST - the original list that will be modified.
 TEST - a boolean function of two parameters.
 KEY - a function applied to each element of the list.
 START - index on the list where removing starts (default 0).
 END - index on the list before which the removing stops (default end of list).
 FROM-END - if true the list will be reversed twice and the last of the
   duplicated items in the sequence is retained.
"
;; TODO(czak): COUNT - number of items to remove.
  (declare (self inline
                 (list &key
                       (:test function$)
                       (:key function$)
                       (:start position)
                       (:end position*)
                       (:from-end t))
                 list))
  (flet ((dedup (list start end)
           (let ((test (coerce test 'function)))
             (cond
               (key (%%delete-adjacent-keyed
                     list test (coerce key 'function) start end))
               (end (%%delete-adjacent-end list test start end))
               (t   (%%delete-adjacent list test start))))))
    (declare (inline dedup))
    (cond
      (from-end
       (let* ((len nil)
              (%start (if end (max 0 (- (setf len (length list)) end)) 0))
              (%end (and (> start 0) (max 0 (- (or len (length list)) start)))))
         (declare (position %start) (type (or null fixnum) len %end))
         (nreverse (dedup (nreverse list) %start %end))))
      (t
       (dedup list start end)))))

;;;
;;; COPY-IF, COPY-IF-NOT
;;;

(defun* %%copy-if (pred list start end)
  (declare (self (function list position position*) list))
  (cond ((null end)
         (loop :for item :in (nthcdr start list)
               :when (funcall pred item)
                 :collect item))
        ((< start end)
         (loop :for item :in (nthcdr start list)
               :for i :of-type position :from start :below end
               :when (funcall pred item)
                 :collect item))))

;; TODO(czak, dougk): Make SBCL inline stuff properly.
(define-compiler-macro %%copy-if
    (&whole w pred list start end &environment env)
  (if (and (constantp end env) (null (eval* end env)))
      ;; inline
      (with-gensyms (%item)
        (once-only (pred list start)
          `(loop :for ,%item
                   :in ,(if (member start '(nil 0))
                            list
                            `(nthcdr ,start ,list))
                 :when (funcall ,pred ,%item)
                   :collect ,%item)))
      w))

(defun* %%copy-if-count (pred list start end count)
  (declare (self inline (function list position position item-count) list))
  (loop
    :while (plusp count)
    :for item :in (nthcdr start list)
    :for i :of-type position :from start :below end
    :when (funcall pred item)
      :do (decf count)
      :and :collect item))

(defun* copy-if-count (predicate list start end count &optional from-end)
  "Copy items matching PREDICATE from the LIST.

The LIST will not be modified and a new copy is returned.

Parameters:
 PREDICATE - a boolean function.
 LIST - the original list that will not be modified.
 START - index on the list where copy starts (default 0).
 END - index on the list before which the copy stops (default end of list).
 COUNT - number of items to copy.
 FROM-END - if true the list will be reversed twice.
"
  (declare
   (self (function list position* position* item-count* &optional t) list))
  (cond
    ((null list) nil)
    ((null count)
     (%%copy-if predicate list (or start 0) end))
    ((zerop count) nil)
    ((let* ((start (or start 0))
            (len (length list))
            (end (if end (min end len) len)))
       (declare (position start end) (item-count len))
       (cond
         ((>= start end) nil)
         ((>= count (- end start))
          ;; Just simply call %%copy-if
          (%%copy-if predicate list start end))
         ((not from-end)
          (%%copy-if-count predicate list start end count))
         (t
           ;; Need to reverse the list twice.
           (nreverse
            (%%copy-if-count
             predicate (nreverse (subseq list start end))
             0 (- end start) count))))))))

(defun* copy-if (predicate list &key from-end (start 0) end key count)
  "Copy any item in the LIST that matches the PREDICATE and return a new
fresh list.

This operation is not destructive to the LIST.
Unlike LIST:REMOVE-IF-NOT this will actually copy every cons in the list.

Parameters:
 PREDICATE - a boolean function.
 LIST - the original list that will not be modified.
 START - index on the list where copy starts (default 0).
 END - index on the list before which the copy stops (default end of list).
 COUNT - number of items to copy.
 FROM-END - if true the list will be reversed twice.
"
  (declare (self (function$ list &key (:key function$)) t))
  (let ((predicate (coerce predicate 'function))
        (key (and key (coerce key 'function))))
    (labels ((pred (x) (funcall predicate x))
             (pred/key (x) (funcall #'pred (funcall key x))))
      (declare (dynamic-extent #'pred #'pred/key) (inline pred pred/key))
      (copy-if-count
       (if key #'pred/key #'pred)
       list start end count from-end))))
(define-compiler-macro copy-if (&whole whole &rest args)
  (apply #'%remove-if-form whole args))

(defun* copy-if-not (predicate list &key from-end (start 0) end key count)
  "Copy any item in the LIST that does not match the PREDICATE and return
a new fresh list.

This operation is not destructive to the LIST.
Unlike LIST:REMOVE-IF this will actually copy every cons in the list.

Parameters:
 PREDICATE - a boolean function.
 LIST - the original list that will not be modified.
 START - index on the list where copy starts (default 0).
 END - index on the list before which the copy stops (default end of list).
 COUNT - number of items to copy.
 FROM-END - if true the list will be reversed twice.
"
  (declare (self (function$ list &key (:key function$)) t))
  (let ((predicate (coerce predicate 'function)))
    (flet ((predicate (x) (not (funcall predicate x))))
      (declare (inline predicate) (dynamic-extent #'predicate))
      (copy-if
       #'predicate list
       :from-end from-end :start start :end end :key key :count count))))
(define-compiler-macro copy-if-not (&whole whole &rest args)
  (apply #'%remove-if-form whole args))

;;;
;;; Modifying macros.
;;;

(defmacro removef (item list &rest rest
                   &key from-end test test-not start end count key
                   &environment env)
  "Remove all elements from the LIST that match the ITEM.
LIST needs to be a place and is assigned the new returned LIST.

The REST parameters are:
 FROM-END - if true, will start deleting from the end,
 TEST - the equality test used to compare ITEM with the elements in the list,
 TEST-NOT - the complement of the TEST,
 START - the START index (default 0),
 END - the END index (default is NIL, the end of list),
 COUNT - the maximum count of elements to be deleted,
 KEY - a function that derives values to be compared with the ITEM."
  (declare (ignore from-end test test-not start end count key))
  (multiple-value-bind (vars vals places setter getter) (get-setf-expansion list env)
    `(let* (,@(mapcar #'list vars vals)
            (,(first places) (remove ,item ,getter ,@rest))
            ,@(rest places))
       ,setter)))

(defmacro removef-if (predicate list &rest rest
                      &key from-end start end count key
                      &environment env)
  "Remove all elements from the LIST that match the PREDICATE.
LIST needs to be a place and is assigned the new returned LIST.

The REST parameters are:
 FROM-END - if true, will start deleting from the end,
 START - the START index (default 0),
 END - the END index (default is NIL, the end of list),
 COUNT - the maximum count of elements to be deleted,
 KEY - a function that derives values to be tested by the predicate."
  (declare (ignore from-end start end count key))
  (multiple-value-bind (vars vals places setter getter) (get-setf-expansion list env)
    `(let* (,@(mapcar #'list vars vals)
            (,(first places) (remove-if ,predicate ,getter ,@rest))
            ,@(rest places))
       ,setter)))

(defmacro removef-if-not (predicate list &rest rest
                          &key from-end start end count key
                          &environment env)
  "Remove all elements from the LIST that does NOT match the PREDICATE.
LIST needs to be a place and is assigned the new returned LIST.

The REST parameters are:
 FROM-END - if true, will start deleting from the end,
 START - the START index (default 0),
 END - the END index (default is NIL, the end of list),
 COUNT - the maximum count of elements to be deleted,
 KEY - a function that derives values to be tested by the predicate."
  (declare (ignore from-end start end count key))
  (multiple-value-bind (vars vals places setter getter) (get-setf-expansion list env)
    `(let* (,@(mapcar #'list vars vals)
            (,(first places) (remove-if-not ,predicate ,getter ,@rest))
            ,@(rest places))
       ,setter)))

(defmacro deletef (item list &rest rest
                   &key from-end test test-not start end count key
                   &environment env)
  "Destructively delete all elements from the LIST that match the ITEM.
LIST needs to be a place and is assigned the returned modified LIST.

The REST parameters are:
 FROM-END - if true, will start deleting from the end,
 TEST - the equality test used to compare ITEM with the elements in the list,
 TEST-NOT - the complement of the TEST,
 START - the START index (default 0),
 END - the END index (default is NIL, the end of list),
 COUNT - the maximum count of elements to be deleted,
 KEY - a function that derives values to be compared with the ITEM."
  (declare (ignore from-end test test-not start end count key))
  (multiple-value-bind (vars vals places setter getter) (get-setf-expansion list env)
    `(let* (,@(mapcar #'list vars vals)
            (,(first places) (delete ,item ,getter ,@rest))
            ,@(rest places))
       ,setter)))

(defmacro deletef-if (predicate list &rest rest
                      &key from-end start end count key
                      &environment env)
  "Destructively delete all elements from the LIST that match the PREDICATE.
LIST needs to be a place and is assigned the returned modified LIST.

The REST parameters are:
 FROM-END - if true, will start deleting from the end,
 START - the START index (default 0),
 END - the END index (default is NIL, the end of list),
 COUNT - the maximum count of elements to be deleted,
 KEY - a function that derives values to be tested by the predicate."
  (declare (ignore from-end start end count key))
  (multiple-value-bind (vars vals places setter getter) (get-setf-expansion list env)
    `(let* (,@(mapcar #'list vars vals)
            (,(first places) (delete-if ,predicate ,getter ,@rest))
            ,@(rest places))
       ,setter)))

(defmacro deletef-if-not (predicate list &rest rest
                          &key from-end start end count key
                          &environment env)
  "Destructively delete all elements from the LIST that does NOT match the PREDICATE.
LIST needs to be a place and is assigned the returned modified LIST.

The REST parameters are:
 FROM-END - if true, will start deleting from the end,
 START - the START index (default 0),
 END - the END index (default is NIL, the end of list),
 COUNT - the maximum count of elements to be deleted,
 KEY - a function that derives values to be tested by the predicate."
  (declare (ignore from-end start end count key))
  (multiple-value-bind (vars vals places setter getter) (get-setf-expansion list env)
    `(let* (,@(mapcar #'list vars vals)
            (,(first places) (delete-if-not ,predicate ,getter ,@rest))
            ,@(rest places))
       ,setter)))

;;;
;;; etc ...
;;;

(defun* car-or-atom (list-or-atom)
  "Return the CAR of the LIST-OR-ATOM or, if it is not a list, return the ATOM.
 Examples:
   (car-or-atom 'foo) => FOO
   (car-or-atom '(1 2 3)) => 1"
  (declare (self inline foldable (t) t))
  (if (consp list-or-atom) (car list-or-atom) list-or-atom))

(defmacro dolist* ((var list &optional result) &body body)
  "Iterate on the LIST, destructuring VAR on successive tails of the LIST starting with LIST itself.
 The RESULT, by default NIL, is returned from DOLIST* unless the body has been left by RETURN.
 Examples:
   (dolist* (x '(1 2 3)) (format t \"~A~%\" x))

   (1 2 3)
   (2 3)
   (3)
   => NIL

   (dolist* ((head . tail) '(1 2 3) head)
      (format t \"Head: ~A, remaining count: ~A~%\" head (length tail)))

   Head: 1, remaining count: 2
   Head: 2, remaining count: 1
   Head: 3, remaining count: 0
   => 3"
  `(loop :for ,var :on ,list :do (progn ,@body) :finally (return ,result)))

(defun* mappend (function list &rest more-lists)
  "Similar to MAPCAN except that it uses APPEND instead of NCONC.
Returns a concatenation of the lists returned by the FUNCTION as applied
to successive arguments derived from the LIST and from MORE-LIST.
FUNCTION must always return a list."
  (declare (self inline ((or symbol cons function) list &rest list) list)
           (dynamic-extent more-lists))
  (let ((function (coerce function 'function)))
    (declare (function function))
    (if more-lists
        (apply #'mapcan
               (lambda (arg &rest args) (copy-list (apply function arg args)))
               list more-lists)
        (loop for item in list append (funcall function item)))))

(define-compiler-macro* mappend (function &rest lists)
  (declare (function function) (inline function) (dynamic-extent function))
  (let ((%items (loop :for i :in lists :collect (gensym* :item))))
    `(loop ,@(mapcan (lambda (%i l) `(:for ,%i :in ,l)) %items lists)
           :append (funcall ,function ,@%items))))

(defun* intersectp (list1 list2 &key (key #'identity) (test #'eql))
  "True if LIST1 and LIST2 intersect. O(N^2) runtime.

Returns NIL or the first item in LIST1 found in LIST2.

Parameters:
 LIST1, LIST2 - the lists to check for common elements,
 KEY - the key function to be applied to the elements of the lists (default #'identity).
 TEST - the test function comparing the elements of the lists (default #'eql)."
  (declare (self (list list &key (:key function) (:test function)) t))
  (loop for i1 in list1 thereis (find (funcall key i1) list2 :test test :key key)))

;;;
;;; List partition function.
;;;

(defmacro define-partition-function (name &key mode documentation)
  "Make a definition for a PARTITION function with NAME.
 MODE is the collector mode used to combine the cells of the resulting lists (or nil).
 DOCUMENTATION is the documentation for the function."
  `(progn
     (defun* ,name (list &rest tests) ; NOLINT
       ,@(when documentation `(,documentation))
       (declare (self inline (list &rest (function (t) t)) list &rest list))
       (flet ((partition-in-two (test)
                (declare (type (function (t) t) test))
                (with-collected-values ((true :mode ,mode) (false :mode ,mode))
                  (dolist* (elt list)
                    (if (funcall test (car elt))
                        (true  ,(if mode 'elt '(car elt)))
                        (false ,(if mode 'elt '(car elt))))))))
         (declare (inline partition-in-two))
         (cond
           ((null tests) list)
           ((null (cdr tests))
            (partition-in-two (first tests)))
           (t
            (multiple-value-bind (true-list rest) (partition-in-two (first tests))
              (multiple-value-call #'values true-list
                (apply #'npartition rest (rest tests))))))))

     (define-compiler-macro ,name (list &rest tests)
       (if tests
           ;; Split the TESTS into a COND expression each calling a corresponding COLLECTOR.
           ;; An element that fails all tests calls the last collector.
           (let* ((collectors (loop repeat (1+ (cl:length tests))
                                    collect (list (gensym "COLLECT") :mode ,mode)))
                  ;; (lambda ...) and (function ...) forms produce better code
                  ;; when (fun)called directly. For other forms, assign them to variables.
                  (tests (loop for test in tests
                               collect
                               (if (and (consp test) (member (car test) '(function lambda)))
                                   (list test)
                                   (list (gensym "TEST") test))))
                  (%tests (cl:remove-if-not #'cdr tests)))
             (with-gensyms (iterator-value elt collect-value)
               `(let ,%tests
                  ,@(when %tests `((declare (type (function (t) t) ,@(mapcar #'car %tests)))))
                  (with-collected-values ,collectors
                    (dolist* (,iterator-value ,list)
                      (let ((,elt (car ,iterator-value))
                            (,collect-value ,,(if mode 'iterator-value ``(car ,iterator-value))))
                        (cond ,@(loop :for test :in tests
                                      :for collector :in collectors
                                      :collect `((funcall ,(car test) ,elt)
                                                (,(car collector) ,collect-value)))
                              (t
                               (,(caar (last collectors)) ,collect-value)))))))))
           list))))

(define-partition-function npartition :mode :cons
  :documentation
  "Return (VALUES list0 list1 ... listN) as a partition of the LIST by the TESTS.
 The last listN value returned is a collection of LIST elements that passed none of the TESTS.
 The LIST is modified destructively in place and the lists returned have cons cells from the LIST.
 Examples:
   (list:npartition (list 1))                          => (1)
   (list:npartition (list 1 2 3 4) #'oddp)             => (1 3) (2 4)
   (list:npartition (list \"A\" 1 2) #'stringp #'oddp) => (\"A\") (1) (2)")

(define-partition-function partition
  :documentation
  "Return (VALUES list0 list1 ... listN) as a partition of the LIST by the TESTS.
 The last listN value returned is a collection of LIST elements that passed none of the TESTS.
 The returned lists are newly allocated and share no cons cells with the LIST.
 Examples:
   (list:partition (list 1))                          => (1)
   (list:partition (list 1 2 3 4) #'oddp)             => (1 3) (2 4)
   (list:partition (list \"A\" 1 2) #'stringp #'oddp) => (\"A\") (1) (2)
 See also:
  Haskell: Data.List partition
  cl-utils/split-sequence: split-sequence/partition (not a set partitioning function)
  Arnesi: partition")

(defmacro push* (source-list destination &environment env)
  "Push all elements of the SOURCE-LIST onto the DESTINATION list.
The DESTINATION needs to be a place with a SETF expansion.
The expression generated will return the full list."
  (multiple-value-bind (vars vals places setter getter) (get-setf-expansion destination env)
    `(let* (,@(mapcar #'list vars vals)
            (,(first places) (append ,source-list ,getter))
            ,@(rest places))
       ,setter)))

(defmacro npush* (source-list destination &environment env)
  "Push all elements of the SOURCE-LIST onto the DESTINATION list.
The SOURCE-LIST will be destructively modified with the CDR of the last cell set to DESTINATION.
The DESTINATION needs to be a place with a SETF expansion.
The expression generated will return the full list."
  (multiple-value-bind (vars vals places setter getter) (get-setf-expansion destination env)
    `(let* (,@(mapcar #'list vars vals)
            (,(first places) (nconc ,source-list ,getter))
            ,@(rest places))
       ,setter)))
