(defpackage #:talk
  (:use #:cl #:iter #:alexandria)
  (:export #:show
           #:say
           #:wait
           #:undefined
           #:*talk-functions*
           #:*talkers*
           #:talk
           #:call-talk
           #:talk-text
           ))
(in-package #:talk)

(defun show (object)
  (declare (ignore object))
  t)

(defun say (object text)
  (format t "~@[~a: ~]~a~%"
          object text)
  (finish-output))

(defun wait ()
  (string-equal "exit" (read-line)))

(defun undefined (fun &key)
  (error "~S not a talk-function" fun))

(defvar *talk-functions*
  (plist-hash-table
   '(show show
     say say
     undefined undefined
     wait wait)))

(defvar *talkers*)

(defun talker (symbol)
  (let ((object
         (gethash symbol *talkers*
                  #1='#:not)))
    (if (eq object #1#)
        (error "Object does not exist")
        object)))

(defun call-talk-object (object)
  (if-let ((fun (gethash (car object) *talk-functions*)))
    (apply fun (talker (cadr object)) (cddr object))
    (apply (gethash 'undefined  *talk-functions*)
           (talker (cadr object)) (cddr object))))
           
(defmacro talk (&body body)
  `',(iter (for element in body)
           (with present)
           (collect
               (etypecase element
                 (symbol (setq present element) `(show ,present))
                 (string `(say ,present ,element))
                 (list `(,(car element) ,present ,@(cdr element)))))))

(defun call-talk (talk &optional new-state
                       &aux (initial-state (or new-state 0)))
  (iter (for state from initial-state below (length talk))
        (for object in (last talk (- (length talk) initial-state)))
        (unless (call-talk-object object)
          (when (funcall (gethash 'wait *talk-functions*))
            (return state)))))

(defun talk-text (talk)
  (iter (for list in talk)
        (when (eq (car list) 'say)
          (collect (caddr list)))))

(defun (setf talk-text) (value talk)
  (iter (for list in talk)
        (when (eq (car list) 'say)
          (setf (caddr list) (car value)
                value (cdr value)))))
