(defpackage #:talk
  (:use #:cl #:iter #:alexandria)
  (:export #:show
           #:say
           #:wait
           #:undefined
           #:*talkers*
           #:talk
           #:make-talk
           #:call-talk
           #:talk-text
           ))
(in-package #:talk)

(defmethod show (interface object)
  t)

(defgeneric say (interface talker text &key)
  (:method (interface talker text &key to)
    (format t "~@[~a: ~]~a~%"
            talker (format nil text to))
    (finish-output)))

(defmethod wait (interface)
  (string-equal "exit" (read-line)))

(defvar *talkers*)

(defun talker (symbol)
  (let ((object
         (gethash symbol *talkers*
                  #1='#:not)))
    (if (eq object #1#)
        (error "Object does not exist")
        object)))
 
(defun call-talk-object (interface object)
  (apply (car object) interface (talker (cadr object)) (cddr object)))
           
(defun make-talk (body)
  (iter (for element in body)
        (with talker)
        (collect
            (etypecase element
              (symbol (setq talker element) `(show ,talker))
              (string `(say ,talker ,element))
              (list `(,(car element) ,talker ,@(cdr element)))))))

(defmacro talk (&body body)
  `',(make-talk body))

(defun call-talk (talk &key state interface
                       &aux (initial-state (or state 0)))
  (iter (for state from initial-state below (length talk))
        (for object in (last talk (- (length talk) initial-state)))
        (unless (call-talk-object interface object)
          (when (wait interface)
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
