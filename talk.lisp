(defpackage #:talk
  (:use #:cl #:iter #:alexandria)
  (:export #:simple-show
           #:simple-say
           #:simple-wait
           #:*show-function*
           #:*say-function*
           #:*wait-function*
           #:show
           #:say
           #:wait
           #:*present-table*
           #:talk
           #:call-talk
           #:talk-text
           ))
(in-package #:talk)

(defun simple-show (object)
  (declare (ignore object))
  t)

(defun simple-say (object text)
  (format t "~a: ~a~%"
          object text)
  (finish-output))

(defun simple-wait ()
  (string-equal (read-line) "exit"))

(defvar *show-function* 'simple-show)

(defvar *say-function* 'simple-say)

(defvar *wait-function* 'simple-wait)

(defun show (object)
  (funcall *show-function* object))

(defun say (object text)
  (funcall *say-function* object text))

(defun wait ()
  (funcall *wait-function*))

(defvar *present-table*)
(defun present (symbol)
  (let ((object (gethash symbol *present-table* #1='#:not)))
    (if (eq object #1#)
        (error "Object does not exist")
        object)))

(defun call-talk-object (object)
  (apply (car object) (present (cadr object)) (cddr object)))

(defmacro talk (&body body)
  `',(iter (for element in body)
           (with present)
           (collect
               (etypecase element
                 (symbol (setq present element) `(show ,present))
                 (string `(say ,present ,element))
                 (list `(,(car element) ,present ,@(cdr element)))))))

(defun call-talk (talk &optional (initial-state 0))
  (or (iter (for state from initial-state below (length talk))
            (for object in (last talk (- (length talk) initial-state)))
            (unless (call-talk-object object)
              (when (wait)
                (return state))))
      0))

(defun talk-text (talk)
  (iter (for list in talk)
        (when (eq (car list) 'say)
            (collect (caddr list)))))

(defun (setf talk-text) (value talk)
  (iter (for list in talk)
        (when (eq (car list) 'say)
          (setf (caddr list) (car value)
                value (cdr value)))))


