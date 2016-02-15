;;;;  we want to create a dialog between the player and a friend
;;;;  we assume some special functionality: changing the feeling of the persons

(defpackage #:talk-example
  (:use #:cl)
  (:export #:example-call
           #:translate-dialog))
(in-package #:talk-example)

;;; because we won't use graphics,  the feeling should be written in brackets
;;; right of the name of the talking person
;;; the person should have a feeling himself
;;; so we define a person struct

(defstruct person
  name feeling)


;;; now we need some new say-function
;;; this function should return nil,
;;; so wait will be called after a person said something

(defmethod talk:say (interface (person person) text &key)
  (with-accessors ((feeling person-feeling)
                   (name person-name)) person
    (format t "~a~@[(~a)~]: ~a~%" name feeling text)))


;;; we need a way to set the persons feeling
;;; this function shouldn't return nil,
;;; else wait will be called after each change of the feeling
;;; the first argument is the talker
;;; let's use methods, so we can define different methods for different interfaces

(defmethod feel (interface (person person) &optional feeling)
  (setf (person-feeling person) feeling)
  t)


;;; there are only a few feelings
;;; it would be easier to write only the feeling in brackets yourself
;;; these can be functions, because you will only modify feel for different interfaces

(defun happy (interface person)
  (feel interface person "happy"))

(defun sad (interface person)
  (feel interface person "sad"))

;;; you also could define them as methods, and let the interface save the language, so 

;;; we may also need a narrator, who should displayed different than persons
;;; we could write a new class, but using nil as a value for the narrator will be enough,
;;; when there is only one narrator with no slots needed
;;; the talker is nil by default, so the text we write before announcing a talker
;;; will be said by the narrator
;;; so we will define a method for our say-function
;;; it can easily be extended for different kinds of talkers

(defmethod talk:say (interface (narrator null) text &key)
  (write-line text)
  nil)                                ;return nil, so wait will be called


(defparameter *talkers*
  (alexandria:plist-hash-table
   (list
     'me (make-person :name "Me")
     'friend (make-person :name "Bernd")
     nil nil)
   :test #'eq))
  
(defparameter *dialog*
  (talk:talk
    "I go to my friend"
    me"Hello"                           ;me says hello
    friend(sad)"Hello"                  ;friends feeling sad, says hello
    me(sad)"Can I help you?"            ;me now also sad
    friend"No"                          ;friend yet sad
    me(feel)"Here you have a present"   ;set feeling to none
    nil"I give him a present"           ;narrator now talking
    friend(happy)                       ;friend gets happy
                                        ;you wont recognize,
                                        ;but it may be important,
                                        ;when porting it graphical
    me(happy)                           ;multiple text of same person
    "Seems you like it"
    "I hope we can play funny games now"
    nil"Now we have much fun together"))

;;; now we want to be able to call the dialog, where we ended
;;; wait gets called after each sentence said
;;; the default wait-function breaks the dialog when entering exit
;;; if call-talk breaks it returns it's current state
;;; we can save the state, and call the talk again with the state
;;; submitting state is optional, default-state is 0

(defparameter *state* nil)

;;; before calling the dialog, we have to set some global variables
;;; they are not given to call-talk, because you may want to call different dialogs
;;; let's use a let for portability

 

(defun example-call (&aux (talk:*talkers* *talkers*))
  (setq *state* (talk:call-talk *dialog* :state *state*)))


;;; if you want to translate the text of the dialog, without worrying about the internals
;;; you can call (talk-text *dialog*) and will get this
#|
("I go to my friend" "Hello" "Hello" "Can I help you?" "No"
 "Here you have a present" "I give him a present" "Seems you like it"
 "I hope we can play funny games now" "Now we have much fun together")
|#

;;; only translate every string here, and setf it to the talk
;;; the names and feelings wont be translated, you'd have to modify your code a bit

(defun translate-dialog (lang)
  (case lang
    (:en
     (setf (talk:talk-text *dialog*)
           '("I go to my friend" "Hello" "Hello" "Can I help you?" "No"
             "Here you have a present" "I give him a present" "Seems you like it"
             "I hope we can play funny games now" "Now we have much fun together")))
    (:de
     (setf (talk:talk-text *dialog*)
           '("Ich gehe zu meinem Freund" "Hallo" "Hallo" "Brauchst du Hilfe?" "Nein"
             "Hier ein Geschenk" "Ich gebe ihm das Geschenk" "Gefällt dir wohl"
             "Ich hoffe wir können jetzt lustige Spiele spielen" "Nun haben wir zusammen viel Spaß")))))
    
;;; test it with (translate-dialog <lang>) where lang is :en or :de
