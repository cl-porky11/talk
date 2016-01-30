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

(defun person-say (person text)
  (with-accessors
        ((feeling person-feeling) (name person-name)) person
    (format t "~a~@[(~a)~]: ~a~%" name feeling text)))


;;; we need a way to set the persons feeling
;;; this function shouldn't return nil,
;;; else wait will be called after each change of the feeling
;;; the first argument is the talker

(defun feel (person &optional feeling)
  (setf (person-feeling person) feeling)
  t)


;;; there are only a few feelings
;;; it would be easier to write only the feeling in brackets yourself

(defun happy (person)
  (feel person "happy"))

(defun sad (person)
  (feel person "sad"))

;;; we may also need a narrator, who should displayed different than persons
;;; we could write a new class, but using nil as a value for the narrator wil be enough,
;;; when there is only one narrator with no slots needed
;;; the talker is nil by default, so the text we write before announcing a talker
;;; will be said by the narrator
;;; so we will define a method for our say-function
;;; it can easily be extended for different kinds of talkers

(defgeneric dispatching-say (person talk)
  (:method ((person null) text)
    ;;the narrator
    (write-line text)
    nil)                                ;return nil, so wait will be called
                                        ;also after the narrator talking
  (:method ((talker person) text)
    (person-say talker text)))




;; we have to create a table with our new functions
;; else the functions wouldn't be reusable for other interfaces

(defparameter *talk-functions*
  (alexandria:plist-hash-table
   '(talk:say dispatching-say                ;our new say-function
     feel feel
     happy happy
     sad sad
     talk:show talk:show                          ;the defualt functions
     talk:wait talk:wait                          ;for wait, show
     talk:undefined talk:undefined)))             ;and undefined

;;; now we can define our persons and write the dialog


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
    nil"I give him a present"             ;narrator now talking
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

 

(defun example-call
    (&aux (talk:*talk-functions* *talk-functions*)
       (talk:*talkers* *talkers*))
  (setq *state* (talk:call-talk *dialog* *state*)))


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
