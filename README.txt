Define dialogs between multiple persons in a portable way

It should be easy to use different interfaces (command-line, GUI, or real games)
can also be used as part for larger programs (handling dialogs in games)
Translation should be easy

Usage:

define some functions for show, say and wait, if needed
(defun my-show (talker) …)
(defun my-say (talker text) …)
(defun my-wait () …)

define some other functions, that you will need for extrastuff in the dialog
(defun change-state (talker …) …)

create a table of present talking objects, that maps symbols to talking objects
(defparameter *talkers*
  (alexandria:plist-hash-table
    '(h "Hans"
      f "Fritz"
      …)
     :test #'eq))

create a dialog
the body can contain symbols, strings and lists
symbols will change the present talking object to the object referenced by symbol in the table and call (show present)
strings will call (say present string)
lists will be interpreted as functoins: (fn …) will become (fn present …)
the arguments wont be evalutated, macros are not possible
the wait-function will be called, if one of these functoins returns nil
if the wait-function returns non-nil the dialog will be canceled and return it's state
normally you want say-functions to return nil, and other functions non-nil
you can also write (say text) and (show) yourself

example:
(talk
  h"Hello"
  (change-state home))

"Hans" says hello, then changes his state to home

to call the talk, you have to bind the functions and talking objects first:
*show-function*: your show-function (my-show here)
*say-functoin*: your say-function (my-say here)
*wait-function*: your wait-function (my-wait here)
*present-table*: your present talking objects (*talkers* here)

then call (call-talk <talk>)

see also talk-example.lisp to see, how to use it




