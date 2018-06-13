(in-package :cl-user)


(defpackage planning-interaction
  (:use #:common-lisp)
  (:export
   :init-interaction 
   :ask-human-to-take-object
   :ask-human-to-move-object   
   :say
   :decide-gripper
   :drive-to-human
   :check-gripper
   :wait-for-handshake))

