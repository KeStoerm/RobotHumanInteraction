(defsystem planning-interaction

  :author "Kevin Störmer"
  :maintainer "Kevin Störmer"
  :license "BSD"

  :depends-on (roslisp
               cl-tf
               actionlib
               geometry_msgs-msg
	       sound_play-msg
               pr2_controllers_msgs-msg
	       object_detection-srv
	       motion_msgs-msg
	       sensor_msgs-msg
	       cram-language)
  :components
  ((:module "src"
    :components
    ((:file "package")
     (:file "interaction" :depends-on ("package"))))))
