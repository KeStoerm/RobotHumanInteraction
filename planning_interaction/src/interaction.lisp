(in-package :planning-interaction)

(defvar *success* 0)

;; Publisher for publishing calculated magnitude of wrench-force see @calculate-wrench-magnitude
(defvar *magnitude-publisher*)

;; Publisher for publishing a 15 when handshake is detected
(defvar *handshake-publisher*)

;; Actionclient for sound-play text-to-speak action
(defvar *sound-play-actionclient*)

;; Fluent for stating if handshake is detected or not
(defvar *handshake-detection* (cram-language:make-fluent))

;; Configurable Positions for open and closed gripper
(defvar *open-gripper-pos* 0.090)
(defvar *closed-gripper-pos* 0.006)

;; Publisher for knowledge taking object 
(defvar *take-object-publisher*) 
 
;; Publisher for knowledge dropping object 
(defvar *drop-object-publisher*) 

;; Configurable position for gripper holding object out to human
(defvar *holding-pose-right*
  (roslisp:make-msg "geometry_msgs/PoseStamped" 
                    (geometry_msgs-msg:z geometry_msgs-msg:position geometry_msgs-msg:pose) 1
                    (geometry_msgs-msg:x geometry_msgs-msg:position geometry_msgs-msg:pose) 0.7
                    (geometry_msgs-msg:w geometry_msgs-msg:orientation geometry_msgs-msg:pose) 1
                    (geometry_msgs-msg:y geometry_msgs-msg:position geometry_msgs-msg:pose) -0.3
                    (std_msgs-msg:frame_id std_msgs-msg:header) "/base_link"))

;; Configurable position for left gripper, if right is filled. to let human shake hand
(defvar *holding-pose-left*
  (roslisp:make-msg "geometry_msgs/PoseStamped" 
                    (geometry_msgs-msg:z geometry_msgs-msg:position geometry_msgs-msg:pose) 1
                    (geometry_msgs-msg:x geometry_msgs-msg:position geometry_msgs-msg:pose) 0.7
                    (geometry_msgs-msg:y geometry_msgs-msg:position geometry_msgs-msg:pose) 0.3
                    (geometry_msgs-msg:w geometry_msgs-msg:orientation geometry_msgs-msg:pose) 1
                    (std_msgs-msg:frame_id std_msgs-msg:header) "/base_link"))


;; ##########################
;; ###   INITIALIZATION   ###
;; ##########################


;; init-interaction()
;;
;; Initialization of all required components for this package.
;; Contains all publishers and subscribers. 
;;
;; 
;; @output  undefined


(defun init-interaction ()
  (setf *magnitude-publisher* (roslisp:advertise "/planning_interaction/wrench_force_magnitude" "std_msgs/Float32"))
  (setf *handshake-publisher* (roslisp:advertise "/planning_interaction/handshake_detection" "std_msgs/Float32"))
  (setf *sound-play-actionclient* (actionlib:make-action-client "/sound_play" "sound_play/SoundRequestAction"))
  (setf *take-object-publisher* (roslisp:advertise "/beliefstate/grasp_object_human_interaction" "knowledge_msgs/GraspObjectHumanInteraction")) 
  (setf *drop-object-publisher* (roslisp:advertise "/beliefstate/delete_object_human_interaction" "std_msgs/String")) 
  (loop until (actionlib:wait-for-server *sound-play-actionclient* 5.0)
        do (roslisp:ros-info "init-interaction" "sound_play node has not been started correctly. Please use roslaunch sound_play soundplay_node.py"))
  (roslisp:ros-info "init-interaction" "Sound_play action initialized")
  (let ((zeroed-sub (roslisp:subscribe "/ft/l_gripper_motor_zeroed" "geometry_msgs/WrenchStamped" #'calculate-wrench-magnitude :max-queue-length 1)))
  (loop while (not
               (roslisp:msg-slot-value
                (roslisp:msg-slot-value zeroed-sub :subscription)
                :publisher-connections))
        do
           (roslisp:ros-info "init-interaction" "Topic /ft/l_gripper_motor_zeroed is empty. Please startup the robot_wrist_ft_tools package.")
           (sleep 5)
           ))
  (roslisp:ros-info "init-interaction" "Everything has been started correctly. Package is running, and ready to use.")
  (return-from init-interaction())
  )



;; ####################
;; ## Core-Functions ##
;; ####################



;; ask-human-to-move-objekt(pose label moving-command)
;;
;; Interacts with Human if Object is not reachable. Drives into Homeposition
;; Points at Object. Says String and then waits for Human to put Object into his hand and
;; shake his left gripper
;;  
;; @input   geometry_msgs/PoseStamped pose - pose of unreachable object
;; @input   string label                   - label of object as String (e.g. "Ja Milch")
;; @input   float32 force                  - force to gripp with
;; @input   int moving-command             - 2 for right, 3 for left arm. Default is 3
;; @input   string statement               - Statement to be made about object
;; @input   string statement2              - Statement to be made after object label is spoken
;; @output  undefined

(defun ask-human-to-move-object (pose label force &optional
                                              (moving-command 3)
                                              (statement "I cannot grasp the Object over there. Can you please move the")
                                              (statement2 " and shake my hand?"))
  (planning-motion::call-motion-move-arm-homeposition 10)
  (planning-logic::publish-sphere (geometry_msgs-msg:Pose pose))
  (let ((pose-to-point
          (build-pointing-pose (cl-tf:to-msg (planning-logic:transformation-pose-stamped pose "/base_link")))))
    (planning-logic::publish-sphere (geometry_msgs-msg:Pose pose-to-point))
    (planning-motion::call-motion-move-arm-to-point pose-to-point "" moving-command)) 
  (say (concatenate 'string statement label statement2))
  (planning-motion::toggle-gripper force (decide-gripper moving-command) *open-gripper-pos*)
  (cram-language:top-level
    (cram-language:pursue
      (cram-language:unwind-protect
           (cram-language:wait-for *handshake-detection*)
        (say "Thanks Human, i will grasp the Object now. Please be carefull.")
        (sleep 5)
	(put-object-in-hand (decide-gripper moving-command) label) 
        (planning-motion::toggle-gripper force (decide-gripper moving-command) *closed-gripper-pos*)))))



;; ask-human-to-take-object(label moving-command)
;;
;; Interacts with Human if object is not placeable. Drives into humanly reachable
;; position and reaches out his filled hand, and if left gripper is filled, also his right one.
;; Waits for user to shake his hand, opens gripper to release object and gives controll back to
;; upper layer.
;;
;; @input   string label                   - label of object as String (e.g. "Ja Milch")
;; @input   int moving-command             - 2 for right, 3 for left arm. Default is 3
;; @output  undefined


(defun ask-human-to-take-object (label &optional (moving-command 3))
  (planning-motion::call-motion-move-arm-homeposition 10) 
  (say (concatenate 'string "I can not place the " label  " in my gripper. Will ask human."))
  (drive-to-human)
  (if (= moving-command 3)
      (planning-motion::call-motion-move-arm-to-point *holding-pose-left* label 3)
      (progn
        (planning-motion::call-motion-move-arm-to-point *holding-pose-left* label 3)
        (planning-motion::call-motion-move-arm-to-point *holding-pose-right* label 2)))
  (say (concatenate 'string "Hello human! I am very sorry to say that i can not place the " label " in my gripper. Can you please take it and shake my right hand?"))
  (cram-language:top-level
    (cram-language:pursue
      (cram-language:unwind-protect
           (cram-language:wait-for *handshake-detection*)
        (print *handshake-detection*)
        (say "Thanks human, i will release the object now. Please be carefull.")
        (sleep 5)
        (planning-motion::toggle-gripper 20.0 (decide-gripper moving-command) *open-gripper-pos*)
        (sleep 5)
        (drop-object-in-hand label) 
        (planning-motion::call-motion-move-arm-homeposition 10)
        )
      )
    )
  )





;; ###########################
;; ## Public used functions ##
;; ###########################

;; check-gripper(func args r l)
;;
;; Checks gripper function against given function
;; If gripper looses object, given function is disturbed
;;
;; @input  string errormsg - string to print if error occured
;; @input  function func   - function to be executed in pursue context
;; @input  list args       - list of arguments to be given to above function
;; @input  0/1 r           - bit to be set if right gripper should be checked
;; @input  0/1 l           - bit to be set if left gripper should be checked
;; @output undefined

(defun check-gripper(errormsg func args &optional (r 0) (l 0))
  (setf *success* 0)
  (cram-language:top-level
    (cram-language:pursue
      (if (= r 1)
          (cram-language:wait-for planning-logic::*gripper-righ-state-fluent*)
          (cram-language:sleep 10000))
      (if (= l 1)
          (cram-language:wait-for planning-logic::*gripper-left-state-fluent*)
          (cram-language:sleep 10000))
      (cram-language:unwind-protect 
           (progn
             (if (listp args)
               (apply func args)
               (funcall func args))
             (setf *success* 1))
        (if (= *success* 0) (wait-for-handshake 'print "Handshake detected" errormsg))
      ))))



;; decide-gripper(moving-command)
;;
;; little function that translates motions terrible magic numbers from move-to-point action
;; to open/close gripper action. Resolves Motions terrible choice of actionnaming
;;
;; @input  int moving-command - Motions identifier for right or left gripper in moving-action
;; @output int gripper        - Motions identifier for right or left gripper in gripper-action


(defun decide-gripper(moving-command)
  (if (= (rem moving-command 2) 0)
      (return-from decide-gripper 2)
      (return-from decide-gripper 1)))




;; drive-to-human ()
;;
;; Drives into a safe position, where he is able to reach a human.
;; Position is near the wooden Table in the iai Kitchen.
;;
;;  
;; @output  undefined


(defun drive-to-human ()
  "Drives into a Position where pr2 is able to interact with Human"
  (say "Driving to my Human now")
  (planning-move::move-base-to-point -0.1566 -0.7442 0 -90 10)
  )




;; say (message)
;;
;; Utilizes sound_play package to play phoenetic string 
;;  
;; @input   string message - the message to speak out loud 
;; @output  sound_play-msg:SoundRequestResult - information about played sound


(defun say (message)
  "Uses sound_play service to let the pr2 say a string out loud"
    (let
        ((actiongoal
      (actionlib:make-action-goal *sound-play-actionclient* sound_request 
        (roslisp:make-msg "sound_play/SoundRequest"
                          :sound -3
                          :command 1
                          :arg message
                          :arg2 ""))))
      (actionlib:call-goal *sound-play-actionclient* actiongoal)))




;; wait-for-handshake (&optional func args errormsg)
;;
;; Generalized function for a simple wait for a handshake
;;
;; @input  symbol func     - function to be called in safe wrapper
;; @input  list/atom args  - list of arguments or single one
;; @input  string errormsg - message to be said when method is called
;; @output undefined
;;
;; EXAMPLE USE
;; ---------------------------------------------------------
;; (wait-for-handshake)
;; (wait-for-handshake '+ '(1 2 3))
;; (wait-for-handshake 'print "Handshake detected")
;; (wait-for-handshake 'say "Hello" "This is an error")

(defun wait-for-handshake (&optional
                             (func 'print)
                             (args "Handshake detected")
                             (errormsg "Human, i will wait now. Please shake my left gripper when you are ready"))
  (say errormsg)
    (cram-language:pursue
      (cram-language:unwind-protect 
           (cram-language:wait-for *handshake-detection*)
        (if (listp args)
            (apply func args)
            (funcall func args)))))



;; ########################
;; ## Private for safety ##
;; ########################


;; build-pointing-pose
;;
;; builds complete pointing pose for pr2, gripper will be oriented accordingly
;;
;; @input geometry_msgs/PoseStamped pose - Pose of object to point at
;; @output geometry_msgs/PoseStamped     - Pose for pr2 to perform pointing

(defun build-pointing-pose (pose)
  (let ((pointing-pose
            (get-pointing-pose pose))
        (pointing-quat
          (cl-tf:to-msg
           (calculate-pointing-quaternion pose))))
    (roslisp:modify-message-copy pointing-pose
                                 (geometry_msgs-msg:orientation
                                  geometry_msgs-msg:pose) pointing-quat)))



;; get-pointing-pose (pose)
;;
;; Gets pointing pose with fixed quaternion. Uses "get-vektor-pr2-reachable" to ensure
;; a kinematic solution is found.
;;  
;; @input   geometry_msgs/PoseStamped pose  - unreachable Pose to Point at
;; @output  geometry_msgs/PoseStamped       - reachable Pose to Point at

(defun get-pointing-pose (pose)
  "Calculates pointing pose for given pose. Will be used to let the Pr2 use his Gripper to point at this pos"
  (let ((pose-in-baselink (cl-tf:to-msg (planning-logic::transformation-pose-stamped pose "/base_link"))))
                          (roslisp:modify-message-copy pose-in-baselink
                                                       (geometry_msgs-msg:position
                                                        geometry_msgs-msg:pose)
                                                       (cl-tf:to-msg
                                                        (get-vektor-pr2-reachable
                                                         (geometry_msgs-msg:position
                                                          (geometry_msgs-msg:pose pose))))
                                                       (geometry_msgs-msg:orientation
                                                        geometry_msgs-msg:pose)
                                                       (roslisp:make-msg "geometry_msgs/quaternion" :x 0 :y 0 :z 0 :w 1)))
  )




;; calculate-wrench-magnitude (msg)
;;
;; Calculates magnitude of the forcevectors given by the force-, and torque-sensors on the
;; Pr2's right gripper. It filles a fluent with nil and Publishes a 15 on the /planning_interaction/handshake_detection topic
;; when a Handshake is detected at the right gripper.
;;
;;
;; @input   geometry_msgs/WrenchStamped - The message containing the wrist sensory data with force and torque
;; @output  undefined


(defun calculate-wrench-magnitude (msg)
  (cram-language:sleep 0.5)
  (let ((magnitude (sqrt 
                   (+ 
                    (planning-logic::square (geometry_msgs-msg:x (geometry_msgs-msg:force (geometry_msgs-msg:wrench msg))))
                    (planning-logic::square (geometry_msgs-msg:y (geometry_msgs-msg:force (geometry_msgs-msg:wrench msg))))
                    (planning-logic::square (geometry_msgs-msg:z (geometry_msgs-msg:force (geometry_msgs-msg:wrench msg))))))))
    (when *magnitude-publisher*
      (roslisp:publish *magnitude-publisher*
                       (roslisp:make-msg
                        "std_msgs/Float32"
                        :data magnitude)))
    (if
     (>= magnitude 15)
     (progn
       (setf (cram-language:value *handshake-detection*) T)
       (roslisp:publish *handshake-publisher* (roslisp:make-msg "std_msgs/Float32" :data 15)))
     (progn
       (setf (cram-language:value *handshake-detection*) nil)
       (roslisp:publish *handshake-publisher* (roslisp:make-msg "std_msgs/Float32" :data 0)))
     )))




;; calculate-pointing-quaternion
;;
;; calculates quaternion which needs to be given
;; to pr2 wrist to point at certain object
;;
;; @input geometry_msgs/PoseStamped object-pos - Pose to Point at
;; @output cl-tf:quaternion                    - Quaternion representing rotation

(defun calculate-pointing-quaternion (object-pos)
  (let ((dir-vec
          (get-directional-vector
           object-pos
           (get-pointing-pose object-pos)))
        (y-vec
          (cl-tf:make-3d-vector 0 0 1)))
    (cl-tf:normalize
     (cl-tf:matrix->quaternion 
      (vectors-to-matrix
       dir-vec
       y-vec
       (cl-tf:cross-product dir-vec y-vec))))))




;; get-directional-vector
;;
;; Calculate directional vector between two points
;; Vektor gets normalized
;;
;; @input PoseStamped pose1 - Pose of first vektor
;; @input PoseStamped pose2 - Pose of second vektor
;; @output ct-tf:vector     - normalized directional vector

(defun get-directional-vector (pose1 pose2)
  "get directional vector for 2 points"
  (cl-tf:normalize-vector
   (cl-tf:v-
    (cl-tf:from-msg
     (geometry_msgs-msg:position
      (geometry_msgs-msg:pose pose1)))
    (cl-tf:from-msg
     (geometry_msgs-msg:position
      (geometry_msgs-msg:pose pose2))))))




;; vector to matrix
;;
;; turns 3 vektors into one matrix (3x3)
;;
;; @input cl-tf/3D-VECTOR vector-x - Vector for first row
;; @input cl-tf/3D-VECTOR vector-y - Vector for second row
;; @input cl-tf/3D-VECTOR vector-z - Vector for third row
;; @output array                   - 3x3 matrix

(defun vectors-to-matrix (vector-x vector-y vector-z)
  (let ((matrix (make-array '(3 3))))
    (setf (aref matrix 0 0) (cl-tf:x vector-x))
    (setf (aref matrix 1 0) (cl-tf:y vector-x))
    (setf (aref matrix 2 0) (cl-tf:z vector-x))
    (setf (aref matrix 0 1) (cl-tf:x vector-y))
    (setf (aref matrix 1 1) (cl-tf:y vector-y))
    (setf (aref matrix 2 1) (cl-tf:z vector-y))
    (setf (aref matrix 0 2) (cl-tf:x vector-z))
    (setf (aref matrix 1 2) (cl-tf:y vector-z))
    (setf (aref matrix 2 2) (cl-tf:z vector-z))
    (return-from vectors-to-matrix matrix)))
              



;; get-vektor-pr2-reachable
;;
;; calculates a pr2-reachable vector out of an existing one
;; first normalizes then halfes vector
;; z is left untouched
;;
;; @input geometry_msgs/Point vektor - vektor to be calculated into reachable
;; @output cl-transforms/3d-vector   - reachable vektor

(defun get-vektor-pr2-reachable (vektor)
  (let ((old-z (cl-tf:z (cl-tf:from-msg vektor)))
        (vec
          (cl-tf:normalize-vector
           (cl-tf:make-3d-vector
            (cl-tf:x (cl-tf:from-msg vektor))
            (cl-tf:y (cl-tf:from-msg vektor))
            (cl-tf:z 0.0)))))
    (cl-tf:make-3d-vector
     (/ (cl-tf:x vec) 2)
     (/ (cl-tf:y vec) 2)
     old-z)))

(defun put-object-in-hand (gripper label) 
  (roslisp:publish *take-object-publisher* 
                   (roslisp:make-msg "knowledge_msgs/GraspObjectHumanInteraction" 
                                     :gripper 
                                     (roslisp:make-msg "knowledge_msgs/gripper" 
                                                       :gripper gripper) 
                                     :object_label label))) 
 
(defun drop-object-in-hand (label) 
  (roslisp:publish *drop-object-publisher* (roslisp:make-msg "std_msgs/String" :data label))) 

