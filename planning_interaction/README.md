# Planning-Interaction

## Purpose 

###### Robot life is hard.

Even if the general purpose of intelligent robots is, that they should navigate their way through a multitude of quests. The future should be, that robots will tend to find a solution for themselfes, regarding any issue.

But as for humans, sometimes help of another beeing is needed. In this case the help of a human. 
Robots are limited in sizes, capabilities and knowledge of the world and are still not as adaptable as a human beeing. I developed this package to help resolve unsolvable issues, to help developers and robots aquire even more knowledge and skill of the real world.

General purpose of this package is to establish certain functions, which enable the Pr2 Robot
to interact with humans in case of some unsolvable Problems, like unreachable objects, unretrievable objects or unplaceable objects. 
Also the robot can use this interactions to solve multiple deadlocks that could occur when interacting with the real world.

In most cases, interaction with humans is only attempted when the robot finds no other solution. It is heavily advised, that the human waits till the robot actively calls for help.

In all cases the interaction is endet, after the human gave a handshake to the left pr2 gripper.

## Usage of interfaces

### __Start this first__

#### init-interaction()

- tested nicely

Initializes this package, and gets all needed publishers and subscribers ready.
Also this starts up the handshake detection in the background. 

Loops till all needed 3rd-party packages are running.

### Core features

#### ask-human-to-move-object()

- tested nicely

Points at certain object which robot is not able to grasp for some reason.
Opens up gripper and asks human to put the named object in his gripper. 
Closes his gripper 5 seconds after handshake with human, and is able to continue normally.

#### ask-human-to-take-object()

- tested nicely

When not able to put down an object, the pr2 drives to a humanly accessable position near the
wooden table. Then points the gripper containing the unplaceable item at the human, and outputs
instructions. After handshake, pr2 opens the gripper containing the object and moves back into 
driving position.


### Safely usable Methods

#### say ()

- tested nicely

Uses the sound_play package to let the Robot say the given string. 
A phonetical text-to-speech interpreter will convert the given String into spoken word.

#### drive-to-human ()

- tested nicely

Drives to a position near the wooden table at the right side of the IAI-Kitchen

#### decide-gripper()

- tested nicely

Because Motion decided to uses terrible magic numbers in theyre services, thinks are like this.
All actions for moving the arms do have magic numbers for which gripper to use. All actions for the right arm are noted with an even number, while all actions for the left arm are noted with an uneven number.

On the contrary, the magic numbers for the gripper-opening and gripper-closing are vise-versa. So this need to be calculated for some reason.

#### wait-for-handshake ()

- tested nicely

Stops everything the robot is doing and waits till handshake gesture is detected.
Can be given a function with parameters, to be called when handshake was detected. 
Given function will be called ins safe enviroment. Parameters for function can either be given in 
a list of parameters like this: '+ '(1 2 3 4 5) or as a single parameter like this 'print "Handshake detected".
Can be used for unknown deadlocks, if errormessage is edited.
This function is as generic as it gets.

top-level functions can not be nested yet.


#### check-gripper()

- untested

Can be given a function to run simultaniously with the other gripper checkup functions.
If given bits for each grippers are set, they will interrupt the given function when grippers are empty (e.g when an object falls out of the gripper.

### Safety methods (not usable publicly)

#### calculate-wrench-magnitude (msg)

- tested nicely

Calculates the magnitude of the given geometry_msgs/WrenchStamped message, containing 
the force-vector of the force/torque sensor at the wrist of the Pr2-Robot.
This function publishes a float32 '15' on the topic '/planning\_interaction/handshake\_detection'
and fills the \*handshake-detection\*-fluent with nil - when a handshake motion is detected.
*will probably get renamed later on*

#### get-pointing-pose (pose)

- tested vaguely

First Transforms given Point into base-link frame and then gives x-value 
of given Pose a static x value to ensure that the PR2 is able to Point in this direction.

Does not point exactly at object yet.

### Special dependencies

1. Planning-Move
2. Planning-Motion
3. Cram
4. sound_play
5. Planning-Logic
6. Robot\_wrist\_ft\_tools

### To be done

1. Generalized function for talking to humans unsing sound_play-package ```done```
2. Method to be called, when Object is unreachable ```done```
3. Method to be called, when Object is unplaceable ```done```
4. Method to be called, when Object fell down ```done```
5. Method to be called, when unknown deadlock happened ```done```
6. Function to drive to a save position ```done```
7. Function calculate point in direction ```done```
8. Function that identifies handshake as a gesture ```done```
9. First testing cycle ```done```
10. Second testing cycle ```done```
11. Integration into Main ```done```
12. Live testing ```done```

#### B-B-B-BONUS!!
13. Point at the object to pick (for real now) ```done```
14. Change texttospeech voice on Robot ```done```
15. Rework all variable and method naming ```done```

### Contact

Regarding any questions please contact
**stoermer@uni-bremen.de**
