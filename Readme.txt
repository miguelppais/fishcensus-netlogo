Changelog


1.2
Visibility is now established with behavior change (based on detectability), instead of every second.

1.1
Fixed some fish not choosing a patch when patch.gathering.w > 0
Added cruise-urge: urge to maintain a constant speed and heading
New weight: cruise.w
Example files updated with new urge weights
Annoying csv import file delimiter user prompt replaced with a chooser next to the "load data" button
Fixed drag vector with wrong direction (duh!)
Species creator: Parameters for drag calculation can now be edited individually on the interface
Species creator: Added button to display the color of fishes after choosing
Burst speed is no longer activated when diver.avoidance.w and predator.avoidance.w are 0 or negative
Import data procedure changed from user input popup to a simple input interface
Import data procedure merged with setup procedure
BehaviorSpace multi-core errors should now be fixed
Species creator: all drag.formula parameters saved on output csv file. All files from previous versions don't work with this version and must be updated
Error messages and import data procedure updated on the main model to account for new csv structure
avoid.obstacle removed from movement model, since it was slowing down the model and was not being used
Output values are now stored as global variables instead of reporters

1.0
World width and length resized for better performance
Frame.rate renamed to behavior.time.step and set to 5ths of second
Interface re-arranged
Divers are now a single breed
max.velocity changed to max.sustained.speed
burst.speed added
behavior.time.step renamed movement.time.step and can be set to 10ths or 5ths of a second

0.8
World width and length resized for better performance
Frame.rate renamed to behavior.time.step and set to 5ths of second
Interface re-arranged

0.7
Previous movement algorithm did not meet requirements and was removed
Entirely new movement algorithm based on vectors and urges
Fish can gather around an area or even stop completely
Schools pick patches collectively
Schooling is now available
Movement time step can be changed to balance performance and accuracy
Separate model called "species creator" is used to observe the species behaviors and export species parameters to a csv file
Fish can be predators and chase other fish, that will respond with avoidance behavior
World now displays a grid of 1m^2 squares for scale
visible.dist implemented as a fish attribute. Divers only count fish if they are within their visible distance
Fish in a school set behaviors collectively
Option to use a fixed seed for the random number generator, so that the same model outcome can be produced consecutively

0.5
Added new movement algorithm loosely based on Reefex model (behavior-driven movement)
Fish change behavior every x seconds
Number of species and variable values are imported from a csv file
approach.dist is a new fish variable. Fish move away from the diver if this distance is reached
visible.dist is now a property of fish species. Global variable changed to max.visibility
New memory algorithm: super memory makes divers remember all fish, if turned off, divers forget fish that leave the FOV

0.1
Based on AnimDens NetLogo
Change transect.diver to timed.transect.diver
time step set to 1 second for everything (inclunding counts)
Add dist.transect.diver for fixed distance belt transect method
Add all procedures for the new diver
Sharks are now fishes
Divers now start at the lower third of the area, to allow for greater distances without reaching the border
Survey time / distance is now specific to each method
The run stops when all divers are finished
Survey times are set in minutes on the interface
Environment color changed to dark blue