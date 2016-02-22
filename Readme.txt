Changelog

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

0.5
Added new movement algorithm loosely based on Reefex model (behavior-driven movement)
Fish change behavior every x seconds
Number of species and variable values are imported from a csv file
approach.dist is a new fish variable. Fish move away from the diver if this distance is reached
visible.dist is now a property of fish species. Global variable changed to max.visibility
New memory algorithm: super memory makes divers remember all fish, if turned off, divers forget fish that leave the FOV

