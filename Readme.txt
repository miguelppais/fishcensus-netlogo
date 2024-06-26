FishCensus model version 2.1

by Miguel Pessanha Pais (mppais@fc.ul.pt)

Programmed in NetLogo 6.4


## Context

Underwater visual census (UVC) methods are used worldwide to monitor shallow marine and freshwater habitats and support management and conservation decisions. However, several sources of bias still undermine the ability of these methods to accurately estimate abundances of some species.

## FishCensus Model

FishCensus is an agent-based model that simulates underwater visual census of fish populations, a method used worldwide to survey shallow marine and freshwater habitats that involves a diver counting fish species to estimate their density. It can help estimate sampling bias, apply correction factors to field surveys and decide on the best method to survey a particular species, given its behavioural traits, detectability or speed.
A modified vector-based boids-like movement submodel is used for fish, and complex behaviours such as schooling or diver avoidance / attraction can be represented.

## How it works

The FishCensus model comes with two separate programs. The Species Creator is used to create new fish species or observe/edit existing ones. Species parameters can be exported as a .csv file and imported into the main model where the simulation happens.

In the main FishCensus program, a virtual diver uses a pre-selected survey method (*e.g.* fixed distance or timed transect, stationary point counts) to count the fish and estimate their density. The true density of fish is pre-determined and known, which allows for the quantification of bias, a measure that is unknown in the field, where determining the true abundance is very difficult.

## Related models

###  AnimDens model, originally programmed in R:

Ward-Paige, C.A., Flemming, J.M., Lotze, H.K., 2010. Overestimating fish counts by non-instantaneous visual censuses: Consequences for population and community descriptions. PLoS One 5, e11722. doi:10.1371/journal.pone.0011722

### Replication of the AnimDens model in NetLogo with some added features:

Pais, M.P., Ward-Paige, C.A. (2015). AnimDens NetLogo model. http://modelingcommons.org/browse/one_model/4408

### Vector-based swarming by Uri Wilensky, in the NetLogo 3D library:

Wilensky, U. (2005).  NetLogo Flocking 3D Alternate model.  http://ccl.northwestern.edu/netlogo/models/Flocking3DAlternate.  Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

## Credits and references

If you use this model, please cite the original publications:

Pais, M.P., Cabral, H.N. 2017. Fish behaviour effects on the accuracy and precision of underwater visual census surveys. A virtual ecologist approach using an individual-based model. Ecological Modelling 346, 58-69.

To cite the model itself, please visit the [CoMSES Net page](https://www.comses.net/codebases/5305/) for citation instructions.


## Acknowledgments

I thank everyone who tested the model and interface and helped find bugs, Christine Ward-Paige for clarifications and suggestions about the AnimDens model, Uri Wilenksy for NetLogo and the base code for vector-based swarming, Kenneth Rose for valuable feedback and suggestions and J.P. Rosa for revising the calculation of drag forces. This study had the support of Funda��o para a Ci�ncia e Tecnologia (FCT), through the strategic project UID/MAR/04292/2013 granted to MARE and the grant awarded to Miguel P. Pais (SFRH/BPD/94638/2013).

## COPYRIGHT AND LICENSE

Copyright 2024 Miguel Pessanha Pais

![CC BY-NC-SA 4.0](http://ccl.northwestern.edu/images/creativecommons/byncsa.png)

This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.  To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-sa/4.0/

## Contact the author

If you want to report bugs, suggest features, share work you did with the model, or even insult me, please send me an email: [mppais@fc.ul.pt](mailto:mppais@fc.ul.pt)

## Changelog

Version 2.1
- Changed folder structure to match COMSES.Net requirements
- Code to save and load files now saves and loads from the ../data/ folder
- Minor UI fixes

Version 2.0
- Code updated to NetLogo 6.0 new syntax. Model no longer compatible with NetLogo 5.3.1.
- Fixed an error when finding a random float between two numbers. It was only affecting roving diver movement, so no big issue.
- Species creator fish color picker is now an actual color picker!
- Video recorder in species creator updated to use the new vid extension. Now saves in mp4 instead of mov.
- Separate instalation of rnd extension no longer needed.
- Model now comes with an example experiment in BehaviorSpace.

Version 1.1
- Added more sliders on the interface to edit the movement initialization time and diver view angles
- During initialization, fish will now pick the FIRST behavior from the list, and not a random one.