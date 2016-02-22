extensions [
  csv              ; used to import species parameters through a csv file
  rnd              ; used by fishes to calculate next behavior with a weighted random pick
  ]          

;Global variables not represented on the interface tab

globals[
  world.area
  file.name                 ; species parameters input csv file name
  species.data              ; imported input data from the csv file
  file.delimiter            ; delimiter for the csv file (user prompt)
  nr.species                ; number of species in the csv file. Variable updated by the go procedure
  total.density
  numb.fishes
  timetransdiver.viewangle
  distransdiver.viewangle
  statdiver.viewangle
  rovdiver.viewangle
  timed.transect.diver.mean.speed
  distance.transect.diver.mean.speed
  transect.time.secs
  stationary.time.secs
  roving.time.secs
  roving.diver.mean.speed
  timed.transect.area
  distance.transect.area
  stationary.area
  divers                    ; an agentset with all active divers
  year                      ; variables for the model clock
  season
  month
  day
  hours
  minutes
  seconds
]

;Agent types

breed [fishes fish]

breed [timetransdivers timetransdiver] ;timed belt transect diver

breed [distransdivers distransdiver] ;fixed distance transect diver

breed [statdivers statdiver] ;stationary point count diver

breed [rovdivers rovdiver] ;roving diver     ; roving divers can't calculate densities accurately, yet they can estimate species richness and frequency of occurence.

;Agent variables

fishes-own [                     ; fish variables
  drag.formula                   ; the coefficient used to calculate drag, given speed and fish size
  current.behavior               ; picked from behavior.list
  behavior.set?                  ; boolean that tells if fish has set its behavior this turn
  schoolmates                    ; for schooling species, agentset of nearby fishes
  nearest-neighbor               ; closest schoolmate
  velocity                       ; vector with x and y components determined by the previous velocity and the current acceleration at each step
  acceleration                   ; vector with x and y components, determined by the sum of all urges
  species                        ; this separates the different types within the fish breed. This is the variable recorded by divers while counting
  prey.type                      ; "benthic" or "fish". Defines the type of feeding behavior (gather around a patch or chase prey)
  detectability                  ; probability of being missed (if = 1 fish is always counted if within visible.dist)
  visible?                       ; boolean variable that is set by detectability every second                  
  visible.dist                   ; maximum distance from diver required to correctly identify the species
  approach.dist                  ; minimum distance from diver that the fish tolerate
  behavior.list                  ; list of up to 4 distinct behaviors
  behavior.freqs                 ; frequency of each behavior
  behavior.params                ; a list of lists with constants for the movement model per behavior
  picked.patch                   ; patch picked by the school to feed on (if prey is benthic)
  picked.patch.dist              ; maximum distance from the feeding patch
  patch.gathering.w              ; weighs the urge to gather at a feeding patch
  perception.dist                ; fish perception distance
  perception.angle               ; fish perception angle (360 for full directional awareness)
  max.velocity                   ; maximum attainable velocity
  max.acceleration               ; maximum attainable acceleration
  diver.avoidance.w              ; weighs the urge to avoid divers. If negative, fish are attracted to divers
  predator.avoidance.w           ; weighs the urge to avoid fish with prey.type = "fish"
  obstacle.avoidance.w           ; weighs the urge to avoid obstacles (brown patches)
  prey.chasing.w                 ; weighs the urge to chase other fish
  cruise.distance                ; distance among schoolmates in a school (in body lengths)
  align.w                        ; weighs the urge to match school direction
  center.w                       ; weighs the urge to stand between nearby schoolmates
  spacing.w                      ; weighs the urge to maintain crusing.distance from schoolmates
  wander.w                       ; weighs the urge to wander around randomly
  rest.w                         ; weighs the urge to stop and rest
  schooling?                     ; boolean variable that determines if fish exhibit schooling behavior
]

timetransdivers-own [            ; timed transect diver variables
  counted.fishes
  snapshot.fishes
  speed
  memory
  t.bias
  finished?
]

distransdivers-own [             ; distance transect diver variables
  counted.fishes
  snapshot.fishes
  speed
  memory
  initial.ycor                   
  final.ycor
  d.bias
  finished?
]


statdivers-own [                 ; stationary diver variables
  counted.fishes
  snapshot.fishes
  memory
  s.bias
  finished?
]

rovdivers-own [                  ; roving diver variables
  counted.fishes
  speed
  memory
  finished?
]

;Interface procedures

to import-dataset       ; imports species data from a .csv file to global variable species.data
  output-print "Importing species data..."
  set file.name (word user-input "Name of the .csv file with species parameters? (exclude extension)" ".csv")
  set file.delimiter user-input "Delimiter symbol in the .csv file? (usually , or ;)"               ; a window prompts for the csv file delimiter
  carefully [set species.data (csv:from-file file.name file.delimiter)                                         ; import csv file into a list of lists
  if length species.data < 2 [user-message "ERROR: input file has only 1 row or is empty. Check if input file has a header plus 1 line per species." stop] ; ERROR MESSAGE
  let delimiter.check item 0 species.data
  if length delimiter.check = 1 [user-message "ERROR: unable to separate parameters in the file. Probably wrong delimiter picked." stop] ; ERROR MESSAGE
  set nr.species length species.data - 1                                                            ; all filled lines minus the header
  ifelse user-yes-or-no? (word "Detected " nr.species " species. Is this correct?") [
    user-message "Species data imported. Press setup."
    output-print "Data successfully loaded."] [                                                     ; confirm number of species in the file
  user-message "Check if input file has a header plus 1 line per species."                          ; ERROR MESSAGE
  output-print "Number of species incorrectly identified. Check file."
  stop]]
  [user-message (word "File " file.name " not found in model folder.") stop]
end

to setup
  if species.data = 0 [user-message "You need to import a dataset first. Import the example.csv file or create one using the species creator." stop] ; ERROR MESSAGE
  if fixed.seed? [random-seed seed]
  clear-ticks                                          ; reset model clock and clear everything except global variables (or else it would erase species.data and nr.species)
  set year 1
  set season 1
  set month 1
  set day 1
  set hours 0
  set minutes 0
  set seconds 0
  clear-turtles
  clear-patches
  clear-drawing
  clear-all-plots
  stop-inspecting-dead-agents                           ; clears diver detail windows from previous simulation runs
  output-print "Drawing display grid..."
  ask patches with [pycor mod 2 = 0 and pxcor mod 2 = 0] [set pcolor 103]
  ask patches with [pycor mod 2 = 1 and pxcor mod 2 = 1] [set pcolor 103]
  ask patches with [pcolor = black] [set pcolor 105]
  output-print "Calculating global constants..."
  set world.area world-height * world-width
  set timetransdiver.viewangle 180                      ; sets viewangles for all divers
  set distransdiver.viewangle 180
  set statdiver.viewangle 160
  set rovdiver.viewangle 160
  set transect.time.secs transect.time * 60             ; convert survey times from minutes to seconds
  set stationary.time.secs stationary.time * 60
  set roving.time.secs roving.time * 60
  set timed.transect.diver.mean.speed (timed.transect.diver.speed / 60)  ; these lines just convert interface speeds (in m/min) to m/s
  set distance.transect.diver.mean.speed (distance.transect.diver.speed / 60)
  set roving.diver.mean.speed (roving.diver.speed / 60)
  set divers no-turtles                                                  ; resets the "divers" agentset (otherwise it would be set to 0 and generate errors during setup)
  
; for the timed transect, the sampled area is distance * width plus max visibility * width at the end (an approximation that ignores the fact that visibility is the radius of a circle).

  set timed.transect.area timed.transect.width * (timed.transect.diver.mean.speed * transect.time.secs) + (timed.transect.width * max.visibility)
  
; for the distance transect, the sampled area is only distance * width
  
  set distance.transect.area distance.transect.width * transect.distance
  
; for the stationary diver, it is the area of the circle around the diver
  
  set stationary.area pi * stationary.radius ^ 2
 
; read the information stored in species.data to place fishes 
  
  output-print "Placing fishes..."
  foreach n-values nr.species [? + 1] [           ; loop that creates fish for each species and sets all fish variables
    let sp.param item ? species.data
    let sp.density item 4 sp.param
    create-fishes (sp.density * world.area) [
      setxy random-xcor random-ycor
      set velocity [0 0]
      set acceleration [0 0]
      set picked.patch false
      set schoolmates no-turtles                  ; sets schoolmates as an empty agentset
      set nearest-neighbor nobody
      set species item 0 sp.param
      if length sp.param != 76 [user-message (word "ERROR: species number " ? " '" species "' has a parameter list that is incomplete or too long. Check file.") stop] ; ERROR MESSAGE
      set shape item 1 sp.param
      set size item 2 sp.param
      set drag.formula (-11297 / (20600 * (size ^ 0.8571))) ; if size is fixed, this can be fixed too.
      set color item 3 sp.param
      set visible.dist item 5 sp.param
      set approach.dist item 6 sp.param
      set perception.dist item 7 sp.param
      set perception.angle item 8 sp.param
      set prey.type item 9 sp.param
      set max.acceleration item 10 sp.param
      set max.velocity item 11 sp.param
      set behavior.list (list item 12 sp.param item 28 sp.param item 44 sp.param item 60 sp.param)
      set behavior.freqs (list item 13 sp.param item 29 sp.param item 45 sp.param item 61 sp.param)
      if reduce + behavior.freqs != 1 [
       user-message (word "ERROR! Behavior frequencies for " species " do not add up to 1.") stop]   ; ERROR MESSAGE
      set behavior.params (list                                ; lists parameter values for all 4 behaviors
        sublist sp.param 12 28
        sublist sp.param 28 44
        sublist sp.param 44 60
        sublist sp.param 60 76)
      set visible? true
      set behavior.set? false
      set.behavior                          ; fish procedure that draws the starting behavior from the list and fills in parameters
    ]
  ]
  set total.density (count fishes / world.area)
  set numb.fishes count fishes
  output-print (word "Imported " count fishes " fishes belonging to " nr.species " species,")
  output-print (word "with a total density of " total.density " fish per square meter.")
  output-print "Stabilizing fish movement model..."
  ask fishes [repeat movement.time.step * 20 [do.fish.movement]] ; lets the fish movement model stabilize for 20 ticks
  
  
  output-print "Placing selected divers..."
  
if timed.transect.diver? = true [                                         ;timed transect diver setup
  create-timetransdivers 1 [
 set heading 0
 set shape "diver"
 set color cyan
 set size 2
 setxy (world-width / 2) (world-height / 4)
 if show.paths? = true [pen-down]                                                           ;this shows the path of the diver
 set speed timed.transect.diver.mean.speed
 set counted.fishes [] ; sets counted.fishes as an empty list
]
]

if distance.transect.diver? = true [                                         ;distance transect diver setup
  create-distransdivers 1 [
 set heading 0
 set shape "diver"
 set color green
 set size 2
 setxy (world-width / 2) (world-height / 4)
 if show.paths? = true [pen-down]                                                           ;this shows the path of the diver
 set speed distance.transect.diver.mean.speed
 set initial.ycor ycor                                                              ; fixed distance transect divers record their start and end points as given by "transect.distance" initially
 set final.ycor ycor + transect.distance                                            ; because coordinates are in meters
 set counted.fishes [] ; sets counted.fishes as an empty list
]
]

if stationary.diver? = true [                                               ;stationary diver setup
  create-statdivers 1 [
 set heading 0
 set shape "diver"
 set color red
 set size 2
 setxy (world-width / 2) (world-height / 4)
 set counted.fishes [] ; sets counted.fishes as an empty list
]
]

if roving.diver? = true [                                                      ;roving diver setup
  create-rovdivers 1 [
 set heading 0
 set shape "diver"
 set color magenta
 set size 2
 setxy (world-width / 2) (world-height / 4)
 if show.paths? = true [pen-down]                                                           ;this shows the path of the diver
 set speed roving.diver.mean.speed
 set counted.fishes [] ; sets counted.fishes as an empty list
]
]

set divers (turtle-set timetransdivers distransdivers statdivers rovdivers) ; creates the agentset containing all divers (all methods)

ask divers [                                                    
 set finished? false                                            ; reset the "finished?" variable
 set memory []                                                  ; reset the memory list
]

output-print "Setup complete. Press GO to run the model"
reset-ticks
if show.diver.detail.windows? = true [
  if any? timetransdivers [inspect one-of timetransdivers]         ; need to use "if any?" because inspect will return an error if it finds nobody
  if any? distransdivers [inspect one-of distransdivers]
  if any? statdivers [inspect one-of statdivers]
  if any? rovdivers [inspect one-of rovdivers]
]
end ;of setup procedure


to go
  if count divers = count divers with [finished?] [             
    do.outputs
    stop]                                   
  if stationary.radius > max.visibility [
   output-print "ERROR: stationary.radius is set to a value greater than max.visibility"              ; if the stationary radius is higher than visibility, stop and output an error description
   output-print "The diver will not be able to see the sample area"
   output-print "Stopping simulation"
   stop 
  ]
  
  
  ask timetransdivers [                                                                  ; divers count the fishes, unless they are finished
   if not finished? [t.count.fishes]
  ]

  ask distransdivers [
   if not finished? [d.count.fishes] 
  ]
  
  ask statdivers [
    if not finished? [s.count.fishes]
  ]

  ask rovdivers [
    if not finished? [r.count.fishes]
  ]
    
  
  repeat movement.time.step [
  
  ask timetransdivers [                                           ; move the divers
    ifelse ticks > transect.time.secs [                           ; if their end of sampling conditions are met, set variable "finished?" to "true" and stop moving
     set finished? true 
     set label "finished"
    ] [
    do.tdiver.movement
    ]
  ]
 
  ask distransdivers [
    ifelse ycor > final.ycor [
     set finished? true 
     set label "finished"
    ] [
    do.ddiver.movement
    ]
  ]

  ask statdivers [
    ifelse ticks > stationary.time.secs [
     set finished? true
     set label "finished"
    ] [
    do.stdiver.movement
    ]
  ]

  ask rovdivers [
    ifelse ticks > roving.time.secs [
     set finished? true 
     set label "finished"
    ] [
    do.rdiver.movement
    ]
  ]

  ask fishes [                                                      ; move the fishes                                                     
    do.fish.movement
    ]
  if smooth.animation? [display]
  ] ; closes repeat movement.time.step                  


  if not super.memory? [                                          ; if super memory is disabled, divers forget fishes they no longer see (a kind of "doorway effect")
   ask timetransdivers [t.forget.fishes]
   ask distransdivers [d.forget.fishes]
   ask statdivers [s.forget.fishes]
   ask rovdivers [r.forget.fishes] 
  ]
  
  
  ifelse ticks mod behavior.change.interval = 0 [ 
    ask fishes [set behavior.set? false
      set.behavior]                                      ; fishes set a new behavior in the end of the go procedure, every x seconds (determined by behavior.change.interval)
  ] [ask fishes [
    if detectability < 1 [
      set visible? random-bernoulli detectability        ; if behavior.change procedure is not called (in which visible? is set), then just set visible? if detectability is lower than 1 (this changes every second and not every x seconds).
      ]
    ]
  ]
  advance-clock
  tick
end  ; of go procedure


to advance-clock
  set seconds seconds + 1
  if seconds = 60 [set minutes minutes + 1 set seconds 0]
  if minutes = 60 and seconds = 0 [set hours hours + 1 set minutes 0]
  if hours = 24 and minutes = 0 and seconds = 0 [set day day + 1 set hours 0]
  if day = 31 and hours = 0 and minutes = 0 and seconds = 0 [set month month + 1 set day 1]
  if month = 13 and day = 1 and hours = 0 and minutes = 0 and seconds = 0 [set year year + 1 set month 1 set season 1]
  if month != 1 and (month - 1) mod 3 = 0 and day = 1 and hours = 0 and minutes = 0 and seconds = 0 [set season season + 1]
end



;Observer procedures

to do.outputs
  ask timetransdivers [
    let real.count length counted.fishes
    let expected.count total.density * timed.transect.area
    set t.bias (real.count - expected.count) / expected.count
    output-type "Timed transect diver bias was " output-print precision t.bias 2            ; outputs bias with 2 decimal places
  ]
  
  ask distransdivers [
    let real.count length counted.fishes
    let expected.count total.density * distance.transect.area
    set d.bias (real.count - expected.count) / expected.count
    output-type "Distance transect diver bias was " output-print precision d.bias 2         ; outputs bias with 2 decimal places
  ]
  
 ask statdivers [
    let real.count length counted.fishes
    let expected.count total.density * stationary.area
    set s.bias (real.count - expected.count) / expected.count
    output-type "Stationary diver bias was " output-print precision s.bias 2                ; outputs bias with 2 decimal places
 ]
 
 ask rovdivers [
    let real.count length counted.fishes
    output-type "Roving diver swam " output-type roving.time.secs * roving.diver.mean.speed output-type "m and counted " output-type real.count output-print " fishes"                     ; the roving diver only tells how many fishes it counted
 ]
end



;FISH PROCEDURES


;fish movement model

to do.fish.movement
  if schooling? [set schoolmates other fishes in-cone perception.dist perception.angle with [species = [species] of myself]]    ; if schooling is true,look for conspecifics in my vicinity
 
  set acceleration (list 0 0)                                                                                                   ; acceleration at each step is determined entirely by the urges

    
    add-urge patch-center-urge patch.gathering.w
    add-urge wander-urge wander.w
    add-urge avoid-obstacle-urge obstacle.avoidance.w
    add-urge avoid-predator-urge predator.avoidance.w
    add-urge avoid-diver-urge diver.avoidance.w
    add-urge rest-urge rest.w

  if count schoolmates > 0 and schooling?                                                        ; if I'm not in a school ignore the school related urges. If schooling is disabled, ignore them as well
  [ add-urge spacing-urge spacing.w
    add-urge center-urge center.w
    add-urge align-urge align.w ]
  
  let deceleration (scale ((drag.formula * ((magnitude velocity) ^ 2))) (normalize velocity))    ; subtract the deceleration due to drag from the acceleration vector
  set acceleration (add acceleration deceleration)
  
  if magnitude acceleration > max.acceleration                                                   ; keep the acceleration within the accepted range. It is important that this is done after accounting for drag, so that max.acceleration can be reached.
  [ set acceleration
    (scale
        max.acceleration
        normalize acceleration) ]

  set velocity (add velocity acceleration)                                                       ; the new velocity of the fish is sum of the acceleration and the old velocity.

  if magnitude velocity > max.velocity                                                           ; keep the velocity within the accepted range
  [ set velocity
    (scale
        max.velocity
        normalize velocity) ]

  let nxcor xcor + ( first velocity )
  let nycor ycor + ( last velocity )
  if magnitude velocity > 0.2 [
    facexy nxcor nycor
    fd (magnitude velocity) / movement.time.step]
  
end ;of do.fish.movement

to add-urge [urge factor]                                              ;fish procedure
  set acceleration add acceleration scale factor normalize urge
end

to pick.patch                                                          ;fish procedure
  if picked.patch = false [
   let chosen.patch one-of patches in-cone picked.patch.dist perception.angle
   
   if [pxcor] of chosen.patch > (world-width - (picked.patch.dist + 1)) [                                              ; create a (picked.patch.dist + 1) -wide buffer so that picked patches are not too close to the edges
    let new.chosen.patch patch ([pxcor] of chosen.patch - (picked.patch.dist + 1)) ([pycor] of chosen.patch)
    set chosen.patch new.chosen.patch 
   ]
   if [pxcor] of chosen.patch < (picked.patch.dist + 1) [
    let new.chosen.patch patch ([pxcor] of chosen.patch + (picked.patch.dist + 1)) ([pycor] of chosen.patch)
    set chosen.patch new.chosen.patch 
   ]
   if [pycor] of chosen.patch > (world-height - (picked.patch.dist + 1)) [
    let new.chosen.patch patch ([pxcor] of chosen.patch) ([pycor] of chosen.patch - (picked.patch.dist + 1))
    set chosen.patch new.chosen.patch 
   ]   
   if [pycor] of chosen.patch < (picked.patch.dist + 1) [
    let new.chosen.patch patch ([pxcor] of chosen.patch) ([pycor] of chosen.patch + (picked.patch.dist + 1))
    set chosen.patch new.chosen.patch 
   ]    
   
   set picked.patch chosen.patch
   if any? schoolmates [
    ask schoolmates [
     set picked.patch chosen.patch 
    ]
   ]]  
end

;procedure that selects a new behavior and fills in behavior parameters

to set.behavior                                                        ; fish procedure
  if not behavior.set? [
    let pairs (map list behavior.list behavior.freqs)                    ; pairs behavior names with probabilities as lists within a list, to work better with the rnd extension: [[b1 0.2] [b2 0.3] [b3 0.5]]
    set current.behavior first rnd:weighted-one-of pairs [ last ? ]      ; pick a random behavior from behavior list, using a weighted random pick
    let param.pos position current.behavior behavior.list                ; check which behavior was picked (1, 2, 3 or 4)
    let params item param.pos behavior.params                            ; retreive list of parameters from the correct position in behavior.params
    set detectability item 2 params
    set schooling? item 3 params
    set cruise.distance item 4 params
    set align.w item 5 params
    set center.w item 6 params
    set spacing.w item 7 params
    set wander.w item 8 params
    set rest.w item 9 params
    set picked.patch.dist item 10 params
    set patch.gathering.w item 11 params
    set obstacle.avoidance.w item 12 params
    set predator.avoidance.w item 13 params
    set prey.chasing.w item 14 params
    set diver.avoidance.w item 15 params
    if detectability < 1 [set visible? random-bernoulli detectability]
    set behavior.set? true
    if any? schoolmates [
      ask schoolmates [
        set current.behavior [current.behavior] of myself                         
        set detectability item 2 params
        set schooling? item 3 params
        set cruise.distance item 4 params
        set align.w item 5 params
        set center.w item 6 params
        set spacing.w item 7 params
        set wander.w item 8 params
        set rest.w item 9 params
        set picked.patch.dist item 10 params
        set patch.gathering.w item 11 params
        set obstacle.avoidance.w item 12 params
        set predator.avoidance.w item 13 params
        set prey.chasing.w item 14 params
        set diver.avoidance.w item 15 params
        if detectability < 1 [set visible? random-bernoulli detectability]
        set behavior.set? true
      ]      
    ]
    ifelse patch.gathering.w > 0 [                                       ; if selected behavior has patch gathering urge, pick a patch. If in a school, ask schoolmates to pick a patch collectively.
      pick.patch] [set picked.patch false]                               ; if patch gathering urge has zero weight, variable picked.patch is set to false (important for pick.patch procedure).
  ]
end

; fish urges

to-report patch-center-urge  ;; fish reporter
  ifelse picked.patch != false [
    let patch-x [pxcor] of picked.patch
    let patch-y [pycor] of picked.patch
    ifelse distancexy patch-x patch-y > picked.patch.dist   ; distance to picked patch
     [ report (list (patch-x - xcor) (patch-y - ycor)) ]   ; number of coordinate units (vertical and horizontal) needed to get to picked.patch
     [ report (list 0 0) ]
    ] [ report (list 0 0) ]
end


to-report center-urge ;; fish reporter
  ;; report the average distance from my schoolmates
  ;; in each direction
  if count schoolmates = 0 or center.w = 0
  [ report (list 0 0) ]
  report
    (map
      [ ?2 - ?1 ]
      (list xcor ycor)
      (list
        mean [ xcor ] of schoolmates
        mean [ ycor ] of schoolmates ) )
end

to-report align-urge ;; fish reporter
  ;; report the average difference in velocity
  ;; from my school mates
  if count schoolmates = 0 or align.w = 0
  [ report (list 0 0) ]
  report normalize (
    ( map
      [ ?1 - ?2 ]
      (list
        mean [ first velocity ] of schoolmates   ; x component
        mean [ last velocity ] of schoolmates )  ; y component
      velocity ))
end

to-report rest-urge ;; fish reporter
  ;; report the difference in velocity
  ;; from [0 0]
  report subtract [0 0] velocity
end

to-report wander-urge ; fish reporter
  ;; report 2 random numbers between -1 and 1
  report n-values 2 [ (random-float 2) - 1 ]
end

to-report spacing-urge ;; fish reporter
  let urge [ 0 0 ]
  ;; report the sum of the distances to fishes
  ;; in my school that are closer to me than
  ;; cruise.distance (in body lengths)
  ask schoolmates with [ distance myself < (cruise.distance * size) ] [
    set urge
      add
        urge
        (subtract
          (list [xcor] of myself [ycor] of myself)
          (list xcor ycor))
  ]
  report urge
end

to-report avoid-obstacle-urge ;; fish reporter
 let urge (list 0 0)
  if obstacle.avoidance.w = 0 [ report urge ]
  ;; report the sum of the distances from
  ;; any patches that are obstacles
  ;; in each direction
  ask patches in-cone perception.dist perception.angle with [ pcolor = brown ]   ; patches that are brown are obstacles
  [ set urge
      add
        urge
        subtract
          (list [xcor] of myself [ycor] of myself)
          (list pxcor pycor)
  ]
  report urge
end

to-report avoid-predator-urge ;; fish reporter
; a normalized vector that is opposite to the position of the predator,
; and scale it by the inverse of the squared distance to the predator, as a proportion of approach.dist
 let urge (list 0 0)
  if predator.avoidance.w = 0 [ report urge ]
  ask other fishes in-cone approach.dist perception.angle with [prey.type = "fish"]
  [ let real.dist magnitude (subtract (list [xcor] of myself [ycor] of myself) (list xcor ycor))
    set urge scale ([approach.dist] of myself ^ 2 / real.dist) (normalize (add urge subtract (list [xcor] of myself [ycor] of myself) (list xcor ycor)))   ; WHAT IF REAL DIST IS ZERO?!!
  ]
  report urge
end

to-report avoid-diver-urge ;; fish reporter
; a normalized vector that is opposite to the position of the diver,
; and scale it by the inverse of the squared distance to the diver, as a proportion of approach.dist
 let urge (list 0 0)
  if diver.avoidance.w = 0 [ report urge ]
  ask divers in-cone approach.dist perception.angle 
  [ let real.dist magnitude (subtract (list [xcor] of myself [ycor] of myself) (list xcor ycor))
    set urge scale ([approach.dist] of myself ^ 2 / real.dist) (normalize (add urge subtract (list [xcor] of myself [ycor] of myself) (list xcor ycor)))
  ]
  report urge
end

; vector operations

to-report add [ v1 v2 ]
  report (map [ ?1 + ?2 ] v1 v2)
end

to-report subtract [ v1 v2 ]
  report (map [ ?1 - ?2 ] v1 v2)
end

to-report scale [ scalar vector ]
  report map [ scalar * ? ] vector
end

to-report magnitude [ vector ]
  report sqrt sum map [ ? * ? ] vector
end

to-report normalize [ vector ]
  let m magnitude vector
  if m = 0 [ report vector ]
  report map [ ? / m ] vector
end

;DIVER PROCEDURES

;Distance transect diver procedures

to do.ddiver.movement
  fd speed / movement.time.step ; each step is a second, so the speed is basically the distance divided by the movement.time.step
end

to d.count.fishes                      ; for fixed distance transects, there is a limit to the y coordinate (the diver doesn't count beyond the end mark of the transect)
  let myxcor xcor
  let my.final.ycor final.ycor
  let seen.fishes fishes in-cone max.visibility distransdiver.viewangle
  let eligible.fishes seen.fishes with [(xcor > myxcor - (distance.transect.width / 2)) and (xcor < myxcor + (distance.transect.width / 2)) and (ycor < my.final.ycor)]  ; distance transects have a limit in ycor as well
  let identifiable.fishes eligible.fishes with [visible.dist <= distance myself and visible?]      ; only fish that are closer to the diver than their visible.dist and are visible are counted
  let diver.memory memory
  let new.fishes identifiable.fishes with [not member? who diver.memory] ; only fishes that are not remembered are counted
  if any? new.fishes [
    let new.records ([species] of new.fishes)
    set counted.fishes sentence counted.fishes new.records
    set memory sentence memory [who] of new.fishes
  ]
    
end

to d.forget.fishes          ; runs if super memory is off
  let seen.fish.id [who] of fishes in-cone max.visibility distransdiver.viewangle
  let diver.memory memory
  set memory filter [member? ? seen.fish.id] diver.memory
end

;Timed transect diver procedures

to do.tdiver.movement
  fd speed / movement.time.step ; each step is a second, so the speed is basically the distance divided by the movement.time.step
end

to t.count.fishes
  let myxcor xcor
  let seen.fishes fishes in-cone max.visibility timetransdiver.viewangle
  let eligible.fishes seen.fishes with [(xcor > myxcor - (timed.transect.width / 2)) and (xcor < myxcor + (timed.transect.width / 2))]  ; this only works for transects heading north, of course
  let identifiable.fishes eligible.fishes with [visible.dist <= distance myself and visible?]
  let diver.memory memory
  let new.fishes identifiable.fishes with [not member? who diver.memory] ; only fishes that are not remembered are counted
  if any? new.fishes [
    let new.records ([species] of new.fishes)
    set counted.fishes sentence counted.fishes new.records
    set memory sentence memory [who] of new.fishes
  ]
end

to t.forget.fishes          ; runs if super memory is off
  let seen.fish.id [who] of fishes in-cone max.visibility timetransdiver.viewangle
  let diver.memory memory
  set memory filter [member? ? seen.fish.id] diver.memory
end


;Stationary diver procedures

to do.stdiver.movement
  set heading heading + (stationary.turning.angle / movement.time.step)            ; turns clockwise every second
  
end

to s.count.fishes
  let eligible.fishes fishes in-cone stationary.radius statdiver.viewangle
  let identifiable.fishes eligible.fishes with [visible.dist <= distance myself and visible?]
  let diver.memory memory
  let new.fishes identifiable.fishes with [not member? who diver.memory] ; only fishes that are not remembered are counted
  if any? new.fishes [
    let new.records ([species] of new.fishes)
    set counted.fishes sentence counted.fishes new.records
    set memory sentence memory [who] of new.fishes
  ]
end

to s.forget.fishes          ; runs if super memory is off
  let seen.fish.id [who] of fishes in-cone stationary.radius statdiver.viewangle
  let diver.memory memory
  set memory filter [member? ? seen.fish.id] diver.memory
end

;Roving diver procedures

to do.rdiver.movement
  if ticks mod 2 = 0 [set heading heading + random-float-between (- roving.diver.turning.angle) roving.diver.turning.angle]         ;turning happens every 2 seconds
  fd speed / movement.time.step ; each step is a second, so the speed is basically the distance divided by the movement.time.step
end

to r.count.fishes
  let eligible.fishes fishes in-cone max.visibility statdiver.viewangle
  let identifiable.fishes eligible.fishes with [visible.dist <= distance myself and visible?]
  let diver.memory memory
  let new.fishes identifiable.fishes with [not member? who diver.memory] ; only fishes that were not previously counted are counted
  if any? new.fishes [
    let new.records ([species] of new.fishes)
    set counted.fishes sentence counted.fishes new.records
    set memory sentence memory [who] of new.fishes
  ]
    end

to r.forget.fishes          ; runs if super memory is off
  let seen.fish.id [who] of fishes in-cone max.visibility statdiver.viewangle
  let diver.memory memory
  set memory filter [member? ? seen.fish.id] diver.memory
end


; useful reporters

to-report random-bernoulli [probability-true]
  if probability-true < 0.0 or probability-true > 1.0 [user-message "WARNING: probability outside 0-1 range in random-bernoulli reporter."]
  report random-float 1.0 < probability-true
end

to-report random-float-between [a b]           ; generate a random float between two numbers
  report random-float (b - a + 1) + a
end

to-report occurrences [x the-list]             ; count the number of occurrences of an item in a list (useful for summarizing species lists)
  report reduce
    [ifelse-value (?2 = x) [?1 + 1] [?1]] (fput 0 the-list)
end
@#$#@#$#@
GRAPHICS-WINDOW
415
10
1025
2441
-1
-1
30.0
1
10
1
1
1
0
1
1
1
0
19
0
79
1
1
1
seconds
5.0

BUTTON
120
160
205
210
Setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
205
160
325
210
Go / Pause
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
325
160
410
210
Go once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

MONITOR
115
70
210
115
Total area (m2)
world.area
0
1
11

SLIDER
1035
55
1250
88
timed.transect.diver.speed
timed.transect.diver.speed
1
10
8
1
1
m/min
HORIZONTAL

TEXTBOX
1035
40
1190
58
Timed transect diver movement
11
0.0
1

TEXTBOX
1035
290
1185
308
Stationary diver movement
11
0.0
1

SLIDER
1035
305
1250
338
stationary.turning.angle
stationary.turning.angle
0
90
4
1
1
º / sec
HORIZONTAL

SLIDER
1270
120
1485
153
max.visibility
max.visibility
5
40
10
1
1
m
HORIZONTAL

SLIDER
1035
340
1250
373
stationary.radius
stationary.radius
1
20
7.5
0.5
1
m
HORIZONTAL

SWITCH
1270
55
1485
88
super.memory?
super.memory?
1
1
-1000

OUTPUT
15
390
410
500
11

SLIDER
1035
430
1250
463
roving.diver.speed
roving.diver.speed
1
10
8
1
1
m/min
HORIZONTAL

TEXTBOX
1035
415
1155
433
Roving diver movement
11
0.0
1

SLIDER
1035
465
1250
498
roving.diver.turning.angle
roving.diver.turning.angle
0
45
4
1
1
º / 2 secs
HORIZONTAL

SWITCH
15
565
190
598
timed.transect.diver?
timed.transect.diver?
1
1
-1000

SWITCH
15
635
190
668
stationary.diver?
stationary.diver?
1
1
-1000

SWITCH
15
670
190
703
roving.diver?
roving.diver?
1
1
-1000

TEXTBOX
20
545
170
563
Select active sampling methods
11
0.0
1

SWITCH
200
305
410
338
show.paths?
show.paths?
1
1
-1000

SWITCH
15
305
201
338
show.diver.detail.windows?
show.diver.detail.windows?
1
1
-1000

TEXTBOX
20
285
95
303
Display options
11
0.0
1

TEXTBOX
1270
10
1505
51
Ability to remember all counted fishes (If turned off, divers forget fishes that leave the FOV). Applies to all divers.
11
0.0
1

BUTTON
15
500
410
535
Clear output
clear-output
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
190
565
255
598
Follow
if any? timetransdivers [follow one-of timetransdivers]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
190
635
255
668
Follow
if any? statdivers [follow one-of statdivers]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
190
670
255
703
Follow
if any? rovdivers [follow one-of rovdivers]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
15
705
255
738
Reset perspective
reset-perspective
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
15
600
190
633
distance.transect.diver?
distance.transect.diver?
0
1
-1000

BUTTON
190
600
255
633
Follow
follow one-of distransdivers
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
1035
125
1250
158
transect.time
transect.time
1
90
1
1
1
minutes
HORIZONTAL

SLIDER
1035
250
1250
283
transect.distance
transect.distance
5
100
10
5
1
meters
HORIZONTAL

SLIDER
1035
375
1250
408
stationary.time
stationary.time
1
90
2
1
1
minutes
HORIZONTAL

SLIDER
1035
500
1250
533
roving.time
roving.time
1
90
2
1
1
minutes
HORIZONTAL

TEXTBOX
1035
165
1205
183
Distance transect diver movement
11
0.0
1

SLIDER
1035
180
1250
213
distance.transect.diver.speed
distance.transect.diver.speed
1
10
8
1
1
m/min
HORIZONTAL

MONITOR
15
70
115
115
Total nr. of fishes
numb.fishes
0
1
11

MONITOR
210
70
310
115
Total density
total.density
3
1
11

MONITOR
310
70
410
115
Number of species
nr.species
17
1
11

SLIDER
1270
185
1485
218
behavior.change.interval
behavior.change.interval
1
20
10
1
1
seconds
HORIZONTAL

TEXTBOX
1270
170
1470
188
Time between behavior changes in fish.
11
0.0
1

BUTTON
270
600
400
670
Focus on a random fish
let chosen.one one-of fishes\nfollow chosen.one\ninspect chosen.one
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
310
25
360
66
NIL
minutes
0
1
10

MONITOR
260
25
310
66
NIL
hours
0
1
10

MONITOR
210
25
260
66
NIL
day
0
1
10

MONITOR
160
25
210
66
NIL
month
0
1
10

MONITOR
110
25
160
66
NIL
season
0
1
10

MONITOR
60
25
110
66
NIL
year
0
1
10

MONITOR
360
25
410
66
NIL
seconds
0
1
10

SWITCH
15
340
200
373
smooth.animation?
smooth.animation?
1
1
-1000

BUTTON
15
160
120
210
Import dataset
import-dataset
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
15
10
165
28
Model clock / calendar
11
0.0
1

TEXTBOX
205
345
335
371
Disable smooth animation for faster model runs.
11
0.0
1

TEXTBOX
35
135
105
153
1. Start here
11
15.0
1

TEXTBOX
125
130
210
160
2. Establish initial conditions
11
15.0
1

TEXTBOX
225
135
320
153
3. Run the model
11
15.0
1

TEXTBOX
1040
15
1250
33
DIVER MOVEMENT____________________
11
0.0
1

SLIDER
1035
90
1250
123
timed.transect.width
timed.transect.width
1
20
2
1
1
m
HORIZONTAL

SLIDER
1035
215
1250
248
distance.transect.width
distance.transect.width
1
20
2
1
1
m
HORIZONTAL

TEXTBOX
65
740
215
766
(Stop following divers or fish)
11
0.0
1

TEXTBOX
1270
100
1510
118
Water visibility (Affects the divers' field of view)
11
0.0
1

CHOOSER
1270
270
1485
315
movement.time.step
movement.time.step
5 10
0

TEXTBOX
1270
225
1505
270
Time step for each decision in the fish movement model. This must match the frame rate in model settings for normal speed to match real time.
11
0.0
1

TEXTBOX
1270
320
1510
375
Please test behaviors in the species creator with the same frame rate. Different frame rates can lead to very different behaviors with the same parameters.
11
15.0
1

SWITCH
15
235
132
268
fixed.seed?
fixed.seed?
1
1
-1000

INPUTBOX
135
220
215
280
seed
123456
1
0
Number

TEXTBOX
225
235
380
265
Select a fixed seed to generate similar consecutive model runs.
11
0.0
1

@#$#@#$#@
## WHAT IS IT?

This is a model that simulates divers counting sharks while deploying the belt-transect, stationary-point-count and roving underwater visual census techniques. The model demonstrates how non-instantaneous sampling techniques produce bias by overestimating the number of counted animals, when they move relative to the person counting them.

This is applied to divers and sharks, but is intended to reflect what happens with many other animal censuses (aerial surveys, bird transects, etc.).

The model can be used to demonstrate that bias increases as the speed of the animals relative to the observer increases.

The model assumes an area that is featureless and flat, with a default size of 400x400 cells with an area of 1 square meter. The origin of the coordinate system is on the bottom left corner and depth is ignored (assumed constant).

## HOW IT WORKS

For each simulation, the divers start in the middle and face north. Sharks and transect divers move every second, but the time step for counting can be set to 2 seconds for smaller computing times (set by default on the original model, but note that some sharks may be missed, particularly if they are very fast).

At each counting time step, divers count the number of sharks they can see based on the direction the diver is facing, the visibility distance and angle. Viewing angles are fixed and cannot be changed on the interface. They are set to 160º for the stationary and roving divers and 180º for the transect diver.

On the belt-transect method, only fish within the transect width are counted, while on the stationary-point-count method, only fish within a pre-determined radius are counted.

Transect diver moves in a fixed direction at a constant speed (default is 4 m/minute).

Stationary diver rotates a given number of degrees clockwise every two seconds (default is 4).

Sharks move at a speed specified by the user, and travel in a direction that is restricted by a turning angle based on the previous direction.

Sharks and divers that reach the boundaries of the area wrap around to the opposite side, to keep the density constant (on the original model they can leave the area, but that doesn't seem to have significant effects on the output, if the area is large enough.

By default, divers keep a memory of every individual shark counted and do not recount.

## HOW TO USE IT

In order to speed up model runs, please disable update view (at the top).

The "setup" button feeds all parameter values into the model, places the selected divers and spreads all the sharks across the area.

The "go" button runs the model for the time specified in survey.time (default is 300 seconds) and then stops. The model can be stopped or paused at any time by pressing the go button while it is running. The "go once" button avances the model run by 1 second each time.

The "setup with default parameter values" button sets all parameters to values used in the first experiment by Ward-Paige et al. (2010) with shark speed set to 1 m/s. The model is automatically setup and you just need to press "go".

"transect.width" sets the width of the belt transect that is sampled as the diver swims in the center (half of the width to each side of the diver).

To set the initial number of sharks, introduce the total number of sharks directly under "numb.sharks" on the interface. Alternatively (and preferably), you can set the real density needed under "shark.density" and the model will place the right number of sharks for the total area. If shark.density is set to a value greater than 0 when the model starts, it has priority over "numb.sharks".

Select the sampling methods that will be in the model run by turning their swiches on and off on the interface. Please note that the roving diver will not calculate densities and thus will not estimate bias. If you want to see the details of every diver in a floating window during the run, switch on "show.diver.detail.windows?".

If you want the transect and roving divers to draw a path as they move, switch on "show.paths?".

Stationary.radius must be set to a value lower than visibility.length.

In the end of each run, each diver reports its relative bias, calculated as:

(real count - expected count) / expected count

where "expected count" is basically the shark density in the model multiplied by the total sampled area.

For the stationary diver:

sampled area = pi * stationary.radius^2

For the transect diver:

sampled area = survey.time * (transect.diver.speed / 60) * transect.width + visibility length * transect.width

Notice that the final part of the transect is approximated to a rectangle of length equal to the visibility length, instead of taking into account the arc produced by the cone of vision. This was done in the original model and, With relatively large visibility lengths, does not seem to have much impact on the final result.

The model will also output the conversion factor value for each method (transect and stationary). The real count (in the field) divided by this factor value should provide an estimate that is corrected to account for bias (given that all parameters are realistically set).

## THINGS TO NOTICE

If "view updates" is enabled, the model will run in real time (every second in the model will run in one second).

To understand the source of the bias, compare a model wun with shark.mean.speed set to 0 m/s (stationary sharks) and another with shark.mean.speed set to 1 m/s. Keep everything else default.

In the first case (stationary sharks), the sampling method will be equivalent to taking a snapshot of the whole sampling area. The sharks will stand still and the diver will count every shark in the sampling area, not repeating any shark (given that "diver.memory?" is switched on). This will produce little to no bias in the estimates.

In the case where sharks move 1 meter every second (while the diver is moving 4 meters every minute), there will be new sharks comming into the sampling area that were not there in the beginning. These sharks will all be counted as they pass in front of the diver, increasing the number of counted sharks, while the real number of sharks in the beginning was much lower. This produces the bias.

If the speed of the sharks is even higher, there are more sharks coming into the field of view of the diver and the bias will be even higher.

## THINGS TO TRY

- Switch on just the transect diver and see how increasing shark speeds increases bias. Why does this happen?

- Do the same for the stationary diver. The results are similar, why?

- With stationary sharks (speed 0) and moving divers, bias is almost zero, but what about stationary divers with moving sharks? Place a stationary diver with stationary.turning.angle set to 0 and sharks with speed > 0 and see the results.

- Run a model in real time (normal speed and "view updates" checked) with the default parameters. While it is running, press the follow button in front of the transect diver switch to focus the view on that diver. Now head to the top right corner of the world window and click "3D" to go to the 3D view. Observing the diver from this perspective as it counts sharks is a great way of understanding the bias in non-instantaneous sampling of moving animals.

- Going to tools -> behaviorSpace, you can see there are 2 experiments created that correspond to the experiments run by Ward-Paige et al. (2010). One to observe the increase in bias with increasing shark speed (30 replicates per run) and one to test every combination of values from a set of realistic candidates (a single run per combination) and observe the resulting bias (prepare for long computing times with this one).

## USING THE BIAS CALCULATOR

The model can be used as a tool to get better estimates from field surveys using the bias calculator at the bottom.

To make corrections to observed values (values observed in the field by non-instantaneous surveys):

1. Decide on targeted fish species and select an appropriate speed and turning angle for that species.
2. Select most appropriate sampling values (e.g. transect width, swim speed, survey time, etc.) Note: for visibility, select the distance you would be sure to detect the targeted fish.
4. Run the model with the selected parameters, making sure that the your sampling method of choice is turned on in the switches.
5. After the survey time finishes the model stops and the conversion factor is calculated.
6. On the calculator, go to the "observed.value" box and input the number of fish from that species you counted in the field
7. Select the appropriate method on the calculator (transect or stationary)
8. Press calculate. The result is the "real" count, corrected for bias. 
5. Repeat for other species.

## NOTES ON THE ADAPTATION TO NETLOGO

On the original model, the sharks could leave the area and come back (or leave permanently). Here the world is set to wrap around the edges. For an area that is large enough, this does not seem to affect the final results. In order to reflect what was made in the original model in R, one can set a buffer area around where no sharks are placed. If for some reason we want sharks to leave and never come back, we can just make them die when they reach the edges.

The sharks in NetLogo have a visual representation, unlike in R, where they where just points. However, in the counting procedure in this NetLogo version, only the coordinates of the sharks are checked to see if they are counted, so their size is basically ignored and does not influence results.

## RELATED MODELS

The orignial AnimDens model was implemented in R by Christine Ward-Paige, Joanna Mills Flemming and Heike K. Lotze. The code can be downloaded at:

http://journals.plos.org/plosone/article/asset?unique&id=info:doi/10.1371/journal.pone.0011722.s001

## CREDITS AND REFERENCES

Original model by Christine Ward-Paige (2009)

Implemented in NetLogo by Miguel Pessanha Pais (2015)

If you use this model, please cite the original publication:

Ward-Paige, C.A., Flemming, J.M., Lotze, H.K., 2010. Overestimating Fish Counts by Non-Instantaneous Visual Censuses: Consequences for Population and Community Descriptions. PLoS ONE 5(7): e11722. doi:10.1371/journal.pone.0011722

URL: http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0011722

Another publication that uses the roving diver:

Ward-Paige, C.A., Lotze, H.K., 2011. Assessing the Value of Recreational Divers for Censusing Elasmobranchs. PLoS ONE 6(10): e25609. doi:10.1371/journal.pone.0025609

URL: http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0025609

In order to cite the NetLogo implementation:

Pais, M.P., Ward-Paige, C.A. (2015). AnimDens model NetLogo implementation. http://modelingcommons.org/model/

In order to cite the NetLogo software:

Wilensky, U. (1999). NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern Institute on Complex Systems, Northwestern University, Evanston, IL.

## COPYRIGHT AND LICENSE

Copyright 2015 Miguel Pessanha Pais and Christine Ward-Paige.

![CC BY-NC-SA 3.0](http://ccl.northwestern.edu/images/creativecommons/byncsa.png)

This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 3.0 License.  To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-sa/3.0/ or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

diver
true
0
Polygon -7500403 true true 105 90 120 180 120 270 105 300 135 300 150 210 165 300 195 300 180 270 180 180 195 90
Rectangle -7500403 true true 135 75 165 90
Polygon -7500403 true true 180 120 195 30 210 45 210 105
Polygon -7500403 true true 120 120 105 30 90 45 90 105
Rectangle -1184463 true false 135 90 165 195
Circle -16777216 true false 120 30 60

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

fish top
true
0
Polygon -7500403 true true 157 1 142 1 104 45 104 75 89 120 104 120 117 191 137 243 147 304 181 240 184 189 194 120 209 120 194 75 194 45

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

shark
true
0
Polygon -7500403 true true 153 17 149 12 146 29 145 -1 138 0 119 53 107 110 117 196 133 246 134 261 99 290 112 291 142 281 175 291 185 290 158 260 154 231 164 236 161 220 156 214 160 168 164 91
Polygon -7500403 true true 161 101 166 148 164 163 154 131
Polygon -7500403 true true 108 112 83 128 74 140 76 144 97 141 112 147
Circle -16777216 true false 129 32 12
Line -16777216 false 134 78 150 78
Line -16777216 false 134 83 150 83
Line -16777216 false 134 88 150 88
Polygon -7500403 true true 125 222 118 238 130 237
Polygon -7500403 true true 157 179 161 195 156 199 152 194

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.2.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
1
@#$#@#$#@
