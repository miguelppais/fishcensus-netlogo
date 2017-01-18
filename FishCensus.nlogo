extensions [
  csv              ; used to import species parameters through a csv file
  rnd              ; used by fishes to calculate next behavior with a weighted random pick
  ]

;Global variables not represented on the interface tab

globals[
  world.area
  species.data              ; imported input data from the csv file
  nr.species                ; number of species in the csv file. Variable updated by the go procedure
  total.density
  wat.dens.value            ; just a step to calculate drag.formula for each species when fish are created
  numb.fishes
  timed.transect.diver.mean.speed
  distance.transect.diver.mean.speed
  transect.time.secs
  stationary.time.secs
  roving.time.secs
  roving.diver.mean.speed
  sampling.area
  persons                   ; an agentset of diver and buddy
  hours                     ; variables for the model clock
  minutes
  seconds
  real.densities
  snapshot.estimates
  density.estimates
  bias.estimates
  output.real              ; outputs for behaviorspace experiments (only works with 1st species in input file)
  output.estimated
  output.difference
  output.inaccuracy
  output.bias
  output.instantaneous
]

;Agent types

breed [fishes fish]

breed [divers diver]

breed [buddies buddy]

;Agent variables

fishes-own [                     ; fish variables
  drag.formula                   ; the coefficient used to calculate drag, given speed and fish size
  current.behavior               ; picked from behavior.list
  behavior.set?                  ; boolean that tells if fish has set its behavior this turn
  schoolmates                    ; for schooling species, agentset of nearby fishes
  velocity                       ; vector with x and y components determined by the previous velocity and the current acceleration at each step
  acceleration                   ; vector with x and y components, determined by the sum of all urges
  species                        ; this separates the different types within the fish breed. This is the variable recorded by divers while counting
  prey.type                      ; "benthic" or "fish". Defines the type of feeding behavior (gather around a patch or chase prey)
  detectability                  ; probability of being missed (if = 1 fish is always counted if within id.distance)
  visible?                       ; boolean variable that is set by detectability every second
  id.distance                    ; maximum distance from diver at which it is still possible to identify the species (and count the fish)
  approach.dist                  ; minimum distance from diver that the fish tolerate
  behavior.list                  ; list of up to 4 distinct behaviors
  behavior.freqs                 ; frequency of each behavior
  behavior.params                ; a list of lists with constants for the movement model per behavior
  picked.patch                   ; patch picked by the school to feed on (if prey is benthic)
  picked.patch.dist              ; maximum distance from the feeding patch
  patch.gathering.w              ; weighs the urge to gather at a feeding patch
  perception.dist                ; fish perception distance
  perception.angle               ; fish perception angle (360 for full directional awareness)
  max.sustained.speed            ; maximum attainable velocity with low energy consumption (cruise)
  burst.speed                    ; maximum attainable velocity with high energy consumption (chase and escape)
  max.acceleration               ; maximum attainable acceleration
  diver.avoidance.w              ; weighs the urge to avoid divers. If negative, fish are attracted to divers
  predator.avoidance.w           ; weighs the urge to avoid fish with prey.type = "fish"
  prey.chasing.w                 ; weighs the urge to chase other fish
  schoolmate.dist                ; distance among schoolmates in a school (in body lengths)
  align.w                        ; weighs the urge to match school direction
  center.w                       ; weighs the urge to stand between nearby schoolmates
  spacing.w                      ; weighs the urge to maintain crusing.distance from schoolmates
  wander.w                       ; weighs the urge to wander around randomly
  rest.w                         ; weighs the urge to stop and rest
  cruise.w                       ; wighs the urge to keep a constant speed and heading
  schooling?                     ; boolean variable that determines if fish exhibit schooling behavior
]


divers-own [             ; distance transect diver variables
  counted.fishes
  snapshot.fishes
  speed
  memory
  viewangle
  initial.ycor                   ; for distance transects
  final.ycor                     ; for distance transects
  finished?
]


;Interface procedures

to startup
  set file.name "example"
  set override.density 0
end


to setup
  ca
  stop-inspecting-dead-agents                           ; clears diver detail windows from previous simulation runs
  output-print "Importing species data..."
  let full.file.name word file.name ".csv"
  carefully [set species.data (csv:from-file full.file.name file.delimiter)]                             ; import csv file into a list of lists
  [user-message (word "File " file.name " not found in model folder. Import the example.csv file or create one using the FishCensus species creator.") stop] ; ERROR MESSAGE
  if species.data = 0 [user-message "You need to import a dataset. Import the example.csv file or create one using the FishCensus species creator." stop] ; ERROR MESSAGE
  if length species.data < 2 [user-message "ERROR: input file has only 1 row or is empty. Check if input file has a header plus 1 line per species." stop] ; ERROR MESSAGE
  if length item 0 species.data != 82 [user-message "ERROR: unable to separate parameters in the file. Ensure the correct file delimiter was picked." stop] ; ERROR MESSAGE
  set nr.species length species.data - 1                                                            ; all filled lines minus the header
  set real.densities []                                                                             ; establishes real.densities as a list
  foreach n-values nr.species [[x] -> x + 1] [                                                      ; Fills real densities with data from file   UPDATED TO NEW CODE SYNTAX FOR NETLOGO 6.0
    [n] -> let sp.param item n species.data
  let real.dens.pair list item 0 sp.param item 4 sp.param
  set real.densities lput real.dens.pair real.densities
    ]
  if fixed.seed? [random-seed seed]
  output-print "Painting floor tiles..."
  ask patches with [pycor mod 2 = 0 and pxcor mod 2 = 0] [set pcolor 103]
  ask patches with [pycor mod 2 = 1 and pxcor mod 2 = 1] [set pcolor 103]
  ask patches with [pcolor = black] [set pcolor 105]
  set world.area world-height * world-width
  set transect.time.secs transect.time * 60             ; convert survey times from minutes to seconds
  set stationary.time.secs stationary.time * 60
  set roving.time.secs roving.time * 60
  set timed.transect.diver.mean.speed (timed.transect.diver.speed / 60)  ; these lines just convert interface speeds (in m/min) to m/s
  set distance.transect.diver.mean.speed (distance.transect.diver.speed / 60)
  set roving.diver.mean.speed (roving.diver.speed / 60)
  set density.estimates []                                               ; establish diver record lists
  set snapshot.estimates []
  set bias.estimates []
  set persons no-turtles                                                  ; resets the "persons" agentset (otherwise it would be set to 0 and generate errors during setup)

; for the timed transect, the sampled area is distance * width plus max visibility * width at the end (an approximation that ignores the fact that visibility is the radius of a circle).

  if sampling.method = "Fixed time transect" [
    set sampling.area timed.transect.width * (timed.transect.diver.mean.speed * transect.time.secs) + (timed.transect.width * max.visibility)
  ]

; for the distance transect, the sampled area is only distance * width

  if sampling.method = "Fixed distance transect" [
    set sampling.area distance.transect.width * transect.distance
  ]

; for the stationary diver, it is the area of the circle around the diver

  if sampling.method = "Stationary point count" [
    set sampling.area pi * stationary.radius ^ 2
  ]

; the roving diver only records counts, so sampling area can be set to 1

  if sampling.method = "Random path" [
    set sampling.area 1
  ]

; read the information stored in species.data to place fishes

  output-print "Placing fishes..."
  foreach n-values nr.species [[x] -> x + 1] [           ; loop that creates fish for each species and sets all fish variables
    [n] -> let sp.param item n species.data
    ifelse override.density = 0 [set total.density item 4 sp.param]
    [set total.density override.density]
    create-fishes (total.density * world.area) [
      setxy random-xcor random-ycor
      set velocity [0 0]
      set acceleration [0 0]
      set picked.patch false
      set schoolmates no-turtles                  ; sets schoolmates as an empty agentset
      set species item 0 sp.param
      if length sp.param != 82 [user-message (word "ERROR: species number " n " '" species "' has a parameter list that is incomplete or too long. Check file.") stop] ; ERROR MESSAGE
      set shape item 1 sp.param
      set size item 2 sp.param
      ifelse item 17 sp.param = "Seawater - 1027 Kg/m3" [set wat.dens.value 1027] [set wat.dens.value 1000]
      set drag.formula ((0.5 * (item 14 sp.param) * wat.dens.value * (((item 13 sp.param) * ((size * 100) ^ 2)) * 0.0001))/(((item 15 sp.param) * ((size * 100) ^ (item 16 sp.param))) * 0.001))   ; formula to calculate k in order to calculate the deceleration due to drag in the form of a = kv^2 (v for velocity).
      set color item 3 sp.param
      set id.distance item 5 sp.param
      set approach.dist item 6 sp.param
      set perception.dist item 7 sp.param
      set perception.angle item 8 sp.param
      set prey.type item 9 sp.param
      set max.acceleration item 10 sp.param
      set max.sustained.speed item 11 sp.param
      set burst.speed item 12 sp.param
      set behavior.list (list item 18 sp.param item 34 sp.param item 50 sp.param item 66 sp.param)
      set behavior.freqs (list item 19 sp.param item 35 sp.param item 51 sp.param item 67 sp.param)
      if reduce + behavior.freqs != 1 [
       user-message (word "ERROR! Behavior frequencies for " species " do not add up to 1.") stop]   ; ERROR MESSAGE
      set behavior.params (list                                ; lists parameter values for all 4 behaviors
        sublist sp.param 18 34
        sublist sp.param 34 50
        sublist sp.param 50 66
        sublist sp.param 66 82)
      set visible? true
      set behavior.set? false
      set.first.behavior                     ; fish procedure that picks the first behavior in behavior.list as the starting behavior and fills in parameters (this sets "visible?" depending on detectability of first behavior)
    ]
  ]
  set total.density (count fishes / world.area) ; this takes into account the actual number of fishes in the area, and not the original input number.
  set numb.fishes count fishes
  output-print (word "Imported " count fishes " fishes belonging to " nr.species " species,")
  output-print (word "with a total density of " total.density " fish per square meter.")
  output-print (word "Stabilizing fish movement for " initial.time.to.stabilize.movement " model seconds...")
  ask fishes [repeat movement.time.step * initial.time.to.stabilize.movement [do.fish.movement]] ; lets the fish movement model stabilize for a pre-determined amount of model seconds (20 by default).


  output-print "Placing diver..."

if sampling.method = "Fixed time transect" [                                         ;timed transect diver setup
  create-divers 1 [
 set heading 0
 set shape "diver"
 set color cyan
 set size 2
 setxy (world-width / 2) 5
 if show.paths? = true [pen-down]                                                           ;this shows the path of the diver
 set speed timed.transect.diver.mean.speed
 set viewangle transect.viewangle
 set counted.fishes [] ; sets counted.fishes as an empty list
 set snapshot.fishes ([species] of (fishes with [(xcor >= (world-width / 2) - timed.transect.width) and (xcor <= (world-width / 2) + timed.transect.width) and (ycor >= (world-height / 4)) and (ycor <= ((world-height / 4) + (transect.time * timed.transect.diver.speed)))]))
  ]
  if buddy? [
   create-buddies 1 [
    set heading 0
    set shape "diver"
    set color cyan
    set size 2
    setxy ([xcor] of one-of divers + 1) ([ycor] of one-of divers - 1)
   ]
  ]
]

if sampling.method = "Fixed distance transect" [                                         ;distance transect diver setup
  create-divers 1 [
 set heading 0
 set shape "diver"
 set color green
 set size 2
 setxy (world-width / 2) 5
 if show.paths? = true [pen-down]                                                           ;this shows the path of the diver
 set speed distance.transect.diver.mean.speed
 set viewangle transect.viewangle
 set initial.ycor ycor                                                              ; fixed distance transect divers record their start and end points as given by "transect.distance" initially
 set final.ycor ycor + transect.distance                                            ; because coordinates are in meters
 set counted.fishes [] ; sets counted.fishes as an empty list
 set snapshot.fishes ([species] of (fishes with [(xcor >= (world-width / 2) - distance.transect.width) and (xcor <= (world-width / 2) + distance.transect.width) and (ycor >= (world-height / 4)) and (ycor <= ((world-height / 4) + (transect.distance)))]))
  ]
  if buddy? [
   create-buddies 1 [
    set heading 0
    set shape "diver"
    set color green
    set size 2
    setxy ([xcor] of one-of divers + 1) ([ycor] of one-of divers - 1)
   ]
  ]
]

if sampling.method = "Stationary point count" [                                               ;stationary diver setup
  create-divers 1 [
 set heading 0
 set shape "diver"
 set color red
 set size 2
 setxy (world-width / 2) (world-height / 2)
 set viewangle stationary.viewangle
 set counted.fishes [] ; sets counted.fishes as an empty list
 set snapshot.fishes ([species] of (fishes in-radius stationary.radius))
]
  if buddy? [
   create-buddies 1 [
    set heading 0
    set shape "diver"
    set color red
    set size 2
    setxy ([xcor] of one-of divers + 1) ([ycor] of one-of divers - 1)
   ]
  ]
]

if sampling.method = "Random path" [                                                      ;roving diver setup
  create-divers 1 [
 set heading 0
 set shape "diver"
 set color magenta
 set size 2
 setxy (world-width / 2) 5
 if show.paths? = true [pen-down]                                                           ;this shows the path of the diver
 set speed roving.diver.mean.speed
 set viewangle rpath.viewangle
 set counted.fishes [] ; sets counted.fishes as an empty list
 set snapshot.fishes []  ; sets snapshot.fishes as an empty list (no snapshot possible for roving divers, as their path is random)
]
  if buddy? [
   create-buddies 1 [
    set heading 0
    set shape "diver"
    set color magenta
    set size 2
    setxy ([xcor] of one-of divers + 1) ([ycor] of one-of divers - 1)
   ]
  ]
]

set persons (turtle-set divers buddies) ; creates the agentset containing divers and buddies

ask divers [
 set finished? false                                            ; reset the "finished?" variable
 set memory []                                                  ; reset the memory list
]

output-print "Setup complete. Press GO to run the model"

reset-ticks

; open diver detail window

if show.diver.detail.window? = true [inspect one-of divers]

end ;of setup procedure


to go
  if count divers = count divers with [finished?] [                                                               ; do.outputs and stop if diver is finished
    do.outputs
    stop]
  if sampling.method = "Stationary point count" and stationary.radius > max.visibility [
    user-message "ERROR: stationary.radius is greater than max.visibility. Diver will not be able to see the sample area."              ; if the stationary radius is higher than visibility, stop and output an error description«
    stop
  ]


  if sampling.method = "Fixed distance transect" [ask divers [                              ; divers count the fishes and then check if finishing conditions are met
    d.count.fishes
    set finished? ycor > final.ycor
  ]
  ]
  if sampling.method = "Fixed time transect" [ask divers [
    t.count.fishes
    set finished? ticks > transect.time.secs
  ]
  ]
  if sampling.method = "Stationary point count" [ask divers [
    s.count.fishes
    set finished? ticks > stationary.time.secs
  ]
  ]
  if sampling.method = "Random path" [ask divers [
    r.count.fishes
    set finished? ticks > roving.time.secs
  ]
  ]



  repeat movement.time.step [

    if sampling.method = "Fixed distance transect" [ask divers [do.ddiver.movement]]         ; move the diver
    if sampling.method = "Fixed time transect" [ask divers [do.tdiver.movement]]
    if sampling.method = "Stationary point count" [ask divers [do.stdiver.movement]]
    if sampling.method = "Random path" [ask divers [do.rdiver.movement]]


    ask buddies [                                                                           ; move the buddy
      move-to one-of divers
      set heading [heading] of one-of divers
      rt 135 fd sqrt 2                           ; keep 1m behind and 1m to the right of the diver
      set heading [heading] of one-of divers

    ]

    ask fishes [                                                                            ; move the fishes
      do.fish.movement
    ]
    if smooth.animation? [display]
  ] ; closes repeat movement.time.step

  if sampling.method = "Random path" and ticks mod 2 = 0 [
    ask divers[set heading heading + random-float-between (- roving.diver.turning.angle) roving.diver.turning.angle]]         ;random path turning happens every 2 seconds (must be out of the "repeat" cycle)


  if not super.memory? [ask divers [forget.fishes]]       ; if super memory is disabled, divers forget fishes they no longer see


  if ticks mod behavior.change.interval = 0 [
    ask fishes [
      set behavior.set? false
      set.behavior
    ]                                      ; fishes set a new behavior in the end of the go procedure, every x seconds (determined by behavior.change.interval)
  ]
  advance-clock
  tick
end  ; of go procedure


to advance-clock
  set seconds seconds + 1
  if seconds = 60 [set minutes minutes + 1 set seconds 0]
  if minutes = 60 and seconds = 0 [set hours hours + 1 set minutes 0]
end



;OBSERVER PROCEDURES

to do.outputs
  ask divers [
    output-print "Density estimates:"
    foreach n-values nr.species [[x] -> x + 1] [
      [n] -> let sp.param item n species.data
      let name item 0 sp.param
      let sp.dens.pair list name precision (occurrences name counted.fishes / sampling.area) 3
      set density.estimates lput sp.dens.pair density.estimates
      output-print (word first sp.dens.pair ": " last sp.dens.pair)
    ]
    output-print "Bias due to non-instantaneous sampling:"
    foreach n-values nr.species [[x] -> x + 1] [
      [n] -> let sp.param item n species.data
      let name item 0 sp.param
      let snapshot.dens.pair list name precision (occurrences name snapshot.fishes / sampling.area) 3
      set snapshot.estimates lput snapshot.dens.pair snapshot.estimates ; this just stores snapshot densities in a variable
      ifelse last snapshot.dens.pair = 0 [
        let bias.pair list name "n/a"       ; if snapshot.density is 0, do not calculate bias due to non-instantaneous sampling
        set bias.estimates lput bias.pair bias.estimates ; this just stores bias estimates in a variable
        output-print (word first bias.pair ": " last bias.pair)
      ] [
        let bias.pair list name precision ((((last item (n - 1) density.estimates)) - (last snapshot.dens.pair)) / (last snapshot.dens.pair)) 3 ; bias is calculated as (counted density - snapshot density) / snapshot density
        set bias.estimates lput bias.pair bias.estimates ; this just stores bias estimates in a variable
        output-print (word first bias.pair ": " last bias.pair)
      ]
    ]
    set output.real total.density                             ; fill output variables for BehaviourSpace (if more than 1 species, these outputs only apply to the 1st)
    set output.estimated last first density.estimates
    set output.difference output.estimated - output.real
    set output.inaccuracy output.difference / output.real
    set output.instantaneous last first snapshot.estimates
    set output.bias last first bias.estimates
  ]
end ; of do.outputs


;FISH PROCEDURES


;fish movement submodel

to do.fish.movement
  if schooling? [set schoolmates other fishes in-cone perception.dist perception.angle with [species = [species] of myself]]    ; if schooling is true,look for conspecifics in my vicinity

  set acceleration (list 0 0)                                                                                                   ; acceleration at each step is determined entirely by the urges

;check if weights are not 0 (to prevent useless calculations) and calculate all urge vectors

  if patch.gathering.w != 0 [add-urge patch-center-urge patch.gathering.w]                    ; check if weights are not 0 (to prevent useless calculations) and calculate all urge vectors
  if wander.w != 0 [add-urge wander-urge wander.w]
  if predator.avoidance.w != 0 [add-urge avoid-predator-urge predator.avoidance.w]
  if diver.avoidance.w != 0 [add-urge avoid-diver-urge diver.avoidance.w]
  if rest.w != 0 [add-urge rest-urge rest.w]
  if cruise.w != 0 [add-urge cruise-urge cruise.w]

  if count schoolmates > 0 and schooling?                                                        ; if I'm not in a school ignore the school related urges. If schooling is disabled, ignore them as well
  [ add-urge spacing-urge spacing.w
    add-urge center-urge center.w
    add-urge align-urge align.w ]

  let deceleration (scale ((drag.formula * ((magnitude velocity) ^ 2))) (subtract (list 0 0) (normalize velocity)))    ; subtract the deceleration due to drag from the acceleration vector
  set acceleration (add acceleration deceleration)

  if magnitude acceleration > max.acceleration                                                   ; keep the acceleration within the accepted range. It is important that this is done after accounting for drag, so that max.acceleration can be reached.
  [ set acceleration
    (scale
        max.acceleration
        normalize acceleration) ]

  set velocity (add velocity acceleration)                                                       ; the new velocity of the fish is sum of the acceleration and the old velocity.

 ;keep velocity within accepted range. When near threats, use burst.speed as limit, but only if avoidance urge weight is greater than 0

  ifelse ((any? (fishes with [prey.type = "fish"]) in-cone approach.dist perception.angle) and (predator.avoidance.w > 0)) or ((any? divers in-cone approach.dist perception.angle) and (diver.avoidance.w > 0)) [
   if magnitude velocity > burst.speed [
     set velocity (scale burst.speed normalize velocity)
     ]
  ] [

  if magnitude velocity > max.sustained.speed
  [ set velocity
    (scale
        max.sustained.speed
        normalize velocity) ]
  ]

  let nxcor xcor + ( first velocity )
  let nycor ycor + ( last velocity )
  if magnitude velocity > 0.2 [                                ; minimum velocity that triggers movement. This is to prevent erratic movement when stopped.
    facexy nxcor nycor
    fd (magnitude velocity) / movement.time.step]

end ;of do.fish.movement

to add-urge [urge factor]                                              ;fish procedure
  set acceleration add acceleration scale factor normalize urge
end

to pick.patch                                                          ;fish procedure
  if picked.patch = false [
   let chosen.patch patch-ahead 2 ; second patch ahead of the fish

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
    let pairs (map list behavior.list behavior.freqs)                          ; pairs behavior names with probabilities as lists within a list, to work better with the rnd extension: [[b1 0.2] [b2 0.3] [b3 0.5]]
    set current.behavior first rnd:weighted-one-of-list pairs [[p] -> last p]  ; pick a random behavior from behavior list, using a weighted random pick
    let param.pos position current.behavior behavior.list                      ; check which behavior was picked (1, 2, 3 or 4)
    let params item param.pos behavior.params                                  ; retreive list of parameters from the correct position in behavior.params
    set detectability item 2 params
    set schooling? item 3 params
    set schoolmate.dist item 4 params
    set align.w item 5 params
    set center.w item 6 params
    set spacing.w item 7 params
    set wander.w item 8 params
    set rest.w item 9 params
    set cruise.w item 10 params
    set picked.patch.dist item 11 params
    set patch.gathering.w item 12 params
    set predator.avoidance.w item 13 params
    set prey.chasing.w item 14 params
    set diver.avoidance.w item 15 params
    ifelse detectability < 1 [
      set visible? random-bernoulli detectability    ; visibility of individual fish is set when behavior changes, using a Bernoulli trial
      ] [
      set visible? true
      ]
    set behavior.set? true
    if any? schoolmates [
      ask schoolmates [
        set current.behavior [current.behavior] of myself
        set detectability item 2 params
        set schooling? item 3 params
        set schoolmate.dist item 4 params
        set align.w item 5 params
        set center.w item 6 params
        set spacing.w item 7 params
        set wander.w item 8 params
        set rest.w item 9 params
        set cruise.w item 10 params
        set picked.patch.dist item 11 params
        set patch.gathering.w item 12 params
        set predator.avoidance.w item 13 params
        set prey.chasing.w item 14 params
        set diver.avoidance.w item 15 params
        ifelse detectability < 1 [
          set visible? random-bernoulli detectability    ; visibility of schoolmates is set independently
          ] [
          set visible? true
          ]
        set behavior.set? true
      ]
    ]
    ifelse patch.gathering.w > 0 [                                       ; if selected behavior has patch gathering urge, pick a patch. If in a school, ask schoolmates to pick a patch collectively.
      pick.patch] [set picked.patch false]                               ; if patch gathering urge has zero weight, variable picked.patch is set to false (important for pick.patch procedure).
  ]
end

to set.first.behavior                                                    ; fish procedure
    set current.behavior first behavior.list                             ; set behavior to first in the list (this should be the most frequent and/or the most typical for the species)
    let params first behavior.params                                     ; retreive list of parameters from the first position in behavior.params
    set detectability item 2 params
    set schooling? item 3 params
    set schoolmate.dist item 4 params
    set align.w item 5 params
    set center.w item 6 params
    set spacing.w item 7 params
    set wander.w item 8 params
    set rest.w item 9 params
    set cruise.w item 10 params
    set picked.patch.dist item 11 params
    set patch.gathering.w item 12 params
    set predator.avoidance.w item 13 params
    set prey.chasing.w item 14 params
    set diver.avoidance.w item 15 params
    ifelse detectability < 1 [
      set visible? random-bernoulli detectability    ; visibility of individual fish is set when behavior changes, using a Bernoulli trial
      ] [
      set visible? true
      ]
    set behavior.set? true
    ifelse patch.gathering.w > 0 [                                       ; if selected behavior has patch gathering urge, pick a patch. If in a school, ask schoolmates to pick a patch collectively.
      pick.patch] [set picked.patch false]                               ; if patch gathering urge has zero weight, variable picked.patch is set to false (important for pick.patch procedure).
end

; fish urges

to-report patch-center-urge  ; a vector directed at the center of the picked patch
  ifelse picked.patch != false [
    let patch-x [pxcor] of picked.patch
    let patch-y [pycor] of picked.patch
    ifelse distancexy patch-x patch-y > picked.patch.dist   ; distance to picked patch
     [ report (list (patch-x - xcor) (patch-y - ycor)) ]   ; number of coordinate units (vertical and horizontal) needed to get to picked.patch
     [ report (list 0 0) ]
    ] [ report (list 0 0) ]
end


to-report center-urge  ; a vector directed at the average location of my schoolmates

  if count schoolmates = 0 or center.w = 0
  [ report (list 0 0) ]
  report
    (map -
      (list mean [ xcor ] of schoolmates mean [ ycor ] of schoolmates)
      (list xcor ycor)
  )
end

to-report align-urge ; a vector obtained by averaging the velocity of schoolmates
  if count schoolmates = 0 or align.w = 0
  [ report (list 0 0) ]
  report
    (map - (list
      mean [ first velocity ] of schoolmates   ; x component
     mean [ last velocity ] of schoolmates     ; y component
     )
  velocity
  )
end

to-report rest-urge ;a vector opposite to current velocity (counters current movement)
  report subtract [0 0] velocity
end

to-report cruise-urge ; a vector that simply reinforces the current velocity
  report normalize velocity
end

to-report wander-urge ; reports 2 random numbers between -1 and 1, leading to a vector with random direction
  report n-values 2 [ (random-float 2) - 1 ]
end

to-report spacing-urge
  let urge [ 0 0 ]
  ;; report the sum of the distances to fishes
  ;; in my school that are closer to me than
  ;; schoolmate.dist (in body lengths)
  ask schoolmates with [ distance myself < (schoolmate.dist * size) ] [
    set urge
      add
        urge
        (subtract
          (list [xcor] of myself [ycor] of myself)
          (list xcor ycor))
  ]
  report urge
end

to-report avoid-predator-urge ; a normalized vector that is opposite to the position of the closest predator
 let urge (list 0 0)
  if predator.avoidance.w = 0 [ report urge ]
  let predators fishes in-cone approach.dist perception.angle with [species != [species] of myself and prey.type = "fish"]
  if any? predators [ask min-one-of predators [distance myself]
  [set urge normalize (add urge subtract (list [xcor] of myself [ycor] of myself) (list xcor ycor))
    ]]
  report urge
end

to-report avoid-diver-urge ; a normalized vector that is opposite to the position of the closest person
 let urge (list 0 0)
  if diver.avoidance.w = 0 [ report urge ]
  let threats persons in-cone approach.dist perception.angle                        ; persons is an agenteset that includes divers and buddies
  if any? threats [ask min-one-of threats [distance myself]
  [set urge normalize (add urge subtract (list [xcor] of myself [ycor] of myself) (list xcor ycor))
    ]]
  report urge
end

;VECTOR OPERATIONS

to-report add [ v1 v2 ]
  report (map + v1 v2)
end

to-report subtract [ v1 v2 ]
  report (map - v1 v2)
end

to-report scale [ scalar vector ]
  report map [ [v] -> scalar * v ] vector
end

to-report magnitude [ vector ]
  report sqrt sum map [ [v] -> v * v ] vector
end

to-report normalize [ vector ]
  let m magnitude vector
  if m = 0 [ report vector ]
  report map [ [v] -> v / m ] vector
end

;DIVER PROCEDURES

;movement

to do.ddiver.movement           ; fixed distance transect diver movement
  fd speed / movement.time.step ; each step is a second, so the speed is basically the distance divided by the movement.time.step
end

to do.tdiver.movement           ; fixed time transect diver movement
  fd speed / movement.time.step ; each step is a second, so the speed is basically the distance divided by the movement.time.step
end

to do.stdiver.movement          ; stationary point count diver movement
  set heading heading + (stationary.turning.angle / movement.time.step)        ; turns clockwise at a constant speed (in degrees per second)
end

to do.rdiver.movement           ; roving diver movement NOTE: turning happens every two seconds, so this part is coded separately in the go procedure
  fd speed / movement.time.step ; each step is a second, so the speed is basically the distance divided by the movement.time.step
end

;count procedures

to d.count.fishes                      ; for fixed distance transects, there is a limit to the y coordinate (the diver does not count beyond the end mark of the transect)
  let myxcor xcor
  let my.final.ycor final.ycor
  let seen.fishes fishes in-cone max.visibility viewangle
  let eligible.fishes seen.fishes with [(xcor > myxcor - (distance.transect.width / 2)) and (xcor < myxcor + (distance.transect.width / 2)) and (ycor < my.final.ycor)]  ; distance transects have a limit in ycor as well
  let identifiable.fishes eligible.fishes with [id.distance >= distance myself and visible?]      ; only fish that within their id.distance and are visible are counted
  let diver.memory memory
  let new.fishes (identifiable.fishes with [not member? who diver.memory]) ; only fishes that are not remembered are counted
  ifelse count new.fishes > count.saturation [                       ; even if there are more, only the max number of fishes per second is counted (count.saturation)
    let new.records ([species] of min-n-of count.saturation new.fishes [distance myself]) ; priority is given to closest fishes
    set counted.fishes sentence counted.fishes new.records
    set memory sentence memory [who] of new.fishes
  ] [
  if any? new.fishes [                                      ; if there are new fishes, but they do not exceed count.saturation, just count them
    let new.records ([species] of new.fishes)
    set counted.fishes sentence counted.fishes new.records
    set memory sentence memory [who] of new.fishes
  ]]
end

to t.count.fishes
  let myxcor xcor
  let seen.fishes fishes in-cone max.visibility viewangle
  let eligible.fishes seen.fishes with [(xcor >= myxcor - (timed.transect.width / 2)) and (xcor <= myxcor + (timed.transect.width / 2))]  ; this only works for transects heading north, of course
  let identifiable.fishes eligible.fishes with [id.distance >= distance myself and visible?]
  let diver.memory memory
  let new.fishes identifiable.fishes with [not member? who diver.memory] ; only fishes that are not remembered are counted
  ifelse count new.fishes > count.saturation [                       ; even if there are more, only the max number of fishes per second is counted (count.saturation)
    let new.records ([species] of min-n-of count.saturation new.fishes [distance myself]) ; priority is given to closest fishes
    set counted.fishes sentence counted.fishes new.records
    set memory sentence memory [who] of new.fishes
  ] [
  if any? new.fishes [                                      ; if there are new fishes, but they do not exceed count.saturation, just count them
    let new.records ([species] of new.fishes)
    set counted.fishes sentence counted.fishes new.records
    set memory sentence memory [who] of new.fishes
  ]]
end

to s.count.fishes
  let eligible.fishes fishes in-cone stationary.radius viewangle
  let identifiable.fishes eligible.fishes with [id.distance >= distance myself and visible?]
  let diver.memory memory
  let new.fishes identifiable.fishes with [not member? who diver.memory] ; only fishes that are not remembered are counted
  ifelse count new.fishes > count.saturation [                       ; even if there are more, only the max number of fishes per second is counted (count.saturation)
    let new.records ([species] of min-n-of count.saturation new.fishes [distance myself]) ; priority is given to closest fishes
    set counted.fishes sentence counted.fishes new.records
    set memory sentence memory [who] of new.fishes
  ] [
  if any? new.fishes [                                      ; if there are new fishes, but they do not exceed count.saturation, just count them
    let new.records ([species] of new.fishes)
    set counted.fishes sentence counted.fishes new.records
    set memory sentence memory [who] of new.fishes
  ]]
end

to r.count.fishes
  let eligible.fishes fishes in-cone max.visibility viewangle
  let identifiable.fishes eligible.fishes with [id.distance >= distance myself and visible?]
  let diver.memory memory
  let new.fishes identifiable.fishes with [not member? who diver.memory] ; only fishes that were not previously counted are counted
  ifelse count new.fishes > count.saturation [                       ; even if there are more, only the max number of fishes per second is counted (count.saturation)
    let new.records ([species] of min-n-of count.saturation new.fishes [distance myself]) ; priority is given to closest fishes
    set counted.fishes sentence counted.fishes new.records
    set memory sentence memory [who] of new.fishes
  ] [
  if any? new.fishes [                                      ; if there are new fishes, but they do not exceed count.saturation, just count them
    let new.records ([species] of new.fishes)
    set counted.fishes sentence counted.fishes new.records
    set memory sentence memory [who] of new.fishes
  ]]
end

;forget fishes

to forget.fishes                                                        ; runs if super memory is off
  let seen.fish.id [who] of fishes in-cone max.visibility viewangle
  let diver.memory memory
  set memory filter [[id] -> member? id seen.fish.id] diver.memory
end


; USEFUL REPORTERS

to-report random-bernoulli [probability-true]
  if probability-true < 0.0 or probability-true > 1.0 [user-message "WARNING: probability outside 0-1 range in random-bernoulli."]
  report random-float 1.0 < probability-true
end

to-report random-float-between [a b]           ; generate a random float between two numbers
  report random-float a + (b - a)
end

to-report occurrences [x the-list]             ; count the number of occurrences of an item in a list (useful for summarizing species lists)
  report reduce
    [[occurrence-count next-item] -> ifelse-value (next-item = x) [occurrence-count + 1] [occurrence-count]] (fput 0 the-list)
end
@#$#@#$#@
GRAPHICS-WINDOW
415
10
1023
2419
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
10.0

BUTTON
135
160
220
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
220
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
8.0
1
1
m/min
HORIZONTAL

TEXTBOX
1035
40
1190
58
Fixed time transect parameters
11
0.0
1

TEXTBOX
1035
290
1235
316
Stationary point count parameters
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
4.0
1
1
º / sec
HORIZONTAL

SLIDER
1270
180
1485
213
max.visibility
max.visibility
2
40
7.0
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
3.0
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
510
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
8.0
1
1
m/min
HORIZONTAL

TEXTBOX
1035
415
1255
441
Random path parameters
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
20.0
1
1
º / 2 secs
HORIZONTAL

TEXTBOX
20
560
405
578
SELECT A SAMPLING METHOD________________________________________
11
0.0
1

SWITCH
200
315
410
348
show.paths?
show.paths?
1
1
-1000

SWITCH
15
315
200
348
show.diver.detail.window?
show.diver.detail.window?
1
1
-1000

TEXTBOX
20
290
400
308
DISPLAY OPTIONS________________________________________________
11
0.0
1

TEXTBOX
1270
10
1505
51
Ability to remember all counted fishes (If turned off, divers forget fishes that leave the FOV). Applies to all divers. Default is off.
11
0.0
1

BUTTON
15
515
410
550
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
15
680
295
713
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

SLIDER
1035
125
1250
158
transect.time
transect.time
1
90
1.0
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
15.0
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
5.0
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
2.0
1
1
minutes
HORIZONTAL

TEXTBOX
1035
165
1235
191
Fixed distance transect parameters
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
10.0
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
235
1485
268
behavior.change.interval
behavior.change.interval
1
20
10.0
1
1
seconds
HORIZONTAL

TEXTBOX
1270
220
1470
238
Time between behavior changes in fish.
11
0.0
1

BUTTON
155
630
295
675
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
65
25
115
66
NIL
minutes
0
1
10

MONITOR
15
25
65
66
NIL
hours
0
1
10

MONITOR
115
25
165
66
NIL
seconds
0
1
10

SWITCH
15
350
200
383
smooth.animation?
smooth.animation?
0
1
-1000

TEXTBOX
170
30
200
60
Model clock
11
0.0
1

TEXTBOX
205
355
335
381
Disable smooth animation for faster model runs.
11
0.0
1

TEXTBOX
20
130
135
171
1. Insert csv file name (without extension)
11
15.0
1

TEXTBOX
135
130
220
160
2. Establish initial conditions
11
15.0
1

TEXTBOX
235
135
330
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
SAMPLING METHODS__________________
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
2.0
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
2.0
1
1
m
HORIZONTAL

TEXTBOX
85
715
235
741
(Stop following divers or fish)
11
0.0
1

TEXTBOX
1270
160
1510
178
Water visibility (Affects the divers' field of view)
11
0.0
1

CHOOSER
1275
515
1490
560
movement.time.step
movement.time.step
5 10
1

TEXTBOX
1275
455
1510
511
Number of decisions per second in the fish movement model. This must match the frame rate in model settings for normal speed to match real time.
11
0.0
1

TEXTBOX
1275
565
1515
621
Please test behaviors in the species creator with the same number of decisions per second. Different values can lead to very different behaviors with the same parameters.
11
15.0
1

SWITCH
230
225
385
258
fixed.seed?
fixed.seed?
1
1
-1000

INPUTBOX
150
225
230
285
seed
123456.0
1
0
Number

TEXTBOX
235
260
390
290
Select a fixed seed to generate similar consecutive model runs.
11
0.0
1

CHOOSER
15
580
295
625
sampling.method
sampling.method
"Fixed time transect" "Fixed distance transect" "Stationary point count" "Random path"
1

BUTTON
15
630
155
675
Follow diver
follow one-of divers with [breed != buddies]
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
300
585
400
618
buddy?
buddy?
1
1
-1000

TEXTBOX
305
620
395
675
Buddy diver only assists main diver, but may affect fish responses.
11
0.0
1

TEXTBOX
1299
538
1434
564
decisions per second
11
0.0
1

INPUTBOX
210
10
310
70
override.density
0.0
1
0
Number

TEXTBOX
320
20
400
65
Set to 0 to use default density from file.
11
0.0
1

SLIDER
1270
90
1485
123
count.saturation
count.saturation
1
10
3.0
1
1
fish per second
HORIZONTAL

TEXTBOX
1295
125
1460
143
(closest fishes are counted first)
11
0.0
1

TEXTBOX
1040
550
1240
590
Scroll down to see the full map\n V
15
15.0
1

CHOOSER
15
220
135
265
file.delimiter
file.delimiter
"," ";"
0

INPUTBOX
15
160
135
220
file.name
example
1
0
String

SLIDER
1270
305
1490
338
transect.viewangle
transect.viewangle
10
360
180.0
10
1
degrees
HORIZONTAL

SLIDER
1270
340
1490
373
stationary.viewangle
stationary.viewangle
10
360
160.0
10
1
degrees
HORIZONTAL

SLIDER
1270
375
1490
408
rpath.viewangle
rpath.viewangle
10
360
160.0
10
1
degrees
HORIZONTAL

TEXTBOX
1275
290
1425
308
Diver view angles per method
11
0.0
1

BUTTON
1495
305
1550
410
defaults
set transect.viewangle 180\nset stationary.viewangle 160\nset rpath.viewangle 160\nset initial.time.to.stabilize.movement 20
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
1490
235
1550
268
default
set behavior.change.interval 10
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
1490
90
1550
123
default
set count.saturation 3
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
1270
415
1550
448
initial.time.to.stabilize.movement
initial.time.to.stabilize.movement
5
50
20.0
5
1
seconds
HORIZONTAL

@#$#@#$#@
## Context

Underwater visual census (UVC) methods are used worldwide to monitor shallow marine and freshwater habitats and support management and conservation decisions. However, several sources of bias still undermine the ability of these methods to accurately estimate abundances of some species.

## FishCensus Model

FishCensus is an agent-based model that simulates underwater visual census of fish populations, a method used worldwide to survey shallow marine and freshwater habitats that involves a diver counting fish species to estimate their density. It can help estimate sampling bias, apply correction factors to field surveys and decide on the best method to survey a particular species, given its behavioural traits, detectability or speed.
A modified vector-based boids-like movement submodel is used for fish, and complex behaviours such as schooling or diver avoidance / attraction can be represented.

## How it works

The FishCensus model comes with two separate programs. The Species Creator is used to create new fish species or observe/edit existing ones. Species parameters can be exported as a .csv file and imported into the main model where the simulation happens.

In the main FishCensus program, a virtual diver uses a pre-selected survey method (*e.g.* fixed distance or timed transect, stationary point counts) to count the fish and estimate their density. The true density of fish is pre-determined and known, which allows for the quantification of bias, a measure that is unknown in the field, where determining the true abundance is very difficult.

For more information and tutorials visit the [wiki at Bitbucket](https://bitbucket.org/MiguelPPais/fishcensus/wiki/Home) and the [openABM](https://www.openabm.org/model/530) page.

## Related models

### Reefex model

Watson, R.A., Carlos, G.M., & Samoilys, M.A., 1995. Bias introduced by the non-random movement of fish in visual transect surveys. Ecological Modelling 77(2–3), 205–214. http://doi.org/10.1016/0304-3800(93)E0085-H

###  AnimDens model

#### Origial publication of the model and R code

Ward-Paige, C.A., Flemming, J.M., Lotze, H.K., 2010. Overestimating fish counts by non-instantaneous visual censuses: Consequences for population and community descriptions. PLoS One 5, e11722. http://doi.org/10.1371/journal.pone.0011722

#### Replication of the AnimDens model in NetLogo with some added features:

Pais, M.P., Ward-Paige, C.A. (2015). AnimDens NetLogo model. http://modelingcommons.org/browse/one_model/4408

### Vector-based swarming by Uri Wilensky, in the NetLogo 3D library:

Wilensky, U. (2005).  NetLogo Flocking 3D Alternate model.  http://ccl.northwestern.edu/netlogo/models/Flocking3DAlternate.  Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

## Credits and references

If you use this model, please cite the original publication:

Pais, M.P., Cabral, H.N. 2017. Fish behaviour effects on the accuracy and precision of underwater visual census surveys. A virtual ecologist approach using an individual-based model. Ecological Modelling, in press.

To cite the model itself, please visit the [OpenABM page](https://www.openabm.org/model/5305) for citation instructions.


## Acknowledgments

I thank everyone who tested the model and interface and helped find bugs, Christine Ward-Paige for clarifications and suggestions about the AnimDens model, Uri Wilenksy for NetLogo and the base code for vector-based swarming, Kenneth Rose for valuable feedback and suggestions and J.P. Rosa for revising the calculation of drag forces. This study had the support of Fundação para a Ciência e Tecnologia (FCT), through the strategic project UID/MAR/04292/2013 granted to MARE and the grant awarded to Miguel P. Pais (SFRH/BPD/94638/2013).

## Contact the author

If you want to report bugs, suggest features, share work you did with the model, or even insult me, please send me an email: [mppais@fc.ul.pt](mailto:mppais@fc.ul.pt)

## COPYRIGHT AND LICENSE

Copyright 2016 Miguel Pessanha Pais

![CC BY-NC-SA 4.0](http://ccl.northwestern.edu/images/creativecommons/byncsa.png)

This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.  To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-sa/4.0/

### Disclaimer

THIS MODEL IS PROVIDED "AS IS" AND ANY EXPRESSED OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR (MIGUEL PAIS), THE MARINE AND ENVIRONMENTAL SCIENCES CENTRE (MARE), THE UNIVERSITY OF LISBON, OR ANY OF THEIR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS MODEL, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Without limiting the foregoing, the author of the model makes no warranty that:

1. the model will meet your requirements.
2. the model will be uninterrupted, timely, secure or error-free.
3. the results that may be obtained from the use of the model will be effective, accurate or reliable.
4. the quality of the model will meet your expectations.
5. any errors in the model obtained from the OpenABM web site will be corrected.

The model and its documentation made available on the OpenABM web site:

1. could include technical or other mistakes, inaccuracies or typographical errors.
2. model contributors may make changes to the software or documentation made available on the web site.
3. may be out of date and the model contributors or MARE make no commitment to update such materials.
4. The author, contributors, and MARE assume no responsibility for errors or ommissions in the model or documentation available from the OpenABM web site.

In no event shall the author, model contributors or MARE be liable to you or any third parties for any special, punitive, incidental, indirect or consequential damages of any kind, or any damages whatsoever, including, without limitation, those resulting from loss of use, data or profits, whether or not the author, model contributors, or MARE have been advised of the possibility of such damages, and on any theory of liability, arising out of or in connection with the use of this model.

The use of the model downloaded through the OpenABM site is done at your own discretion and risk and with agreement that you will be solely responsible for any damage to your computer system or loss of data that results from such activities. No advice or information, whether oral or written, obtained by you from OpenABM, the author, model contributors, or MARE shall create any warranty for the model.
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
NetLogo 6.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="3% monthly decline from 0.5, seasonal sampling, stationary" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>real</metric>
    <metric>estimated</metric>
    <metric>difference</metric>
    <metric>instantaneous</metric>
    <metric>bias</metric>
    <enumeratedValueSet variable="sampling.method">
      <value value="&quot;Stationary point count&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="smooth.animation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="super.memory?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stationary.time">
      <value value="2"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show.paths?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement.time.step">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="override.density">
      <value value="0.5"/>
      <value value="0.46"/>
      <value value="0.42"/>
      <value value="0.38"/>
      <value value="0.35"/>
      <value value="0.32"/>
      <value value="0.29"/>
      <value value="0.26"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stationary.radius">
      <value value="2.5"/>
      <value value="3.6"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fixed.seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stationary.turning.angle">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max.visibility">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="behavior.change.interval">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="buddy?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="count.saturation">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="3% monthly decline from 0.5, seasonal sampling, transect" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>real</metric>
    <metric>estimated</metric>
    <metric>difference</metric>
    <metric>instantaneous</metric>
    <metric>bias</metric>
    <enumeratedValueSet variable="sampling.method">
      <value value="&quot;Fixed distance transect&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="smooth.animation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="super.memory?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transect.distance">
      <value value="10"/>
      <value value="20"/>
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show.paths?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement.time.step">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance.transect.diver.speed">
      <value value="4"/>
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="override.density">
      <value value="0.5"/>
      <value value="0.46"/>
      <value value="0.42"/>
      <value value="0.38"/>
      <value value="0.35"/>
      <value value="0.32"/>
      <value value="0.29"/>
      <value value="0.26"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fixed.seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance.transect.width">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max.visibility">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="behavior.change.interval">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="buddy?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="count.saturation">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="3% monthly decline from 0.1, seasonal sampling, stationary" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>real</metric>
    <metric>estimated</metric>
    <metric>difference</metric>
    <metric>instantaneous</metric>
    <metric>bias</metric>
    <enumeratedValueSet variable="sampling.method">
      <value value="&quot;Stationary point count&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="smooth.animation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="super.memory?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stationary.time">
      <value value="2"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show.paths?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement.time.step">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="override.density">
      <value value="0.1"/>
      <value value="0.091"/>
      <value value="0.083"/>
      <value value="0.076"/>
      <value value="0.069"/>
      <value value="0.063"/>
      <value value="0.058"/>
      <value value="0.053"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stationary.radius">
      <value value="2.5"/>
      <value value="3.6"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fixed.seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stationary.turning.angle">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max.visibility">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="behavior.change.interval">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="buddy?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="count.saturation">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="3% monthly decline from 0.1, seasonal sampling, transect" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>real</metric>
    <metric>estimated</metric>
    <metric>difference</metric>
    <metric>instantaneous</metric>
    <metric>bias</metric>
    <enumeratedValueSet variable="sampling.method">
      <value value="&quot;Fixed distance transect&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="smooth.animation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="super.memory?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transect.distance">
      <value value="10"/>
      <value value="20"/>
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show.paths?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement.time.step">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance.transect.diver.speed">
      <value value="4"/>
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="override.density">
      <value value="0.1"/>
      <value value="0.091"/>
      <value value="0.083"/>
      <value value="0.076"/>
      <value value="0.069"/>
      <value value="0.063"/>
      <value value="0.058"/>
      <value value="0.053"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fixed.seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance.transect.width">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max.visibility">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="behavior.change.interval">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="buddy?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="count.saturation">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="5% monthly decline from 0.5, seasonal sampling, stationary" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>real</metric>
    <metric>estimated</metric>
    <metric>difference</metric>
    <metric>instantaneous</metric>
    <metric>bias</metric>
    <enumeratedValueSet variable="sampling.method">
      <value value="&quot;Stationary point count&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="smooth.animation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="super.memory?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stationary.time">
      <value value="2"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show.paths?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement.time.step">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="override.density">
      <value value="0.5"/>
      <value value="0.43"/>
      <value value="0.37"/>
      <value value="0.32"/>
      <value value="0.27"/>
      <value value="0.23"/>
      <value value="0.2"/>
      <value value="0.17"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stationary.radius">
      <value value="2.5"/>
      <value value="3.6"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fixed.seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stationary.turning.angle">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max.visibility">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="behavior.change.interval">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="buddy?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="count.saturation">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="5% monthly decline from 0.5, seasonal sampling, transect" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>real</metric>
    <metric>estimated</metric>
    <metric>difference</metric>
    <metric>instantaneous</metric>
    <metric>bias</metric>
    <enumeratedValueSet variable="sampling.method">
      <value value="&quot;Fixed distance transect&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="smooth.animation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="super.memory?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transect.distance">
      <value value="10"/>
      <value value="20"/>
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show.paths?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement.time.step">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance.transect.diver.speed">
      <value value="4"/>
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="override.density">
      <value value="0.5"/>
      <value value="0.43"/>
      <value value="0.37"/>
      <value value="0.32"/>
      <value value="0.27"/>
      <value value="0.23"/>
      <value value="0.2"/>
      <value value="0.17"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fixed.seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance.transect.width">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max.visibility">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="behavior.change.interval">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="buddy?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="count.saturation">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="5% monthly decline from 0.1, seasonal sampling, stationary" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>real</metric>
    <metric>estimated</metric>
    <metric>difference</metric>
    <metric>instantaneous</metric>
    <metric>bias</metric>
    <enumeratedValueSet variable="sampling.method">
      <value value="&quot;Stationary point count&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="smooth.animation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="super.memory?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stationary.time">
      <value value="2"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show.paths?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement.time.step">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="override.density">
      <value value="0.1"/>
      <value value="0.086"/>
      <value value="0.074"/>
      <value value="0.063"/>
      <value value="0.054"/>
      <value value="0.046"/>
      <value value="0.04"/>
      <value value="0.034"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stationary.radius">
      <value value="2.5"/>
      <value value="3.6"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fixed.seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stationary.turning.angle">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max.visibility">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="behavior.change.interval">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="buddy?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="count.saturation">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="5% monthly decline from 0.1, seasonal sampling, transect" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>real</metric>
    <metric>estimated</metric>
    <metric>difference</metric>
    <metric>instantaneous</metric>
    <metric>bias</metric>
    <enumeratedValueSet variable="sampling.method">
      <value value="&quot;Fixed distance transect&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="smooth.animation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="super.memory?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transect.distance">
      <value value="10"/>
      <value value="20"/>
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show.paths?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement.time.step">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance.transect.diver.speed">
      <value value="4"/>
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="override.density">
      <value value="0.1"/>
      <value value="0.086"/>
      <value value="0.074"/>
      <value value="0.063"/>
      <value value="0.054"/>
      <value value="0.046"/>
      <value value="0.04"/>
      <value value="0.034"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fixed.seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance.transect.width">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max.visibility">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="behavior.change.interval">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="buddy?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="count.saturation">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Precision and accuracy experiment, transect 0.05" repetitions="30" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>real</metric>
    <metric>estimated</metric>
    <metric>difference</metric>
    <metric>instantaneous</metric>
    <metric>bias</metric>
    <enumeratedValueSet variable="sampling.method">
      <value value="&quot;Fixed distance transect&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transect.distance">
      <value value="10"/>
      <value value="20"/>
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance.transect.width">
      <value value="2"/>
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance.transect.diver.speed">
      <value value="4"/>
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max.visibility">
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="override.density">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="smooth.animation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="super.memory?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show.paths?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement.time.step">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fixed.seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="behavior.change.interval">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="buddy?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="count.saturation">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Precision and accuracy experiment, transect 0.1" repetitions="30" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>real</metric>
    <metric>estimated</metric>
    <metric>difference</metric>
    <metric>instantaneous</metric>
    <metric>bias</metric>
    <enumeratedValueSet variable="sampling.method">
      <value value="&quot;Fixed distance transect&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transect.distance">
      <value value="10"/>
      <value value="20"/>
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance.transect.width">
      <value value="2"/>
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance.transect.diver.speed">
      <value value="4"/>
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max.visibility">
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="override.density">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="smooth.animation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="super.memory?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show.paths?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement.time.step">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fixed.seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="behavior.change.interval">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="buddy?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="count.saturation">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Precision and accuracy experiment, transect 0.2" repetitions="30" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>real</metric>
    <metric>estimated</metric>
    <metric>difference</metric>
    <metric>instantaneous</metric>
    <metric>bias</metric>
    <enumeratedValueSet variable="sampling.method">
      <value value="&quot;Fixed distance transect&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transect.distance">
      <value value="10"/>
      <value value="20"/>
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance.transect.width">
      <value value="2"/>
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance.transect.diver.speed">
      <value value="4"/>
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max.visibility">
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="override.density">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="smooth.animation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="super.memory?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show.paths?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement.time.step">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fixed.seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="behavior.change.interval">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="buddy?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="count.saturation">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Precision and accuracy experiment, stationary 0.05" repetitions="30" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>real</metric>
    <metric>estimated</metric>
    <metric>difference</metric>
    <metric>instantaneous</metric>
    <metric>bias</metric>
    <enumeratedValueSet variable="sampling.method">
      <value value="&quot;Stationary point count&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stationary.radius">
      <value value="2.5"/>
      <value value="3.6"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stationary.turning.angle">
      <value value="4"/>
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stationary.time">
      <value value="3"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max.visibility">
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="override.density">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="smooth.animation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="super.memory?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show.paths?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement.time.step">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fixed.seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="behavior.change.interval">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="buddy?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="count.saturation">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Precision and accuracy experiment, stationary 0.1" repetitions="30" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>real</metric>
    <metric>estimated</metric>
    <metric>difference</metric>
    <metric>instantaneous</metric>
    <metric>bias</metric>
    <enumeratedValueSet variable="sampling.method">
      <value value="&quot;Stationary point count&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stationary.radius">
      <value value="2.5"/>
      <value value="3.6"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stationary.turning.angle">
      <value value="4"/>
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stationary.time">
      <value value="3"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max.visibility">
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="override.density">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="smooth.animation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="super.memory?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show.paths?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement.time.step">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fixed.seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="behavior.change.interval">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="buddy?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="count.saturation">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="effect of behavior, stationary 0.2" repetitions="30" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>output.real</metric>
    <metric>output.estimated</metric>
    <metric>output.difference</metric>
    <metric>output.instantaneous</metric>
    <metric>output.bias</metric>
    <enumeratedValueSet variable="file.name">
      <value value="&quot;schooling&quot;"/>
      <value value="&quot;no schooling&quot;"/>
      <value value="&quot;large shy&quot;"/>
      <value value="&quot;large bold&quot;"/>
      <value value="&quot;large indifferent&quot;"/>
      <value value="&quot;cryptic&quot;"/>
      <value value="&quot;not cryptic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sampling.method">
      <value value="&quot;Stationary point count&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stationary.radius">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stationary.turning.angle">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stationary.time">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max.visibility">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="override.density">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="smooth.animation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="super.memory?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show.paths?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement.time.step">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fixed.seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="behavior.change.interval">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="buddy?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="count.saturation">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="effect of behavior, stationary 0.1" repetitions="30" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>output.real</metric>
    <metric>output.estimated</metric>
    <metric>output.difference</metric>
    <metric>output.instantaneous</metric>
    <metric>output.bias</metric>
    <enumeratedValueSet variable="file.name">
      <value value="&quot;schooling&quot;"/>
      <value value="&quot;no schooling&quot;"/>
      <value value="&quot;large shy&quot;"/>
      <value value="&quot;large bold&quot;"/>
      <value value="&quot;large indifferent&quot;"/>
      <value value="&quot;cryptic&quot;"/>
      <value value="&quot;not cryptic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sampling.method">
      <value value="&quot;Stationary point count&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stationary.radius">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stationary.turning.angle">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stationary.time">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max.visibility">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="override.density">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="smooth.animation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="super.memory?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show.paths?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement.time.step">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fixed.seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="behavior.change.interval">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="buddy?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="count.saturation">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="effect of behavior, stationary 0.05" repetitions="30" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>output.real</metric>
    <metric>output.estimated</metric>
    <metric>output.difference</metric>
    <metric>output.instantaneous</metric>
    <metric>output.bias</metric>
    <enumeratedValueSet variable="file.name">
      <value value="&quot;schooling&quot;"/>
      <value value="&quot;no schooling&quot;"/>
      <value value="&quot;large shy&quot;"/>
      <value value="&quot;large bold&quot;"/>
      <value value="&quot;large indifferent&quot;"/>
      <value value="&quot;cryptic&quot;"/>
      <value value="&quot;not cryptic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sampling.method">
      <value value="&quot;Stationary point count&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stationary.radius">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stationary.turning.angle">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stationary.time">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max.visibility">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="override.density">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="smooth.animation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="super.memory?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show.paths?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement.time.step">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fixed.seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="behavior.change.interval">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="buddy?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="count.saturation">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="effect of behavior, transect 0.2" repetitions="30" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>output.real</metric>
    <metric>output.estimated</metric>
    <metric>output.difference</metric>
    <metric>output.instantaneous</metric>
    <metric>output.bias</metric>
    <enumeratedValueSet variable="file.name">
      <value value="&quot;schooling&quot;"/>
      <value value="&quot;no schooling&quot;"/>
      <value value="&quot;large shy&quot;"/>
      <value value="&quot;large bold&quot;"/>
      <value value="&quot;large indifferent&quot;"/>
      <value value="&quot;cryptic&quot;"/>
      <value value="&quot;not cryptic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sampling.method">
      <value value="&quot;Fixed distance transect&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transect.distance">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance.transect.diver.speed">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance.transect.width">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max.visibility">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="override.density">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="smooth.animation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="super.memory?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show.paths?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement.time.step">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fixed.seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="behavior.change.interval">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="buddy?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="count.saturation">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="effect of behavior, transect 0.1" repetitions="30" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>output.real</metric>
    <metric>output.estimated</metric>
    <metric>output.difference</metric>
    <metric>output.instantaneous</metric>
    <metric>output.bias</metric>
    <enumeratedValueSet variable="file.name">
      <value value="&quot;schooling&quot;"/>
      <value value="&quot;no schooling&quot;"/>
      <value value="&quot;large shy&quot;"/>
      <value value="&quot;large bold&quot;"/>
      <value value="&quot;large indifferent&quot;"/>
      <value value="&quot;cryptic&quot;"/>
      <value value="&quot;not cryptic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sampling.method">
      <value value="&quot;Fixed distance transect&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transect.distance">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance.transect.diver.speed">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance.transect.width">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max.visibility">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="override.density">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="smooth.animation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="super.memory?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show.paths?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement.time.step">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fixed.seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="behavior.change.interval">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="buddy?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="count.saturation">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="effect of behavior, transect 0.05" repetitions="30" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>output.real</metric>
    <metric>output.estimated</metric>
    <metric>output.difference</metric>
    <metric>output.instantaneous</metric>
    <metric>output.bias</metric>
    <enumeratedValueSet variable="file.name">
      <value value="&quot;schooling&quot;"/>
      <value value="&quot;no schooling&quot;"/>
      <value value="&quot;large shy&quot;"/>
      <value value="&quot;large bold&quot;"/>
      <value value="&quot;large indifferent&quot;"/>
      <value value="&quot;cryptic&quot;"/>
      <value value="&quot;not cryptic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sampling.method">
      <value value="&quot;Fixed distance transect&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transect.distance">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance.transect.diver.speed">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance.transect.width">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max.visibility">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="override.density">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="smooth.animation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="super.memory?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show.paths?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement.time.step">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fixed.seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="behavior.change.interval">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="buddy?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="count.saturation">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="everything transect" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>output.real</metric>
    <metric>output.estimated</metric>
    <metric>output.difference</metric>
    <metric>output.instantaneous</metric>
    <metric>output.bias</metric>
    <enumeratedValueSet variable="file.name">
      <value value="&quot;schooling&quot;"/>
      <value value="&quot;cryptic 2&quot;"/>
      <value value="&quot;cryptic 3&quot;"/>
      <value value="&quot;large bold&quot;"/>
      <value value="&quot;large shy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sampling.method">
      <value value="&quot;Fixed distance transect&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transect.distance">
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
      <value value="40"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance.transect.width">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance.transect.diver.speed">
      <value value="2"/>
      <value value="4"/>
      <value value="6"/>
      <value value="8"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max.visibility">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="override.density">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="smooth.animation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="super.memory?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show.paths?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement.time.step">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fixed.seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="behavior.change.interval">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="buddy?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="count.saturation">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="everything stationary" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>output.real</metric>
    <metric>output.estimated</metric>
    <metric>output.difference</metric>
    <metric>output.instantaneous</metric>
    <metric>output.bias</metric>
    <enumeratedValueSet variable="file.name">
      <value value="&quot;schooling&quot;"/>
      <value value="&quot;cryptic 2&quot;"/>
      <value value="&quot;cryptic 3&quot;"/>
      <value value="&quot;large bold&quot;"/>
      <value value="&quot;large shy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sampling.method">
      <value value="&quot;Stationary point count&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stationary.turning.angle">
      <value value="2"/>
      <value value="4"/>
      <value value="6"/>
      <value value="8"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stationary.radius">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stationary.time">
      <value value="3"/>
      <value value="5"/>
      <value value="7"/>
      <value value="9"/>
      <value value="11"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max.visibility">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="override.density">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="smooth.animation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="super.memory?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show.paths?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement.time.step">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fixed.seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="behavior.change.interval">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="buddy?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="count.saturation">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="effect of behavior transect" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>output.real</metric>
    <metric>output.estimated</metric>
    <metric>output.difference</metric>
    <metric>output.instantaneous</metric>
    <metric>output.bias</metric>
    <enumeratedValueSet variable="file.name">
      <value value="&quot;schooling&quot;"/>
      <value value="&quot;no schooling&quot;"/>
      <value value="&quot;large shy&quot;"/>
      <value value="&quot;large bold&quot;"/>
      <value value="&quot;large indifferent&quot;"/>
      <value value="&quot;cryptic 2&quot;"/>
      <value value="&quot;not cryptic&quot;"/>
      <value value="&quot;cryptic 3&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sampling.method">
      <value value="&quot;Fixed distance transect&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transect.distance">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance.transect.diver.speed">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance.transect.width">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max.visibility">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="override.density">
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="smooth.animation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="super.memory?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show.paths?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement.time.step">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fixed.seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="behavior.change.interval">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="buddy?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="count.saturation">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="effect of behavior stationary" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>output.real</metric>
    <metric>output.estimated</metric>
    <metric>output.difference</metric>
    <metric>output.instantaneous</metric>
    <metric>output.bias</metric>
    <enumeratedValueSet variable="file.name">
      <value value="&quot;schooling&quot;"/>
      <value value="&quot;no schooling&quot;"/>
      <value value="&quot;large shy&quot;"/>
      <value value="&quot;large bold&quot;"/>
      <value value="&quot;large indifferent&quot;"/>
      <value value="&quot;cryptic 2&quot;"/>
      <value value="&quot;cryptic 3&quot;"/>
      <value value="&quot;not cryptic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sampling.method">
      <value value="&quot;Stationary point count&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stationary.radius">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stationary.turning.angle">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stationary.time">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max.visibility">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="override.density">
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="smooth.animation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="super.memory?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show.paths?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement.time.step">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fixed.seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="behavior.change.interval">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="buddy?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="count.saturation">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="sensitivity" repetitions="15" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>output.estimated</metric>
    <enumeratedValueSet variable="smooth.animation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement.time.step">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="file.name">
      <value value="&quot;schoolmate minus20&quot;"/>
      <value value="&quot;schoolmate plus20&quot;"/>
      <value value="&quot;detectability minus0.1&quot;"/>
      <value value="&quot;detectability plus0.1&quot;"/>
      <value value="&quot;speed minus0.1&quot;"/>
      <value value="&quot;speed plus0.1&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sampling.method">
      <value value="&quot;Fixed distance transect&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance.transect.diver.speed">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fixed.seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="file.delimiter">
      <value value="&quot;,&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transect.distance">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="behavior.change.interval">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="buddy?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max.visibility">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show.diver.detail.window?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="override.density">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="count.saturation">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance.transect.width">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show.paths?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="super.memory?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
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
