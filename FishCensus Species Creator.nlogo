extensions [
 csv 
]

breed [ fishes fish ]
breed [ predators predator ]
breed [ divers diver ]

globals [
sp.param
b1.sp.param
b2.sp.param
b3.sp.param
b4.sp.param
loaded.behavior
]

predators-own [
 satiety             ; when =100, the predator is full and does not seek prey. Decreases with time.
]
  
fishes-own [
  schoolmates        ; agentset of nearby fishes
  velocity           ; vector with x and y components determined by the previous velocity and the current acceleration at each step
  acceleration       ; vector with x and y components, determined by the sum of all urges
  picked.patch
  drag.formula
]

patches-own [

]


;; Setup Procedures

to estimate-speeds   ; from caudal fin aspect ratio and fish size, acording to Sambilay, Jr. (1990)
let size.cm fish.size * 100
let rel.speed 10 ^ ((0.616 - (0.3804 * log size.cm 10)) + (0.3478 * (log aspect.ratio 10)))
let rel.burst 10 ^ ((0.616 - (0.3804 * log size.cm 10)) + (0.3478 * (log aspect.ratio 10)) + 0.7621)
set max.sustained.speed precision (fish.size * rel.speed) 1
set burst.speed precision (fish.size * rel.burst) 1
end

to setup
  output-print "Clearing display and resetting time..."
  clear-ticks                                          ; clear everything except global variables
  clear-turtles
  clear-patches
  clear-drawing
  clear-all-plots
  stop-inspecting-dead-agents                         ; close diver detail windows
  output-print "Drawing display grid..."
  ask patches with [pycor mod 2 = 0 and pxcor mod 2 = 0] [set pcolor 103]
  ask patches with [pycor mod 2 = 1 and pxcor mod 2 = 1] [set pcolor 103]
  ask patches with [pcolor = black] [set pcolor 105]
  output-print "Placing fishes..." 
  create-fishes (fish.density * (world-width * world-height))
    [
      setxy random-xcor random-ycor
      set size fish.size
      set shape "fish top"
      set velocity [ 0 0 ]
      set acceleration [ 0 0 ]
      set color fish.color
      set drag.formula (-11297 / (20600 * (size ^ 0.8571)))
      set schoolmates no-turtles                                  ;sets schoolmates as an empty agentset (otherwise it would be set to 0)
      set picked.patch false
    ]
  reset-ticks
end

;;
;; Runtime Procedures
;;
to go
  repeat movement.time.step [                             ; variable movement.time.step in the interface should match the frame rate in settings
    
    ask fishes [
      do.fish.movement
    ]
    ask predators [                                    ; simple movement algorithm for predators
      ifelse satiety < 1 [
        set heading heading + random-float-between -10 10
        fd 0.1 * predator.normal.speed
        let prey fishes in-cone predator.vision.distance predator.vision.angle
        if any? prey [
        turn-at-most (subtract-headings towards one-of prey heading) predator.max.turn
        fd 0.1 * predator.burst.speed
        if any? prey in-cone 0.1 30 [ask one-of prey in-cone 0.1 30 [
        ask myself [set satiety 100] die]
      ]]]
      [set heading heading + random-float-between -5 5
        fd 0.1 * 0.5 * predator.normal.speed]
    ]
  ask divers [fd 8 / (60 * movement.time.step)]
  if smooth.animation?  [display]
  ]
  ask predators [if satiety > 0 [set satiety satiety - 5]]
  tick
end

to turn-at-most [turn max-turn]                      ; simple turning algorithm for predators
  ifelse abs turn > max-turn
    [ ifelse turn > 0
        [ rt max-turn ]
        [ lt max-turn ] ]
    [ rt turn ]
end

to pick.patch
  ask fishes with [picked.patch = false] [
   let chosen.patch patch-ahead 2 ; one-of patches in-cone 4 perception.angle
   
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

to deploy-diver
  output-print "Diver created at the bottom"
  output-print "of the display."
  create-divers 1 [
   set shape "diver"
   set size 1.7
   set color green
   set heading 0
   setxy (world-width / 2) 0 
  ]
end

to deploy-predator
  output-print "Placing 1 predator..."
  create-predators 1 [
    set shape "fish top"
    set size predator.size
    set color red
    set satiety random 101
    setxy random-xcor random-ycor
    ]
end


to do.fish.movement  ;; fish procedure
  ;; look for fishes in my vicinity
  set schoolmates other fishes in-cone perception.dist perception.angle

  ;; acceleration at each step is determined
  ;; entirely by the urges
  set acceleration (list 0 0)

    
    add-urge patch-center-urge patch.gathering.w
    add-urge wander-urge wander.w
    add-urge avoid-obstacle-urge obstacle.avoidance.w
    add-urge avoid-predator-urge predator.avoidance.w
    add-urge avoid-diver-urge diver.avoidance.w
    add-urge rest-urge rest.w


  ;; if I'm not in a school ignore the school related
  ;; urges. If schooling is disabled, ignore them as well
  if count schoolmates > 0 and schooling?
  [ add-urge spacing-urge spacing.w
    add-urge center-urge center.w
    add-urge align-urge align.w ]
  
  
  ; subtract the deceleration due to drag from the acceleration vector
  let deceleration (scale ((drag.formula * ((magnitude velocity) ^ 2))) (normalize velocity))
  set acceleration (add acceleration deceleration)
  

  ;; keep the acceleration within the accepted range
  if magnitude acceleration > max.acceleration
  [ set acceleration
    (scale
        max.acceleration
        normalize acceleration) ]

  ;; the new velocity of the fish is sum of the acceleration
  ;; and the old velocity.
  set velocity (add velocity acceleration)


  ;; keep the velocity within the accepted range
  
  ifelse (any? predators in-cone approach.dist perception.angle) or (any? divers in-cone approach.dist perception.angle) [
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
  if magnitude velocity > 0.2 [
    facexy nxcor nycor
    fd (magnitude velocity) / movement.time.step]
  
end ;of do.fish.movement

to add-urge [urge factor] ;; fish procedure
  set acceleration add acceleration scale factor normalize urge
end


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
; a normalized vector that is opposite to the position of the closest predator
 let urge (list 0 0)
  if predator.avoidance.w = 0 [ report urge ]
  let threats predators in-cone approach.dist perception.angle
  if any? threats [ask min-one-of threats [distance myself]
  [set urge normalize (add urge subtract (list [xcor] of myself [ycor] of myself) (list xcor ycor))
    ]]
  report urge
end

to-report avoid-diver-urge ;; fish reporter
; a normalized vector that is opposite to the position of the closest diver
 let urge (list 0 0)
  if diver.avoidance.w = 0 [ report urge ]
  let human_threats divers in-cone approach.dist perception.angle
  if any? human_threats [ask min-one-of human_threats [distance myself]
  [set urge normalize (add urge subtract (list [xcor] of myself [ycor] of myself) (list xcor ycor))
    ]]
  report urge
end

to create-obstacles      ; observer procedure
  ask n-of 10 patches [
   set pcolor brown 
  ]
end


to-report random-float-between [a b]
  report random-float (b - a + 1) + a
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


; Saving and loading species data

to save.species.data
  if (b1.freq + b2.freq + b3.freq + b4.freq) != 1 [user-message "ERROR: Sum of behavior frequencies must be 1." stop] ;check if behavior frequencies add up to 1
  set sp.param (list species.name "fish top" fish.size fish.color fish.density visible.dist approach.dist perception.dist perception.angle prey.type max.acceleration max.sustained.speed burst.speed)
  if b1.freq = 0 [set b1.name "n/a" set b1.sp.param ["n/a" 0 0 false 0 0 0 0 0 0 0 0 0 0 0 0] ; check behaviors to see if any of them have freq 0 and fill them with zeros
    output-print "Behavior 1 saved as an empty slot."]
  if b2.freq = 0 [set b2.name "n/a" set b2.sp.param ["n/a" 0 0 false 0 0 0 0 0 0 0 0 0 0 0 0] 
    output-print "Behavior 2 saved as an empty slot."]
  if b3.freq = 0 [set b3.name "n/a" set b3.sp.param ["n/a" 0 0 false 0 0 0 0 0 0 0 0 0 0 0 0] 
    output-print "Behavior 3 saved as an empty slot."]
  if b4.freq = 0 [set b4.name "n/a" set b4.sp.param ["n/a" 0 0 false 0 0 0 0 0 0 0 0 0 0 0 0] 
    output-print "Behavior 4 saved as an empty slot."]
  let behavior.param (sentence b1.sp.param b2.sp.param b3.sp.param b4.sp.param)
  let file.name word user-input "Choose name for the csv file. Any file with the same name will be overwritten." ".csv"
  (csv:to-file file.name (list (list
      "Species" "shape" "size" "color" "density" "visible.dist" "approach.dist" "perception.dist" "perception.angle" "prey.type"
      "max.acceleration" "max.sustained.speed" "burst.speed" "B1.name" "B1.frequency" "B1.detectability" "B1.schooling?" "B1.cruise.distance" "B1.align.w"
      "B1.center.w" "B1.spacing.w" "B1.wander.w" "B1.rest.w" "B1.picked.patch.dist" "B1.patch.gathering.w" "B1.obstacle.avoidance.w"
      "B1.predator.avoidance.w" "B1.prey.chasing.w" "B1.diver.avoidance.w" "B2.name" "B2.frequency" "B2.detectability" "B2.schooling?"
      "B2.cruise.distance" "B2.align.w" "B2.center.w" "B2.spacing.w" "B2.wander.w" "B2.rest.w" "B2.picked.patch.dist" "B2.patch.gathering.w"
      "B2.obstacle.avoidance.w" "B2.predator.avoidance.w" "B2.prey.chasing.w" "B2.diver.avoidance.w" "B3.name" "B3.frequency" "B3.detectability"
      "B3.schooling?" "B3.cruise.distance" "B3.align.w" "B3.center.w" "B3.spacing.w" "B3.wander.w" "B3.rest.w" "B3.picked.patch.dist" "B3.patch.gathering.w"
      "B3.obstacle.avoidance.w" "B3.predator.avoidance.w" "B3.prey.chasing.w" "B3.diver.avoidance.w" "B4.name" "B4.frequency" "B4.detectability" "B4.schooling?"
      "B4.cruise.distance" "B4.align.w" "B4.center.w" "B4.spacing.w" "B4.wander.w" "B4.rest.w" "B4.picked.patch.dist" "B4.patch.gathering.w" "B4.obstacle.avoidance.w"
      "B4.predator.avoidance.w" "B4.prey.chasing.w" "B4.diver.avoidance.w") sentence sp.param behavior.param) ",")
  output-print "Species data saved."
  file-close-all                                                                 ; prevents csv file from being impossible to delete (DOES IT?)
  user-message (word "File " file.name " was created and can be found in the model folder.")
end

to load.species.data
  output-print "Importing species data..."
  let import.name (word user-input "Name of the .csv file with species parameters? (exclude extension)" ".csv")
  let import.delimiter user-input "Delimiter symbol in the .csv file? (usually , or ;)"               ; a window prompts for the csv file delimiter
  carefully [let species.data (csv:from-file import.name import.delimiter)                            ; import csv file into a list of lists
  let nr.species length species.data - 1                                                            ; all filled lines minus the header
  user-message (word "Detected " nr.species " species in the file. If more than one, only the first will be imported.")
  output-print "Setting species parameter values..."
  let params item 1 species.data
  set species.name item 0 params
  set fish.size item 2 params
  set fish.color item 3 params
  set fish.density item 4 params
  set visible.dist item 5 params
  set approach.dist item 6 params
  set perception.dist item 7 params
  set perception.angle item 8 params
  set prey.type item 9 params
  set max.acceleration item 10 params
  set max.sustained.speed item 11 params
  set burst.speed item 12 params
  set b1.sp.param sublist params 13 29
  set b2.sp.param sublist params 29 45
  set b3.sp.param sublist params 45 61
  set b4.sp.param sublist params 61 77
    ]                           
  [user-message (word "File " import.name " not found in model folder.") stop]
  output-print "Loading behavior parameters..."
  setup
  load.b1 load.b2 load.b3 load.b4
  load.b1
  output-print "Done"
  file-close-all
  user-message (word "Species data imported from " import.name ". Behavior 1 has been automatically loaded.")
end

; Saving behavior parameters

to save.b1
  if b1.freq = 0 [set b1.name "n/a" set b1.sp.param ["n/a" 0 0 false 0 0 0 0 0 0 0 0 0 0 0 0] 
    output-print "Behavior 1 saved as an empty slot."
    stop]
  set b1.sp.param (list b1.name b1.freq detectability schooling? cruise.distance align.w center.w spacing.w wander.w rest.w picked.patch.dist patch.gathering.w obstacle.avoidance.w predator.avoidance.w prey.chasing.w diver.avoidance.w)
  set loaded.behavior b1.name
  output-print "Behavior 1 saved."
end

to save.b2
  if b2.freq = 0 [set b2.name "n/a" set b2.sp.param ["n/a" 0 0 false 0 0 0 0 0 0 0 0 0 0 0 0]
    output-print "Behavior 2 saved as an empty slot."
    stop]
  set b2.sp.param (list b2.name b2.freq detectability schooling? cruise.distance align.w center.w spacing.w wander.w rest.w picked.patch.dist patch.gathering.w obstacle.avoidance.w predator.avoidance.w prey.chasing.w diver.avoidance.w)
  set loaded.behavior b2.name
  output-print "Behavior 2 saved."
end

to save.b3
  if b3.freq = 0 [set b3.name "n/a" set b3.sp.param ["n/a" 0 0 false 0 0 0 0 0 0 0 0 0 0 0 0]
    output-print "Behavior 3 saved as an empty slot."
    stop]
  set b3.sp.param (list b3.name b3.freq detectability schooling? cruise.distance align.w center.w spacing.w wander.w rest.w picked.patch.dist patch.gathering.w obstacle.avoidance.w predator.avoidance.w prey.chasing.w diver.avoidance.w)
  set loaded.behavior b3.name
  output-print "Behavior 3 saved."
end

to save.b4
  if b4.freq = 0 [set b4.name "n/a" set b4.sp.param ["n/a" 0 0 false 0 0 0 0 0 0 0 0 0 0 0 0]
    output-print "Behavior 4 saved as an empty slot."
    stop]
  set b4.sp.param (list b4.name b4.freq detectability schooling? cruise.distance align.w center.w spacing.w wander.w rest.w picked.patch.dist patch.gathering.w obstacle.avoidance.w predator.avoidance.w prey.chasing.w diver.avoidance.w)
  set loaded.behavior b4.name
  output-print "Behavior 4 saved."
end


; Loading behavior parameters

to load.b1
  set b1.name item 0 b1.sp.param
  set b1.freq item 1 b1.sp.param
  if b1.freq = 0 [output-print "Behavior slot 1 is empty." stop]
  set detectability item 2 b1.sp.param
  set schooling? item 3 b1.sp.param
  set cruise.distance item 4 b1.sp.param
  set align.w item 5 b1.sp.param
  set center.w item 6 b1.sp.param
  set spacing.w item 7 b1.sp.param
  set wander.w item 8 b1.sp.param
  set rest.w item 9 b1.sp.param
  set picked.patch.dist item 10 b1.sp.param
  set patch.gathering.w item 11 b1.sp.param
  set obstacle.avoidance.w item 12 b1.sp.param
  set predator.avoidance.w item 13 b1.sp.param
  set prey.chasing.w item 14 b1.sp.param
  set diver.avoidance.w item 15 b1.sp.param
  set loaded.behavior b1.name
  ifelse patch.gathering.w > 0 [
  ask fishes with [picked.patch = false] [
   let chosen.patch one-of patches in-cone picked.patch.dist perception.angle
   set picked.patch chosen.patch
   if any? schoolmates [
    ask schoolmates [
     set picked.patch chosen.patch 
    ]
   ]]] [
   ask fishes [
    set picked.patch false 
   ]]
end

to load.b2
  set b2.name item 0 b2.sp.param
  set b2.freq item 1 b2.sp.param
  if b2.freq = 0 [output-print "Behavior slot 2 is empty." stop]
  set detectability item 2 b2.sp.param
  set schooling? item 3 b2.sp.param
  set cruise.distance item 4 b2.sp.param
  set align.w item 5 b2.sp.param
  set center.w item 6 b2.sp.param
  set spacing.w item 7 b2.sp.param
  set wander.w item 8 b2.sp.param
  set rest.w item 9 b2.sp.param
  set picked.patch.dist item 10 b2.sp.param
  set patch.gathering.w item 11 b2.sp.param
  set obstacle.avoidance.w item 12 b2.sp.param
  set predator.avoidance.w item 13 b2.sp.param
  set prey.chasing.w item 14 b2.sp.param
  set diver.avoidance.w item 15 b2.sp.param
  set loaded.behavior b2.name 
  ifelse patch.gathering.w > 0 [
  ask fishes with [picked.patch = false] [
   let chosen.patch one-of patches in-cone picked.patch.dist perception.angle
   set picked.patch chosen.patch
   if any? schoolmates [
    ask schoolmates [
     set picked.patch chosen.patch 
    ]
   ]]] [
   ask fishes [
    set picked.patch false 
   ]]
end

to load.b3
  set b3.name item 0 b3.sp.param
  set b3.freq item 1 b3.sp.param
  if b3.freq = 0 [output-print "Behavior slot 3 is empty." stop]
  set detectability item 2 b3.sp.param
  set schooling? item 3 b3.sp.param
  set cruise.distance item 4 b3.sp.param
  set align.w item 5 b3.sp.param
  set center.w item 6 b3.sp.param
  set spacing.w item 7 b3.sp.param
  set wander.w item 8 b3.sp.param
  set rest.w item 9 b3.sp.param
  set picked.patch.dist item 10 b3.sp.param
  set patch.gathering.w item 11 b3.sp.param
  set obstacle.avoidance.w item 12 b3.sp.param
  set predator.avoidance.w item 13 b3.sp.param
  set prey.chasing.w item 14 b3.sp.param
  set diver.avoidance.w item 15 b3.sp.param
  set loaded.behavior b3.name 
  ifelse patch.gathering.w > 0 [
  ask fishes with [picked.patch = false] [
   let chosen.patch one-of patches in-cone picked.patch.dist perception.angle
   set picked.patch chosen.patch
   if any? schoolmates [
    ask schoolmates [
     set picked.patch chosen.patch 
    ]
   ]]] [
   ask fishes [
    set picked.patch false 
   ]]
end

to load.b4
  set b4.name item 0 b4.sp.param
  set b4.freq item 1 b4.sp.param
  if b4.freq = 0 [output-print "Behavior slot 4 is empty." stop]
  set detectability item 2 b4.sp.param
  set schooling? item 3 b4.sp.param
  set cruise.distance item 4 b4.sp.param
  set align.w item 5 b4.sp.param
  set center.w item 6 b4.sp.param
  set spacing.w item 7 b4.sp.param
  set wander.w item 8 b4.sp.param
  set rest.w item 9 b4.sp.param
  set picked.patch.dist item 10 b4.sp.param
  set patch.gathering.w item 11 b4.sp.param
  set obstacle.avoidance.w item 12 b4.sp.param
  set predator.avoidance.w item 13 b4.sp.param
  set prey.chasing.w item 14 b4.sp.param
  set diver.avoidance.w item 15 b4.sp.param
  set loaded.behavior b4.name
  ifelse patch.gathering.w > 0 [
  ask fishes with [picked.patch = false] [
   let chosen.patch one-of patches in-cone picked.patch.dist perception.angle
   set picked.patch chosen.patch
   if any? schoolmates [
    ask schoolmates [
     set picked.patch chosen.patch 
    ]
   ]]] [
   ask fishes [
    set picked.patch false 
   ]]
end


; START WITH A CLEAN SLATE

to create-new-species
  ca
  set b1.freq 0
  set b2.freq 0
  set b3.freq 0
  set b4.freq 0
  save.b1 save.b2 save.b3 save.b4
  set species.name "new species"
  set fish.size 0.3
  set fish.color 9.9
  set fish.density 0.3
  set visible.dist 5
  set approach.dist 1.5
  set perception.dist 0.6
  set perception.angle 320
  set prey.type "benthic"
  set max.acceleration 0.15
  set max.sustained.speed 0.5
  set burst.speed 2.9
  set aspect.ratio 3
  set detectability 1
  set schooling? true
  set cruise.distance 1
  set align.w 10
  set center.w 5
  set spacing.w 20
  set wander.w 8
  set rest.w 0
  set picked.patch.dist 1
  set patch.gathering.w 0
  set obstacle.avoidance.w 10
  set predator.avoidance.w 100
  set prey.chasing.w 0
  set diver.avoidance.w 10
  set loaded.behavior "new behavior"
end
@#$#@#$#@
GRAPHICS-WINDOW
530
10
1140
941
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
29
1
1
1
seconds
10.0

SLIDER
260
10
525
43
fish.density
fish.density
0
1
0.66
0.01
1
fishes / m^2
HORIZONTAL

SLIDER
15
85
255
118
perception.dist
perception.dist
0.05
5
0.35
0.05
1
meters
HORIZONTAL

SLIDER
15
195
207
228
max.sustained.speed
max.sustained.speed
0
10
0.5
0.1
1
m/s
HORIZONTAL

SLIDER
15
290
255
323
max.acceleration
max.acceleration
0
2
0.2
0.05
1
m/s^2
HORIZONTAL

SLIDER
20
555
210
588
cruise.distance
cruise.distance
0.1
10
1
0.1
1
body lenghts
HORIZONTAL

SLIDER
20
520
210
553
spacing.w
spacing.w
0
50
15
1
1
NIL
HORIZONTAL

SLIDER
20
485
210
518
center.w
center.w
0
20
6
1
1
NIL
HORIZONTAL

SLIDER
20
450
210
483
align.w
align.w
0
20
5
1
1
NIL
HORIZONTAL

SLIDER
265
450
437
483
wander.w
wander.w
0
10
3
1
1
NIL
HORIZONTAL

SLIDER
20
615
190
648
obstacle.avoidance.w
obstacle.avoidance.w
0
100
10
1
1
NIL
HORIZONTAL

BUTTON
260
253
390
313
NIL
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
395
253
525
313
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
260
315
390
360
Create obstacles
create-obstacles
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
265
590
440
623
picked.patch.dist
picked.patch.dist
0
10
1
0.5
1
meters
HORIZONTAL

SLIDER
20
650
190
683
predator.avoidance.w
predator.avoidance.w
0
100
10
1
1
NIL
HORIZONTAL

SWITCH
20
415
210
448
schooling?
schooling?
0
1
-1000

SLIDER
15
120
255
153
perception.angle
perception.angle
45
360
320
5
1
degrees
HORIZONTAL

SLIDER
20
685
190
718
diver.avoidance.w
diver.avoidance.w
-1
100
10
1
1
NIL
HORIZONTAL

SLIDER
265
630
440
663
patch.gathering.w
patch.gathering.w
0
20
0
1
1
NIL
HORIZONTAL

SLIDER
15
155
255
188
approach.dist
approach.dist
0.5
10
1
0.1
1
meters
HORIZONTAL

INPUTBOX
15
10
255
70
species.name
Schooling
1
0
String

SWITCH
1145
515
1390
548
smooth.animation?
smooth.animation?
0
1
-1000

TEXTBOX
20
390
170
408
Schooling parameters
11
0.0
1

TEXTBOX
20
595
170
613
Avoidance urges
11
0.0
1

TEXTBOX
15
70
165
88
Fixed species parameters
11
0.0
1

TEXTBOX
265
390
415
408
Individual movement
11
0.0
1

TEXTBOX
265
570
415
588
Gathering parameters
11
0.0
1

INPUTBOX
20
800
165
860
B1.name
wandering
1
0
String

BUTTON
265
800
340
860
Save as B1
save.b1
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
20
860
165
920
B2.name
feeding
1
0
String

INPUTBOX
20
920
165
980
B3.name
stationary
1
0
String

INPUTBOX
20
980
165
1040
B4.name
n/a
1
0
String

BUTTON
265
860
340
920
Save as B2
save.b2
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
265
920
340
980
Save as B3
save.b3
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
265
980
340
1040
Save as B4
save.b4
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
340
800
410
860
Load B1
load.b1
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
340
860
410
920
Load B2
load.b2
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
340
920
410
980
Load B3
load.b3
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
340
980
410
1040
Load B4
load.b4
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
260
45
525
78
fish.size
fish.size
0.05
1
0.2
0.01
1
meters
HORIZONTAL

CHOOSER
260
80
525
125
fish.color
fish.color
0 5 9.9 25 35 45 55 65 75 85 95 100 105 115 125 135
2

BUTTON
20
1045
410
1090
Save species data to file
save.species.data
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
20
1095
410
1140
Load species data from file
load.species.data
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
15
325
255
358
visible.dist
visible.dist
0.5
20
4
0.5
1
meters
HORIZONTAL

MONITOR
395
315
525
360
Nr. fishes
count fishes
0
1
11

SLIDER
165
825
265
858
b1.freq
b1.freq
0
1
0.5
0.05
1
NIL
HORIZONTAL

SLIDER
165
885
265
918
b2.freq
b2.freq
0
1
0.2
0.05
1
NIL
HORIZONTAL

SLIDER
165
945
265
978
b3.freq
b3.freq
0
1
0.3
0.05
1
NIL
HORIZONTAL

SLIDER
165
1005
265
1038
b4.freq
b4.freq
0
1
0
0.05
1
NIL
HORIZONTAL

MONITOR
20
750
265
795
Sum of frequencies (must be 1)
b1.freq + b2.freq + b3.freq + b4.freq
2
1
11

CHOOSER
260
130
525
175
prey.type
prey.type
"benthic" "fish"
0

SLIDER
265
415
437
448
detectability
detectability
0
1
1
0.1
1
NIL
HORIZONTAL

TEXTBOX
20
370
520
388
BEHAVIOR PARAMETERS______________________________________________________________
11
0.0
1

MONITOR
265
670
440
715
Current behavior
loaded.behavior
17
1
11

SLIDER
265
520
437
553
prey.chasing.w
prey.chasing.w
0
50
0
1
1
NIL
HORIZONTAL

TEXTBOX
270
750
425
791
To save an empty behavior, simply set frequency to 0 and save.
11
0.0
1

BUTTON
445
590
515
623
Pick patch
pick.patch
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

OUTPUT
1145
405
1390
480
11

TEXTBOX
1150
10
1300
28
Predator movement
11
0.0
1

SLIDER
1145
25
1390
58
predator.size
predator.size
0.1
2
0.5
0.1
1
meters
HORIZONTAL

SLIDER
1145
60
1390
93
predator.normal.speed
predator.normal.speed
0.01
4
0.5
0.01
1
m/s
HORIZONTAL

SLIDER
1145
130
1390
163
predator.max.turn
predator.max.turn
5
90
20
1
1
degrees
HORIZONTAL

SLIDER
1145
95
1390
128
predator.burst.speed
predator.burst.speed
0.01
4
1.17
0.01
1
m/s
HORIZONTAL

SLIDER
1145
165
1390
198
predator.vision.angle
predator.vision.angle
90
360
270
1
1
degrees
HORIZONTAL

SLIDER
1145
200
1390
233
predator.vision.distance
predator.vision.distance
0.1
5
2
0.1
1
meters
HORIZONTAL

SLIDER
265
485
437
518
rest.w
rest.w
0
20
0
1
1
NIL
HORIZONTAL

BUTTON
1145
305
1390
338
Deploy diver
deploy-diver
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
1155
375
1375
405
Divers move forward at a constant speed of 8m per minute.
11
0.0
1

BUTTON
1145
480
1390
513
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
1145
670
1390
703
Focus on random fish
let chosen-one one-of fishes\nfollow chosen-one\ninspect chosen-one
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
1145
740
1390
773
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

CHOOSER
1145
550
1390
595
movement.time.step
movement.time.step
5 10
1

MONITOR
205
190
255
235
L/s
max.sustained.speed / fish.size
2
1
11

BUTTON
335
180
525
216
Estimate speeds
estimate-speeds
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
260
180
335
240
aspect.ratio
3
1
0
Number

TEXTBOX
345
220
525
255
from caudal fin aspect ratio and body length.
11
0.0
1

BUTTON
1145
235
1390
268
Deploy predator
deploy-predator
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
1145
270
1390
303
Remove predators
ask predators [die]
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
1145
340
1390
373
Remove divers
ask divers [die]
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
1169
573
1274
591
decisions per second
11
0.0
1

TEXTBOX
1150
600
1385
630
Number of speed and heading decisions each fish makes in a second.
11
0.0
1

TEXTBOX
1150
630
1400
660
Match model frame rate in model settings to this value so that normal speed equals real time.
11
15.0
1

SLIDER
15
245
205
278
burst.speed
burst.speed
0
10
2.6
0.1
1
m/s
HORIZONTAL

MONITOR
205
240
255
285
L/s
burst.speed / fish.size
1
1
11

BUTTON
1145
785
1390
860
Create new species
create-new-species
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
1145
705
1390
738
Focus on a diver
if any? divers [\nlet chosen-diver one-of divers\nfollow chosen-diver\ninspect chosen-diver\n]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

@#$#@#$#@
## WHAT IS IT?

This is a vector-based flocking model, based on Jon Klein's implementation of Craig Reynolds' Boids algorithm. Each bird is influenced by a series of urges. By assigning different weights to each urge, the birds exhibit different flocking behaviors.

## HOW IT WORKS

Each bird has vectors for velocity and acceleration, that is, a velocity and acceleration in the x and y directions.  Each of these vectors is influenced by six urges, to be near the center of the flock, to have the same velocity as the rest of the flock, to keep spacing correct, to avoid colliding with obstacles, to be near to the center of a patch and to wander throughout the world.  Each of these factors affects the resulting velocity and acceleration; however, the sliders in the interface weight the amount that each has an effect.

## HOW TO USE IT

Choose the number of birds with the POPULATION slider. Press SETUP to create the birds. Press GO to start the simulation.

BUILD-CUBES randomly places floating-ball obstacles throughout the world. BUILD-WALL creates a randomly placed wall.

VISION is the distance that a bird can see around it. MAX-VELOCITY is the fastest a bird can go and MAX-ACCELERATION is the fastest a bird can accelerate. CRUISE-DISTANCE is the minimum distance from a nearby bird for a bird to feel comfortable.

A bird's movement is determined by the set of urges acting on that bird:

CENTER-CONSTANT - urge to move towards the center of its flock  
VELOCITY-CONSTANT - urge to align its velocity with the velocity of her flockmates  
SPACING-CONSTANT - urge to be no closer than CRUISE-DISTANCE from other birds  
AVOIDANCE-CONSTANT - urge to avoid colliding with obstacles  
WORLD-CENTER-CONSTANT - urge to avoid the edges of the world  
WANDER-CONSTANT - urge to move in a random way

The urge-constant sliders control the weight of each urge in determining the birds' behavior.

## NETLOGO FEATURES

This model uses lists and map fairly extensively to represent and manipulate vectors.

## CREDITS AND REFERENCES

This model is based on Jon Klein's "Swarm" demo for breve, (see http://www.spiderland.org/breve/) which was inspired by Craig Reynolds classic Boids flocking algorithm.

## HOW TO CITE

If you mention this model in an academic publication, we ask that you include these citations for the model itself and for the NetLogo software:  
- Wilensky, U. (2005).  NetLogo Flocking 3D Alternate model.  http://ccl.northwestern.edu/netlogo/models/Flocking3DAlternate.  Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.  
- Wilensky, U. (1999). NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

In other publications, please use:  
- Copyright 2005 Uri Wilensky. All rights reserved. See http://ccl.northwestern.edu/netlogo/models/Flocking3DAlternate for terms of use.

## COPYRIGHT NOTICE

Copyright 2005 Uri Wilensky. All rights reserved.

Permission to use, modify or redistribute this model is hereby granted, provided that both of the following requirements are followed:  
a) this copyright notice is included.  
b) this model will not be redistributed for profit without permission from Uri Wilensky. Contact Uri Wilensky for appropriate licenses for redistribution for profit.
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

fish rotate
true
0
Polygon -1 true false 131 256 87 279 86 285 120 300 150 285 180 300 214 287 212 280 166 255
Polygon -1 true false 195 165 235 181 218 205 210 224 204 254 165 240
Polygon -1 true false 45 225 77 217 103 229 114 214 78 134 60 165
Polygon -7500403 true true 136 270 77 149 81 74 119 20 146 8 160 8 170 13 195 30 210 105 212 149 166 270
Circle -16777216 true false 106 55 30

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
