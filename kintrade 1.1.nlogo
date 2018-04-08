globals
[
 kin-links
 village-spacing
]

; global variables defined and controlled from the interface:
; full-output -- a switch that controls whether the model runs the output procedure, which produces three csv files for each run of the model with detailed information on agents, links, and pots produced
; nvillages -- the number of villages created during setup
; village-size -- This is used to set the mean size of villages at the start. Actual village size varies around the mean, as initial village populations are randomly drawn (separately for males and females) from a Poisson distribution.
; birth-probability -- this controls the birth rate (which should really be thought of as combining the birth rate and childhood mortality; i.e, it's equivalent to the number of children who survive to adulthood). Since potential maternity age is set as 16-40, birth-probability * 24 should be the mean number of children per married female
; max-marriage-radius -- the maximum distance (measured in number of villages away from the home village) from which potential spouses can come.
; start-exchange -- this sets the length of the burn-in period (in ticks) that creates the kin network
; exchange-length -- the time period (number of ticks) during which agents exchange goods
; annual-production -- the number of pots each producer makes annually
; annual-demand -- the maximum number of pots each consumer can acquire annually
; exchange-threshold -- the number of pots an agent must possess before being willing to pass one along to a link-neighbor

patches-own
[
  village-no
]

breed [villages village]
breed [males male]
breed [females female]
breed [pots pot]

undirected-link-breed [marriages marriage]
undirected-link-breed [parents parent]
undirected-link-breed [siblings sibling]
undirected-link-breed [in-laws in-law]
undirected-link-breed [my-pots my-pot]

villages-own
[
  village-number
  producing-village?
]

males-own
[
  birth-village
  current-village
  married?
  pots-owned
  pots-given
  age
  wife
  mother
  father
  network-size
]

females-own
[
  birth-village
  current-village
  married?
  pots-owned
  pots-given
  age
  husband
  mother
  father
  network-size
]

pots-own
[
  producer ; this stores the who number of the producer, so it doesn't get lost if the producer dies
  producer2 ;this stores the producer as an agent, which makes it easy to tell if the producer has died (there's probably an easier way to just check if the producer is still alive)
  source-village
  current-village
  owner
  owner2
]


;===================================================================================================================================================
to setup
  clear-all
  reset-ticks
  ask patches
  [
  set pcolor 99
  set village-no 0
  ]
  set kin-links (link-set marriages siblings in-laws parents)

  locate-villages
  populate-villages
  marry



  end
;=====================================================================================================================================
to go

  kill-turtles
  reproduce
  marry

  ask males
  [
    set age (age + 1)
  ]
  ask females
  [
    set age (age + 1)
  ]
  ask males
  [
    if wife = nobody
    [
      set married? false
    ]
  ]
  ask females
  [
    if husband = nobody
    [
      set married? false
    ]
  ]
  if ticks >= start-exchange
   [
      produce
      exchange
   ]
  ask males
   [
     set network-size count (turtle-set parent-neighbors sibling-neighbors marriage-neighbors in-law-neighbors)
   ]
   ask females
   [
     set network-size count (turtle-set parent-neighbors sibling-neighbors marriage-neighbors in-law-neighbors)
   ]
    set kin-links (link-set marriages siblings in-laws parents)
    if ticks = (start-exchange + exchange-length)
      [
        if full-output = true
          [
            output
          ]
        stop
      ]
  tick
end


;=======================================================================================================================================================

to locate-villages ; this runs from the setup procedure. It creates seeds for "villages" and assigns each village seed a village number

    let x 1
    set village-spacing (max-pxcor / nvillages) ; sets distance between village seeds, measured only on the x-axis
    while [x <= nvillages]
    [
      ask patch ((x * village-spacing) - (.5 * village-spacing)) ((max-pycor / 2) + ((random 50) - 25)) ; this creates village seeds evenly spaced in a not-quite-straight line onthe display (it's a not-quite-straight line so inter-village links are easier to see)
     [
       set pcolor red
       sprout-villages 1
       [
         set village-number x
         set label x

       ]
     ]
     set x x + 1
     ]



   ask villages
  [
    set village-no village-number
    ifelse pxcor <= 100
      [
        set producing-village? true ;this defines villages at the left side of the world as producing villages
        set label-color black
      ]
      [
        set producing-village? false
      ]
  ]
    ask patches with [pcolor = red]
    [
    ask patches in-radius 15 ; this draws circles representing villages around the village seeds created by the locate-villages procedure, and numbers the villages. This is purely for visualization purposes and affects nothing but the display.
     [
       set village-no [village-no] of myself
       set pcolor red
     ]
    ]



end

;==================================================================================================================================================================
to populate-villages ;This runs from the setup procedure. It populates villages with males and females and assigns birth-village and current-village numbers,

  ask villages
      [
        hatch-males (random-poisson (village-size / 2))        ; this creates a first generation of men within the villages
        hatch-females (random-poisson (village-size / 2))      ; this creates a first generation of women within the villages
      ]
  ask males
       [
        set color blue
        set size 3
        set label ""
        set married? false
        set age (random 45) ; sets ages of 1st generation men, making them all 45 or younger
        set mother nobody
        set father nobody
        set wife nobody
        set birth-village village-no
        set current-village village-no
       ]

  ask females
      [
        set color yellow
        set size 3
        set label ""
        set married? false
        set age (random 45) ; sets ages of 1st generation women, making them all 45 or younger (so they are young enough to reproduce)
        set mother nobody
        set father nobody
        set husband nobody
        set birth-village village-no
        set current-village village-no
        move-to one-of patches with [village-no = [current-village] of myself]
      ]




end
;=============================================================================================================
to kill-turtles

 let people (turtle-set males females)

 ask people with [age > 16]
 [
   ifelse age = 60
   [
     die
   ]
   [
     if random-float 1 < death-probability
     [
       die
     ]
   ]
 ]


end

;===========================================================================================================
to reproduce
  let potential-mothers females with [age > 16]
   set potential-mothers potential-mothers with [age < 41]
    set potential-mothers potential-mothers with [married? = true]

  ask potential-mothers
   [if random-float 1 < birth-probability
    [
      ifelse random-float 1 < .5 ; this builds in a 50/50 gender ratio with each birth having a 50 % chance of being male (if the random number is < 0.5) or female (if the random number >= .5)
      [hatch-males 1
        [
          set married? false
          set color blue
          set age 0
          set size 3
          set birth-village village-no
          set current-village village-no
          set mother myself
          set father [husband] of myself
          set wife nobody
          set pots-owned 0
          create-parent-with mother [set color red]
          create-parent-with father [set color red]
          let brothers other males with [mother != nobody]
           set brothers brothers with [mother = [mother] of myself]
           If any? brothers
              [
               ask brothers
               [
                 create-sibling-with myself [set color blue]
               ]
             ]
          let sisters females with [mother != nobody]
           set sisters sisters with [mother = [mother] of myself]
           if any? sisters
             [
               ask sisters
               [
                 create-sibling-with myself [set color blue]
               ]
             ]
        ]
      ]
      [  ;this starts the else part of the ifelse statement
        hatch-females 1
        [
          set married? false
          set color yellow
          set age 0
          set size 3
          set birth-village village-no
          set current-village village-no
          move-to one-of patches with [village-no = [current-village] of myself]
          set mother myself
          set father [husband] of myself
          set husband nobody
          set pots-owned 0
          create-parent-with mother [set color red]
          create-parent-with father [set color red]
          let brothers males with [mother != nobody]
           set brothers brothers with [mother = [mother] of myself]
           if any? brothers
              [
               ask brothers
               [
                create-sibling-with myself [set color blue]
               ]
              ]
          let sisters other females with [mother != nobody]
           set sisters sisters with [mother = [mother] of myself]
           if any? sisters
              [
               ask sisters
               [
                 create-sibling-with myself [set color blue]
               ]
             ]
        ]
      ]
    ]
  ]

end;
;==================================================================================================================================================================
to marry
   ask females with [age > 16 and not married?]
    [
      let potential-husbands males with [age > 16]
       set potential-husbands potential-husbands with [not married?]  ; this requires marriages to be monogamous
        set potential-husbands potential-husbands with [abs(age - [age] of myself)  < 10] ;this limits marriages to potential partners within ten years of the same age
          if mother != nobody
            [set potential-husbands potential-husbands with [mother != [mother] of myself]] ; this enforces an incest taboo
          if random-float 1 <= endogamy-preference
          [
           set potential-husbands potential-husbands with [current-village = [current-village] of myself] ;this looks first for a potential spouse within the home village
             if any? potential-husbands
              [
                set husband one-of potential-husbands
                set married? true
                create-marriage-with husband  [set color black]
               if [father] of husband != nobody
                [create-in-law-with [father] of husband]
               if [mother] of husband != nobody
                [create-in-law-with [mother] of husband]
               let brothers-in-law males with [mother != nobody]
                 set brothers-in-law brothers-in-law with [mother = [mother] of [husband] of myself]
                   if any? brothers-in-law
                    [
                      ask brothers-in-law
                        [create-in-law-with myself]
                    ]
               let sisters-in-law other females with [mother != nobody]
                 set sisters-in-law sisters-in-law with [mother = [mother] of [husband] of myself]
                   if any? sisters-in-law
                     [
                       ask sisters-in-law
                         [create-in-law-with myself]
                     ]

               ask husband
                [
                 set married? true
                 set wife myself
                 if [father] of wife != nobody
                   [create-in-law-with [father] of wife]
                 if [mother] of wife != nobody
                   [create-in-law-with [mother] of wife]
                 let brothers-in-law2 other males with [mother != nobody]
                   set brothers-in-law2 brothers-in-law2 with [mother = [mother] of [wife] of myself]
                     if any? brothers-in-law2
                      [
                        ask brothers-in-law2
                          [create-in-law-with myself]
                      ]
                 let sisters-in-law2 females with [mother != nobody]
                   set sisters-in-law2 sisters-in-law2 with [mother = [mother] of [wife] of myself]
                     if any? sisters-in-law2
                       [
                         ask sisters-in-law2
                           [create-in-law-with myself]
                       ]
                 move-to patch ([xcor] of myself) ([ycor] of myself)
                 set current-village village-no
                ]
              ]
          ]
      if husband = nobody
        [
         let potential-husbands2 males with [age > 16]
          set potential-husbands2 potential-husbands2 with [abs(current-village - [current-village] of myself) <= max-marriage-radius]
           set potential-husbands2 potential-husbands2 with [not married?]
            set potential-husbands2 potential-husbands2 with [abs(age - [age] of myself)  < 10]
             if any? potential-husbands2
              [
               set husband one-of potential-husbands2
               set married? true
               create-marriage-with husband  [set color black]
               if [father] of husband != nobody
                 [create-in-law-with [father] of husband]
               if [mother] of husband != nobody
                 [create-in-law-with [mother] of husband]
               let brothers-in-law3 males with [mother != nobody]
                 set brothers-in-law3 brothers-in-law3 with [mother = [mother] of [husband] of myself]
                   if any? brothers-in-law3
                    [
                      ask brothers-in-law3
                        [create-in-law-with myself]
                    ]
               let sisters-in-law3 other females with [mother != nobody]
                 set sisters-in-law3 sisters-in-law3 with [mother = [mother] of [husband] of myself]
                   if any? sisters-in-law3
                     [
                       ask sisters-in-law3
                         [create-in-law-with myself]
                     ]
               ask husband
                [
                 set married? true
                 set wife myself
                  if [father] of wife != nobody
                   [create-in-law-with [father] of wife]
                  if [mother] of wife != nobody
                   [create-in-law-with [mother] of wife]
                  let brothers-in-law4 other males with [mother != nobody]
                    set brothers-in-law4 brothers-in-law4 with [mother = [mother] of [wife] of myself]
                      if any? brothers-in-law4
                       [
                         ask brothers-in-law4
                           [create-in-law-with myself]
                       ]
                  let sisters-in-law4 females with [mother != nobody]
                    set sisters-in-law4 sisters-in-law4 with [mother = [mother] of [wife] of myself]
                      if any? sisters-in-law4
                        [
                          ask sisters-in-law4
                            [create-in-law-with myself]
                        ]
                 move-to patch ([xcor] of myself) ([ycor] of myself)
                 set current-village village-no
                ]
             ]
     ]
    ]

end


;=======================================================================================

to produce
  let potters females with [pxcor < 100]
    set potters potters with [age > 16]
  ask potters
    [
      if pots-owned < (annual-production * 3)
      [
        set pots-owned (pots-owned + annual-production)
        hatch-pots annual-production
         [
           set source-village [current-village] of myself
           set current-village [current-village] of myself
           set producer [who] of myself
           set producer2 myself
           set owner [who] of myself
           set owner2 myself
           create-my-pot-with myself
         ]
      ]
    ]


end
;======================================================================
to exchange

  let consumers (turtle-set males females)
    set consumers consumers with [pxcor >= 100]
      set consumers consumers with [age > 16]
    let i 2
  while [i <= nvillages]
  [
    ask consumers with [current-village = i]
     [
      let y 0
      let current-consumer self
      let my-network (turtle-set parent-neighbors sibling-neighbors marriage-neighbors in-law-neighbors)
        set my-network my-network with [current-village <= [current-village] of myself]
      ask my-network
        [
         if pots-owned >= exchange-threshold
           [
             if y < annual-demand
                [
                 ask one-of my-pot-neighbors
                   [
                     create-my-pot-with current-consumer
                     set current-village [current-village] of current-consumer
                     set owner [who] of current-consumer
                     set owner2 current-consumer
                     ask my-pot-with myself [die]
                   ]
                    set pots-owned (pots-owned - 1)
                    set pots-given (pots-given + 1)
                    set y (y + 1)
                ]
          ]
       ]
          set pots-owned (pots-owned + y)
     ]
    set i (i + 1)
  ]


end

;====================================================================================================
to output

  file-open "potlist.csv"
    file-type "pot number"
    file-type ","
    file-type "current village"
    file-type ","
    file-type "owner"
    file-type ","
    file-type "owner2"
    file-type ","
    file-type "source village"
    file-type ","
    file-type "producer"
    file-type ","
    file-print "producer2"
    ask pots
      [
        file-type who
        file-type ","
        file-type current-village
        file-type ","
        file-type owner
        file-type ","
        file-type owner2
        file-type ","
        file-type source-village
        file-type ","
        file-type producer
        file-type ","
        file-print producer2
      ]
  file-close
  file-open "agentlist.csv"
    file-type "sex"
    file-type ","
    file-type "agent number"
    file-type ","
    file-type "birth village"
    file-type ","
    file-type "current village"
    file-type ","
    file-type "pots owned"
    file-type ","
    file-type "pots traded"
    file-type ","
    file-type "age"
    file-type ","
    file-type "mother"
    file-type ","
    file-type "father"
    file-type ","
    file-type "spouse"
    file-type ","
    file-type "number of links"
    file-type ","
    file-type "links outside home village"
    file-type ","
    file-type "links > 1 village"
    file-type ","
    file-type "links > 2 villages"
    file-type ","
    file-type "links > 3 villages"
    file-type ","
    file-type "links > 4 villages"
    file-type ","
    file-type "links > 5 villages"
    file-type ","
    file-type "links > 6 villages"
    file-type ","
    file-print "links > 7 villages"

    let everyone (turtle-set males females)
      ask everyone
        [
          file-type breed
          file-type ","
          file-type who
          file-type ","
          file-type birth-village
          file-type ","
          file-type current-village
          file-type ","
          file-type pots-owned
          file-type ","
          file-type pots-given
          file-type ","
          file-type age
          file-type ","
          file-type mother
          file-type ","
          file-type father
          file-type ","
          ifelse breed = males
            [
              file-type wife
            ]
            [
              file-type husband
            ]
          file-type ","
          file-type (count my-marriages + count my-siblings + count my-parents + count my-in-laws)
          file-type ","
          file-type (count my-siblings with [abs ([current-village] of end1 - [current-village] of end2) > 0] + count my-parents with [abs ([current-village] of end1 - [current-village] of end2) > 0] + count my-in-laws with [abs ([current-village] of end1 - [current-village] of end2) > 0])
          file-type ","
          file-type (count my-siblings with [abs ([current-village] of end1 - [current-village] of end2) > 1] + count my-parents with [abs ([current-village] of end1 - [current-village] of end2) > 1] + count my-in-laws with [abs ([current-village] of end1 - [current-village] of end2) > 1])
          file-type ","
          file-type (count my-siblings with [abs ([current-village] of end1 - [current-village] of end2) > 2] + count my-parents with [abs ([current-village] of end1 - [current-village] of end2) > 2] + count my-in-laws with [abs ([current-village] of end1 - [current-village] of end2) > 2])
          file-type ","
          file-type (count my-siblings with [abs ([current-village] of end1 - [current-village] of end2) > 3] + count my-parents with [abs ([current-village] of end1 - [current-village] of end2) > 3] + count my-in-laws with [abs ([current-village] of end1 - [current-village] of end2) > 3])
          file-type ","
          file-type (count my-siblings with [abs ([current-village] of end1 - [current-village] of end2) > 4] + count my-parents with [abs ([current-village] of end1 - [current-village] of end2) > 4] + count my-in-laws with [abs ([current-village] of end1 - [current-village] of end2) > 4])
          file-type ","
          file-type (count my-siblings with [abs ([current-village] of end1 - [current-village] of end2) > 5] + count my-parents with [abs ([current-village] of end1 - [current-village] of end2) > 5] + count my-in-laws with [abs ([current-village] of end1 - [current-village] of end2) > 5])
          file-type ","
          file-type (count my-siblings with [abs ([current-village] of end1 - [current-village] of end2) > 6] + count my-parents with [abs ([current-village] of end1 - [current-village] of end2) > 6] + count my-in-laws with [abs ([current-village] of end1 - [current-village] of end2) > 6])
          file-type ","
          file-print (count my-siblings with [abs ([current-village] of end1 - [current-village] of end2) > 7] + count my-parents with [abs ([current-village] of end1 - [current-village] of end2) > 7] + count my-in-laws with [abs ([current-village] of end1 - [current-village] of end2) > 7])

      ]
  file-close
  file-open "linklist.csv"
    file-type "breed"
    file-type ","
    file-type "sex"
    file-type ","
    file-type "agent number"
    file-type ","
    file-type "sex"
    file-type ","
    file-type "agent number"
    file-type ","
    file-type "current-village of end1"
    file-type ","
    file-type "current-village of end2"
    file-type ","
    file-print "link length"
    ask kin-links
      [
        file-type breed
        file-type ","
        file-type [breed] of end1
        file-type ","
        file-type [who] of end1
        file-type ","
        file-type [breed] of end2
        file-type ","
        file-type [who] of end2
        file-type ","
        file-type [current-village] of end1
        file-type ","
        file-type [current-village] of end2
        file-type ","
        file-print abs([current-village] of end1 - [current-village] of end2)]
  file-close

end
@#$#@#$#@
GRAPHICS-WINDOW
551
10
1561
521
-1
-1
2.0
1
16
1
1
1
0
0
0
1
0
500
0
250
1
1
1
ticks
30.0

BUTTON
43
113
110
146
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

SLIDER
14
259
186
292
nvillages
nvillages
1
10
8.0
1
1
NIL
HORIZONTAL

SLIDER
13
305
185
338
village-size
village-size
10
1000
100.0
10
1
NIL
HORIZONTAL

BUTTON
43
150
106
183
step
go
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
42
192
105
225
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

SLIDER
13
350
185
383
birth-probability
birth-probability
0
1
0.17
.01
1
NIL
HORIZONTAL

PLOT
218
10
532
195
plot 1
NIL
Total Population
0.0
100.0
0.0
1000.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot (count males + count females)"

SLIDER
13
393
185
426
death-probability
death-probability
0
1
0.05
.01
1
NIL
HORIZONTAL

SLIDER
10
433
185
466
max-marriage-radius
max-marriage-radius
1
10
6.0
1
1
NIL
HORIZONTAL

PLOT
215
210
529
360
plot 2
ticks
Links
0.0
100.0
0.0
100.0
true
false
"" ""
PENS
"long-links" 1.0 0 -16777216 true "" "if ticks > 0\n[plot count kin-links with [link-length > (1.5 * village-spacing)]]"
"short-links" 1.0 0 -7500403 true "" "if ticks > 0\n[plot count kin-links with [link-length < (village-spacing + 30)]]"

PLOT
214
377
531
527
plot 3
Links
Individuals
0.0
20.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "let people (turtle-set males females)\nhistogram [network-size] of people"

SLIDER
555
530
728
563
start-exchange
start-exchange
0
500
100.0
20
1
NIL
HORIZONTAL

SLIDER
556
572
729
605
exchange-length
exchange-length
0
100
0.0
1
1
NIL
HORIZONTAL

SLIDER
753
532
926
565
annual-production
annual-production
0
25
5.0
1
1
NIL
HORIZONTAL

SLIDER
754
573
927
606
annual-demand
annual-demand
0
20
5.0
1
1
NIL
HORIZONTAL

SLIDER
952
533
1125
566
exchange-threshold
exchange-threshold
0
20
2.0
1
1
NIL
HORIZONTAL

PLOT
554
616
1568
766
plot 4
Village
Pots
1.0
9.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "histogram [current-village] of pots"

SWITCH
13
46
126
79
full-output
full-output
0
1
-1000

SLIDER
12
478
185
511
endogamy-preference
endogamy-preference
0
1
1.0
.1
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

This model is designed to explore the relationship between kinship networks and exchange of material goods in a simulated small-scale, village-based society. The model is loosely based on archaeological and ethnographic societies in which realtively small communities produce goods for exchange to other similarly sized communities. 

## HOW IT WORKS

The model creates an abstract landscape with a linear arrangement of villages. The villages are populated with agents, representing people, who are born, marry (moving between villages as necessary to coreside with spouses), reproduce, and die, creating kinship ties based on birth and marriage. Residents of the villages at one end of the system make ceramic vessels ("pots") and exchange them down the line of villages through their kinship networks.

## HOW TO USE IT

The interface includes three buttons, 11 sliders for global variables defined and controlled from the interface, four plots, and one switch.

The three buttons are standard for NetLogo: "setup", "step", and "run". To run the model, you first press setup, then (usually) the run button. "Run" will start the model, and it will run until it reaches the specified number of ticks (= start-exchange + exchange-length; see below). Pushing the run button while the simulation is running will pause the model. If you want to step through the model one tick at a time, you can use the step button.


Global variables defined and controlled from the interface:

nvillages -- the number of villages created during setup.

village-size -- This is used to set the mean size of villages at the start. Actual 
village size varies around the mean, as initial village populations are randomly drawn (separately for males and females) from a Poisson distribution.

birth-probability -- this controls the birth rate (which should really be thought of as combining the birth rate and childhood mortality; i.e, it's equivalent to the number of children who survive to adulthood). Since potential maternity age is set as 16-40, birth-probability * 24 should be the mean number of children per married female

max-marriage-radius -- the maximum distance (measured in number of villages away from the home village) from which potential spouses can come.

start-exchange -- this sets the length of the burn-in period (in ticks) that creates the 
kin network. The model runs for n = start-exchange ticks before production and exchange begin.

exchange-length -- the time period (number of ticks) during which agents exchange goods. exchange-length + start-exchange = the total number of ticks for each run of the simulation.

annual-production -- the number of pots each producer makes annually'

annual-demand -- the maximum number of pots each consumer can acquire annually.

exchange-threshold -- the number of pots an agent must possess before being willing to pass one along to a link-neighbor.

Plots:

There are four plots on the interface that allow some aspects of the simulation to be observed. The data in the plots is mostly useful for getting a genreal sense of whether things are working approximately as they are supposed to, and for visualizing major differences between runs of the model. The plots are not useful for detailed analysis (which I do either through NetLogo's BehaviorSpace tool or by detailed analysis of the files created by the output procedure [see below]).

Plot 1 shows the total population at each step of the simulation. The main purpose of the plot is to show whether the population is growing, declining, or staying approximately the same.

Plot 2 plots two lines showing the total numer of kin-links created between agents. One line shows the number of links within the same village or betwewen adjacent villages. This will always (or almost always?) be the higher line in the graph, since most links are relatively short.The second line shows the number of links with agents who reside more than one village away.

Plot 3 shows a histogram of the number of kinship links individuals have, or, in network science terms, the degree distribution of the extended network created by the model. The x-axis shows the number of kinship links and the y-axis the number of individual agents with a given number of links. 

Plot 4 shows the number of pots within each village. This will always be blank until the end of the burn-in period (controlled by the start-exchange slider). Once pottery production starts, it will plot the total number of pots currently in each village (including pots that belonged to deceased individuals). Generally there will be many pots in producing villages, then a decline in the number of pots with distance from the producing villages, but the decline is not always regular. Sometimes this can be due to demographic effects, especially when the total population size is small (when population is small, some villages may have few inhabitants or even die out altogether). Also, if pots are moving rapidly through the system (as they will with small values for exchange-threshhold and large values for annual-production and annual-demand), pots may pile up in the village at the end of the system (because the model doesn't allow them to move out of the system).


Switch:

full-output -- This is a switch that controls whether the model runs the output procedure. The output procedure produces three csv files for each run of the model with detailed information on agents, links, and pots produced. 

With the switch off, it is easier to make large numbers of runs of the simulation (using NetLogo's BehaviorSpace tool, for instance), and collect aggregate data on how the model reacts to changes in parameters.

The files created with the full-output command are intended to be useful for examining the details of individual runs of the model. The files created are named agentlist.csv, linklist.csv, and potlist.csv. 

Agentlist.csv includes 19 variables, with one line of data for each individual agent still alive at the end of the simulation. The variables	are: 
			1) the breed of the agent (reported as "males" or "females" because the breed names are plural); 

2) the agent number; 

3) the birth village; 

4) the current village (birth village and current village will always be the same for females and children of either sex under the age of 16); 

5) the number of pots owned by the agent at the end of the simulation

6) the number of pots acquired by the agent then traded away before the end of the simulation

7) age; 

8) the agent number of the mother (if still living, "nobody" if the mother has died); 

9) The agent number of the father (only if the father is still living, "nobody if the father has died); 

10) the agent number of the spouse, if any (if the agent has never married, or if their spouse has died, it will be "nobody");

11) the total number of kin links the agent has at the end of the simulation (parents, spouses, or other kin who died before the end of the simulation will not be counted);

12) the total number of kin links the agent has that are to agents who live in other villages;

13-19) the total number of links more than the specified number	of villages away from the village in which the agent lives (note for future revisions: it would be easier to work with these data	if this reports how many links are the specified number of villages away rather than how many are more than the specified number of villages away).

Linklist.csv reports information about the links that connect the agents in the simulation. It reports eight columns of data about each link:

1) The breed of the link. There are four possible breeds: 

a) "marriages", for links between spouses; 

b) "parents" for links between parents and children; 

c) "siblings" for links between brothers and sisters (currently the model defines    siblings as agents who share the same mother, which means it recognizes full siblings	(who share both parents), and half-siblings from the same mother, but not half-siblings from the same father. This is something ethat should be fixed the next time the	model is revised); and 

d) "in-laws" for anyone linked to an agent's spouse by one of the other three kinds of links.

2) the breed of the agent at End1 of the link;

3) the agent number of the agent at End1 of the link;

4) the breed of the agent at End2 of the link;

5) the agent numberof the agent at End2 of the link;

6) the current village of the agent at End1:

7) the current village of the agent at End2;

8) the length of the link, measured in villages away (the absolute value of (column 6 -column 7)).


The third file, potlist.csv, is designed to allow tracking of information about all the pots created during the simulation, although so far I have never used this information in any rigorous way. Potlist.csv reports 7 variables for each pot:

1) the pot number (a unique identifier for each pot);

2) the current village of the pot, in other words, where it ended up
			
3) "owner", the last owner of the pot (reported by a variable that stores the "who" number of the owner);

4) "owner2", the last owner of the pot stored as an agent. This	overlaps with the information in "owner", except that, if the owner has died by the end of the simulation, it reports "nobody".This makes it easy to tell whether the owner has died (but probably isn't the best way to track or report that information).

5) The source village of the pot, i.e., the village lived in by	the producer

6-7) the producer of the pot, stored twice, once as the "who" number of the producer and once as the agent number, in the same way as information about the last owner is stored
## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
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
NetLogo 6.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="village size" repetitions="1" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="150"/>
    <metric>count marriages</metric>
    <metric>count parents with [abs (([current-village] of end1) - ([current-village] of end2)) = 0]</metric>
    <metric>count parents with [abs (([current-village] of end1) - ([current-village] of end2)) = 1]</metric>
    <metric>count parents with [abs (([current-village] of end1) - ([current-village] of end2)) = 2]</metric>
    <metric>count parents with [abs (([current-village] of end1) - ([current-village] of end2)) = 3]</metric>
    <metric>count parents with [abs (([current-village] of end1) - ([current-village] of end2)) = 4]</metric>
    <metric>count parents with [abs (([current-village] of end1) - ([current-village] of end2)) = 5]</metric>
    <metric>count parents with [abs (([current-village] of end1) - ([current-village] of end2)) = 6]</metric>
    <metric>count parents with [abs (([current-village] of end1) - ([current-village] of end2)) = 7]</metric>
    <metric>count siblings with [abs (([current-village] of end1) - ([current-village] of end2)) = 0]</metric>
    <metric>count siblings with [abs (([current-village] of end1) - ([current-village] of end2)) = 1]</metric>
    <metric>count siblings with [abs (([current-village] of end1) - ([current-village] of end2)) = 2]</metric>
    <metric>count siblings with [abs (([current-village] of end1) - ([current-village] of end2)) = 3]</metric>
    <metric>count siblings with [abs (([current-village] of end1) - ([current-village] of end2)) = 4]</metric>
    <metric>count siblings with [abs (([current-village] of end1) - ([current-village] of end2)) = 5]</metric>
    <metric>count siblings with [abs (([current-village] of end1) - ([current-village] of end2)) = 6]</metric>
    <metric>count siblings with [abs (([current-village] of end1) - ([current-village] of end2)) = 7]</metric>
    <metric>count in-laws with [abs (([current-village] of end1) - ([current-village] of end2)) = 0]</metric>
    <metric>count in-laws with [abs (([current-village] of end1) - ([current-village] of end2)) = 1]</metric>
    <metric>count in-laws with [abs (([current-village] of end1) - ([current-village] of end2)) = 2]</metric>
    <metric>count in-laws with [abs (([current-village] of end1) - ([current-village] of end2)) = 3]</metric>
    <metric>count in-laws with [abs (([current-village] of end1) - ([current-village] of end2)) = 4]</metric>
    <metric>count in-laws with [abs (([current-village] of end1) - ([current-village] of end2)) = 5]</metric>
    <metric>count in-laws with [abs (([current-village] of end1) - ([current-village] of end2)) = 6]</metric>
    <metric>count in-laws with [abs (([current-village] of end1) - ([current-village] of end2)) = 7]</metric>
    <metric>count males</metric>
    <metric>count females</metric>
    <metric>count males with [age &gt; 16]</metric>
    <metric>count females with [age &gt; 16]</metric>
    <metric>count males with [age &gt; 16 and not married?]</metric>
    <metric>count females with [age &gt; 16 and not married?]</metric>
    <metric>count males with [current-village = 1]</metric>
    <metric>count females with [current-village = 1]</metric>
    <metric>count males with [current-village = 2]</metric>
    <metric>count females with [current-village = 2]</metric>
    <metric>count males with [current-village = 3]</metric>
    <metric>count females with [current-village = 3]</metric>
    <metric>count males with [current-village = 4]</metric>
    <metric>count females with [current-village = 4]</metric>
    <metric>count males with [current-village = 5]</metric>
    <metric>count females with [current-village = 5]</metric>
    <metric>count males with [current-village = 6]</metric>
    <metric>count females with [current-village = 6]</metric>
    <metric>count males with [current-village = 7]</metric>
    <metric>count females with [current-village = 7]</metric>
    <metric>count males with [current-village = 8]</metric>
    <metric>count females with [current-village = 8]</metric>
    <metric>count red-pots with [current-village = 1]</metric>
    <metric>count red-pots with [current-village = 2]</metric>
    <metric>count red-pots with [current-village = 3]</metric>
    <metric>count red-pots with [current-village = 4]</metric>
    <metric>count red-pots with [current-village = 5]</metric>
    <metric>count red-pots with [current-village = 6]</metric>
    <metric>count red-pots with [current-village = 7]</metric>
    <metric>count red-pots with [current-village = 8]</metric>
    <enumeratedValueSet variable="max-marriage-radius">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="village-size">
      <value value="100"/>
      <value value="200"/>
      <value value="500"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="test" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="50"/>
    <metric>count males</metric>
    <metric>count females</metric>
    <metric>count males with [married?] = true</metric>
    <metric>count links</metric>
    <metric>count links with [link-length &gt; (village-spacing + 26)]</metric>
    <enumeratedValueSet variable="max-marriage-radius">
      <value value="250"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nvillages">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-probability">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="village-size">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="birth-probability">
      <value value="0.17"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="village size simplified" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>count links</metric>
    <metric>count links with [abs (([current-village] of end1) - ([current-village] of end2)) = 1]</metric>
    <metric>count links with [abs (([current-village] of end1) - ([current-village] of end2)) = 2]</metric>
    <metric>count links with [abs (([current-village] of end1) - ([current-village] of end2)) = 3]</metric>
    <metric>count links with [abs (([current-village] of end1) - ([current-village] of end2)) = 4]</metric>
    <metric>count links with [abs (([current-village] of end1) - ([current-village] of end2)) = 5]</metric>
    <metric>count links with [abs (([current-village] of end1) - ([current-village] of end2)) = 6]</metric>
    <metric>count links with [abs (([current-village] of end1) - ([current-village] of end2)) = 7]</metric>
    <metric>count males</metric>
    <metric>count females</metric>
    <enumeratedValueSet variable="max-marriage-radius">
      <value value="280"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="nvillages">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="death-probability">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="village-size">
      <value value="200"/>
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="birth-probability">
      <value value="0.17"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="village size marriage radius production test" repetitions="1" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="150"/>
    <metric>count marriages</metric>
    <metric>count parents with [abs (([current-village] of end1) - ([current-village] of end2)) = 0]</metric>
    <metric>count parents with [abs (([current-village] of end1) - ([current-village] of end2)) = 1]</metric>
    <metric>count parents with [abs (([current-village] of end1) - ([current-village] of end2)) = 2]</metric>
    <metric>count parents with [abs (([current-village] of end1) - ([current-village] of end2)) = 3]</metric>
    <metric>count parents with [abs (([current-village] of end1) - ([current-village] of end2)) = 4]</metric>
    <metric>count parents with [abs (([current-village] of end1) - ([current-village] of end2)) = 5]</metric>
    <metric>count parents with [abs (([current-village] of end1) - ([current-village] of end2)) = 6]</metric>
    <metric>count parents with [abs (([current-village] of end1) - ([current-village] of end2)) = 7]</metric>
    <metric>count siblings with [abs (([current-village] of end1) - ([current-village] of end2)) = 0]</metric>
    <metric>count siblings with [abs (([current-village] of end1) - ([current-village] of end2)) = 1]</metric>
    <metric>count siblings with [abs (([current-village] of end1) - ([current-village] of end2)) = 2]</metric>
    <metric>count siblings with [abs (([current-village] of end1) - ([current-village] of end2)) = 3]</metric>
    <metric>count siblings with [abs (([current-village] of end1) - ([current-village] of end2)) = 4]</metric>
    <metric>count siblings with [abs (([current-village] of end1) - ([current-village] of end2)) = 5]</metric>
    <metric>count siblings with [abs (([current-village] of end1) - ([current-village] of end2)) = 6]</metric>
    <metric>count siblings with [abs (([current-village] of end1) - ([current-village] of end2)) = 7]</metric>
    <metric>count in-laws with [abs (([current-village] of end1) - ([current-village] of end2)) = 0]</metric>
    <metric>count in-laws with [abs (([current-village] of end1) - ([current-village] of end2)) = 1]</metric>
    <metric>count in-laws with [abs (([current-village] of end1) - ([current-village] of end2)) = 2]</metric>
    <metric>count in-laws with [abs (([current-village] of end1) - ([current-village] of end2)) = 3]</metric>
    <metric>count in-laws with [abs (([current-village] of end1) - ([current-village] of end2)) = 4]</metric>
    <metric>count in-laws with [abs (([current-village] of end1) - ([current-village] of end2)) = 5]</metric>
    <metric>count in-laws with [abs (([current-village] of end1) - ([current-village] of end2)) = 6]</metric>
    <metric>count in-laws with [abs (([current-village] of end1) - ([current-village] of end2)) = 7]</metric>
    <metric>count males</metric>
    <metric>count females</metric>
    <metric>count males with [age &gt; 16]</metric>
    <metric>count females with [age &gt; 16]</metric>
    <metric>count males with [age &gt; 16 and not married?]</metric>
    <metric>count females with [age &gt; 16 and not married?]</metric>
    <metric>count males with [current-village = 1]</metric>
    <metric>count females with [current-village = 1]</metric>
    <metric>count males with [current-village = 2]</metric>
    <metric>count females with [current-village = 2]</metric>
    <metric>count males with [current-village = 3]</metric>
    <metric>count females with [current-village = 3]</metric>
    <metric>count males with [current-village = 4]</metric>
    <metric>count females with [current-village = 4]</metric>
    <metric>count males with [current-village = 5]</metric>
    <metric>count females with [current-village = 5]</metric>
    <metric>count males with [current-village = 6]</metric>
    <metric>count females with [current-village = 6]</metric>
    <metric>count males with [current-village = 7]</metric>
    <metric>count females with [current-village = 7]</metric>
    <metric>count males with [current-village = 8]</metric>
    <metric>count females with [current-village = 8]</metric>
    <metric>count red-pots with [current-village = 1]</metric>
    <metric>count red-pots with [current-village = 2]</metric>
    <metric>count red-pots with [current-village = 3]</metric>
    <metric>count red-pots with [current-village = 4]</metric>
    <metric>count red-pots with [current-village = 5]</metric>
    <metric>count red-pots with [current-village = 6]</metric>
    <metric>count red-pots with [current-village = 7]</metric>
    <metric>count red-pots with [current-village = 8]</metric>
    <enumeratedValueSet variable="max-marriage-radius">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="village-size">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="annual-production">
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exchange-threshold">
      <value value="2"/>
      <value value="5"/>
      <value value="10"/>
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
0
@#$#@#$#@
