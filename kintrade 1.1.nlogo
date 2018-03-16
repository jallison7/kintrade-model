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
