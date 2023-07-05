globals [
  gini-index-reserve ;; gini index is calcultated after taxes, based on the wealth of the peope (although it could also measure incomes).
  lorenz-points
  died-working-mans ;; the amount of mans who died because lack of sugar
  died-retired-mans ;; the amount of mans who died because lack of sugar
  piggy-bank ;; pensions piggy bank
  productivity-decay-list;; list with productivity porcentage for ages from 0 to 100 (101 values).
  social-services-use;; number of uses of social services
  accumulated-gdp ;; accumulated gross domestic product, sum of the sugar recollected every year before taxes (gdp is accumulated-gdp /ticks)
  population
;  population-growth-rate ;; the rate at which the population grows (e.g., 0.02 for 2% growth)
]

mans-own [
  sugar           ;; the amount of sugar this turtle has
  metabolism      ;; the amount of sugar that each mans loses each tick
  vision          ;; the distance that this turtle can see in the horizontal and vertical directions
  vision-points   ;; the points that this turtle can see in relative to it's current position (based on vision)
  age             ;; the current age of this turtle (in ticks)
  max-age         ;; the age at which this turtle will die of natural causes
  retired         ;; is retired
  final-num-children    ;; number of children
  age-for-child
]

patches-own [
  psugar           ;; the amount of sugar on this patch
  max-psugar       ;; the maximum amount of sugar that can be on this patch
]

;;
;; Setup Procedures
;;

breed [mans man]

to setup
  if maximum-sugar-endowment <= minimum-sugar-endowment [
    user-message "Oops: the maximum-sugar-endowment must be larger than the minimum-sugar-endowment"
    stop
  ]
  clear-all
  create-mans initial-population [ turtle-setup ]
  ;; if random-initial-age, random initial age is set in mans. Note that turtle setup is called not only for the initial population as here in setup.
  if ( random-initial-age )
  [
    ask mans [set age (random max-age) ]
;    ask mans [set age 0 ]
  ]
  ;; initialization for productivity decay list
  set productivity-decay-list [0.00 0.08 0.16 0.24 0.31 0.38 0.45 0.51 0.57 0.63 0.68 0.72 0.76 0.79 0.82 0.84 0.86 0.87 0.88 0.90 0.91 0.92 0.93 0.94 0.95 0.96 0.97 0.97 0.98 0.99 0.99 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 0.99 0.99 0.99 0.98 0.98 0.98 0.97 0.97 0.96 0.96 0.95 0.94 0.93 0.92 0.91 0.90 0.89 0.88 0.86 0.85 0.83 0.82 0.80 0.79 0.77 0.75 0.73 0.72 0.70 0.68 0.66 0.64 0.62 0.60 0.58 0.55 0.53 0.51 0.49 0.46 0.44 0.41 0.39 0.36 0.34 0.31 0.29 0.26 0.23 0.20 0.18 0.15 0.12 0.09 0.06 0.03 0.0]
  setup-patches
  update-lorenz-and-gini
;  set population-growth-rate 0.011 ; Example: 2% population growth per tick
  reset-ticks
end

to turtle-setup ;; turtle procedure
  set color red
  set shape "circle"
  move-to one-of patches with [not any? other mans-here]
  set sugar random-in-range minimum-sugar-endowment maximum-sugar-endowment
  set metabolism random-in-range 1 4
  set max-age random-in-range 60 100
  set final-num-children random 3 + 1
  set age 0
  set vision random-in-range 1 6
  set age-for-child random (50 - 15 + 1) + 15

  ;; mans can look horizontally and vertically up to vision patches
  ;; but cannot look diagonally at all
  set vision-points []
  foreach (range 1 (vision + 1)) [ n ->
    set vision-points sentence vision-points (list (list 0 n) (list n 0) (list 0 (- n)) (list (- n) 0))
  ]
  set retired false
  run visualization ;; run the procedure selected in the GUI
end

to setup-patches
  ;list from "sugar-map.txt"
  let max-sugar-list [ 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3 2 2 2 2 2 2 2 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 3 3 3 3 3 3 3 3 3 3 2 2 2 2 2 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 2 2 2 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 2 2 2 2 2 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 2 2 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 2 2 2 2 2 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 2 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 2 2 2 2 2 2 3 3 3 3 3 3 3 4 4 4 4 3 3 3 3 3 3 3 2 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 2 2 2 2 2 3 3 3 3 3 3 4 4 4 4 4 4 4 4 3 3 3 3 3 3 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 2 2 2 2 2 2 3 3 3 3 3 4 4 4 4 4 4 4 4 4 4 3 3 3 3 3 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 2 2 2 2 2 3 3 3 3 3 3 4 4 4 4 4 4 4 4 4 4 3 3 3 3 3 3 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 3 3 3 3 3 4 4 4 4 4 4 4 4 4 4 4 4 3 3 3 3 3 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 3 3 3 3 3 4 4 4 4 4 4 4 4 4 4 4 4 3 3 3 3 3 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 3 3 3 3 3 4 4 4 4 4 4 4 4 4 4 4 4 3 3 3 3 3 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 3 3 3 3 3 4 4 4 4 4 4 4 4 4 4 4 4 3 3 3 3 3 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 3 3 3 3 3 3 4 4 4 4 4 4 4 4 4 4 3 3 3 3 3 3 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 3 3 3 3 3 3 4 4 4 4 4 4 4 4 4 4 3 3 3 3 3 2 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 3 3 3 3 3 3 3 4 4 4 4 4 4 4 4 3 3 3 3 3 3 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3 3 3 3 4 4 4 4 3 3 3 3 3 3 3 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 2 2 2 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 2 2 2 2 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3 3 3 3 3 3 3 3 2 2 2 2 2 2 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3 3 3 3 2 2 2 2 2 2 2 2 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3 3 3 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3 3 3 3 3 3 3 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 2 2 2 2 2 2 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 3 3 3 3 3 3 3 4 4 4 4 3 3 3 3 3 3 3 3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 3 3 3 3 3 3 4 4 4 4 4 4 4 4 3 3 3 3 3 3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 0 0 0 2 2 2 2 2 3 3 3 3 3 4 4 4 4 4 4 4 4 4 4 3 3 3 3 3 3 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 2 2 2 2 3 3 3 3 3 3 4 4 4 4 4 4 4 4 4 4 3 3 3 3 3 3 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 2 2 2 2 3 3 3 3 3 4 4 4 4 4 4 4 4 4 4 4 4 3 3 3 3 3 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 2 2 2 2 3 3 3 3 3 4 4 4 4 4 4 4 4 4 4 4 4 3 3 3 3 3 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 2 2 2 2 3 3 3 3 3 4 4 4 4 4 4 4 4 4 4 4 4 3 3 3 3 3 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 2 2 2 2 3 3 3 3 3 4 4 4 4 4 4 4 4 4 4 4 4 3 3 3 3 3 2 2 2 2 2 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 2 2 2 2 3 3 3 3 3 3 4 4 4 4 4 4 4 4 4 4 3 3 3 3 3 3 2 2 2 2 2 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 2 2 2 2 2 3 3 3 3 3 4 4 4 4 4 4 4 4 4 4 3 3 3 3 3 2 2 2 2 2 2 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 2 2 2 2 2 3 3 3 3 3 3 4 4 4 4 4 4 4 4 3 3 3 3 3 3 2 2 2 2 2 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 2 2 2 2 2 2 3 3 3 3 3 3 3 4 4 4 4 3 3 3 3 3 3 3 2 2 2 2 2 2 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 2 2 2 2 2 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 2 2 2 2 2 2 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 2 2 2 2 2 2 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 2 2 2 2 2 2 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 2 2 2 2 2 2 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3 3 3 3 3 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ]
  let counter 0
  foreach sort patches [ p ->
    ask p [
      set max-psugar (item counter max-sugar-list)
      set counter (counter + 1)
      set psugar max-psugar
      patch-recolor
    ]
  ]
end

;;
;; Runtime Procedures
;;

to go
;  if ticks mod 10 = 0 [ ; Adjust the frequency of population growth according to your needs
;    grow-population]
  if not any? mans [
    stop
  ]
  ask patches [
    patch-growback
    patch-recolor
  ]
  ask mans [
    if(age > retirement-age) [
      set retired true
    ]

    ifelse(retired) ;;if retired, mans do not move or eat
    [
      retired-turtle-eat
    ]
    [
      turtle-move
      turtle-eat
    ]

    set age (age + 1)

    ;;social services inclusion: working agents can get sugar from the piggy bank if they run out of sugar.
    ;;unlike retired agents, they still move and get sugar considering their productivity if productivity decay is activated
    if(social-services and  ( not retired ) and sugar <= 0 and  (piggy-bank + (sugar - 1) > 0) )
    [
      ;;print ( "social services being used by turtle:")
      ;;print(who)
      set social-services-use (social-services-use + 1)
      set piggy-bank (piggy-bank + (sugar - 1) ) ;;sugar is 0 or less.
      set sugar 1 ;; the agent survive to the next instructions because of social services
    ]


    ;;social exclusion output calculation
    if sugar <= 0 and ( not retired ) [set died-working-mans (died-working-mans + 1)];; count mans which die because starvation
    if sugar <= 0 and retired [set died-retired-mans (died-retired-mans + 1)];; count mans which die because starvation

    ;;conditions to die
    if sugar <= 0 or age > max-age [
;      hatch 1 [ turtle-setup ]
      die
    ]
    run visualization
  ]
  update-lorenz-and-gini
  tick
  reproduce-agents
end

;to grow-population
;  let target-population ceiling (count mans * (1 + population-growth-rate))
;
;  if count mans < target-population [
;    let num-new-mans target-population - count mans
;    create-mans num-new-mans [ turtle-setup ]
;  ]
;end


to turtle-move ;; turtle procedure


  let patches-candidates (patch-set patch-here (patches at-points vision-points))
  let move-candidates patches-candidates
  set move-candidates  patches-candidates with [not any? (mans-here with [retired = false])]

;  ifelse(resources-occupation)
;  [
;    set move-candidates  patches-candidates with [not any? mans-here]
;  ]
;  [;; if not resource occupation, a worker agent can use a retired agent position and get the sugar
;    ;;useful debuggin commands: ask patches  [ show mans-here ]
;    set move-candidates  patches-candidates with [not any? (mans-here with [retired = false]) ]
;  ]

  let possible-winners move-candidates with-max [psugar]
  if any? possible-winners [
    ;; if there are any such patches move to one of the patches that is closest
    move-to min-one-of possible-winners [distance myself]

    ;;for debugging the resources-occupation
    ;;ask patches  [ if( count mans-here > 1 ) [show mans-here]  ]

  ]
end

to turtle-eat ;; turtle procedure
  set sugar (sugar - metabolism + ( psugar * (1 - pension-taxes / 100) * ifelse-value(productivity-decay)[item age productivity-decay-list][1] )  )
  set piggy-bank (piggy-bank +   (psugar * pension-taxes / 100) * ifelse-value(productivity-decay)[item age productivity-decay-list][1] )
  ;;Japanese system, you get charged a fixed amount (if no sugar to pay it, it is not payed assuming the exemption system)
  if ( ( sugar - fixed-fee ) > 0 )   ; the agent dies if sugar is equal to 0
  [
    set sugar (sugar - fixed-fee)
    set piggy-bank (piggy-bank + fixed-fee)
  ]
  ;; the sugar retreived according to productivity is accumulated in the GDP
  set accumulated-gdp accumulated-gdp + ( psugar * ifelse-value(productivity-decay)[item age productivity-decay-list][1])
  ;;sugar in the patch is 0 if no productivity decay considered, but if activate, the remainng sugar is still available
  set psugar ifelse-value(productivity-decay)[psugar * ( 1 - ( item age productivity-decay-list ) )][0]


end

to retired-turtle-eat ;; turtle procedure for eat from the pensions piggy bank
  ifelse (piggy-bank - metabolism > 0)
  [
    set piggy-bank (piggy-bank - metabolism) ;; retired turtle eat from the piggy bank
  ]
  [
    set sugar (sugar - metabolism) ;; if no money in the piggy-bank, the turtle needs to eat from the savings
  ]

end


to patch-recolor ;; patch procedure
  ;; color patches based on the amount of sugar they have
  set pcolor (yellow + 4.9 - psugar)
end

to patch-growback ;; patch procedure
  ;; gradually grow back all of the sugar for the patch
  set psugar min (list max-psugar (psugar + 1))
end

to update-lorenz-and-gini
  let num-people count mans
  let sorted-wealths sort [sugar] of mans
  let total-wealth sum sorted-wealths
  let wealth-sum-so-far 0
  let index 0
  set gini-index-reserve 0
  set lorenz-points []
  repeat num-people [
    set wealth-sum-so-far (wealth-sum-so-far + item index sorted-wealths)
    set lorenz-points lput ((wealth-sum-so-far / total-wealth) * 100) lorenz-points
    set index (index + 1)
    set gini-index-reserve
      gini-index-reserve +
      (index / num-people) -
      (wealth-sum-so-far / total-wealth)
  ]
end

to reproduce-agents
  ask mans [
    ; Check if it's time to reproduce
    if age = age-for-child [
      set color red
      set sugar (sugar / 2)
      let inherited-sugar (sugar / (2 * final-num-children))
      let inherited-metabolism metabolism
      let inherited-vision vision

      ; Create and add children to the agent's children list
      repeat final-num-children [
        hatch 1 [
          set color green
          set shape "circle"
;          setxy xcor ycor
          setxy random-xcor random-ycor
          set sugar inherited-sugar
          set metabolism inherited-metabolism
          set max-age random-in-range 60 100
          set final-num-children random 1 + 1
          set age 0
          set vision inherited-vision
          set vision-points []
          foreach (range 1 (vision + 1)) [ n ->
            set vision-points sentence vision-points (list (list 0 n) (list n 0) (list 0 (- n)) (list (- n) 0))
          ]
          set retired false
        ]
      ]
    ]
  ]
end



;;
;; Utilities
;;

to-report random-in-range [low high]
  report low + random (high - low + 1)
end

;;
;; Visualization Procedures
;;

to no-visualization ;; turtle procedure
  ifelse(retired)
  [
    set color blue
  ]
  [
;   set color red
  ]

end

to color-agents-by-vision ;; turtle procedure
  set color red - (vision - 3.5)
end

to color-agents-by-metabolism ;; turtle procedure
  set color red + (metabolism - 2.5)
end
@#$#@#$#@
GRAPHICS-WINDOW
330
10
738
419
-1
-1
8.0
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
49
0
49
1
1
1
ticks
30.0

BUTTON
110
215
200
248
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
110
250
200
283
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
0

BUTTON
200
215
290
248
go once
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

CHOOSER
10
365
290
410
visualization
visualization
"no-visualization" "color-agents-by-vision" "color-agents-by-metabolism"
0

PLOT
750
10
955
140
Wealth distribution
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "set-histogram-num-bars 10\nset-plot-x-range 0 (max [sugar] of turtles + 1)\nset-plot-pen-interval (max [sugar] of turtles + 1) / 10\nhistogram [sugar] of turtles"

SLIDER
10
10
290
43
initial-population
initial-population
10
1000
300.0
10
1
NIL
HORIZONTAL

SLIDER
10
50
290
83
minimum-sugar-endowment
minimum-sugar-endowment
0
200
5.0
1
1
NIL
HORIZONTAL

PLOT
750
145
955
275
Lorenz curve
Pop %
Wealth %
0.0
100.0
0.0
100.0
false
true
"" ""
PENS
"equal" 100.0 0 -16777216 true ";; draw a straight line from lower left to upper right\nset-current-plot-pen \"equal\"\nplot 0\nplot 100" ""
"lorenz" 1.0 0 -2674135 true "" "plot-pen-reset\nset-plot-pen-interval 100 / count turtles\nplot 0\nforeach lorenz-points plot"

PLOT
750
285
955
415
Gini index vs. time
Time
Gini
0.0
100.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -13345367 true "" "plot (gini-index-reserve / count turtles) * 2"

SLIDER
10
90
290
123
maximum-sugar-endowment
maximum-sugar-endowment
0
200
26.0
1
1
NIL
HORIZONTAL

PLOT
960
285
1160
415
Social Exclusion
Time
Agents
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"Workers" 1.0 0 -2674135 true "" "plot died-working-turtles"
"Retirees" 1.0 0 -13345367 true "" "plot died-retired-turtles"

SLIDER
10
135
290
168
retirement-age
retirement-age
0
100
50.0
1
1
NIL
HORIZONTAL

SWITCH
10
330
150
363
productivity-decay
productivity-decay
1
1
-1000

SLIDER
10
175
290
208
pension-taxes
pension-taxes
0
100
2.0
1
1
NIL
HORIZONTAL

SWITCH
150
290
290
323
resources-occupation
resources-occupation
1
1
-1000

PLOT
960
145
1160
275
Pensions piggy bank
Time
Sugar
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"Piggy bank" 1.0 0 -10899396 true "" "plot piggy-bank "

SWITCH
10
290
150
323
random-initial-age
random-initial-age
1
1
-1000

SWITCH
150
330
290
363
social-services
social-services
0
1
-1000

SLIDER
10
215
102
248
fixed-fee
fixed-fee
0
4
2.0
0.1
1
NIL
HORIZONTAL

PLOT
960
10
1160
140
Retirees vs workers
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"Retirees" 1.0 0 -13345367 true "" "plot count (turtles with [retired = true ])"
"Workers" 1.0 0 -2674135 true "" "plot count (turtles with [retired = false ])"

PLOT
1190
15
1390
165
Population
Time
Population
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot count turtles"

@#$#@#$#@
## WHAT IS IT?

This model extends Epstein & Axtell's Sugarscape Wealth Distribution model,  described in chapter 2 of their book Growing Artificial Societies: Social Science from the Bottom Up. More specifically, Uri Wilensky NetLogo implementation, licensed under a creative common license in 2009, is used as base for this implementation.

The model extension allows pension laws and social services effects to be studied.

The original simulation provides a ground-up simulation of inequality in wealth. Only a minority of the population have above average wealth, while most agents have wealth near the same level as the initial endowment. The inequity of the resulting distribution can be described graphically by the Lorenz curve and quantitatively by the Gini coefficient.

The new graphical descriptions include among others: working and retired agents, savings in a pension piggy bank, working agents and retired agents in socially excluded, and agents who used social services.


## HOW IT WORKS

### Original simulation

As in the original simulation, each patch contains some sugar, the maximum amount of which is predetermined. At each tick, each patch regains one unit of sugar, until it reaches the maximum amount.

The amount of sugar a patch currently contains is indicated by its color; the darker the yellow, the more sugar.

At setup, agents are placed at random within the world. Each agent can only see a certain distance horizontally and vertically. At each tick, each agent will move to the nearest unoccupied location within their vision range with the most sugar, and collect all the sugar there.  If its current location has as much or more sugar than any unoccupied location it can see, it will stay put.

Agents also use (and thus lose) a certain amount of sugar each tick, based on their metabolism rates. If an agent runs out of sugar, it dies.

Each agent also has a maximum age, which is assigned randomly from the range 60 to 100 ticks.  When the agent reaches an age beyond its maximum age, it dies.

Whenever an agent dies (either from starvation or old age), a new randomly initialized agent is created somewhere in the world; hence, in this model the global population count stays constant.

### Extended simulation 

In the simulation extension, agents can retired at certain age and they are displayed in blue in the environment. At that point: (1) they stop moving and collecting sugar, (2) the sugar they consume based on their metabolism is taken from the pension piggy bank, (3) they consume sugar from their savings if the piggy bank is empty. Depending on the RESOURCES-OCUPPATION parameter, working agents can move to the positions where retired agents stay and collect the sugar.

Before being retired, agents are taxed a percentage of the sugar retrieved in each tick that go to the pension piggy bank. Moreover, if the SOCIAL- SERVICES parameter is activated,  working agents can get sugar from the piggy bank to avoid dying but, unlike retired agents, after moving and collecting sugar.

Other parameters include:

* RANDOM-INITIAL-AGE, it gives a random age to the first population of agents to avoid a first massive wave of retirements when reaching the retirement-age. 
* PRODUCTIVITY-DECAY, it makes agents to be able to recollect only part of the sugar in a patch based on a productivity-decay rate that depends on the age.  These values have been obtained using a univariate Akima interpolation over productivity values per age given in “Does Mental Productivity Decline with Age? Evidence from Chess Players” (2013) by Bertoni et al.


## HOW TO USE IT

### Original simulation

The INITIAL-POPULATION slider sets how many agents are in the world.

The MINIMUM-SUGAR-ENDOWMENT and MAXIMUM-SUGAR-ENDOWMENT sliders set the initial amount of sugar ("wealth") each agent has when it hatches. The actual value is randomly chosen from the given range.

Press SETUP to populate the world with agents and import the sugar map data. GO will run the simulation continuously, while GO ONCE will run one tick.

The VISUALIZATION chooser gives different visualization options and may be changed while the GO button is pressed. When NO-VISUALIZATION is selected all the agents will be red. When COLOR-AGENTS-BY-VISION is selected the agents with the longest vision will be darkest and, similarly, when COLOR-AGENTS-BY-METABOLISM is selected the agents with the lowest metabolism will be darkest.

The WEALTH-DISTRIBUTION histogram on the right shows the distribution of wealth.

The LORENZ CURVE plot shows what percent of the wealth is held by what percent of the population, and the the GINI-INDEX V. TIME plot shows a measure of the inequity of the distribution over time.  A GINI-INDEX of 0 equates to everyone having the exact same amount of wealth (collected sugar), and a GINI-INDEX of 1 equates to the most skewed wealth distribution possible, where a single person has all the sugar, and no one else has any.

### Extended simulation 

#### Parameters
* The RETIREMENT-AGE slider states the age of retirement for agents.
* The  PENSION-TAXES slider states the percentage of sugar collected that goes to the pension piggy bank in each tick.
* FIXED-FEE slider states a fixed fee of sugar to be paid in every tick to contribute to the pension piggy bank.
* RANDOM-INITIAL-AGE switch gives a random age to the first population of agents to avoid a first massive wave of retirements when reaching the retirement-age. 
* PRODUCTIVITY-DECAY switch makes agents to be able to recollect only part of the sugar in a patch based on a productivity-decay rate that depends on the age.  
* RESOURCES-OCUPPATION switch allows working agents to move to the positions where retired agents stay and collect the sugar.
* SOCIAL-SERVICES switch allows working agents can get sugar from the piggy bank to avoid dying but, unlike retired agents, after moving and collecting sugar.


#### Outputs:

* RETIREES VS WORKERS, plots the number of working agents per time in red and the number of retired agents in blue per time.
* PENSIONS PIGGY BANK, plots the amount of sugar per time in the pensions piggy bank.
* SOCIAL EXCLUSION, plots the working agents who run out of sugar in red, the retired agents who run out of sugar in blue.

## THINGS TO NOTICE

### Original simulation

After running the model for a while, the wealth distribution histogram shows that there are many more agents with low wealth than agents with high wealth.

Some agents will have less than the minimum initial wealth (MINIMUM-SUGAR-ENDOWMENT), if the minimum initial wealth was greater than 0.

### Extended simulation 

Work in progress

## THINGS TO TRY

### Original simulation

How does the initial population affect the wealth distribution? How long does it take for the skewed distribution to emerge?

How is the wealth distribution affected when you change the initial endowments of wealth?

### Extended simulation 

Work in progress.

## NETLOGO FEATURES

All of the Sugarscape models create the world by using `file-read` to import data from an external file, `sugar-map.txt`. This file defines both the initial and the maximum sugar value for each patch in the world.

Since agents cannot see diagonally we cannot use `in-radius` to find the patches in the agents' vision.  Instead, we use `at-points`.

## RELATED MODELS

Other models in the NetLogo Sugarscape suite include:

* Sugarscape 1 Immediate Growback
* Sugarscape 2 Constant Growback
* Sugarscape 3 Wealth distribution

For more explanation of the Lorenz curve and the Gini index, see the Info tab of the Wealth Distribution model.  (That model is also based on Epstein and Axtell's Sugarscape model, but more loosely.)

## CREDITS AND REFERENCES

* Epstein, J. and Axtell, R. (1996). Growing Artificial Societies: Social Science from the Bottom Up.  Washington, D.C.: Brookings Institution Press.
*  Li, J. and Wilensky, U. (2009).  NetLogo Sugarscape 3 Wealth Distribution model.  http://ccl.northwestern.edu/netlogo/models/Sugarscape3WealthDistribution.  Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.
* Wilensky, U. (1999). NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.


## HOW TO CITE
Authors: 

*  Emilio Serrano from the Ontology Engineering Group, Deparment of Artificial Intelligence, Universidad Politécnica de Madrid, Spain.
*  Ken Satoh from Principles of Informatics Research Division, NII(National Institute of Informatics), and Sokendai (The Graduate University of Advanced Studies), Japan.
 
<p align="center"> 
<img src="https://www.upm.es/sfs/Rectorado/Gabinete%20del%20Rector/Logos/UPM/Escudo%20con%20Leyenda/ESCUDO%20leyenda%20color%20JPG.jpg" width="200">
<br />
<img src="https://www.realwire.com/writeitfiles/01_nii_logo_A.jpg" width="200">
</p>

We are working in a paper, until then, the github repository containing the code can be cited:  https://github.com/emilioserra/SugarscapePensions 

## COPYRIGHT AND LICENSE

Copyright 2019 Emilio Serrano and Ken Satoh.

![CC BY-NC-SA 3.0](http://ccl.northwestern.edu/images/creativecommons/byncsa.png)

This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 3.0 License.  To view a copy of this license, visit https://creativecommons.org/licenses/by-nc-sa/3.0/ or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.

Commercial licenses are also available. To inquire about commercial licenses, please contact Emilio Serrano  at https://emilioserra.oeg-upm.net/

<!-- 2009 Cite: Li, J. -->
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

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.2.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="initialModelWithPopulationChangeAndRandomInitialAge" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>died-working-turtles ;; the amount of turtles who died because lack of sugar</metric>
    <metric>(gini-index-reserve / count turtles) * 2</metric>
    <enumeratedValueSet variable="maximum-sugar-endowment">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resources-occupation">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="productivity-decay">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pension-taxes">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-sugar-endowment">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="&quot;no-visualization&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-initial-age">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-services">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="initial-population" first="200" step="100" last="600"/>
    <enumeratedValueSet variable="fixed-fee">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="retirement-age">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="pensionsExperiments" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>precision piggy-bank 2</metric>
    <metric>precision  (   accumulated-gdp / ( ticks + 1) )  2</metric>
    <metric>precision  ( (gini-index-reserve / count turtles) * 2)  2</metric>
    <metric>died-working-turtles</metric>
    <metric>died-retired-turtles</metric>
    <metric>(died-retired-turtles + died-working-turtles)</metric>
    <enumeratedValueSet variable="minimum-sugar-endowment">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maximum-sugar-endowment">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="&quot;no-visualization&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-initial-age">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resources-occupation">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-services">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="productivity-decay">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-population">
      <value value="400"/>
      <value value="800"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="retirement-age">
      <value value="100"/>
      <value value="65"/>
      <value value="67"/>
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pension-taxes">
      <value value="0"/>
      <value value="4.7"/>
      <value value="10"/>
      <value value="28.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fixed-fee">
      <value value="0"/>
      <value value="0.1"/>
      <value value="0.5"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="pensionsExperimentsWithProductivityDecay" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>precision piggy-bank 2</metric>
    <metric>precision  (   accumulated-gdp / ( ticks + 1) )  2</metric>
    <metric>precision  ( (gini-index-reserve / count turtles) * 2)  2</metric>
    <metric>died-working-turtles</metric>
    <metric>died-retired-turtles</metric>
    <metric>(died-retired-turtles + died-working-turtles)</metric>
    <enumeratedValueSet variable="minimum-sugar-endowment">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maximum-sugar-endowment">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="&quot;no-visualization&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-initial-age">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resources-occupation">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-services">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="productivity-decay">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-population">
      <value value="400"/>
      <value value="800"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="retirement-age">
      <value value="100"/>
      <value value="65"/>
      <value value="67"/>
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pension-taxes">
      <value value="0"/>
      <value value="4.7"/>
      <value value="10"/>
      <value value="28.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fixed-fee">
      <value value="0"/>
      <value value="0.1"/>
      <value value="0.5"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="pensionsExperimentsWithProductivityDecayAndSocialServices" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>precision piggy-bank 2</metric>
    <metric>precision  (   accumulated-gdp / ( ticks + 1) )  2</metric>
    <metric>precision  ( (gini-index-reserve / count turtles) * 2)  2</metric>
    <metric>died-working-turtles</metric>
    <metric>died-retired-turtles</metric>
    <metric>(died-retired-turtles + died-working-turtles)</metric>
    <enumeratedValueSet variable="minimum-sugar-endowment">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maximum-sugar-endowment">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="visualization">
      <value value="&quot;no-visualization&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-initial-age">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resources-occupation">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-services">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="productivity-decay">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-population">
      <value value="400"/>
      <value value="800"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="retirement-age">
      <value value="100"/>
      <value value="65"/>
      <value value="67"/>
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pension-taxes">
      <value value="0"/>
      <value value="4.7"/>
      <value value="10"/>
      <value value="28.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fixed-fee">
      <value value="0"/>
      <value value="0.1"/>
      <value value="0.5"/>
      <value value="1"/>
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
