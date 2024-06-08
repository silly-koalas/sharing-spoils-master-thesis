extensions [matrix]

globals[
  ;; BASE SETTING GLOBALS
  ; winning-coalition-size
  ; initial-coalition-size
  ; penalty
  ; debug-display?
  ; MCTS-tree-search?
  ; MCTS-trials
  ; UCT-constant
  ; current-random-seed
  ; use-user-seed?
  ; manually-specified-power
  ; manually-specify-power?
  ; manual-agent-types
  ; manual-reward-thresholds
  ; manual-agent-type-input?
  ; constant-sum-payoff-transfers?
  ; one-step-power-transfers?
  ;; PATRONAGE NETWORK PAYOFFS GLOBALS
  ; network-payoffs?
  ; client-to-patron-payoff-transfer
  ; patron-to-client-payoff-transfer
  ;; PATRONAGE NETWORK POWER GLOBALS
  ; network-power?
  ; patronage-power-transfer
  ; max-patronage-power-transfer
  ;; POSITIONAL POWER GLOBALS
  ; positional-power?
  ; positions-power-string
  ; positions-assignment-string
  positions-power
  ;; NETWORK STRUCTURE GLOBALS
  ; patronage-network-structure
  ; number-of-factions
  ; percentage-rewired
  ; manual-patronage-matrix
  ;; GAME STATE GLOBALS
  initial-coalition
  subcoalition
  out-network-matrix
  positions-assignment
  position-assignment-proposal
  power-levels
  agent-types
  reward-thresholds
  residual-power-levels
  completed?
  current-coalition-vote
  agenda-setter-vote
  potential-agenda-setters-vote
  game-tree-vote
  expectation-vote
  latest-coalition-proposal-decision
  ;; COUNTERS
  decision-counter
  proposal-counter
  agenda-counter
  other-counter
]

turtles-own[
  residual-power
  positional-power
  network-power
  is-proposer?
  has-proposed?
  voting-position
  positions-voting-position
  isolated-payoff
  payoff
  my-faction
  in-coalition?
  transitions
  agent-type
  reward-threshold
]

directed-link-breed [patron-of-ties patron-of-tie]

; ENVIRONMENTAL/STRATEGY-INDEPENDENT PROCEDURES
to setup
  ; Clean the environment and set global conditions
  clear-all

  ; Set and save the RNG seed
  if (not use-user-seed?) [
    set current-random-seed new-seed
  ]

  random-seed current-random-seed

  ; Begin setting up the world
  set completed? False
  ask patches [
    set pcolor 89
  ]

  ; Create agents
  create-turtles initial-coalition-size [
    set shape "face neutral"
    set color 24
    set residual-power ifelse-value (manually-specify-power?) [item who read-from-string manually-specified-power] [(50 + random 11 - random 11) / 10]
    set positional-power 0
    set network-power 0
    set payoff 0
    set in-coalition? True
    set has-proposed? False
    set is-proposer? False
    set transitions 0
    set agent-type ifelse-value(manual-agent-type-input?) [item who read-from-string manual-agent-types] ["Maximiser"]
    set reward-threshold ifelse-value(manual-agent-type-input?) [item who read-from-string manual-reward-thresholds] [1]
  ]

  ; Save the initial conditions as lists to serve as function input
  set initial-coalition sort [who] of turtles
  set residual-power-levels map [t -> [residual-power] of turtle t] initial-coalition

  ; Create a patronage network
  create-network

  ; Create and assign positions
  if (MCTS-tree-search?) [
    set positions-power read-from-string positions-power-string
    set positions-assignment ifelse-value (manually-assign-positions?) [read-from-string positions-assignment-string] [n-values (length positions-power) [one-of [who] of turtles]]
  ]

  ; Update the matrix recording the network structure to accurately represent the current network structure
  if (MCTS-tree-search?) [
    set out-network-matrix n-values (count turtles) [n-values (count turtles) ["Maximiser"]]
    foreach sort [who] of turtles [i ->
      foreach sort [who] of turtles [j ->
        ask turtle i [set out-network-matrix replace-item i out-network-matrix (replace-item j item i out-network-matrix ifelse-value(out-patron-of-tie-neighbor? turtle j) [1] [0])]
      ]
    ]
  ]

  ; Update the power of players if required
  ifelse (MCTS-tree-search?) [
    ; Determine positional power levels
    ask turtles [set positional-power sum map [i -> ifelse-value (item i positions-assignment = who) [item i positions-power] [0]] range length positions-assignment]

    ; Create an object to store after-transfer power levels, since it cannot be defined inside of the if-else statement
    let network-power-list []

    ; Only use direct transfers when running a robustness check, use iterative transfers otherwise
    ifelse (one-step-power-transfers?) [
      set network-power-list robustness-check-calculate-network-power map [t -> [residual-power + positional-power] of t] sort turtles map [t -> ifelse-value ([in-coalition?] of t) [1] [0]] sort turtles out-network-matrix patronage-power-transfer max-patronage-power-transfer
    ] [
      set network-power-list calculate-network-power map [t -> [residual-power + positional-power] of t] sort turtles map [t -> ifelse-value ([in-coalition?] of t) [1] [0]] sort turtles out-network-matrix patronage-power-transfer max-patronage-power-transfer power-transfer-error
    ]

    ; Assign the calculated after-transfer power levels to agents
    ask turtles [set network-power item who network-power-list]
  ] [
    ; If MCTS tree search is not used, ignore networks and positions
    ask turtles [
      set positional-power 0
      set network-power residual-power
    ]
  ]

  ; Save power levels as a global variable for ease of handling
  set power-levels map [t -> [network-power] of turtle t] initial-coalition

  ; Also save agent types and reward thresholds
  set agent-types map [t -> [agent-type] of turtle t] initial-coalition
  set reward-thresholds map [t -> [reward-threshold] of turtle t] initial-coalition

  ; If the MCTS decision tree behind each decision is saved, set the correct destination directory and create
  ; a file by the right name if necessary
  if (write-decision-trees-to-file?) [
    set-current-directory user-directory; decision-tree-file-directory
    file-close-all
    if (file-exists? decision-tree-file) [file-delete decision-tree-file]
    file-open decision-tree-file
    file-print "agent,decision_type,tree,decision"
  ]

  reset-ticks
end

to go
  ; Stop if a stopping condition has been reached
  if (completed?) [
    file-close-all
    stop
  ]

  ; If not, mark the turn transition
  tick

  ; Step 1: Nature randomly picks an agenda setter from among the agents in the coalition that have not been an agenda setter so far
  ask one-of turtles with [in-coalition? and not has-proposed?] [
  ; Step 2: The agenda setter makes a coalition proposal, some subset of the set of available agents
    set is-proposer? True
    ifelse (MCTS-tree-search?) [choose-subcoalition-MCTS] [choose-subcoalition-comprehensive]
    set has-proposed? True
  ]

  ; Step 3: Players in the current coalition vote sequentially over the proposal

  ; When comprehensive search is used, set some parameters that are the same for all players for efficiency
  if (not MCTS-tree-search?) [
    set potential-agenda-setters-vote (sort [who] of turtles with [in-coalition? and not has-proposed?])
    set current-coalition-vote sort [who] of turtles with [in-coalition?]
    set agenda-setter-vote [who] of turtles with [in-coalition? and is-proposer?]
    set game-tree-vote (grow-tree "Coalition proposal" potential-agenda-setters-vote current-coalition-vote agenda-setter-vote (sort [who] of subcoalition) "dolor sit amet" [])
    set expectation-vote (evaluate-tree-by-branches game-tree-vote "Vote" initial-coalition residual-power-levels potential-agenda-setters-vote agent-types reward-thresholds)
  ]

  ; Let players cast votes
  ask turtles with [in-coalition?] [
    ifelse (MCTS-tree-search?) [vote-MCTS] [vote-comprehensive]
  ]

  ; Step 3:  If the players supporting the proposal form a winning coalition within the current coalition...
  ifelse sum([network-power] of turtles with [in-coalition? and voting-position = "In favour"]) > winning-coalition-size * sum([network-power] of turtles with [in-coalition?]) [

    ; Print the acceptance and reset who can be an agenda setter
    set latest-coalition-proposal-decision "Accepted"
    ask turtles with [has-proposed?] [set has-proposed? False]

  ; Step 4: If the accepted proposal is the same as the preceding coalition...
    ifelse subcoalition = turtles with [in-coalition?] [
      ; Update players' network power insofar as required
      if (MCTS-tree-search?) [
        ; Create an object to store after-transfer power levels, since it cannot be defined inside of the if-else statement
        let network-power-list []

        ; Only use direct transfers when running a robustness check, use iterative transfers otherwise
        ifelse (one-step-power-transfers?) [
          set network-power-list robustness-check-calculate-network-power map [t -> [residual-power + positional-power] of t] sort turtles map [t -> ifelse-value([in-coalition?] of t) [1] [0]] sort turtles out-network-matrix patronage-power-transfer max-patronage-power-transfer
        ] [
          set network-power-list calculate-network-power map [t -> [residual-power + positional-power] of t] sort turtles map [t -> ifelse-value([in-coalition?] of t) [1] [0]] sort turtles out-network-matrix patronage-power-transfer max-patronage-power-transfer power-transfer-error
        ]

        ; Assign the calculated after-transfer power levels to agents
        ask turtles [set network-power item who network-power-list]
      ]

      set power-levels map [t -> [network-power] of turtle t] initial-coalition

      ; Make players update the number of transitions they've experienced
      ask turtles with [in-coalition?] [set transitions transitions + 1]

      ; With a coalition proposal decision in place, vote over position assignments
      if (MCTS-tree-search?) [
        decide-positions
      ]

  ; Step 6: Each players receives pay-off
      URC-reached
    ] [
  ; Step 4: If the accepted proposal and the preceding coalition are different, players that are in the preceding coalition but not in the accepted proposal are removed
  ;         from the coalition, players that are not in the preceding coalition but are in the accepted proposal are admitted to the coalition, and the game returns to Step 1
      if (print-decisions?) [print (word "Turtles " [who] of turtles with [in-coalition? and not member? self subcoalition] " are removed from the coalition.")]
      ask turtles with [in-coalition? and not member? self subcoalition] [
        set in-coalition? False
        set color 88
        set shape "face sad"
      ]

      ; Make players update the number of transitions they've experienced
      ask turtles with [in-coalition?] [set transitions transitions + 1]

      ; Update the power of players if required
      if (MCTS-tree-search?) [
        let network-power-list []

        ifelse (one-step-power-transfers?) [
          set network-power-list robustness-check-calculate-network-power map [t -> [residual-power + positional-power] of t] sort turtles map [t -> ifelse-value ([in-coalition?] of t) [1] [0]] sort turtles out-network-matrix patronage-power-transfer max-patronage-power-transfer
        ] [
          set network-power-list calculate-network-power map [t -> [residual-power + positional-power] of t] sort turtles map [t -> ifelse-value ([in-coalition?] of t) [1] [0]] sort turtles out-network-matrix patronage-power-transfer max-patronage-power-transfer power-transfer-error
        ]

        ask turtles [set network-power item who network-power-list]
      ]

      ; Also update the power levels global
      set power-levels map [t -> [network-power] of turtle t] initial-coalition

      ; With a coalition proposal decision in place, vote over position assignments
      if (MCTS-tree-search?) [
        decide-positions
      ]
    ]
  ] [
    set latest-coalition-proposal-decision "Rejected"

  ;  Step 5: If a coalition proposal is rejected but there are still agents in the current coalition that have not proposed a coalition, the game returns to Step 2
    ifelse not any? turtles with [in-coalition? and not has-proposed?] [

      ; With a coalition proposal decision in place, vote over position assignments
      if (MCTS-tree-search?) [
        decide-positions
      ]

      ; If there are no agents in the current coalition that have not proposed a coalition yet, the game goes to Step 6
      URC-reached
    ] [

      ; With a coalition proposal decision in place, vote over position assignments
      if (MCTS-tree-search?) [
        decide-positions
      ]
    ]
  ]

  ; At the end of a turn, remove the marking for the current agenda setter
  ask turtles with [is-proposer?] [set is-proposer? False]
end

to be-in-favour
  set voting-position "In favour"
  if (print-decisions?) [print (word "Turtle " [who] of self " is in favour of the proposed subcoalition.")]
end

to be-against
  set voting-position "Against"
  if (print-decisions?) [print (word "Turtle " [who] of self " is against the proposed subcoalition.")]
end

to URC-reached
  if (print-decisions?) [print ("An ultimate ruling subcoalition has been reached.")]

  ; If patronage networks are used in payoff calculation, avoid dependence of the payoff of one member of the network on the payoff
  ; of all others by first calculating payoffs and transition penalties without accounting for network dependence
  ask turtles [
    set isolated-payoff ifelse-value (in-coalition?) [
      ([network-power] of self / sum [network-power] of turtles with [in-coalition? = True]) - penalty * transitions
    ] [
      0 - penalty * transitions
    ]
  ]

  ifelse (constant-sum-payoff-transfers?) [
    ; If the payoff transfer robustness check is active, first calculate all after-transfer payoffs
    let after-transfer-payoffs calculate-network-power map [i -> [isolated-payoff] of turtle i] range initial-coalition-size n-values initial-coalition-size [1] out-network-matrix patron-to-client-payoff-transfer ifelse-value(patron-to-client-payoff-transfer > 0.5) [patron-to-client-payoff-transfer] [0.5] power-transfer-error

    ; Assign the after-transfer payoffs to agents
    ask turtles [
      set payoff item who after-transfer-payoffs

      ; Correct satisficers' payoff if necessary
      if (agent-type = "Satisficer"and payoff > reward-threshold) [set payoff reward-threshold]
     ]
  ] [
    ; If not running a robustness test...
    ; After calculating payoffs and penalties not accounting for network dependence, calculate the definitive payoffs and penalties by updating for transfers
    ask turtles [
      set payoff isolated-payoff + client-to-patron-payoff-transfer * sum [isolated-payoff] of out-patron-of-tie-neighbors + patron-to-client-payoff-transfer *  sum [isolated-payoff] of in-patron-of-tie-neighbors

      ; Correct satisficers' payoff if necessary
      if (agent-type = "Satisficer"and payoff > reward-threshold) [set payoff reward-threshold]
    ]
  ]

  ; Mark that the run has finished
  ask turtles with [in-coalition? = True] [set shape "face happy"]
  set completed? True
end

to decide-positions
  ; Ask the agenda setter to propose a position distribution
  ask turtles with [is-proposer?] [choose-position-distribution-MCTS]

  ; Ask all turtles to vote over the position distribution proposal
  ask turtles with [in-coalition?] [
    vote-position-assignment-MCTS
  ]

  ; If there is sufficient support for the proposal, update the position assignments with the proposed and accepted position assignments
  if sum ([network-power] of turtles with [in-coalition? = True and positions-voting-position = "In favour"]) > winning-coalition-size * sum ([network-power] of turtles with [in-coalition? = True]) [
    set positions-assignment position-assignment-proposal
  ]

  ; Update the power of players if required
  ;  Calculate positional power
  ask turtles [set positional-power sum map [i -> ifelse-value (item i positions-assignment = who) [item i positions-power] [0]] range length positions-assignment]

  ;  Calculate power after network transfers, either according to the robustness test procedure or according to the normal procedure
  let network-power-list []
  ifelse (one-step-power-transfers?) [
    set network-power-list robustness-check-calculate-network-power map [t -> [residual-power + positional-power] of t] sort turtles map [t -> ifelse-value ([in-coalition?] of t) [1] [0]] sort turtles out-network-matrix patronage-power-transfer max-patronage-power-transfer
  ] [
    set network-power-list calculate-network-power map [t -> [residual-power + positional-power] of t] sort turtles map [t -> ifelse-value ([in-coalition?] of t) [1] [0]] sort turtles out-network-matrix patronage-power-transfer max-patronage-power-transfer power-transfer-error
  ]

  ;  Update players' after-transfer power
  ask turtles [set network-power item who network-power-list]

  ;  Update the global list storing after-transfer power levels
  set power-levels map [t -> [network-power] of turtle t] initial-coalition
end

to be-in-favour-positions
  set positions-voting-position "In favour"
  if (print-decisions?) [print (word "Turtle " [who] of self " is in favour of the proposed position assignments.")]
end

to be-against-positions
  set positions-voting-position "Against"
  if (print-decisions?) [print (word "Turtle " [who] of self " is against the proposed position assignments.")]
end

; STRATEGY-DEPENDENT PROCEDURES
to choose-subcoalition-comprehensive
  ; Set the basic parameters for growing and evaluating the correct game tree
  let potential-agenda-setters (sort [who] of turtles with [in-coalition? = True and not has-proposed?])
  let current-coalition sort [who] of turtles with [in-coalition? = True]
  let agenda-setter who

  ; Grow the game tree
  let game-tree (grow-tree "Agenda setter" potential-agenda-setters current-coalition agenda-setter [] "Lorem ipsum" [])

  ; Evaluate the game tree
  let expectation (evaluate-tree-by-branches game-tree "Coalition proposal" initial-coalition residual-power-levels potential-agenda-setters agent-types reward-thresholds)

  ; Use the collectively defined expectation to determine the agenda setter's payoff from each coalition proposal
  let payoffs map [b -> item who first b] expectation

  ; Propose the subcoalition that has the highest expected payoff (or a random one among the subcoalitions that do so), as output by the tree evaluation
  let one-of-best-choices last one-of filter [b -> item who first b = max payoffs] expectation
  set subcoalition turtles with [member? [who] of self one-of-best-choices]
  if (print-decisions?) [print (word "Turtle " [who] of self " proposes the following subcoalition: " [who] of subcoalition)]
end

to choose-subcoalition-MCTS
  ; Set the basic parameters for growing and evaluating the correct game tree
  let potential-agenda-setters sort [who] of turtles with [in-coalition? = True and not has-proposed?]
  let current-coalition sort [who] of turtles with [in-coalition? = True]
  let agenda-setter who

  ; Generate the branches containing the candidate coalition proposals with expected payoffs
  let branches-to-evaluate evaluate-tree-monte-carlo-nodelist"Agenda setter"current-coalition positions-assignment power-levels power-levels out-network-matrix potential-agenda-setters agenda-setter [] initial-coalition (one-of initial-coalition) "Lorem ipsum" [] initial-coalition (one-of initial-coalition) "Lorem ipsum" initial-coalition residual-power-levels patronage-power-transfer max-patronage-power-transfer power-transfer-error client-to-patron-payoff-transfer patron-to-client-payoff-transfer positions-power  agent-types reward-thresholds MCTS-trials UCT-constant

  ; Save the generated MCTS decision tree if asked
  if (write-decision-trees-to-file?) [
    file-type (word who ",")
    file-type "'Coalition proposal',"
    file-type "'"
    file-write branches-to-evaluate
    file-type "'"
    file-type ","
  ]

  ; Filter the coalition proposals that the player can make from the MCTS tree
  let root-children item 5 first filter [n -> item 0 n = "Root"] branches-to-evaluate
  set root-children filter [n -> member? item 0 n root-children] branches-to-evaluate

  ; Extract the coalition proposal selection statistic (the number of times a node was visited)
  let visits map [b -> item 4 b] root-children

  ; Propose the subcoalition that the MCTS algorithm visited most often
  set subcoalition turtles with [member? ([who] of self) item 2 one-of filter [b -> item 4 b = max visits] root-children]

  ; Save the decision if asked
  if (write-decision-trees-to-file?) [
    file-print (word "'" [who] of subcoalition"'")
  ]

  ; Announce the decision in the command center
  if (print-decisions?) [print (word "Turtle " [who] of self " proposes the following subcoalition: " [who] of subcoalition)]
end

to choose-position-distribution-MCTS
  ; Set the basic parameters for growing and evaluating the correct game tree
  let potential-agenda-setters sort [who] of turtles with [in-coalition? = True and not has-proposed?]
  let current-coalition sort [who] of turtles with [in-coalition? = True]
  let agenda-setter who
  let coalition-proposal sort [who] of subcoalition

  ; Generate the branches containing the candidate position assignment proposals with expected payoffs
  let branches-to-evaluate evaluate-tree-monte-carlo-nodelist"Power levels (Stage I)"current-coalition positions-assignment power-levels power-levels out-network-matrix potential-agenda-setters agenda-setter coalition-proposal initial-coalition (one-of initial-coalition) latest-coalition-proposal-decision[] initial-coalition (one-of initial-coalition) "Lorem ipsum" initial-coalition residual-power-levels patronage-power-transfer max-patronage-power-transfer power-transfer-error client-to-patron-payoff-transfer patron-to-client-payoff-transfer positions-power  agent-types reward-thresholds MCTS-trials UCT-constant

  ; Save the generated MCTS decision tree if asked
  if (write-decision-trees-to-file?) [
    file-type (word who ",")
    file-type "'Position assignment proposal',"
    file-type "'"
    file-write branches-to-evaluate
    file-type "'"
    file-type ","
  ]

  ; Filter the position assignment proposals that the player can make from the MCTS tree
  let root-children item 5 first filter [n -> item 0 n = "Root"] branches-to-evaluate
  set root-children filter [n -> member? item 0 n root-children] branches-to-evaluate

  ; Extract the position assignment proposal selection statistic (the number of times a node was visited)
  let visits map [b -> item 4 b] root-children

  ; Propose the position assignment that the MCTS algorithm visited most often
  set position-assignment-proposal item 2 one-of filter [b -> item 4 b = max visits] root-children

  ; Save the decision if asked
  if (write-decision-trees-to-file?) [
    file-print (word "'" position-assignment-proposal"'")
  ]

  ; Announce the decision in the command center
  if (print-decisions?) [print (word "Turtle " [who] of self " proposes to assign positions as follows: " position-assignment-proposal)]
end

to vote-comprehensive
  ; Use the collectively defined expectation to determine the payoff from accepting versus
  ; rejecting the current coalition proposal
  let payoffs map [b -> item who first b] expectation-vote

  ; If accepting the coalition proposal has a higher expected payoff than rejecting it,
  ; vote in favour of the coalition proposal.
  ; Otherwise, vote against it.
  ifelse (last item (position max payoffs payoffs) expectation-vote = "Accepted") [be-in-favour] [be-against]
end

to vote-MCTS
  ; Set up the inputs to the MCTS procedure
  let potential-agenda-setters sort [who] of turtles with [in-coalition? = True and not has-proposed?]
  let current-coalition sort [who] of turtles with [in-coalition? = True]
  let agenda-setter first sort [who] of turtles with [in-coalition? = True and is-proposer?]
  let coalition-proposal sort [who] of subcoalition

  ; Generate the branches with the current coalition proposal as the root node that have voting sequences as their first few nodes
  let branches-to-evaluate evaluate-tree-monte-carlo-nodelist "Coalition proposal" current-coalition positions-assignment power-levels power-levels out-network-matrix potential-agenda-setters agenda-setter coalition-proposal initial-coalition who "Lorem ipsum" position-assignment-proposal initial-coalition who "Lorem ipsum" initial-coalition residual-power-levels patronage-power-transfer max-patronage-power-transfer power-transfer-error client-to-patron-payoff-transfer patron-to-client-payoff-transfer positions-power  agent-types reward-thresholds MCTS-trials UCT-constant

  ; Save the generated MCTS decision tree if asked
  if (write-decision-trees-to-file?) [
    file-type (word who ",")
    file-type "'Coalition proposal vote',"
    file-type "'"
    file-write branches-to-evaluate
    file-type "'"
    file-type ","
  ]

  ; Extract the mean number of visits to the node in which the player votes against the coalition proposal and the node in which the player
  ; votes in favour of the coalition proposal
  let mean-payoff voting-mean-payoff branches-to-evaluate first filter [n -> item 0 n = "Root"] branches-to-evaluate "Coalition proposal voter" who

  ; Save the decision if asked
  if (write-decision-trees-to-file?) [
    file-print (word ifelse-value((item 0 mean-payoff / item 1 mean-payoff) > (item 2 mean-payoff / item 3 mean-payoff)) ["'In Favour'"] ["'Against'"])
  ]

  ; If the mean number of visits to nodes in favour is larger than the mean number of visits to nodes against, voting in favour has the higher expected
  ; payoff for the focal agent and is the better choice
  ifelse ((item 0 mean-payoff / item 1 mean-payoff) > (item 2 mean-payoff / item 3 mean-payoff)) [be-in-favour] [be-against]
end


; Procedure to extract the information required to calculate the mean payoff per node that is a vote in favour
; or against a proposal by a specific focal agent
to-report voting-mean-payoff [tree current-node voter-type focal-agent]
  ; Extract the children of the current node
  let vote-series-endings filter [n -> member? item 0 n item 5 current-node] tree
  ; Establish the type of node that marks the end of a sequence of votes
  let stop-type (ifelse-value (voter-type = "Coalition proposal voter") ["Coalition proposal decision"] (voter-type = "Position proposal voter") ["Position proposal decision"] ["The voter type should be either a coalition proposal voter or a Position proposal voter. At the moment, it is neither, so you are seeing this error."])
  ; Create the counters that will become the outputs
  let visits-in-favour 0
  let nodes-in-favour 0
  let visits-against 0
  let nodes-against 0

  ; Collect all nodes marking the end of a vote sequence.
  ; As long as there are nodes in the object initially containing the children of the current node that are vote nodes with children...
  while [not empty? filter [v -> not (item 1 v = stop-type or empty? item 5 v)] vote-series-endings] [
    ; Go through each of there nodes
    foreach vote-series-endings [v ->
      if (not (item 1 v = stop-type or empty? item 5 v)) [
        ; Add the children of those nodes to the object
        set vote-series-endings sentence vote-series-endings (filter [n -> member? item 0 n item 5 v] tree)
        ; Remove the initial node
        set vote-series-endings remove-item position v vote-series-endings vote-series-endings
      ]
    ]
  ]

  ; Create an empty object to store nodes that have already been used in the counting process to
  ; avoid double-counting
  let already-visited []

  ; Now actually count the number of nodes and visits to those nodes for votes in favour and against the proposal from which the function starts.
  ; Go through each of the collected vote sequence endings...
  foreach vote-series-endings [v ->
    ; Extract the branch from that ending to the root node
    let branch extract-branch tree item 0 v
    ; Exclude nodes that have already been incorporated into the counting process
    ; to avoid double-counting
    set branch filter [n -> not member? n already-visited] branch
    ; Add the visits of nodes and nodes in favour and against the proposal to the counters, if the vote is cast by the focal agent
    set visits-in-favour visits-in-favour + (sum map [n -> ifelse-value (item 1 n = voter-type and item 0 item 2 n = focal-agent and item 1 item 2 n = "In favour") [item 3 n] [0]] branch)
    set nodes-in-favour nodes-in-favour + (sum map [n -> ifelse-value (item 1 n = voter-type and item 0 item 2 n = focal-agent and item 1 item 2 n = "In favour") [1] [0]] branch)
    set visits-against visits-against + (sum map [n -> ifelse-value (item 1 n = voter-type and item 0 item 2 n = focal-agent and item 1 item 2 n = "Against") [item 3 n] [0]] branch)
    set nodes-against nodes-against + (sum map [n -> ifelse-value (item 1 n = voter-type and item 0 item 2 n = focal-agent and item 1 item 2 n = "Against") [1] [0]] branch)
    ; Add the newly visited nodes to the list of nodes that should not be double-counted
    set already-visited (sentence already-visited branch)
  ]

  ; Report the numbers of nodes and visits to those nodes for nodes that contain votes in favour or against a proposal cast by the focal agent
  report (list visits-in-favour  nodes-in-favour visits-against  nodes-against)
end

to vote-position-assignment-MCTS
  ; Set up the inputs to the MCTS procedure
  let potential-agenda-setters sort [who] of turtles with [in-coalition? = True and not has-proposed?]
  let current-coalition sort [who] of turtles with [in-coalition? = True]
  let agenda-setter first sort [who] of turtles with [in-coalition? = True and is-proposer?]
  let coalition-proposal sort [who] of subcoalition

  ; Generate the branches with the current position assignment proposal as the root node that have voting sequences as their first few nodes
  let branches-to-evaluate evaluate-tree-monte-carlo-nodelist "Position assignment proposal" current-coalition positions-assignment power-levels power-levels out-network-matrix potential-agenda-setters agenda-setter coalition-proposal initial-coalition who latest-coalition-proposal-decision position-assignment-proposal initial-coalition who "Lorem ipsum" initial-coalition residual-power-levels patronage-power-transfer max-patronage-power-transfer power-transfer-error client-to-patron-payoff-transfer patron-to-client-payoff-transfer positions-power  agent-types reward-thresholds MCTS-trials UCT-constant

  ; Save the generated MCTS decision tree if asked
  if (write-decision-trees-to-file?) [
    file-type (word who ",")
    file-type "'Position assignment vote',"
    file-type "'"
    file-write branches-to-evaluate
    file-type "'"
    file-type ","
  ]

  ; Extract the mean payoff of the nodes in which the player votes against the postion assignment proposal and the nodes in which the player
  ; votes in favour of the position assignment proposal
  let mean-payoff voting-mean-payoff branches-to-evaluate first filter [n -> item 0 n = "Root"] branches-to-evaluate "Position proposal voter" who

  ; Save the decision if asked
  if (write-decision-trees-to-file?) [
    file-print (word ifelse-value((item 0 mean-payoff / item 1 mean-payoff) > (item 2 mean-payoff / item 3 mean-payoff)) ["'In Favour'"] ["'Against'"])
  ]

  ; If the mean payoff to nodes in favour is larger than the mean payoff of nodes against, voting in favour has the higher expected
  ; payoff for the focal agent and is the better choice
  ifelse ((item 0 mean-payoff / item 1 mean-payoff) > (item 2 mean-payoff / item 3 mean-payoff)) [be-in-favour-positions] [be-against-positions]
end

; FULL MINIMAX TREE EVALUATION

; Create a tree structure from a certain starting point
to-report grow-tree [before-step-type potential-agenda-setters current-coalition agenda-setter coalition-proposal proposal-decision current-tree]
  ; If the step type for the procedure is a coalitions, the next step should be listing
  ; the agenda setters and continuing to go down the tree for each
  if (before-step-type = "Coalition") [
    set current-tree lput current-coalition current-tree
    report map [A -> grow-tree "Agenda setter" potential-agenda-setters current-coalition A coalition-proposal proposal-decision current-tree] potential-agenda-setters
  ]
  ; If the step type for the procedure is an agenda setter, the next step should be listing
  ; the possible coalitions and continuing to go down the tree for each
  if (before-step-type = "Agenda setter") [
    let potential-coalition-proposals filter [s -> member? agenda-setter s] turn-list-numbers-to-coalition-whos current-coalition
    set current-tree lput agenda-setter current-tree
    report map [P -> grow-tree "Coalition proposal" potential-agenda-setters current-coalition agenda-setter P proposal-decision current-tree] potential-coalition-proposals
  ]
  ; If the step type for the procedure is a coaltion proposal, branch out to the proposal
  ; being accepted and the proposal being rejected
  if (before-step-type = "Coalition proposal") [
    set current-tree lput coalition-proposal current-tree
    report map [D -> grow-tree "Proposal decision" potential-agenda-setters current-coalition agenda-setter coalition-proposal D current-tree] (list "Accepted" "Rejected")
  ]
  ; If the step type for the procedure is a proposal decision, let further growth of the tree depend on whether the proposal is accepted of rejected
  if (before-step-type = "Proposal decision") [
    ; If the proposal is accepted, stop if the accepted coalition is the same as the
    ; current coalition
    if (proposal-decision = "Accepted") [
      ifelse (coalition-proposal = current-coalition) [
        report (sentence current-tree "Accepted" list coalition-proposal "STOP")
      ]
      ; Continue growing from the accepted coalition if the accepted and current
      ; coalition are not the same
      [
        set current-coalition coalition-proposal
        set potential-agenda-setters current-coalition
        set current-tree lput "Accepted" current-tree
        report grow-tree "Coalition" potential-agenda-setters current-coalition agenda-setter coalition-proposal proposal-decision current-tree
      ]
    ]
    ; If the proposal is rejected, check if there are any potential agenda setters left
    if (proposal-decision = "Rejected") [
      set potential-agenda-setters remove agenda-setter potential-agenda-setters
      ifelse (empty? potential-agenda-setters) [
        ; If there are no potential agenda setters left, report the final tree
        report (sentence current-tree "Rejected" list current-coalition "STOP")
      ] [
        ; If there are potential agenda setters left, remove the current agenda setter
        ; from the list of potential agenda setters and continue growing the tree
        set potential-agenda-setters remove agenda-setter potential-agenda-setters
        set current-tree lput "Rejected" current-tree
        report grow-tree "Coalition" potential-agenda-setters current-coalition agenda-setter coalition-proposal proposal-decision current-tree
      ]
    ]
  ]
end

; Procedure to go from a fully expanded game tree to a decision at a decision point
to-report evaluate-tree-by-branches [grown-tree decision-point initial-coalition-arg power-levels-arg agenda-setters-arg agent-types-arg reward-thresholds-arg]
  ; Generate the coordinates of all branches
  let branch-coordinates leaf-coordinates grown-tree []

  ; Generate the accompanying branches
  let branches map [l -> tree-descent l grown-tree] branch-coordinates

  ; Create an empty list to store outcomes in
  let outcomes []

  ; Fill the empty list of outcomes by going through each branch...
  foreach branches [branch ->
    ; ...determining the URC at that branch...
    let URC last but-last branch

    ; ...calculating the number of coalitions that each agent has been part of...
    let branch-transitions map [m -> sum map [l -> ifelse-value (member? (item (l - 1) branch) (list "Accepted" "Rejected") and member? m item l branch) [1] [0]] (range 1 (length branch))] initial-coalition-arg

    ; ...and calculating the payoff for each agent at that branch's URC
    set outcomes lput (
      map [m -> ifelse-value (member? m URC)
        [item m power-levels-arg / sum power-levels-arg - penalty * item m branch-transitions]
          [0 - penalty * item m branch-transitions]
        ] initial-coalition-arg
      ) outcomes

    ; With a correction in the case of satisficers if necessary
    set outcomes (map [[o t r] -> ifelse-value(t = "Satisficer" and o > r) [r] [o] ] outcomes agent-types-arg reward-thresholds-arg)
  ]

  ; Add each branch's outcomes as the first entry for that branch
  set branches map [b -> fput item (position b branches) outcomes b] branches

  ; Cut down branches step by step, adapting the modification of the branches
  ; to the stage of the game the final entry represents
  while [not (empty? filter [x -> length x > 3] branches)] [

    set decision-counter 0
    set proposal-counter 0
    set agenda-counter 0
    set other-counter 0

    ; For each of those branch, determine what stage of the game its tail represents
    foreach branches [branch ->
      if (length branch > 3) [
        (ifelse
          ; Proposal decision
          (member? last branch (list "Accepted" "Rejected")) [
            set branches prune-at-proposal-decision branch branches initial-coalition-arg

            set decision-counter decision-counter + 1
          ]

          ; If the tail is three steps after a proposal decision,
          ; it is a coalition proposal.
          (member? (item (length branch - 4) branch) (list "Accepted" "Rejected")) [
            set branches prune-at-coalition-proposal branch branches initial-coalition-arg

            set proposal-counter proposal-counter + 1
          ]

          ; If the tail is two steps after a proposal decision and not STOP,
          ; it is an agenda setter.
          (member? (item (length branch - 3) branch) (list "Accepted" "Rejected") and (last branch != "STOP")) [
            set branches prune-at-agenda-setter branch branches

            set agenda-counter agenda-counter + 1
          ]
          ; If the tail is neither a proposal decision nor a coalition proposal
          ; nor an agenda setter, it is a ruling coalition, an agenda setter,
          ; or stop, and nothing has to be done before removing the tail
          [
            let pruned-branch but-last branch
            set branches lput pruned-branch filter [b -> b != branch] branches

            set other-counter other-counter + 1
          ]
        )
      ]
    ]

    ; If in debugging mode, print progress
    if (debug-display?) [
          beep
          print (word "The number of branches is:                " length branches)
          print (word "The branches have a mean length of:       " mean map length branches)
          print "The folowing pruning procedures were carried out:"
          print (word "1. Proposal decisions:                    " decision-counter)
          print (word "2. Coalition proposal:                    " proposal-counter)
          print (word "3. Agenda setters:                        " agenda-counter)
          print (word "4. Other kinds of entries:                " other-counter)
          print (word "The last branch in the tree is currently: " last branches)
          print "------------------------------------------------------------"
    ]
  ]

  ; Remove duplicates insofar as that is necessary
  set branches remove-duplicates branches

  ; If in debugging mode, report that the procedure is finished
  if (debug-display?) [print "Done!"]

  ; Report the final set of choices
  report branches
end


to-report prune-at-proposal-decision [branch branches initial-coalition-arg]
  ; Collect all branches that are equal to the branch up to the first item
  ; (payoffs) and last item (proposal decision)
  let comparison-set remove-duplicates filter [c -> sublist branch 2 length branch = sublist c 2 length c] branches

  ; If there is at least one similar branch...
  (ifelse (length comparison-set > 1) [
    ; Determine and aggregate agents' votes by subtracting votes against
    ; a coalition from votes in favour
    let comparison-set-votes map [o ->
      ifelse-value (member? o item (length first comparison-set - 4) first comparison-set) [
        ifelse-value (item (position o initial-coalition-arg) first first filter [p -> last p = "Accepted"] comparison-set > item (position o initial-coalition-arg) first first filter [p -> last p = "Rejected"] comparison-set)
        [o]
        [-1 * o]
      ] [0]
    ] initial-coalition-arg

    ; Record whether agents in the last preceding coalition whose payoff
    ; from the branch ending with "Accepted" is greater than the payoff
    ; from the branch ending with "Rejected" have up at least a share
    ; Alpha of total power in the preceding coalition
    let voting-result (
      sum comparison-set-votes >
      (
        winning-coalition-size *
        sum (filter [o -> member? o item (length (first comparison-set) - 4) first comparison-set] initial-coalition-arg) -
        sum (filter [i -> i < 0] comparison-set-votes)
      )
    )

    ifelse voting-result [
      ; If the coalition proposal is accepted, select the branch with "Accepted"
       let pruned-branch first filter [b -> last b = "Accepted"] comparison-set

       ; Remove the last item from the branch that has "Accepted" up to that point
       set pruned-branch but-last pruned-branch

       ; Remove the branches that are evaluated from the other branches
       set branches filter [b -> not member? b comparison-set] branches

       ; Return the branch with the proposal decision removed
       set branches lput pruned-branch branches
    ] [
      ; If the coalition proposal is rejected, select the branch with "Rejected"
      let pruned-branch first filter [b -> last b = "Rejected"] comparison-set

      ; Remove the last item from the branch that has "Rejected" up to that point
      set pruned-branch but-last pruned-branch

      ; Remove the branches that are evaluated from the other branches
      set branches filter [b -> not member? b comparison-set] branches

      ; Return the branch with the proposal decision removed
      set branches lput pruned-branch branches
    ]
  ] (length comparison-set = 1) [
    ; If there are no similar branches but the examined branch is still present,
    ; check whether there are branches that might become similar after shortening,
    ; and directly remove the tail if this is not the case.
    if (empty? filter [b -> length b > length branch] branches) [
      let pruned-branch but-last branch
      set branches lput pruned-branch filter [b -> b != branch] branches
    ]
    ; Otherwise, do nothing
  ]
    ; If there are no branches in the comparison set and the branch has already
    ; been dealt with, do nothing
  )

  ; Return the branches, whether modified or not, as result
  report branches
end

to-report prune-at-coalition-proposal [branch branches initial-coalition-arg]
  ; Collect all branches that are equal to the branch up to the first item (payoffs)
  ; and last item (coalition proposal)
  let comparison-set remove-duplicates filter [c ->
    sublist branch 2 length branch = sublist c 2 length c
  ] branches

  ; If the branch has any similar branches...
  (ifelse (length comparison-set > 1) [
    ; Extract agenda setter
    let a position (item (length branch - 2) branch) initial-coalition-arg

    let possible-payoffs map [o -> item a first o] comparison-set

    ; Select the branch that maximises the payoff of the agenda setter.
    ; If multiple branches give equal payoff, select the first branch encountered.
    let pruned-branch item (position max possible-payoffs possible-payoffs) comparison-set

    ; Remove the last item from the selected branch
    set pruned-branch but-last pruned-branch

     ; Remove all branches but the one that maximises the payoff of the agenda setter.
    set branches filter [b -> not member? b comparison-set] branches

    ; Return the branch with the coalition proposal removed
    set branches lput pruned-branch branches
  ] (length comparison-set = 1) [
    ; If there are no similar branches but the examined branch is still present,
    ; check whether there are branches that might become similar after shortening,
    ; and directly remove the tail if this is not the case.
    if (empty? filter [b -> length b > length branch] branches) [
      let pruned-branch but-last branch
      set branches lput pruned-branch filter [b -> b != branch] branches
    ]
    ; Otherwise, do nothing
  ]
  ; If there are no branches in the comparison set and the branch has already been dealt with,
  ; do nothing
  )

  ; Return the branches, whether modified or not, as result
  report branches
end

to-report prune-at-agenda-setter [branch branches]
  ; Collect all branches that are equal to the branch up to the first item (payoffs)
  ; and last item (agenda setter)
  let comparison-set remove-duplicates filter [c ->
    sublist branch 2 (length branch - 1) = sublist c 2 (length c - 1)
  ] branches

  ; If the are any similar branches...
  (ifelse (length comparison-set > 1) [
    ; Calculate the average payoff across the selected branches
    let expected-payoff map [i -> mean map [c -> item i first c] comparison-set] range (length first branch)

    ; Select a random branch among those that are equal up to their agenda setter
    let pruned-branch one-of comparison-set

    ; Replace remaining branch's payoff with average payoff across branches
    set pruned-branch replace-item 0 pruned-branch expected-payoff

    ; Remove the last item from the selected branch
    set pruned-branch but-last pruned-branch

    ; Remove the branches that are evaluated from the other branches
    set branches filter [b -> not member? b comparison-set] branches

    ; Return the branch with the coalition proposal removed
    set branches lput pruned-branch branches
  ] (length comparison-set = 1) [
    ; If there are no similar branches but the examined branch is still present,
    ; check whether there are branches that might become similar after shortening,
    ; and directly remove the tail if this is not the case.
    if (empty? filter [b -> length b > length branch] branches) [
      let pruned-branch but-last branch
      set branches lput pruned-branch filter [b -> b != branch] branches
    ]
    ; Otherwise, do nothing
  ]
  ; If there are no branches in the comparison set and the branch has already been
  ; dealt with, do nothing
  )

  ; Return the branches, whether modified or not, as result
  report branches
end

; Utility procedure to create a list of lists
to-report create-search-space-reporter [input-coalition]
  ; Create an empty list in which to store the lists
  let reporter-space []

  ; For each possible list length, generate all possible combinations of that size from the input list
  foreach (range 1 (length input-coalition + 1)) [s ->
    set reporter-space (generate-combinations2 (range length input-coalition) range s reporter-space)
  ]

  ; Report the list of lists
  report reporter-space
end

; Utility procedure to retrieve all leaf coordinate vectors, requesting the strings
; with coordinate vectors from the leaf-coordinates-strings procedure
to-report leaf-coordinates [input-branch route]
  report map read-from-string leaf-coordinates-strings input-branch route
end

; Utility procedure to generate a list containing a coordinate vector for each leaf
; of the game tree, with the coordinate vector taking the form of a string to remove
; the deep nesting that the procedure creates
to-report leaf-coordinates-strings [input-branch route]
  ; If a leaf has been reached, output the list of indexes from the root to that leaf as a string
  ifelse (last input-branch = "STOP") [
    report (word route)
  ] [
    ; If a leaf has not been reached yet, take each node at the current nesting level and continue from that node with that
    ; node's index added to the list of indices
    report reduce sentence map [b -> leaf-coordinates-strings b (lput (position b input-branch) route)] input-branch
  ]
end

; Utility procedure to report a tree branch based on its coordinates
to-report tree-descent [coordinates tree]
  ifelse empty? coordinates [
    ; If there are no coordinates left to traverse, report the branch
    report tree
  ] [
    ; If there are still coordinates to use, remove the coordinate, continue from the node with the coordinate,
    ; and continue from the reduced list of coordinates and expanded branch
    report tree-descent but-first coordinates item (first coordinates) tree
  ]
end

; MONTE CARLO TREE SEARCH EVALUATION

; Main MCTS procedure
to-report evaluate-tree-monte-carlo-nodelist [root-type current-coalition position-assignment-arg power-levels-1 power-levels-2 out-network-matrix-arg potential-agenda-setters agenda-setter coalition-proposal potential-coalition-proposal-voters coalition-proposal-voter coalition-proposal-decision position-assignment-proposal-arg potential-position-assignment-voters position-assignment-voter position-assignment-decision initial-coalition-arg residual-power-levels-arg power-transfer-constant max-power-transfer-parameter power-transfer-error-parameter client-to-patron-payoff-transfer-constant patron-to-client-payoff-transfer-constant positions-power-levels  agent-types-arg reward-thresholds-arg n-trials exploitation-exploration-constant]
  ; Set a counter for the number of simulations
  let counter 0

  ; Initialise the search tree by creating a root node
  let tree (ifelse-value
    (root-type = "Power levels (Stage I)") [(list (list "Root" "Power levels (Stage I)" power-levels-1 0 0 []))]
    (root-type = "Agenda setter") [(list (list "Root" "Agenda setter" agenda-setter 0 0 []))]
    (root-type = "Coalition proposal") [(list (list "Root" "Coalition proposal" coalition-proposal 0 0 []))]
    (root-type = "Position assignment proposal") [(list (list "Root" "Position assignment proposal"position-assignment-proposal-arg 0 0 []))]
    ["You should not have this in your output"]
  )

  ; For N simulation trials...
  while [counter < n-trials] [
    ; Start at the root
    let traversal-id "Root"
    let current-node first filter [c -> item 0 c = traversal-id] tree

    ; While...
    while [
      ; ...the current node is not unvisited...
      item 4 current-node != 0
      ; ...nor is it not expanded at all...
      and not empty? item 5 current-node
      ; ...and a stopping condition has not been reached either...
      and not member? "STOP" item 5 current-node
    ] [
      ; List the child nodes of the current node
      let children filter [t -> member? (item 0 t) (item 5 current-node)] tree

      ; If there are no child nodes that are unvisited
      ifelse (empty? filter [t -> item 4 t = 0] children) [
        ; Calculate the UCTs of the child nodes
        let UCTs map [n -> calculate-UCT-nodelist n exploitation-exploration-constant current-node] children

        ; Select the child node with the highest UCT as the next node to traverse
        let max-UCT-index position max UCTs UCTs
        set traversal-id item 0 item max-UCT-index children
      ] [
        ; If there are unvisited children, select a random one among them
        let unvisited-children filter [t -> item 4 t = 0] children
        set traversal-id item 0 one-of unvisited-children
      ]

      ; Switch to the selected node
      set current-node first filter [c -> item 0 c = traversal-id] tree
    ]

    ; Save the branch leading up to the node to expand
    let twig-generation-branch extract-branch tree traversal-id

    ; Expand that branch one step
    let expanded-generation-branch expand-node-nodelist twig-generation-branch current-coalition position-assignment-arg power-levels-1 power-levels-2 out-network-matrix-arg potential-agenda-setters agenda-setter coalition-proposal potential-coalition-proposal-voters coalition-proposal-voter coalition-proposal-decision position-assignment-proposal-arg potential-position-assignment-voters position-assignment-voter position-assignment-decision initial-coalition-arg residual-power-levels-arg power-transfer-constant max-power-transfer-parameter power-transfer-error-parameter client-to-patron-payoff-transfer-constant patron-to-client-payoff-transfer-constant positions-power-levels agent-types-arg reward-thresholds-arg

    ; If the branch has been expanded into a stopping condition...
    ifelse (member? "STOP" item 5 last expanded-generation-branch) [
      ; Select the expanded nodes
      let nodes-to-add filter [n -> not member? n twig-generation-branch] expanded-generation-branch

      ; Remove the nodes that have been expanded
      let new-ids map [n -> item 0 n] nodes-to-add
      set tree filter [n -> not member? (item 0 n) new-ids] tree

      ; Add the expanded nodes back in in their expanded form
      set tree (sentence tree nodes-to-add)

      ; Update the arguments for the agents that mark which payoff should be added to each node in back-propagation
      let coalition-proposal-voter-backpropagation update-single-value-nodelist "Coalition proposal voter" coalition-proposal-voter tree traversal-id 1
      if (is-list? coalition-proposal-voter-backpropagation) [set coalition-proposal-voter-backpropagation item 0 coalition-proposal-voter-backpropagation]
      let position-assignment-voter-backpropagation update-single-value-nodelist "Position assignment voter" position-assignment-voter tree traversal-id 1
      if (is-list? position-assignment-voter-backpropagation) [set position-assignment-voter-backpropagation item 0 position-assignment-voter-backpropagation]
      let agenda-setter-backpropagation update-single-value-nodelist "Agenda setter" agenda-setter tree traversal-id 1

      ; Propagate back the result
      set tree backpropagate-tree-nodelist expanded-generation-branch tree coalition-proposal-voter-backpropagation position-assignment-voter-backpropagation agenda-setter-backpropagation
    ] [
      ; Select the expanded nodes
      let nodes-to-add filter [n -> not member? n twig-generation-branch] expanded-generation-branch

      ; Remove the nodes that have been expanded
      let new-ids map [n -> item 0 n] nodes-to-add
      set tree filter [n -> not member? (item 0 n) new-ids] tree

      ; Add the expanded nodes back in in their expanded form
      set tree (sentence tree nodes-to-add)

      ; Select a random one of the newly added unvisited nodes to run a playout from
      let playout-id item 0 one-of filter [n -> empty? item 5 n] nodes-to-add

      ; Run a playout
      let playout playout-branch-nodelist (extract-branch tree playout-id) current-coalition position-assignment-arg power-levels-1 power-levels-2 out-network-matrix-arg potential-agenda-setters agenda-setter coalition-proposal potential-coalition-proposal-voters coalition-proposal-voter coalition-proposal-decision position-assignment-proposal-arg potential-position-assignment-voters position-assignment-voter position-assignment-decision initial-coalition-arg residual-power-levels-arg power-transfer-constant max-power-transfer-parameter power-transfer-error-parameter client-to-patron-payoff-transfer-constant patron-to-client-payoff-transfer-constant positions-power-levels agent-types-arg reward-thresholds-arg

      ; Update the arguments for the agents that mark which payoff should be added to each node in back-propagation
      let coalition-proposal-voter-backpropagation update-single-value-nodelist "Coalition proposal voter" coalition-proposal-voter tree playout-id 1
      if (is-list? coalition-proposal-voter-backpropagation) [set coalition-proposal-voter-backpropagation item 0 coalition-proposal-voter-backpropagation]
      let position-assignment-voter-backpropagation update-single-value-nodelist "Position assignment voter" position-assignment-voter tree playout-id 1
      if (is-list? position-assignment-voter-backpropagation) [set position-assignment-voter-backpropagation item 0 position-assignment-voter-backpropagation]
      let agenda-setter-backpropagation update-single-value-nodelist "Agenda setter" agenda-setter tree playout-id 1

      ; Propagate back the result
      set tree backpropagate-tree-nodelist playout tree coalition-proposal-voter-backpropagation position-assignment-voter-backpropagation agenda-setter-backpropagation
    ]

    ; When one trial is completed, increment the trial counter
    set counter counter + 1
    if (debug-display?) [
      print (word "Trail " counter " completed")
      print "The tree is currently:"
      print tree
      print "The branch that has been expanded is:"
      print twig-generation-branch
      print "From node"
      print traversal-id
      print "------------------------------------------------------------"
    ]
  ]

  ; After running N trials, report the tree
  report tree
end

; Expand a single node in a tree structure
to-report expand-node-nodelist [current-tree current-coalition position-assignment-arg power-levels-1 power-levels-2 out-network-matrix-arg potential-agenda-setters agenda-setter coalition-proposal potential-coalition-proposal-voters coalition-proposal-voter coalition-proposal-decision position-assignment-proposal-arg potential-position-assignment-voters position-assignment-voter position-assignment-decision initial-coalition-arg residual-power-levels-arg power-transfer-constant max-power-transfer-parameter power-transfer-error-parameter client-to-patron-payoff-transfer-constant patron-to-client-payoff-transfer-constant positions-power-levels agent-types-arg reward-thresholds-arg]
  ; Create a temporary copy of the tree and use that for extracting nodes to prevent faulty indexing due to expanding the non-temporary tree
  let current-tree-temp current-tree

  ; If there are nodes that can be expanded, select down to those...
  foreach filter [n -> empty? item 5 n] current-tree-temp [n ->

    ; Create an ID variable, as that will be used in any case
    let id id_generation

    ; If expanding a coalition proposal decision...
    if (item 1 n = "Coalition proposal decision") [

      ; Update the coalition proposal decision, current coalition, and coalition proposal to be in line with the current position in the tree
      let coalition-proposal-decision-latest update-single-value-nodelist "Coalition proposal decision" coalition-proposal-decision current-tree item 0 n 1
      let current-coalition-latest update-single-value-nodelist "Coalition" current-coalition current-tree item 0 n 1
      let coalition-proposal-latest update-single-value-nodelist "Coalition proposal" coalition-proposal current-tree item 0 n 1

      ; Update the current coalition argument to accord with the proposal decision
      set current-coalition-latest ifelse-value (coalition-proposal-decision-latest = "Accepted") [coalition-proposal-latest] [current-coalition-latest]

      ; Update the ID for the resulting coalition if required
      while [member? id map [t -> item 0 t] current-tree] [
        set id id_generation
      ]

      ; Update the lists of potential voters in accordance with the realised coalition
      set potential-coalition-proposal-voters current-coalition-latest
      set potential-position-assignment-voters current-coalition-latest

      ; Add the realised coalition to the tree
      set current-tree lput (list id "Coalition" current-coalition-latest 0 0 []) current-tree

      ; Mark the realised coalition as a child node of the coalition proposal decision
      let n-index position n current-tree
      set n replace-item 5 n (lput id (item 5 n))
      set current-tree replace-item n-index current-tree n
    ]
    ; If expanding a realised coalition...
    if (item 1 n = "Coalition") [

      ; Update the patronage network, in case it is dynamic and was changed by the coalition's development
      let out-network-matrix-latest update-single-value-nodelist "Patronage network" out-network-matrix-arg current-tree item 0 n 1

      ; Generate an ID for the updated patronage network matrix
      while [member? id map [t -> item 0 t] current-tree] [
        set id id_generation
      ]

      ; Add the updated patronage network to the tree
      set current-tree lput (list id "Patronage network" out-network-matrix-latest 0 0 []) current-tree

      ; Mark the updated patronage network as a child node of the realised coalition
      let n-index position n current-tree
      set n replace-item 5 n (lput id (item 5 n))
      set current-tree replace-item n-index current-tree n
    ]
    ; If expanding a patronage network...
    if (item 1 n = "Patronage network") [

      ; Update the power levels to update, position assignment, current coalition and patronage matrix to reflect the current position in the tree
      let position-assignment-latest update-single-value-nodelist"Position assignment"position-assignment-arg current-tree item 0 n 1
      let current-coalition-latest update-single-value-nodelist "Coalition" current-coalition current-tree item 0 n 1
      let out-network-matrix-latest update-single-value-nodelist "Patronage network" out-network-matrix-arg current-tree item 0 n 1

      ; Update the stage I power levels
      let residual-positional-summed-power residual-power-levels-arg

      (foreach position-assignment-latest positions-power-levels [[a p] ->
        set residual-positional-summed-power replace-item a  residual-positional-summed-power (item a residual-positional-summed-power + p)
      ])
      let coalition-membership-list map [m -> ifelse-value (member? m current-coalition-latest) [1] [0]] initial-coalition-arg

      let power-levels-1-latest[]

      ifelse (one-step-power-transfers?) [
        set power-levels-1-latest robustness-check-calculate-network-power residual-positional-summed-power coalition-membership-list out-network-matrix-latest power-transfer-constant max-power-transfer-parameter
      ] [
        set power-levels-1-latest calculate-network-power residual-positional-summed-power coalition-membership-list out-network-matrix-latest power-transfer-constant max-power-transfer-parameter power-transfer-error-parameter
      ]

      ; Generate an ID for the updated stage I power levels
      while [member? id map [t -> item 0 t] current-tree] [
        set id id_generation
      ]

      ; Add the updated power levels to the tree
      set current-tree lput (list id "Power levels (Stage I)" power-levels-1-latest 0 0 []) current-tree

      ; Mark the updated patronage network as a child node of the patronage network
      let n-index position n current-tree
      set n replace-item 5 n (lput id (item 5 n))
      set current-tree replace-item n-index current-tree n
    ]
    ; If expanding stage I power levels...
    if (item 1 n = "Power levels (Stage I)") [

      ; Update the current coalition to reflect the current position in the tree
      let current-coalition-latest update-single-value-nodelist "Coalition" current-coalition current-tree item 0 n 1

      ; Create a list of position assignment proposals
      let position-assignment-proposal-possibilities generate-sequences-with-replacement length positions-power-levels current-coalition-latest[]

      ; Add a branch for each of the potential position assignment proposals
      foreach position-assignment-proposal-possibilities[PAP ->
        ; Generate an ID for the position assignment proposal
        while [member? id map [t -> item 0 t] current-tree] [
          set id id_generation
        ]

        ; Add the position assignment proposal to the tree
        set current-tree lput (list id "Position assignment proposal" PAP 0 0 []) current-tree

        ; Mark the position assignment proposal as a child node of the patronage network
        let n-index position n current-tree
        set n replace-item 5 n (lput id (item 5 n))
        set current-tree replace-item n-index current-tree n
      ]
    ]
    ; If expanding a position assignment proposal...
    if (item 1 n = "Position assignment proposal") [

      ; Update the list of remaining voters and current coalition to reflect the current position in the tree
      let remaining-position-proposal-voters update-remaining-voters-nodelist initial-coalition-arg "Position proposal voter"current-tree item 0 n
      let current-coalition-latest update-single-value-nodelist "Coalition" current-coalition current-tree item 0 n 1

      ; Systematically go through each potential voter and each vote it could cast, and add a branch for each one
      foreach remaining-position-proposal-voters[V ->
        ; If the voter is not a member of the current coalition...
        ifelse (not member? V current-coalition-latest) [
          ; Generate an ID for the voter
          while [member? id map [t -> item 0 t] current-tree] [
            set id id_generation
          ]

          ; Add the voter to the tree with a "Not in the coalition" vote
          set current-tree lput (list id "Position proposal voter" (list V "Not in coalition") 0 0 []) current-tree

          ; Mark the voting decision as a child node of the position assignment proposal
          let n-index position n current-tree
          set n replace-item 5 n (lput id (item 5 n))
          set current-tree replace-item n-index current-tree n
        ] [
          ; Otherwise, go through each possible voting decision...
          foreach (list "In favour" "Against") [D ->
            ; Generate an ID for the voter
            while [member? id map [t -> item 0 t] current-tree] [
              set id id_generation
            ]

            ; Add the voter to the tree with the voting decision
            set current-tree lput (list id "Position proposal voter" (list V D) 0 0 []) current-tree

            ; Mark the voting decision as a child node of the position assignment proposal
            let n-index position n current-tree
            set n replace-item 5 n (lput id (item 5 n))
            set current-tree replace-item n-index current-tree n
          ]
        ]
      ]
    ]
    ; If expanding a voting decision node...
    if (item 1 n = "Position proposal voter") [
      ; Update the list of remaining voters and current coalition to reflect the current position in the tree
      let remaining-position-proposal-voters update-remaining-voters-nodelist initial-coalition-arg "Position proposal voter"current-tree item 0 n
      let current-coalition-latest update-single-value-nodelist "Coalition" current-coalition current-tree item 0 n 1

      ; If there are still voters to add...
      ifelse (not empty? remaining-position-proposal-voters) [
        ; Systematically go through each potential voter and each vote it could cast, and add a branch for each one
        foreach remaining-position-proposal-voters[V ->
          ; If the voter is not a member of the current coalition...
          ifelse (not member? V current-coalition-latest) [
            ; Generate an ID for the voter
            while [member? id map [t -> item 0 t] current-tree] [
              set id id_generation
            ]

            ; Add the voter to the tree with a "Not in the coalition" vote
            set current-tree lput (list id "Position proposal voter" (list V "Not in coalition") 0 0 []) current-tree

            ; Mark the voting decision as a child node of the position assignment proposal
            let n-index position n current-tree
            set n replace-item 5 n (lput id (item 5 n))
            set current-tree replace-item n-index current-tree n
          ] [
            ; Otherwise, go through each possible voting decision...
            foreach (list "In favour" "Against") [D ->
              ; Generate an ID for the voter
              while [member? id map [t -> item 0 t] current-tree] [
                set id id_generation
              ]

              ; Add the voter to the tree with the voting decision
              set current-tree lput (list id "Position proposal voter" (list V D) 0 0 []) current-tree

              ; Mark the voting decision as a child node of the position assignment proposal
              let n-index position n current-tree
              set n replace-item 5 n (lput id (item 5 n))
              set current-tree replace-item n-index current-tree n
            ]
          ]
        ]
      ] [
        ; If there are no further voters to add...
        ; Update the Stage I power levels, position assignment proposal and actual position assignment to reflect the current position in the tree
        let power-levels-1-latest update-single-value-nodelist "Power levels (Stage I)" power-levels-1 current-tree item 0 n 1
        let position-assignment-proposal-latest update-single-value-nodelist "Position assignment proposal" position-assignment-proposal-arg current-tree item 0 n 1
        let position-assignment-latest update-single-value-nodelist "Position assignment" position-assignment-arg current-tree item 0 n 1

        ; Start listing the votes that have been cast in this branch and their voters by setting a starting point in traversing up the tree and setting an empty
        ; list of voters and their votes
        let parent-node n
        let votes-cast []

        ; Fill in the list by adding the voter and vote of ancestor nodes as long as there are voters in the same batch of votes
        while [item 1 parent-node = "Position proposal voter"] [
          set votes-cast lput (item 2 parent-node) votes-cast
          set parent-node first filter [t -> member? (item 0 parent-node) item 5 t] current-tree
        ]

        ; Derive a position proposal decision
        let position-assignment-decision-latest ifelse-value (
          (sum (map [v ->
            ifelse-value ((item 1 v) = "In favour") [item (item 0 v) power-levels-1-latest] [0]
            ] votes-cast
            )
          ) >
          (winning-coalition-size * (sum (map [v ->
            ifelse-value ((item 1 v) != "Not in coalition") [item (item 0 v) power-levels-1-latest] [0]
            ] votes-cast
            )
            )
          )
          ) ["Accepted"] ["Rejected"]


        ; Generate an ID for the position proposal decision
        while [member? id map [t -> item 0 t] current-tree] [
          set id id_generation
        ]

        ; Add the proposal decision to the tree
        set current-tree lput (list id "Position proposal decision" position-assignment-decision-latest 0 0 []) current-tree

        ; Mark the proposal decision as a child node of the voting decision
        let n-index position n current-tree
        set n replace-item 5 n (lput id (item 5 n))
        set current-tree replace-item n-index current-tree n

        ; Update the position assignment argument to be in line with the proposal decision just taken
        set position-assignment-latest ifelse-value (position-assignment-decision-latest = "Accepted") [position-assignment-proposal-latest] [position-assignment-latest]

        ; Generate an ID for the position assignment
        while [member? id map [t -> item 0 t] current-tree] [
          set id id_generation
        ]

        ; Mark the position assignment as a child node of the position proposal decision
        set current-tree replace-item (length current-tree - 1) current-tree replace-item 5 item (length current-tree - 1) current-tree lput id (item 5 item (length current-tree - 1) current-tree)

        ; Add the position assignment to the tree
        set current-tree lput (list id "Position assignment" position-assignment-latest 0 0 []) current-tree
      ]
    ]
    ; If expanding a position assignment node...
    if (item 1 n = "Position assignment") [
      ; Update the patronage network matrix, position assignment, and current coalition to reflect the current position in the tree
      let out-network-matrix-latest update-single-value-nodelist "Patronage network" out-network-matrix-arg current-tree item 0 n 1
      let position-assignment-latest update-single-value-nodelist "Position assignment"position-assignment-arg current-tree item 0 n 1
      let current-coalition-latest update-single-value-nodelist "Coalition"current-coalition current-tree item 0 n 1

      ; Update the stage II power levels
      let residual-positional-summed-power residual-power-levels-arg

      (foreach position-assignment-latest positions-power-levels[[a p] ->
        set residual-positional-summed-power replace-item a  residual-positional-summed-power (item a residual-positional-summed-power + p)
      ])
      let coalition-membership-list map [m -> ifelse-value (member? m current-coalition-latest) [1] [0]] initial-coalition-arg

      let power-levels-2-latest []

      ifelse (one-step-power-transfers?) [
        set power-levels-2-latest robustness-check-calculate-network-power residual-positional-summed-power coalition-membership-list out-network-matrix-latest power-transfer-constant max-power-transfer-parameter
      ] [
        set power-levels-2-latest calculate-network-power residual-positional-summed-power coalition-membership-list out-network-matrix-latest power-transfer-constant max-power-transfer-parameter power-transfer-error-parameter
      ]

      ; Generate an ID for the stage II power levels
      while [member? id map [t -> item 0 t] current-tree] [
        set id id_generation
      ]

      ; Add the updated power levels to the branch
      set current-tree lput (list id "Power levels (Stage II)" power-levels-2-latest 0 0 []) current-tree

      ; Mark the updated power levels as a child node of the position assignment
      let n-index position n current-tree
      set n replace-item 5 n (lput id (item 5 n))
      set current-tree replace-item n-index current-tree n
    ]
    ; If expanding a set of stage II power levels...
    if (item 1 n = "Power levels (Stage II)") [
      ; Update the current coalition, preceding coalition, coalition proposal, coalition proposal decision, and list of potential agenda setters to reflect the current position in the tree
      let current-coalition-latest update-single-value-nodelist "Coalition" current-coalition current-tree item 0 n 1
      let current-coalition-one-before update-single-value-nodelist"Coalition" current-coalition current-tree item 0 n 2
      let coalition-proposal-latest update-single-value-nodelist "Coalition proposal" coalition-proposal current-tree item 0 n 1
      let coalition-proposal-decision-latest update-single-value-nodelist "Coalition proposal decision" coalition-proposal-decision current-tree item 0 n 1
      let potential-agenda-setters-latest update-potential-agenda-setters-nodelist potential-agenda-setters current-coalition-latest current-tree item 0 n

      ; If a stopping condition has been reached, mark that on the tree and report the resulting branch
      ifelse ((coalition-proposal-decision-latest = "Accepted" and current-coalition-one-before = current-coalition-latest) or (coalition-proposal-decision-latest = "Rejected" and empty? potential-agenda-setters-latest)) [
        let power-levels-2-latest update-single-value-nodelist "Power levels (Stage II)" power-levels-2 current-tree item 0 n 1
        let out-network-matrix-latest update-single-value-nodelist "Patronage network" out-network-matrix-arg current-tree item 0 n 1
        let current-branch extract-branch current-tree item 0 n

        ; Generate an ID for the stopping condition
        while [member? id map [t -> item 0 t] current-branch] [
          set id id_generation
        ]

        ; Calculate payoffs, starting with an empty list to avoid problems with payoffs only being defined within the if-else clause
        let payoffs []

        ; Calculate the payoffs in the way that corresponds with whether or not the effect of payoff transfers is checked for robustness
        ifelse (constant-sum-payoff-transfers?) [
          set payoffs robustness-check-calculate-payoff-branch-nodelist current-branch current-coalition-latest initial-coalition-arg power-levels-2-latest out-network-matrix-latest item 0 n client-to-patron-payoff-transfer-constant patron-to-client-payoff-transfer-constant agent-types-arg reward-thresholds-arg
        ] [
          set payoffs calculate-payoff-branch-nodelist current-branch current-coalition-latest initial-coalition-arg power-levels-2-latest out-network-matrix-latest item 0 n client-to-patron-payoff-transfer-constant patron-to-client-payoff-transfer-constant agent-types-arg reward-thresholds-arg
        ]

        ; Add the stopping condition to the tree
        set current-tree lput (list id "Payoff node" payoffs 0 0 ["STOP"]) current-branch

        ; Mark the stopping condition as a child node of the stage II power levels
        let n-index position n current-tree
        set n replace-item 5 n (lput id (item 5 n))
        set current-tree replace-item n-index current-tree n
      ] [
        ; Otherwise, for each viable agenda setter...
        foreach potential-agenda-setters-latest[A ->
          ; Generate an ID
          while [member? id map [t -> item 0 t] current-tree] [
            set id id_generation
          ]

          ; Add the agenda setter to the tree
          set current-tree lput (list id "Agenda setter" A 0 0 []) current-tree

          ; Mark the agenda setter as a child node of the stage II power levels
          let n-index position n current-tree
          set n replace-item 5 n (lput id (item 5 n))
          set current-tree replace-item n-index current-tree n
        ]
      ]
    ]
    ; If expanding an agenda setter...
    if (item 1 n = "Agenda setter") [
      ; Update the current coalition and agenda setter to reflect the current position in the tree
      let current-coalition-latest update-single-value-nodelist "Coalition" current-coalition current-tree item 0 n 1
      let agenda-setter-latest update-single-value-nodelist "Agenda setter" agenda-setter current-tree item 0 n 1

      ; Generate a list of all coalition proposals that the agenda setter could do
      let potential-coalition-proposals filter [s -> member? agenda-setter-latest s] turn-list-numbers-to-coalition-whos current-coalition-latest

      ; For each possible coalition proposal...
      foreach potential-coalition-proposals[PCP ->
        ; Generate an ID for the coalition proposal
        while [member? id map [t -> item 0 t] current-tree] [
          set id id_generation
        ]

        ; Add the coalition proposal to the tree
        set current-tree lput (list id "Coalition proposal" PCP 0 0 []) current-tree

        ; Mark the coalition proposal as a child node of the agenda setter
        let n-index position n current-tree
        set n replace-item 5 n (lput id (item 5 n))
        set current-tree replace-item n-index current-tree n
      ]
    ]
    ; If expanding a coalition proposal...
    if (item 1 n = "Coalition proposal") [
      ; Update the list of remaining voters and the current coalitionto reflect the current position in the tree
      let potential-coalition-proposal-voters-latest update-remaining-voters-nodelist initial-coalition-arg "Coalition proposal voter" current-tree item 0 n
      let current-coalition-latest update-single-value-nodelist "Coalition" current-coalition current-tree item 0 n 1

      ; Systematically go through each potential voter and each vote it could cast...
      foreach potential-coalition-proposal-voters-latest [V ->
        ; If the potential voter is not a member of the current coalition...
        ifelse (not member? V current-coalition-latest) [
          ; Generate an ID for the voter
          while [member? id map [t -> item 0 t] current-tree] [
            set id id_generation
          ]

          ; Add the voter to the tree with a 'vote' marking that it is not in the coalition and cannot vote
          set current-tree lput (list id "Coalition proposal voter" (list V "Not in coalition") 0 0 []) current-tree

          ; Mark the voter and voting decision as a child node of the coalition proposal
          let n-index position n current-tree
          set n replace-item 5 n (lput id (item 5 n))
          set current-tree replace-item n-index current-tree n
        ] [
          ; If the potential voter is a member of the current coalition, consider both a vote in favour and a vote against...
          foreach (list "In favour" "Against") [D ->
            ; Generate an ID for the voter
            while [member? id map [t -> item 0 t] current-tree] [
              set id id_generation
            ]

            ; Add the voter and its decision to the tree
            set current-tree lput (list id "Coalition proposal voter" (list V D) 0 0 []) current-tree

            ; Mark the voter and voting decision as a child node of the coalition proposal
            let n-index position n current-tree
            set n replace-item 5 n (lput id (item 5 n))
            set current-tree replace-item n-index current-tree n
          ]
        ]
      ]
    ]
    ; If expanding from a coalition proposal voter...
    if (item 1 n = "Coalition proposal voter") [
      ; Update the list of remaining voters, the current coalition, the stage II power levels and the coalition proposal decision
      let remaining-coalition-proposal-voters update-remaining-voters-nodelist initial-coalition-arg "Coalition proposal voter"current-tree item 0 n
      let current-coalition-latest update-single-value-nodelist "Coalition" current-coalition current-tree item 0 n 1
      let power-levels-2-latest update-single-value-nodelist "Power levels (Stage II)" power-levels-2 current-tree item 0 n 1
      let coalition-proposal-decision-latest update-single-value-nodelist "Coalition proposal decision" coalition-proposal-decision current-tree item 0 n 1

      ; If the list of remaining coalition proposal voters is not empty...
      ifelse (not empty? remaining-coalition-proposal-voters) [
        ; Systematically go through each remaining voter and each vote it could cast...
        foreach remaining-coalition-proposal-voters[V ->
          ; If the potential voter is not a member of the current coalition...
          ifelse (not member? V current-coalition-latest) [
            ; Generate an ID for the voter
            while [member? id map [t -> item 0 t] current-tree] [
              set id id_generation
            ]

            ; Add the voter to the tree with a 'vote' marking that it is not in the coalition and cannot vote
            set current-tree lput (list id "Coalition proposal voter" (list V "Not in coalition") 0 0 []) current-tree

            ; Mark the voter and voting decision as a child node of the coalition proposal
            let n-index position n current-tree
            set n replace-item 5 n (lput id (item 5 n))
            set current-tree replace-item n-index current-tree n
          ] [
            ; If the potential voter is a member of the current coalition, consider both a vote in favour and a vote against...
            foreach (list "In favour" "Against") [D ->
              ; Generate an ID for the voter
              while [member? id map [t -> item 0 t] current-tree] [
                set id id_generation
              ]

              ; Add the voter and its decision to the tree
              set current-tree lput (list id "Coalition proposal voter" (list V D) 0 0 []) current-tree

              ; Mark the voter and voting decision as a child node of the coalition proposal
              let n-index position n current-tree
              set n replace-item 5 n (lput id (item 5 n))
              set current-tree replace-item n-index current-tree n
            ]
          ]
        ]
      ] [
        ; If expanding from a coalition proposal voter and the list of coalition proposal voters is empty...
        ; Start listing the votes that have been cast in this branch and their voters by setting a starting point in traversing up the tree and setting an empty
        ; list of voters and their votes
        let parent-node n
        let votes-cast []

        ; Fill in the list by adding the voter and vote of ancestor nodes as long as there are voters in the same batch of votes
        while [item 1 parent-node = "Coalition proposal voter"] [
          set votes-cast lput (item 2 parent-node) votes-cast
          set parent-node first filter [t -> member? (item 0 parent-node) item 5 t] current-tree
        ]

        ; Use the votes cast and their voters' power to derive a voting decision
        set coalition-proposal-decision-latest ifelse-value (
          (sum (map [v ->
            ifelse-value ((item 1 v) = "In favour") [item (item 0 v) power-levels-2-latest] [0]
            ] votes-cast
            )
          ) >
          (winning-coalition-size * (sum (map [v ->
            ifelse-value ((item 1 v) != "Not in coalition") [item (item 0 v) power-levels-2-latest] [0]
            ] votes-cast
            )
            )
          )
          ) ["Accepted"] ["Rejected"]

        ; Generate an ID for the coalition proposal decision
        while [member? id map [t -> item 0 t] current-tree] [
          set id id_generation
        ]

        ; Add the coalition proposal decision to the branch
        set current-tree lput (list id "Coalition proposal decision" coalition-proposal-decision-latest 0 0 []) current-tree

        ; Mark the coalition proposal decision as a child node of the coalition proposal voter
        let n-index position n current-tree
        set n replace-item 5 n (lput id (item 5 n))
        set current-tree replace-item n-index current-tree n
      ]
    ]
  ]

  ; Once the node is expanded, report the tree
  report current-tree
end

; Play out a branch in a tree structure from its current state to an end state
to-report playout-branch-nodelist [current-branch current-coalition position-assignment-arg power-levels-1 power-levels-2 out-network-matrix-arg potential-agenda-setters agenda-setter coalition-proposal potential-coalition-proposal-voters coalition-proposal-voter coalition-proposal-decision position-assignment-proposal-arg potential-position-assignment-voters position-assignment-voter position-assignment-decision initial-coalition-arg residual-power-levels-arg power-transfer-constant max-power-transfer-parameter power-transfer-error-parameter client-to-patron-payoff-transfer-constant patron-to-client-payoff-transfer-constant positions-power-levels agent-types-arg reward-thresholds-arg]
  ; Initialise the debugging counter
  let counter 0

  ; Grow the branch as long as there is a node at the end that can be expanded
  while [not empty? filter [n -> empty? item 5 n] current-branch] [
    ; Copy the current branch object and use that for safely extracting information about the branch without using the nodes that are being added
    let current-branch-temp current-branch

    ; If there are nodes that can be expanded, select down to those...
    foreach filter [n -> empty? item 5 n] current-branch-temp [n ->

      ; Create an ID variable, as that will be used in any case
      let id id_generation

      ; If expanding a coalition proposal decision...
      if (item 1 n = "Coalition proposal decision") [

        ; Update the coalition proposal decision, current coalition, and coalition proposal to be in line with the current position in the tree
        let coalition-proposal-decision-latest update-single-value-nodelist "Coalition proposal decision" coalition-proposal-decision current-branch item 0 n 1
        let current-coalition-latest update-single-value-nodelist "Coalition" current-coalition current-branch item 0 n 1
        let coalition-proposal-latest update-single-value-nodelist "Coalition proposal" coalition-proposal current-branch item 0 n 1

        ; Update the current coalition argument to accord with the proposal decision
        set current-coalition-latest ifelse-value (coalition-proposal-decision-latest = "Accepted") [coalition-proposal-latest] [current-coalition-latest]

        ; Update the ID for the resulting coalition if required
        while [member? id map [t -> item 0 t] current-branch] [
          set id id_generation
        ]

        ; Update the lists of potential voters in accordance with the realised coalition
        set potential-coalition-proposal-voters current-coalition-latest
        set potential-position-assignment-voters current-coalition-latest

        ; Add the realised coalition to the tree
        set current-branch lput (list id "Coalition" current-coalition-latest 0 0 []) current-branch

        ; Mark the realised coalition as a child node of the coalition proposal decision
        let n-index position n current-branch
        set n replace-item 5 n (lput id (item 5 n))
        set current-branch replace-item n-index current-branch n
      ]
      ; If expanding a realised coalition...
      if (item 1 n = "Coalition") [

        ; Update the patronage network, in case it is dynamic and was changed by the coalition's development
        let out-network-matrix-latest update-single-value-nodelist "Patronage network" out-network-matrix-arg current-branch item 0 n 1

        ; Generate an ID for the updated patronage network matrix
        while [member? id map [t -> item 0 t] current-branch] [
          set id id_generation
        ]

        ; Add the updated patronage network to the tree
        set current-branch lput (list id "Patronage network" out-network-matrix-latest 0 0 []) current-branch

        ; Mark the updated patronage network as a child node of the realised coalition
        let n-index position n current-branch
        set n replace-item 5 n (lput id (item 5 n))
        set current-branch replace-item n-index current-branch n
      ]
      ; If expanding a patronage network...
      if (item 1 n = "Patronage network") [

        ; Update the power levels to update, position assignment, current coalition and patronage matrix to reflect the current position in the tree
        let position-assignment-latest update-single-value-nodelist"Position assignment"position-assignment-arg current-branch item 0 n 1
        let current-coalition-latest update-single-value-nodelist "Coalition" current-coalition current-branch item 0 n 1
        let out-network-matrix-latest update-single-value-nodelist "Patronage network" out-network-matrix-arg current-branch item 0 n 1

        ; Update the stage I power levels
        let residual-positional-summed-power residual-power-levels-arg

        (foreach position-assignment-latest positions-power-levels [[a p] ->
          set residual-positional-summed-power replace-item a  residual-positional-summed-power (item a residual-positional-summed-power + p)
        ])
        let coalition-membership-list map [m -> ifelse-value (member? m current-coalition-latest) [1] [0]] initial-coalition-arg

        let power-levels-1-latest[]
        ifelse (one-step-power-transfers?) [
          set power-levels-1-latest robustness-check-calculate-network-power residual-positional-summed-power coalition-membership-list out-network-matrix-latest power-transfer-constant max-power-transfer-parameter
        ] [
          set power-levels-1-latest calculate-network-power residual-positional-summed-power coalition-membership-list out-network-matrix-latest power-transfer-constant max-power-transfer-parameter power-transfer-error-parameter
        ]


        ; Generate an ID for the updated stage I power levels
        while [member? id map [t -> item 0 t] current-branch] [
          set id id_generation
        ]

        ; Add the updated power levels to the tree
        set current-branch lput (list id "Power levels (Stage I)" power-levels-1-latest 0 0 []) current-branch

        ; Mark the updated patronage network as a child node of the patronage network
        let n-index position n current-branch
        set n replace-item 5 n (lput id (item 5 n))
        set current-branch replace-item n-index current-branch n
      ]
      ; If expanding stage I power levels...
      if (item 1 n = "Power levels (Stage I)") [

        ; Update the current coalition to reflect the current position in the tree
        let current-coalition-latest update-single-value-nodelist "Coalition" current-coalition current-branch item 0 n 1

        ; Choose a random item in the list of possible position assignment proposals
        let random-position-assignment-proposal one-of generate-sequences-with-replacement length positions-power-levels current-coalition-latest[]

        ; Generate an ID for the position assignment proposal
        while [member? id map [t -> item 0 t] current-branch] [
          set id id_generation
        ]

        ; Add the position assignment proposal to the tree
        set current-branch lput (list id "Position assignment proposal" random-position-assignment-proposal 0 0 []) current-branch

        ; Mark the position assignment proposal as a child node of the patronage network
        let n-index position n current-branch
        set n replace-item 5 n (lput id (item 5 n))
        set current-branch replace-item n-index current-branch n
      ]
      ; If expanding a position assignment proposal...
      if (item 1 n = "Position assignment proposal") [

        ; Update the list of remaining voters and current coalition to reflect the current position in the tree
        let remaining-position-proposal-voters update-remaining-voters-nodelist initial-coalition-arg "Position proposal voter"current-branch item 0 n
        let current-coalition-latest update-single-value-nodelist "Coalition" current-coalition current-branch item 0 n 1

        ; Select a random voter and voting decision from the remaining voters and their possible voting decisions
        let V one-of remaining-position-proposal-voters
        let D one-of (list "In favour" "Against")

        ; If the voter is not a member of the current coalition...
        ifelse (not member? V current-coalition-latest) [
          ; Generate an ID for the voter
          while [member? id map [t -> item 0 t] current-branch] [
            set id id_generation
          ]

          ; Add the voter to the tree with a "Not in the coalition" vote
          set current-branch lput (list id "Position proposal voter" (list V "Not in coalition")  0 0 []) current-branch

          ; Mark the voting decision as a child node of the position assignment proposal
          let n-index position n current-branch
          set n replace-item 5 n (lput id (item 5 n))
          set current-branch replace-item n-index current-branch n
        ] [
          ; Otherwise, choose a random possible voting decision...

          ; Generate an ID for the voter
          while [member? id map [t -> item 0 t] current-branch] [
            set id id_generation
          ]

          ; Add the voter to the tree with the voting decision
          set current-branch lput (list id "Position proposal voter" (list V D) 0 0 []) current-branch

          ; Mark the voting decision as a child node of the position assignment proposal
          let n-index position n current-branch
          set n replace-item 5 n (lput id (item 5 n))
          set current-branch replace-item n-index current-branch n
        ]
      ]
      ; If expanding a voting decision node...
      if (item 1 n = "Position proposal voter") [

        ; Update the list of remaining voters and current coalition to reflect the current position in the tree
        let remaining-position-proposal-voters update-remaining-voters-nodelist initial-coalition-arg "Position proposal voter"current-branch item 0 n
        let current-coalition-latest update-single-value-nodelist "Coalition" current-coalition current-branch item 0 n 1

        ; If there are still voters to add...
        ifelse (not empty? remaining-position-proposal-voters) [
          let V one-of remaining-position-proposal-voters
          let D one-of (list "In favour" "Against")

          ; If the voter is not a member of the current coalition...
          ifelse (not member? V current-coalition-latest) [
            ; Generate an ID for the voter
            while [member? id map [t -> item 0 t] current-branch] [
              set id id_generation
            ]

            ; Add the voter to the tree with a "Not in the coalition" vote
            set current-branch lput (list id "Position proposal voter" (list V "Not in coalition") 0 0 []) current-branch

            ; Mark the voting decision as a child node of the position assignment proposal
            let n-index position n current-branch
            set n replace-item 5 n (lput id (item 5 n))
            set current-branch replace-item n-index current-branch n
          ] [
            ; Otherwise, use the random voting decision

            ; Generate an ID for the voter
            while [member? id map [t -> item 0 t] current-branch] [
              set id id_generation
            ]

            ; Add the voter to the tree with the voting decision
            set current-branch lput (list id "Position proposal voter" (list V D) 0 0 []) current-branch

            ; Mark the voting decision as a child node of the position assignment proposal
            let n-index position n current-branch
            set n replace-item 5 n (lput id (item 5 n))
            set current-branch replace-item n-index current-branch n
          ]
        ] [
          ; If there are no further voters to add...
          let power-levels-1-latest update-single-value-nodelist "Power levels (Stage I)" power-levels-1 current-branch item 0 n 1
          let position-assignment-proposal-latest update-single-value-nodelist "Position assignment proposal" position-assignment-proposal-arg current-branch item 0 n 1
          let position-assignment-latest update-single-value-nodelist "Position assignment" position-assignment-arg current-branch item 0 n 1

          ; Start listing the votes that have been cast in this branch and their voters by setting a starting point in traversing up the tree and setting an empty
          ; list of voters and their votes
          let parent-node n
          let votes-cast []

          ; Fill in the list by adding the voter and vote of ancestor nodes as long as there are voters in the same batch of votes
          while [item 1 parent-node = "Position proposal voter"] [
            set votes-cast lput (item 2 parent-node) votes-cast
            set parent-node first filter [t -> member? (item 0 parent-node) item 5 t] current-branch
          ]

          ; Derive a position proposal decision
          let position-assignment-decision-latest ifelse-value (
            (sum (map [v ->
              ifelse-value ((item 1 v) = "In favour") [item (item 0 v) power-levels-1-latest] [0]
              ] votes-cast
              )
            ) >
            (winning-coalition-size * (sum (map [v ->
              ifelse-value ((item 1 v) != "Not in coalition") [item (item 0 v) power-levels-1-latest] [0]
              ] votes-cast
              )
              )
            )
            ) ["Accepted"] ["Rejected"]


          ; Generate an ID for the position proposal decision
          while [member? id map [t -> item 0 t] current-branch] [
            set id id_generation
          ]

          ; Add the proposal decision to the branch
          set current-branch lput (list id "Position proposal decision" position-assignment-decision-latest 0 0 []) current-branch

          ; Mark the proposal decision as a child node of the voting decision
          let n-index position n current-branch
          set n replace-item 5 n (lput id (item 5 n))
          set current-branch replace-item n-index current-branch n

          ; Update the position assignment argument to be in line with the proposal decision just taken
          set position-assignment-latest ifelse-value (position-assignment-decision-latest = "Accepted") [position-assignment-proposal-latest] [position-assignment-latest]

          ; Generate an ID for the position assignment
          while [member? id map [t -> item 0 t] current-branch] [
            set id id_generation
          ]

          ; Mark the position assignment as a child node of the position proposal decision
          set current-branch replace-item (length current-branch - 1) current-branch replace-item 5 item (length current-branch - 1) current-branch lput id (item 5 item (length current-branch - 1) current-branch)

          ; Add the position assignment to the branch
          set current-branch lput (list id "Position assignment" position-assignment-latest 0 0 []) current-branch
        ]
      ]
      ; If expanding a position assignment node...
      if (item 1 n = "Position assignment") [
        ; Update the patronage network matrix, position assignment, and current coalition to reflect the current position in the tree
        let out-network-matrix-latest update-single-value-nodelist "Patronage network" out-network-matrix-arg current-branch item 0 n 1
        let position-assignment-latest update-single-value-nodelist "Position assignment"position-assignment-arg current-branch item 0 n 1
        let current-coalition-latest update-single-value-nodelist "Coalition"current-coalition current-branch item 0 n 1

        ; Update the stage II power levels
        let residual-positional-summed-power residual-power-levels-arg

        (foreach position-assignment-latest positions-power-levels[[a p] ->
          set residual-positional-summed-power replace-item a  residual-positional-summed-power (item a residual-positional-summed-power + p)
        ])
        let coalition-membership-list map [m -> ifelse-value (member? m current-coalition-latest) [1] [0]] initial-coalition-arg

        let power-levels-2-latest[]
        ifelse (one-step-power-transfers?) [
          set power-levels-2-latest robustness-check-calculate-network-power residual-positional-summed-power coalition-membership-list out-network-matrix-latest power-transfer-constant max-power-transfer-parameter
        ] [
          set power-levels-2-latest calculate-network-power residual-positional-summed-power coalition-membership-list out-network-matrix-latest power-transfer-constant max-power-transfer-parameter power-transfer-error-parameter
        ]

        ; Generate an ID for the stage II power levels
        while [member? id map [t -> item 0 t] current-branch] [
          set id id_generation
        ]

        ; Add the updated power levels to the branch
        set current-branch lput (list id "Power levels (Stage II)" power-levels-2-latest 0 0 []) current-branch

        ; Mark the updated power levels as a child node of the position assignment
        let n-index position n current-branch
        set n replace-item 5 n (lput id (item 5 n))
        set current-branch replace-item n-index current-branch n
      ]
      ; If expanding a set of stage II power levels...
      if (item 1 n = "Power levels (Stage II)") [
        ; Update the current coalition, preceding coalition, coalition proposal, coalition proposal decision, and list of potential agenda setters to reflect the current position in the tree
        let current-coalition-latest update-single-value-nodelist "Coalition" current-coalition current-branch item 0 n 1
        let current-coalition-one-before update-single-value-nodelist "Coalition" current-coalition current-branch item 0 n 2
        let coalition-proposal-latest update-single-value-nodelist "Coalition proposal" coalition-proposal current-branch item 0 n 1
        let coalition-proposal-decision-latest update-single-value-nodelist "Coalition proposal decision" coalition-proposal-decision current-branch item 0 n 1
        let potential-agenda-setters-latest update-potential-agenda-setters-nodelist potential-agenda-setters current-coalition-latest current-branch item 0 n

        ; If a stopping condition has been reached, mark that on the tree and report the resulting branch
        ifelse ((coalition-proposal-decision-latest = "Accepted" and current-coalition-one-before = current-coalition-latest) or (coalition-proposal-decision-latest = "Rejected" and empty? potential-agenda-setters-latest)) [
          ; Additionally retrieve the latest Stage II power levels and patronage network matrix
          let power-levels-2-latest update-single-value-nodelist "Power levels (Stage II)" power-levels-2 current-branch item 0 n 1
          let out-network-matrix-latest update-single-value-nodelist "Patronage network" out-network-matrix-arg current-branch item 0 n 1

          ; Generate an ID for the stopping condition
          while [member? id map [t -> item 0 t] current-branch] [
            set id id_generation
          ]

          ; Calculate payoffs, starting with an empty list to avoid problems with payoffs only being defined within the if-else clause
          let payoffs []

          ; Calculate the payoffs in the way that corresponds with whether or not the effect of payoff transfers is checked for robustness using this model run
          ifelse (constant-sum-payoff-transfers?) [
            set payoffs robustness-check-calculate-payoff-branch-nodelist current-branch current-coalition-latest initial-coalition-arg power-levels-2-latest out-network-matrix-latest item 0 n client-to-patron-payoff-transfer-constant patron-to-client-payoff-transfer-constant agent-types-arg reward-thresholds-arg
          ] [
            set payoffs calculate-payoff-branch-nodelist current-branch current-coalition-latest initial-coalition-arg power-levels-2-latest out-network-matrix-latest item 0 n client-to-patron-payoff-transfer-constant patron-to-client-payoff-transfer-constant agent-types-arg reward-thresholds-arg
          ]

          ; Add the stopping condition to the tree
          set current-branch lput (list id "Payoff node" payoffs 0 0 ["STOP"]) current-branch

          ; Mark the stopping condition as a child node of the stage II power levels
          let n-index position n current-branch
          set n replace-item 5 n (lput id (item 5 n))
          set current-branch replace-item n-index current-branch n
        ] [
          ; Otherwise, for a random viable agenda setter...
          let A one-of potential-agenda-setters-latest

          ; Generate an ID
          while [member? id map [t -> item 0 t] current-branch] [
            set id id_generation
          ]

          ; Add the agenda setter to the tree
          set current-branch lput (list id "Agenda setter" A 0 0 []) current-branch

          ; Mark the agenda setter as a child node of the stage II power levels
          let n-index position n current-branch
          set n replace-item 5 n (lput id (item 5 n))
          set current-branch replace-item n-index current-branch n
        ]
      ]
      ; If expanding an agenda setter...
      if (item 1 n = "Agenda setter") [
        ; Update the current coalition and agenda setter to reflect the current position in the tree
        let current-coalition-latest update-single-value-nodelist "Coalition" current-coalition current-branch item 0 n 1
        let agenda-setter-latest update-single-value-nodelist "Agenda setter" agenda-setter current-branch item 0 n 1

        ; Generate a list of all coalition proposals that the agenda setter could do
        let potential-coalition-proposals filter [s -> member? agenda-setter-latest s] turn-list-numbers-to-coalition-whos current-coalition-latest

        ; For each possible coalition proposal...
        let PCP one-of potential-coalition-proposals

        ; Generate an ID for the coalition proposal
        while [member? id map [t -> item 0 t] current-branch] [
          set id id_generation
        ]

        ; Add the coalition proposal to the tree
        set current-branch lput (list id "Coalition proposal" PCP 0 0 []) current-branch

        ; Mark the coalition proposal as a child node of the agenda setter
        let n-index position n current-branch
        set n replace-item 5 n (lput id (item 5 n))
        set current-branch replace-item n-index current-branch n
      ]
      ; If expanding a coalition proposal...
      if (item 1 n = "Coalition proposal") [
        ; Update the list of remaining voters and the current coalition
        let potential-coalition-proposal-voters-latest update-remaining-voters-nodelist initial-coalition-arg "Coalition proposal voter" current-branch item 0 n
        let current-coalition-latest update-single-value-nodelist "Coalition" current-coalition current-branch item 0 n 1

        ; Systematically go through each potential voter and each vote it could cast...
        let V one-of potential-coalition-proposal-voters-latest
        let D one-of (list "In favour" "Against")

        ; If the potential voter is not a member of the current coalition...
        ifelse (not member? V current-coalition-latest) [
          ; Generate an ID for the voter
          while [member? id map [t -> item 0 t] current-branch] [
            set id id_generation
          ]

          ; Add the voter to the tree with a 'vote' marking that it is not in the coalition and cannot vote
          set current-branch lput (list id "Coalition proposal voter" (list V "Not in coalition") 0 0 []) current-branch

          ; Mark the voter and voting decision as a child node of the coalition proposal
          let n-index position n current-branch
          set n replace-item 5 n (lput id (item 5 n))
          set current-branch replace-item n-index current-branch n
        ] [
          ; If the potential voter is a member of the current coalition, consider both a vote in favour and a vote against...
            ; Generate an ID for the voter
            while [member? id map [t -> item 0 t] current-branch] [
              set id id_generation
            ]

            ; Add the voter and its decision to the tree
            set current-branch lput (list id "Coalition proposal voter" (list V D) 0 0 []) current-branch

            ; Mark the voter and voting decision as a child node of the coalition proposal
            let n-index position n current-branch
            set n replace-item 5 n (lput id (item 5 n))
            set current-branch replace-item n-index current-branch n
        ]
      ]
      ; If expanding from a coalition proposal voter...
      if (item 1 n = "Coalition proposal voter") [
        ; Update the list of remaining voters, the current coalition, the stage II power levels and the coalition proposal decision
        let remaining-coalition-proposal-voters update-remaining-voters-nodelist initial-coalition-arg "Coalition proposal voter"current-branch item 0 n
        let current-coalition-latest update-single-value-nodelist "Coalition" current-coalition current-branch item 0 n 1
        let power-levels-2-latest update-single-value-nodelist "Power levels (Stage II)" power-levels-2 current-branch item 0 n 1
        let coalition-proposal-decision-latest update-single-value-nodelist "Coalition proposal decision" coalition-proposal-decision current-branch item 0 n 1

        ; If the list of remaining coalition proposal voters is not empty...
        ifelse (not empty? remaining-coalition-proposal-voters) [
          let V one-of remaining-coalition-proposal-voters
          let D one-of (list "In favour" "Against")

          ; If the potential voter is not a member of the current coalition...
          ifelse (not member? V current-coalition-latest) [
            ; Generate an ID for the voter
            while [member? id map [t -> item 0 t] current-branch] [
              set id id_generation
            ]

            ; Add the voter to the tree with a 'vote' marking that it is not in the coalition and cannot vote
            set current-branch lput (list id "Coalition proposal voter" (list V "Not in coalition") 0 0 []) current-branch

            ; Mark the voter and voting decision as a child node of the coalition proposal
            let n-index position n current-branch
            set n replace-item 5 n (lput id (item 5 n))
            set current-branch replace-item n-index current-branch n
          ] [
            ; If the potential voter is a member of the current coalition, consider both a vote in favour and a vote against...
              ; Generate an ID for the voter
              while [member? id map [t -> item 0 t] current-branch] [
                set id id_generation
              ]

              ; Add the voter and its decision to the tree
              set current-branch lput (list id "Coalition proposal voter" (list V D) 0 0 []) current-branch

              ; Mark the voter and voting decision as a child node of the coalition proposal
              let n-index position n current-branch
              set n replace-item 5 n (lput id (item 5 n))
              set current-branch replace-item n-index current-branch n
          ]
        ] [
          ; If expanding from a coalition proposal voter and the list of coalition proposal voters is empty...
          ; Start listing the votes that have been cast in this branch and their voters by setting a starting point in traversing up the tree and setting an empty
          ; list of voters and their votes
          let parent-node n
          let votes-cast []

          ; Fill in the list by adding the voter and vote of ancestor nodes as long as there are voters in the same batch of votes
          while [item 1 parent-node = "Coalition proposal voter"] [
            set votes-cast lput (item 2 parent-node) votes-cast
            set parent-node first filter [t -> member? (item 0 parent-node) item 5 t] current-branch
          ]

          ; Use the votes cast and their voters' power to derive a voting decision
          set coalition-proposal-decision-latest ifelse-value (
            (sum (map [v ->
              ifelse-value ((item 1 v) = "In favour") [item (item 0 v) power-levels-2-latest] [0]
              ] votes-cast
              )
            ) >
            (winning-coalition-size * (sum (map [v ->
              ifelse-value ((item 1 v) != "Not in coalition") [item (item 0 v) power-levels-2-latest] [0]
              ] votes-cast
              )
              )
            )
            ) ["Accepted"] ["Rejected"]

          ; Generate an ID for the coalition proposal decision
          while [member? id map [t -> item 0 t] current-branch] [
            set id id_generation
          ]

          ; Add the coalition proposal decision to the branch
          set current-branch lput (list id "Coalition proposal decision" coalition-proposal-decision-latest 0 0 []) current-branch

          ; Mark the coalition proposal decision as a child node of the coalition proposal voter
          let n-index position n current-branch
          set n replace-item 5 n (lput id (item 5 n))
          set current-branch replace-item n-index current-branch n
        ]
      ]
    ]

    ; For debugging purposes, it is possible to track the playout as it runs
    if (debug-display?) [
      print "The branch currently looks like:"
      show current-branch
      print (word "And has a length of "length current-branch " nodes")
      set counter counter + 1
      print (word "At the end of iteration " counter)
      print "-----------------------------------------------------"
    ]
  ]

  ; Once there aren't any further nodes to be expanded, report the tree
  report current-branch
end

; Utility procedure to backpropagate a result down a nodelist tree
to-report backpropagate-tree-nodelist [branch tree coalition-proposal-voter position-assignment-voter agenda-setter]
  ; List the IDs of nodes that need to be updated
  let ids-to-update map [n -> item 0 n] branch

  ; For each of the nodes in the tree
  foreach tree [n ->
    ; If it needs to be updated...
    if (member? item 0 n ids-to-update) [
      ifelse (item 1 n = "Agenda setter") [
        ; When updating an agenda setter (that should not receive payoff since it is a decision by the observer)
        ; Mark its position in the tree
        let n-index position n tree
        ; Mark that the node has been visited an additional time
        set n replace-item 4 n (item 4 n + 1)
        ; Replace the node with its updated version
        set tree replace-item n-index tree n
      ] [
        ; When updating anything other than an agenda setter...
        ; Mark its position in the tree
        let n-index position n tree
        ; Determine which player's payoff is relevant at that node
        let payoff-player determine-payoff-agent-nodelist (extract-branch tree item 0 n) coalition-proposal-voter position-assignment-voter agenda-setter
        ; Extract that player's payoff from the playout
        let payoff-to-add item payoff-player item 2 last branch
        ; Mark that the node has been visited an additional time
        set n replace-item 4 n (item 4 n + 1)
        ; Add the new payoff
        set n replace-item 3 n (item 3 n + payoff-to-add)
        ; Replace the node with its updated version
        set tree replace-item n-index tree n
      ]
    ]
  ]

  ; Report the updated tree
  report tree
end

; Utility procedure to update an argument that is a single value contained in the N-to-latest node (1 = latest node, 2 = second-to-latest node, 3 = third-to-latest node, ...)
to-report update-single-value-nodelist [node-type default current-tree node-id counter-max]
  ; Start with the stopping counter at zero
  let counter 0

  ; Check if the root node is of the desired type, and increment the counter if it is
  if (item 1 first filter [t -> item 0 t = node-id] current-tree = node-type) [set counter counter + 1]

  ; To find the node from which to take the updated value if there is such a node, traverse upwards through the tree until either reaching the top of the
  ; tree or having passed enough nodes of the required type
  while [counter < counter-max and not empty? filter [t -> member? node-id item 5 t] current-tree] [
    set node-id item 0 first filter [t -> member? node-id item 5 t] current-tree
    if (item 1 first filter [t -> item 0 t = node-id] current-tree = node-type) [set counter counter + 1]
  ]

  ; Once traversing the tree has stopped, report the value in the node of the required type found. If no such node has been found, revert to the default
  report ifelse-value (not empty? filter [t -> member? node-id item 5 t] current-tree) [item 2 first filter [t -> item 0 t = node-id] current-tree] [default]
end

; Utility procedure to update a set of potential agenda setters
to-report update-potential-agenda-setters-nodelist [potential-agenda-setters current-coalition current-tree node-id]
  ; To find previous agenda setters, start with an empty list of previous agenda setters at the current node
  let previous-agenda-setters []

  ; As long as no coalition proposal acceptance has been reached (at which point the set of potential agenda setters was reset) and it is still possible to traverse
  ; up the tree further...
  while [
    not (item 1 first filter [t -> item 0 t = node-id] current-tree = "Coalition proposal decision"
      and item 2 first filter [t -> item 0 t = node-id] current-tree = "Accepted")
    and not empty? filter [t -> member? node-id item 5 t] current-tree
  ] [
    ; If passing an agenda setter node, add that agenda setter to the list of previous agenda setters
    if (item 1 first filter [t -> item 0 t = node-id] current-tree = "Agenda setter") [
      set previous-agenda-setters lput (item 2 first filter [t -> item 0 t = node-id] current-tree) previous-agenda-setters
    ]

    set node-id item 0 first filter [t -> member? node-id item 5 t] current-tree
  ]

  if (node-id = "Root" and item 1 first filter [t -> item 0 t = node-id] current-tree = "Agenda setter") [
    set previous-agenda-setters lput (item 2 first filter [t -> item 0 t = node-id] current-tree) previous-agenda-setters
  ]

  ; If traversing upward through the tree ended at a coalition proposal acceptance...
  ifelse (item 1 first filter [t -> item 0 t = node-id] current-tree = "Coalition proposal decision"and item 2 first filter [t -> item 0 t = node-id] current-tree = "Accepted" ) [
    ; Go to the coalition proposal that preceded the proposal decision
    while [item 1 first filter [t -> item 0 t = node-id] current-tree != "Coalition proposal"] [
      set node-id item 0 first filter [t -> member? node-id item 5 t] current-tree
    ]

    ; Report the set of potential agenda setters as the accepted proposal minus all agenda setters that have proposed a coalition afterwards
    report filter [a -> not member? a previous-agenda-setters] item 2 first filter [t -> item 0 t = node-id] current-tree
  ] [
    ; If traversing upward through the tree ended at the root, report the set of porential agenda setters as the default set minus agenda setter that have
    ; proposed a coalition afterwards
    report filter [a -> not member? a previous-agenda-setters] potential-agenda-setters
  ]
end

; Utility procedure to update a set of potential voters
to-report update-remaining-voters-nodelist [potential-voters voter-type current-tree node-id]
  ; To start listing the agents that have already voted at the point that this node is reached, go to the parent node
  let parent-node first filter [t -> item 0 t = node-id] current-tree
  let previous-voters []
  ; Go up the tree until either encountering a realised coalition (a point at which the set of potential voters is reset)
  ; or the root of the tree
  while [item 1 parent-node != "Coalition" and not empty? filter [t -> member? node-id item 5 t] current-tree] [
    ; Each time a voter of the right type is encountered, add it to the list of previous voters
    if (item 1 parent-node = voter-type) [
      set previous-voters lput (item 0 item 2 parent-node) previous-voters
    ]

    ; Continue traversing up the tree
    set parent-node first filter [t -> member? (item 0 parent-node) item 5 t] current-tree
    set node-id item 0 parent-node
  ]

  ; Update the list of remaining voters by filtering out agents that have already voted in the current vote
  report filter [v -> not member? v previous-voters] potential-voters
end

; Utility procedure to calculate a payoff at a stopping node in the tree
to-report calculate-payoff-branch-nodelist [branch payoff-coalition initial-coalition-arg power-levels-arg out-network-matrix-arg final-node-id client-to-patron-payoff-transfer-constant patron-to-client-payoff-transfer-constant agent-types-arg reward-thresholds-arg]
  ; Transpose the patronage network to have client-to-patron transfers as well as patron-to-client transfers
  let in-network-matrix-arg matrix:to-row-list matrix:transpose matrix:from-row-list out-network-matrix-arg

  ; Calculate the number of turns a turtle has been part of the ruling coalition, correcting for the initial coalition
  let branch-transitions map [m -> sum map [l -> ifelse-value (item 1 l = "Coalition"and member? m item 2 l) [1] [0]] branch] initial-coalition-arg

  ; Sum the power of agents that are members of the current coalition
  let total-coalition-power sum map [n -> ifelse-value (member? n payoff-coalition) [item n power-levels-arg][0]] range length power-levels-arg

  ; Calculate the payoff for each agent at that branch's URC, first calculating the payoffs of agents disregarding their network connections
  let isolated-payoffs (map [[m t] -> ifelse-value (member? m payoff-coalition)
    [item m power-levels-arg / total-coalition-power - penalty * t]
    [0 - penalty * t]
  ] initial-coalition-arg branch-transitions)

  ; Then calculate what payoffs and penalties agents receive because of their patronage network position
  let client-to-patron-payoff-transfers map [m ->
    client-to-patron-payoff-transfer-constant * sum map [i ->
      ifelse-value (item i item m in-network-matrix-arg = 1) [item i isolated-payoffs] [0]
    ] range length item m out-network-matrix-arg
  ] range length out-network-matrix-arg

  let patron-to-client-payoff-transfers map [m ->
    patron-to-client-payoff-transfer-constant * sum map [i ->
      ifelse-value (item i item m out-network-matrix-arg = 1) [item i isolated-payoffs] [0]
  ] range length item m in-network-matrix-arg
  ] range length in-network-matrix-arg


  ; Then calculate the total payoffs and penalties
  let total-payoffs map [i ->
    item i isolated-payoffs + item i client-to-patron-payoff-transfers + item i patron-to-client-payoff-transfers
  ] range length isolated-payoffs

  ; Correct the payoff of satisficing agents if necessary
  set total-payoffs (map [[p t r] -> ifelse-value (t = "Satisficer" and p > r) [r] [p]] total-payoffs agent-types-arg reward-thresholds-arg)

  ; Report the final payoffs
  report total-payoffs
end

; Procedure to calculate the agent for which a payoff has to be calculated at a specific branch in a nodelist tree
to-report determine-payoff-agent-nodelist [branch coalition-proposal-voter position-assignment-voter agenda-setter]
  report (ifelse-value
    (member? item 1 last branch (list "Coalition" "Patronage network""Power levels (Stage I)""Coalition proposal voter""Coalition proposal decision")) [
      ; If the point from which the branch is currently played out is a coalition, patronage network, stage I power levels, a coalition proposal voter, or a coalition proposal decision,
      ; calculate the payoff from a random agent that could vote in the last voting round
      ifelse-value (not empty? filter [i -> item 1 i = "Coalition proposal voter"] branch) [
        first item 2 last filter [i -> item 1 i = "Coalition proposal voter"] branch
      ] [
        coalition-proposal-voter
      ]
    ]
    (member? item 1 last branch (list "Position assignment proposal""Coalition proposal""Agenda setter")) [
      ; If the point from which the branch is played out is a position assignment proposal, coalition proposal, or agenda setter,
      ; calculate the payoff from the perspective of its agenda setter
      ifelse-value (not empty? filter [i -> item 1 i = "Agenda setter"] branch) [
        item 2 last filter [i -> item 1 i = "Agenda setter"] branch
      ] [
        agenda-setter
      ]
    ]
    (member? item 1 last branch (list "Position proposal voter""Position proposal decision" "Power levels (Stage II)""Position assignment""Payoff node")) [
      ; If the point from which the branch is played out is a position proposal voter, position proposal decision, stage II power levels, or a position assignment,
      ; calculate the payoff from the perspective of the latest position proposal voter
      ifelse-value (not empty? filter [i -> item 1 i = "Position proposal voter"] branch) [
        first item 2 last filter [i -> item 1 i = "Position proposal voter"] branch
      ] [
        position-assignment-voter
      ]
    ]
  )
end

; Utility procedure to determine which set of power levels is appropriate
to-report determine-power-levels-to-use [branch power-levels-1 power-levels-2]
  ; See whether there are any power level nodes in the branch
  let filtered-branch filter [i -> member? first i (list "Power levels (Stage I)" "Power levels (Stage II)")] branch

  ; If there are any power level nodes in the branch, use that. If the latest node is position assignment-related, use stage I power levels. Otherwise, use stage II power levels
  report (ifelse-value
    (not empty? filtered-branch) [item 1 last filtered-branch]
    (member? first last branch (list "Position assignment proposal" "Position proposal voter" "Position proposal decision" "Position assignment")) [power-levels-1]
    [power-levels-2]
  )
end

; Utility procedure to extract a branch from a nodelist tree
to-report extract-branch [current-tree node-id]
  ; Create a container for the branch
  let branch []

  ; Select the end node of the branch
  let node first filter [t -> node-id = item 0 t] current-tree

  ; Put the end node in the container
  set branch fput node branch

  ; Go up the tree until encountering the root of the tree
  while [not empty? filter [t -> member? node-id item 5 t] current-tree] [
    ; Go to the next node up the branch
    set node first filter [t -> member? node-id item 5 t] current-tree

    ; Add that node to the branch, representing it as being higher up the branch
    set branch fput node branch

    ; Set the node's ID as the ID that the next node has to have as its child node
    set node-id item 0 node
  ]

  ; Once the root is reached, report the branch found
  report branch
end


; Utility procedure to calculate the UCT of a node
to-report calculate-UCT-nodelist [node constant parent-node]
  report item 3 node / item 4 node + constant * sqrt (ln (item 4 parent-node) / item 4 node)
end

; Utility procedure to generate a nine-character alphanumeric ID
to-report id_generation
  ; Start with an empty ID
  let id ""

  ; String nine alphanumeric characters together
  repeat 9 [set id word id one-of (list 0 1 2 3 4 5 6 7 8 9 "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z")]

  ; Report the result
  report id
end

; NETWORK PAYOFF AND NETWORK POWER

; Network creation procedures
to create-network
  if (patronage-network-structure = "None") [
    ; If no network is to be created, just arrange the agents
    layout-circle sort turtles 10
  ]
  if (patronage-network-structure = "Variable cohesion") [
    ; To create avariable cohesion network, first connect all agents
    ask turtles [create-patron-of-ties-to other turtles who-are-not in-patron-of-tie-neighbors]

    ; Remove a random subset of patronage ties
    ask n-of ceiling (count patron-of-ties * (percentage-rewired / 100)) patron-of-ties[die]

    ; Lay out the agents in an ordered circle clockwise from agents with low who numbers to agents with high who numbers
    layout-circle sort turtles 10
  ]
  if (patronage-network-structure = "Variable cohesion (Symmetric)") [
    ; To create avariable cohesion network, first connect all agents
    ask turtles [create-patron-of-ties-to other turtles]

    ; Remove a random subset of patronage ties
    ask n-of ceiling (count patron-of-ties * (percentage-rewired / 100) / 2) patron-of-ties[
      ask patron-of-tie [who] of end2 [who] of end1 [die]
      die
    ]

    ; Lay out the agents in an ordered circle clockwise from agents with low who numbers to agents with high who numbers
    layout-circle sort turtles 10
  ]
  if (patronage-network-structure = "Factions, variable cohesion") [
    ask turtles [set my-faction "None"]

    ; To create avariable cohesion network, first connect all agents
    foreach range number-of-factions [f ->
      ask n-of (count turtles / number-of-factions) turtles with [my-faction = "None"] [set my-faction f]

      ask turtles with [my-faction = f] [create-patron-of-ties-to other turtles with [my-faction = f] who-are-not in-patron-of-tie-neighbors]

      ; Remove a random subset of patronage ties
      ask n-of ceiling (count patron-of-ties with [member? end1 turtles with [my-faction = f]] * (percentage-rewired / 100 )) patron-of-ties with [member? end1 turtles with [my-faction = f]] [die]
    ]

    ; Lay out the agents using spring embedding
    repeat 30 [layout-spring turtles patron-of-ties 0.1 10 5]
  ]
  if (patronage-network-structure = "Factions, variable cohesion (Symmetric)") [
    ; Start with none of the agents in a faction
    ask turtles [set my-faction "None"]

    ; Assign agents to each faction
    foreach range number-of-factions [f ->
      ask n-of (count turtles / number-of-factions) turtles with [my-faction = "None"] [set my-faction f]

      ask turtles with [my-faction = f] [
        ; To create avariable cohesion network, first connect all agents
        create-patron-of-ties-to other turtles with [my-faction = f]
      ]

      ; Remove a random subset of patronage ties
      ask n-of ceiling (count patron-of-ties with [member? end1 turtles with [my-faction = f]] * (percentage-rewired / 100) / 2 ) patron-of-ties with [member? end1 turtles with [my-faction = f]] [
        ask patron-of-tie [who] of end2 [who] of end1 [die]
        die
      ]
    ]

    ; Lay out the agents using spring embedding
    repeat 30 [layout-spring turtles patron-of-ties 0.1 10 5]
  ]
  if (patronage-network-structure = "Star(s)") [
    ; To create a star network, create groups (can be one group) and choose a random member of each group that all other members connect to
    ask turtles [set my-faction "None"]
    foreach range number-of-factions [n ->
      ask n-of floor (count turtles / number-of-factions) turtles with [my-faction = "None"] [
        set my-faction n
      ]
      ask one-of turtles with [my-faction = n] [create-patron-of-ties-to other turtles with [my-faction = n]]
    ]

    ; Lay out the agents using spring embedding
    repeat 30 [layout-spring turtles patron-of-ties 0.1 10 10]
  ]
  if (patronage-network-structure = "Star(s) (Symmetric)") [
    ; To create a star network, create groups (can be one group) and choose a random member of each group that all other members connect to
    ask turtles [set my-faction "None"]
    foreach range number-of-factions [n ->
      ask n-of floor (count turtles / number-of-factions) turtles with [my-faction = "None"] [
        set my-faction n
      ]
      ask one-of turtles with [my-faction = n] [create-patron-of-ties-to other turtles with [my-faction = n]]
    ]

    ; Symmetrise the network
    ask turtles [create-patron-of-ties-to in-patron-of-tie-neighbors who-are-not out-patron-of-tie-neighbors]

    ; Lay out the network using spring embedding
    repeat 30 [layout-spring turtles patron-of-ties 0.1 10 10]
  ]
  if (patronage-network-structure = "Manual") [
    ; If a network is specified manually, start by determining how the input should be interpreted
    if (manual-network-input-method = "Matrix") [
      ; For matrices, go through all the entries and create a directed link from the agent with the row index
      ; to the agent with the column index whenever the entry with those coordinates is 1
      let manual-patronage-matrix read-from-string manual-patronage-network-input
      foreach range length manual-patronage-matrix [r ->
        foreach range length item r manual-patronage-matrix[c ->
          if (item c item r manual-patronage-matrix = 1) [
            ask turtle r [create-patron-of-tie-to turtle c]
          ]
        ]
      ]
    ]
    if (manual-network-input-method = "Edgelist") [
      ; For edgelist notation, go through the edgelist...
      let manual-patronage-edgelist read-from-string manual-patronage-network-input
      foreach manual-patronage-edgelist[edge ->
        ; If an edge is specified as <agent-A> "--" <agent-B>, create a symmetric
        ; tie from agent A to agent B
        if (item 1 edge = "--") [
          ask turtle item 0 edge [
            create-patron-of-tie-to turtle item 2 edge
          ]
          ask turtle item 2 edge [
            create-patron-of-tie-to turtle item 0 edge
          ]
        ]
        ; If an edge is specified as <agent-A> "--" <agent-B>, create a directed
        ; tie from agent A to agent B
        if (item 1 edge = "->") [
          ask turtle item 0 edge [
            create-patron-of-tie-to turtle item 2 edge
          ]
        ]
      ]
    ]

    ; Lay out the resulting network using a spring embedding algorithm, since this should be fairly
    ; flexible
    repeat 500 [layout-spring turtles patron-of-ties 0.1 5 5]
  ]
end

; Utility procedure to calculate total power after network power transfers
to-report calculate-network-power[base-power coalition-membership-list out-network-matrix-arg power-transfer-constant power-transfer-max unassigned-power-proportion]
  ; Turn arguments into matrices where necessary
  set base-power matrix:from-column-list (list base-power)
  let coalition-membership-matrix matrix:from-column-list (list coalition-membership-list)
  set coalition-membership-matrix coalition-membership-matrix matrix:* matrix:transpose coalition-membership-matrix
  set out-network-matrix-arg matrix:from-row-list out-network-matrix-arg

  ; Filter to active ties
  let active-in-ties-matrix (matrix:times-element-wise coalition-membership-matrix matrix:transpose out-network-matrix-arg)

  ; Calculate the initial stored and transferable power vectors
  let power-transfer-counts map [i -> sum (matrix:get-row active-in-ties-matrix i)] range item 0 matrix:dimensions base-power
  let power-transfer-shares matrix:from-column-list (list map [c -> min (list ((ifelse-value (c < 1) [0.01] [c]) ^ -1) 1)] power-transfer-counts)
  let power-residuals matrix:from-column-list (list map [c -> 1 - min (list (c * power-transfer-constant) power-transfer-max)] power-transfer-counts)

  let stored-power matrix:times-element-wise power-residuals base-power
  let transferable-power matrix:times-element-wise (1 matrix:- power-residuals) base-power

  ; Until the share of unassigned power looping through the network is small enough...
  while [sum reduce sentence matrix:to-row-list transferable-power >= unassigned-power-proportion * sum reduce sentence matrix:to-row-list base-power] [
    ; Calculate the power transfers
    set transferable-power (matrix:times-element-wise coalition-membership-matrix out-network-matrix-arg) matrix:* (matrix:times-element-wise power-transfer-shares transferable-power)

    ; Add part of the transfers to the stored power
    set stored-power stored-power matrix:+ matrix:times-element-wise power-residuals transferable-power

    ; Assigned the rest of the transfers for further transfers
    set transferable-power matrix:times-element-wise (1 matrix:- power-residuals) transferable-power
  ]

  ; Once the share of unassigned power looping through the network has become small enough, simply assign it to the current holder
  ; to ensure that no power is lost
  set stored-power stored-power matrix:+ transferable-power

  ; Report the resulting matrix
  report first matrix:to-column-list stored-power
  ;report reduce sentence matrix:to-row-list stored-power
end

; ROBUSTNESS ANALYSIS PROCEDURES

; Robustness test version of the procedure to calculate a payoff at a stopping node in the tree
to-report robustness-check-calculate-payoff-branch-nodelist [branch payoff-coalition initial-coalition-arg power-levels-arg out-network-matrix-arg final-node-id client-to-patron-payoff-transfer-constant patron-to-client-payoff-transfer-constant agent-types-arg reward-thresholds-arg]
  ; Transpose the patronage network to have client-to-patron transfers as well as patron-to-client transfers
  let in-network-matrix-arg matrix:to-row-list matrix:transpose matrix:from-row-list out-network-matrix-arg

  ; Calculate the number of turns a turtle has been part of the ruling coalition, correcting for the initial coalition
  let branch-transitions map [m -> sum map [l -> ifelse-value (item 1 l = "Coalition"and member? m item 2 l) [1] [0]] branch] initial-coalition-arg

  let total-coalition-power sum map [n -> ifelse-value (member? n payoff-coalition) [item n power-levels-arg][0]] range length power-levels-arg

  ; Calculate the payoff for each agent at that branch's URC, first calculating the payoffs of agents disregarding their network connections
  let isolated-payoffs (map [[m t] -> ifelse-value (member? m payoff-coalition)
    [item m power-levels-arg / total-coalition-power - penalty * t]
    [0 - penalty * t]
  ] initial-coalition-arg branch-transitions)

    ; Then calculate the total payoffs and penalties
  let total-payoffs calculate-network-power isolated-payoffs (n-values length isolated-payoffs [1]) out-network-matrix-arg patron-to-client-payoff-transfer-constant (ifelse-value (patron-to-client-payoff-transfer-constant > 0.5) [patron-to-client-payoff-transfer-constant] [0.5]) power-transfer-error
  set total-payoffs (map [[p t r] -> ifelse-value (t = "Threshold" and p > r) [r] [p]] total-payoffs agent-types-arg reward-thresholds-arg)

  ; Report the final payoffs
  report total-payoffs
end

; Robustness test version of the network power transfer procedure
to-report robustness-check-calculate-network-power[base-power coalition-membership-list out-network-matrix-arg power-transfer-constant power-transfer-max]
  ; Turn arguments into matrices where necessary
  set base-power matrix:from-column-list (list base-power)
  let coalition-membership-matrix matrix:from-column-list (list coalition-membership-list)
  set coalition-membership-matrix coalition-membership-matrix matrix:* matrix:transpose coalition-membership-matrix
  set out-network-matrix-arg matrix:from-row-list out-network-matrix-arg

  ; Filter to active ties
  let active-in-ties-matrix (matrix:times-element-wise coalition-membership-matrix matrix:transpose out-network-matrix-arg)

  ; Calculate the initial stored and transferable power vectors
  let power-transfer-counts map [i -> sum (matrix:get-row active-in-ties-matrix i)] range item 0 matrix:dimensions base-power
  let power-transfer-shares matrix:from-column-list (list map [c -> min (list ((ifelse-value (c < 1) [0.01] [c]) ^ -1) 1)] power-transfer-counts)
  let power-residuals matrix:from-column-list (list map [c -> 1 - min (list (c * power-transfer-constant) power-transfer-max)] power-transfer-counts)

  let stored-power matrix:times-element-wise power-residuals base-power
  let transferable-power matrix:times-element-wise (1 matrix:- power-residuals) base-power

  ; Calculate the power transfers
  set transferable-power (matrix:times-element-wise coalition-membership-matrix out-network-matrix-arg) matrix:* (matrix:times-element-wise power-transfer-shares transferable-power)

  ; Once the share of unassigned power looping through the network has become small enough, simply assign it to the current holder
  ; to ensure that no power is lost
  set stored-power stored-power matrix:+ transferable-power

  ; Report the resulting matrix
  report first matrix:to-column-list stored-power
end

; AUXILIARY PROCEDURES

; A utility procedure to list all possible sequences of draws with replacement from a certain set
to-report generate-sequences-with-replacement [sample-size sample-space possible-samples]
  (ifelse
    ; To start the process from an empty list, create entries for every item in the sample space
    (empty? possible-samples) [
      set possible-samples map [s -> lput s possible-samples] sample-space
      report generate-sequences-with-replacement sample-size sample-space possible-samples
    ]
    ; Until the sequences are of the desired length, continue creating new ones by replacing each entry in the current
    ; list with a series of entries starting with the current list and continuing with each of the items in the sample space
    (length one-of possible-samples < sample-size) [
      set possible-samples reduce sentence map [p -> map [s -> lput s p] sample-space] possible-samples
      report generate-sequences-with-replacement sample-size sample-space possible-samples
    ] [
      ; Once the entries are at the target length, output the result
      report possible-samples
  ])
end

; Utility procedure to create a list of coalitions with who numbers instead of
; positions in a list
to-report turn-list-numbers-to-coalition-whos [current-coalition]
  report map [c -> map [m -> item m current-coalition] c] create-search-space-reporter range length current-coalition
end

; Utility procedure to check whether list1 is a subset of list2
to-report subset-of? [list1 list2]
  ; For each element of list 1, give 1 if it is an element of list 2 and 0 if it is not
  let any-counter sum (map [i -> ifelse-value (member? i list2) [1] [0] ] list1)

  ; If there are fewer ones than elements in list 1, there is an element of list 1 that
  ; is not an element of list 2 and list 1 is not a subset of list 2
  report any-counter = length list1
end

; Utility procedure to calculate d factorial
to-report factorial [d]
  let tot 1
  ifelse d = 0 [] [
    foreach (range d) [
      x ->
      set tot tot * (x + 1)
    ]
  ]
  report tot
end

; Utility procedure to calculate binomial coefficient n choose k
to-report combination [n k]
  report factorial n / (factorial k * factorial (n - k))
end

; A recursive procedure to generate all possible combinations of players/who numbers
; of a set length. The inputs are the set of players from which to choose (player-set),
; a first combination of the desired length (should be range k for combinations of size k),
; and a container for the coalitions.
to-report generate-combinations2 [player-set last-coalition coalition-list]
  ; Create a counter to access the right element to increment
  let counter (length last-coalition - 1)
  ; Create a set of maximum values for each entry. Due to the way the algorithm works, this is necessarily
  ; the last set that is created.
  let thresholds sublist player-set (length player-set - length last-coalition) length player-set
  ; Add the current coalition to the list of coalitions
  set coalition-list lput last-coalition coalition-list
  ; Once the final set is reached, exit the procedure and report the complete list
  if (last-coalition = thresholds) [report coalition-list]
  ; If the list element pointed to by the counter is at its maximum value...
  ifelse (item counter last-coalition = item counter thresholds) [
    ; ...move to earlier values in the list to find the first one that is not yet at its maximum value. Since
    ; the list is not yet equal to thresholds, there has to be at least one such element
    while [item counter last-coalition = item counter thresholds] [set counter counter - 1]
    ; Once the counter points to an element that is not yet at its maximum, increment that element by one
    set last-coalition replace-item counter last-coalition ((item counter last-coalition) + 1)
    ; Set all elements after the incremented element to the values immediately following the new value
    let length-check length last-coalition
    set last-coalition (sublist last-coalition 0 (counter + 1))
    while [length-check != length last-coalition] [set last-coalition lput (last last-coalition + 1) last-coalition]
  ] [
    ; If the counter points to an element that is not at maximum value from the start, just increment that value
    set last-coalition replace-item counter last-coalition ((item counter last-coalition) + 1)
  ]
  ; Use recursion to go back to adding the new coalition to the set of coalitions, checking whether the end state has
  ; been reached, and continuing the process of incrementing elements upwards
  report generate-combinations2 player-set last-coalition coalition-list
end
@#$#@#$#@
GRAPHICS-WINDOW
210
10
647
448
-1
-1
13.0
1
10
1
1
1
0
0
0
1
-16
16
-16
16
0
0
1
ticks
30.0

SLIDER
20
87
193
120
winning-coalition-size
winning-coalition-size
0
1
0.5
0.01
1
NIL
HORIZONTAL

SLIDER
21
51
193
84
initial-coalition-size
initial-coalition-size
1
60
3.0
1
1
agents
HORIZONTAL

BUTTON
20
191
108
224
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
110
191
192
224
Go!
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
20
122
192
155
penalty
penalty
0
1
0.01
0.01
1
NIL
HORIZONTAL

SWITCH
20
156
192
189
debug-display?
debug-display?
1
1
-1000

SWITCH
18
229
191
262
MCTS-tree-search?
MCTS-tree-search?
0
1
-1000

INPUTBOX
16
267
192
327
MCTS-trials
100.0
1
0
Number

INPUTBOX
15
333
192
393
UCT-constant
1.5
1
0
Number

INPUTBOX
15
398
192
458
current-random-seed
1.137490239E9
1
0
Number

SWITCH
14
464
192
497
use-user-seed?
use-user-seed?
1
1
-1000

INPUTBOX
14
502
192
562
manually-specified-power
[3 4 5]
1
0
String

SWITCH
13
567
193
600
manually-specify-power?
manually-specify-power?
0
1
-1000

TEXTBOX
23
15
193
45
Settings and parameters for the base model
12
0.0
1

TEXTBOX
661
17
931
35
Settings and parameters for model extensions
12
0.0
1

INPUTBOX
661
400
890
460
positions-power-string
[0]
1
0
String

CHOOSER
914
90
1227
135
patronage-network-structure
patronage-network-structure
"None" "Variable cohesion" "Variable cohesion (Symmetric)" "Factions, variable cohesion" "Factions, variable cohesion (Symmetric)" "Star(s)" "Star(s) (Symmetric)" "Manual"
0

SLIDER
915
180
1232
213
number-of-factions
number-of-factions
1
10
1.0
1
1
faction(s)
HORIZONTAL

SLIDER
657
91
884
124
client-to-patron-payoff-transfer
client-to-patron-payoff-transfer
0
1
0.0
0.01
1
NIL
HORIZONTAL

SLIDER
658
128
884
161
patron-to-client-payoff-transfer
patron-to-client-payoff-transfer
0
1
0.0
0.01
1
NIL
HORIZONTAL

SLIDER
916
139
1231
172
percentage-rewired
percentage-rewired
0
100
0.0
1
1
%
HORIZONTAL

SLIDER
658
186
885
219
patronage-power-transfer
patronage-power-transfer
0
1
0.0
0.01
1
NIL
HORIZONTAL

TEXTBOX
661
55
881
85
Patronage network and networked payoff settings
12
0.0
1

TEXTBOX
661
167
852
185
Network power settings
12
0.0
1

TEXTBOX
916
66
1163
84
Network structure settings
12
0.0
1

TEXTBOX
662
338
885
356
Positional power settings
12
0.0
1

SWITCH
14
730
192
763
print-decisions?
print-decisions?
0
1
-1000

INPUTBOX
914
220
1233
365
manual-patronage-network-input
NIL
1
1
String

TEXTBOX
917
431
1230
538
Edgelist syntax:\nThe input string should follow the format\n[[<edge-1>][<edge-2>][<edge-3>][<edge-4>]...]\nwhere each edge has the structure\n[<vertex-1> <type-symbol> <vertex-2>]\nType symbols are \"--\" for undirected ties and \"->\" for a directed tie from vertex 1 to vertex 2.
12
0.0
1

CHOOSER
914
373
1233
418
manual-network-input-method
manual-network-input-method
"Matrix" "Edgelist"
1

SWITCH
661
361
889
394
manually-assign-positions?
manually-assign-positions?
0
1
-1000

INPUTBOX
661
465
891
525
positions-assignment-string
[0]
1
0
String

SWITCH
212
482
484
515
write-decision-trees-to-file?
write-decision-trees-to-file?
1
1
-1000

TEXTBOX
15
605
207
740
Manually specified power syntax:\nThe input string should follow the format [<power-level-1> <power-level-2> ... <power-level-n>]\nwhere there are as many power levels as agents in the initial coalition
12
0.0
1

TEXTBOX
663
531
887
744
Manually specified position power and position assignment syntax:\nIn both cases, the input string should follow the format\n[<position-1> <position-2> ... <position-k>]\nwhere for the position power string the number is the power that position confers, and for the position assignment string it is the agent that the position is initially assigned to. The position power string and position assignment string should both have the same length k, the number of positions.
12
0.0
1

INPUTBOX
211
520
485
580
decision-tree-file
NIL
1
0
String

TEXTBOX
213
458
488
478
Settings for saving decision trees
12
0.0
1

SLIDER
658
222
885
255
max-patronage-power-transfer
max-patronage-power-transfer
patronage-power-transfer
1
0.0
0.01
1
NIL
HORIZONTAL

INPUTBOX
658
260
887
320
power-transfer-error
1.0E-6
1
0
Number

SWITCH
1246
89
1467
122
manual-agent-type-input?
manual-agent-type-input?
1
1
-1000

INPUTBOX
1247
125
1468
186
manual-agent-types
NIL
1
0
String

INPUTBOX
1253
303
1472
363
manual-reward-thresholds
NIL
1
0
String

TEXTBOX
1254
369
1476
534
Manual reward threshold syntax:\nThe input string should follow the format\n[<threshold-1> <threshold-2> ... <threshold-n>]\nwhere there are as many thresholds as agents in the initial coalition, even if they are maximisers
12
0.0
1

TEXTBOX
1253
193
1469
306
Agent type syntax:\nThe input string should follow the format\n[<type-1> <type-2> ... <type-n>]\nwith as many types as agents in the initial coalition, and where each type is one of \"Maximiser\" or \"Satisficer\".
12
0.0
1

TEXTBOX
1247
62
1397
80
Agent type settings
12
0.0
1

SWITCH
1481
89
1714
122
constant-sum-payoff-transfers?
constant-sum-payoff-transfers?
1
1
-1000

TEXTBOX
1484
65
1723
83
Robustness analysis settings
12
0.0
1

TEXTBOX
918
549
1230
674
Matrix syntax:\nThe input string should follow the format\n[[<row-1>][<row-2>][<row-3>]...]\nwhere each row has the structure\n[<column-1> <column-2> <column-3> ...]\nand <column-j> of <row-i> indicates whether the agent with who number i has a tie from itself to the agent with who number j.
12
0.0
1

SWITCH
1482
132
1717
165
one-step-power-transfers?
one-step-power-transfers?
1
1
-1000

TEXTBOX
216
585
483
633
Usage note:\nA dialog will ask you to select a directory to which to write the decision-tree-file
12
0.0
1

@#$#@#$#@
# Coalition Formation in Non-Democracies
Siebren Kooistra (sieko734@student.liu.se), May 21 2024
##	Overview
###	Purpose and patterns
This model looks at the stability of coalitions among members of an autocratic ruling elite. It is an agent-based elaboration of the coalition-formation game outlined in Acemoglu et al. (2008). Players (the elites) have varying levels of power and try to maximise their own share in the total power in the coalition. Which subset of players is part of the coalition and which players fall outside the coalition evolves over time.

This agent-based computational implementation of the game outlined in Agemoglu et al. (2008) is meant to add to the game-theoretical analytical results from that paper by using the flexibility of computational models to examine how sensible the results that the model gives are under a broader range of conditions than the original model could cover. The focus of this shift is making power, a vague concept in the original model which would be very hard to operationalise, into a multi-dimensional concept that is easier to operationalise. More particularily, the model extends an Acemoglu et al. (2008, 2009) style model in three ways:

1. It adds patronage networks that make the payoffs of patrons and clients interdependent;

2. It allows these patronage networks to also transfer power from clients to patrons;

3. It adds positions that confer power on their occupant.

###	Entities, state variables, and scales
When considering the base model and all extensions, this model has three kinds of entities. These entities are the _players_ of the game, _patronage ties_ between players, and _positions_ that can be filled by players and give them a certain amount of power.  

The players of the game represent members of an autocratic elite, or actors in any other context where 1) they vie for power, 2) this power derives from a group whose members can determine and change each other's membership, and 3) players have differing levels of power that give them more or less influence on membership decisions. Players have three time-stable state variables. The first is _residual power_, a value that is either set by the user or set to a uniform random value between 4 and 6 (increments of 0.1) at initialisation. The second is _faction_, a value that determines which network they are assigned to when creating subdivided coalitions. Last, agents can have an agent type, either "Maximiser" or "Satisficer", that affects how they calculate their payoff.

In addition to the time-stable variables, players have a fairly extensive set of time-varying state variables. The first of these, _network power_ and _positional power_, are further power variables that record the power agents have because of the positions that have been assigned to them if there are meaningful positions (_positional power_) and the power that players have after transfers of power through the patronage network are taken into consideration (_network power_). Beyond these power state variables, players have _voting positions_ (for or against) for coalition proposals and position assignment proposals as time-varying state variables. They also have state variables registering whether they have proposed a subcoalition when others have not done so yet and whether they are the current proposer. To record their payoff at the end of the game/a model run, players have a _payoff_ state variable. To calculate payoffs when patronage networks are active, there is also an _isolated payoff_ state variable. And players' _in-coalition?_ state variable represents whether they are in the coalition or outside of it at any point in time.

Patronage ties do not have any substantively significant state variables.

Positions have a time-stable _power_ state variable recording how much power they would confer on a player that is assigned to it, and a time-varying _assignee_ variable that records which player currently receives this power.

The model does not explicitly represent space, and time does not have a definitive scale. However, the global environment contains a counter _t_ for the number of transitions from one coalition to another that has occured.

###	Process overview and scheduling
Figure 1 illustrates the schedule of the model. At initialisation, players (whose number is set by the user) receive their residual power level and have their time-varying variables set to an appropiate starting value. All players are in the coalition, and none are marked as current or past agenda setters. Positions are assinged according to a user-set specification or randomly assigned, and players update their positional power to reflect their position. A patronage network is set up according to one of eight possible structures (an asymmetric random network, a symmetric random network, a grouped asymmetric random network, a grouped symmetric random network, a set of asymmetric stars, a set of symmetric stars, or a manually specified network). Once the network is set up, player's power taking network transfers into account is calculated for the start of the simulation.

After initialisation, the players take turns in proposing a subcoalition and a position distribution and having all players (including themselves) vote over their proposals. If a subcoalition proposal is not accepted, a new proposer is chosen after the position assignment proposal has also been voted over and the current proposer cannot propose again until some subcoalition has been accepted. If a subcoalition proposal is accepted, players that are not part of the proposed subcoalition (if any) are sidelined, the the proposer also proposed a coalition assignment, and the process starts again with the new subcoalition. A model run ends either when none of the players had their proposal for a subcoalition accepted (i.e., there is an impasse) or the proposed subcoalition is the same as the current coalition (i.e., apparently a group of agents with sufficient power is content with keeping the coalition as is, presumably for an indefinite time period).

To compare the model's scheduling to the structure outlined for the original coalition-formation game, see page 993 of Acemoglu et al. (2008).

__Figure 1__
Flowchart of model scheduling
![Flowchart of model schedule](file:Figures\flowchart_game_scheduling.png)

##	Design concepts
####	Basic principles
This model takes a game from the coalition-formation literature (Acemoglu et al., 2008) and extends it with an eye on empirical applicability and robustness to the presence of networks. The work on which this model is based uses game-theory to derive interesting general results on coalition formation in non-democracies. But the way in which the model is defined omits some empirically prominent parts of power politics in autocracies, such as patronage networks and the interdependencies they incur. To make the model more empirically relevant, a fairly promising strategy seems to be to break down power into a multi-dimensional concept, as the empirical literature neither uses nor suggests a central role for some time-stable, one-dimensional attribute of individual 'power'. Instead, while personal traits doubtlessly underlie part of people's ability to gain power (Baturo & Elkink, 2017), power also inheres in networks of loyalty and interdependence (Baturo & Elkink, 2021; Francois et al., 2023; Newson & Trebbi, 2018) and in positions (Geddes et al., 2018; Gill, 2021).

####	Emergence
Emergence in this model relates to the possible tensions between individual interests and strategising, and the coalition structure that results from this interactive strategising. For example, one way in which coalitions might stabilise is that players block a coalition that they prefer to the current one because the changed balance of power in that coalition would expose them to a greater risk of being removed from the coalition.

####	Adaptation
The players in this model strategise in order to maximise their payoff at the end of a model run, which makes them directly objective-seeking. To maximise their power share, players consider expected payoffs of proposing or voting in favour of a certain coalition by either comprehensively examining the game tree or using Monte Carlo Tree Search. They choose the coalitions for which this expected payoff is highest.

####	Objectives
The only objective of players in this model is to maximise their payoff. In the base model without any extensions, this amounts to maximising their share in the total power of all members of the ultimate ruling coalition in as few coalition changes as possible. For this, players need to eliminate as many other players that are as powerful as possible from the coalition, under the constraint that does not end them up being eliminated later. When including patronage networks, an additional concern becomes the payoff that clients and patrons exchange. This can shift players' strategic considerations by partly decoupling individual payoff and individual power. Another extension adds a cap to agents' payoff function such that payoffs beyond a threshold are counted to be equal to that threshold. This means that seeking power beyond the amount that reaches the payoff cap is not of any further use to the agent.

####	Learning
In this model, players are exclusively forward-looking and do not learn.

####	Prediction
To predict their expected payoff from a coalition, players either use comprehensive tree search or Monte Carlo Tree Search. Under comprehensive tree search, players look at all possible game trajectories and the accompanying payoffs, and make the coalition proposal or voting decision whose expected payoff based on looking at all game trajectories that follow it is highest. Under Monte Carlo Tree Search, players expand the game tree in a way that balances focusing on trajectories whose expected reward is relatively high and exploring new parts of the game tree. To estimate the value of various choices, they run random games from the state at the leaves of the expanded tree and propagate that back through the tree.

####	Sensing
Players have complete access to all information in the model world, and indeed include all of this in their representation of their environment in their strategising. Sensing was kept simple in this way to keep as close as possible to the Acemoglu et al. (2008) game. Perfect information might be a problematic assumption, but this will have to show from testing of the model. Until then, it is a useful simplifying assumption.

####	Interaction
Players interact indirectly by proposing and voting over coalitions and position distributions. By accepting coalitions and position distributions, players try to determine who is part of the ultimate ruling coalition, how powerful they and others are, and by that maximise their payoff. The model also includes transfers of payoffs and power between players as a form of direct interaction. The model does not include other interactions.

####	Stochasticity
The model includes stochasticity in the _residual power_ levels that players receive at the start of a model run if those are not manually set, and stochasticity in the Monte Carlo Tree Search algorithm. Residual power and its stochastic variation represents variation in power stemming from sources that are not incorporated into the model, such as personality differences, age and experience, or other aspects.

####	Collectives
The most interesting kind of collective represented in the model are patronage networks, in which players transfer payoffs and/or power. These networks are created at initialisation with a pre-determined structure.

Coalitions, proposed and current, can also be seen as a form of collective in this model. Coalitions are sets of players that can be generated by players as proposals to vote over. They are the focus of players' strategic consideration and determine which players remain in the coalition or are sidelined.

####	Observation
To monitor the model as it is running, players are visualised as smileys arrayed in a circle (if there is no patronage network or the patronage network is a small world network) or according to a spring-embedding algorithm (Fruchterman & Reingold, 1991) applied to the patronage network. The patronage network is visualised by directed links (arrows) running from a patron to a client.  Voting decisions, subcoalition proposals, and the attainment of a stopping condition are printed in the command center. To see how model features and parameter settings affect what coalitions are stable, the initial coalition might be compared to the ultimate ruling coalition, or the composition of the ruling coalition can be tracked over time.

##	Details
###	Initialisation
At the start of a model run, there are _N_ agents as determined by the user of the model (_N_ between 1 and 15). The players each have a residual power level either set by the user or drawn from a uniform distribution running from 4 to 6 (increments of 0.1). None of the players is marked as already having proposed a subcoalition or currently proposing a subcoalition. Payoff is zero for all players. The patronage network is set up in line with one of the eight possible structures (1. No network 2. Random asymmetric network, 3. Random symmetric network, 4. Parameter _f_ random asymmetric networks of equal size, 5. Parameter _f_ random symmetric networks of equal size, 6. Parameter _f_ asymmetric star networks of equal size, 7. Parameter _f_ symmetric star networks of equal size, 8. A user-specified network). Positions as set up by the user are distributed at random or according to the user's specification. After a network is set up and positions are assigned, agents' positional power and power after network transfers are calculated.

###	Input Data
This model does not use any input data.

###	Submodels

####	Tree structures

Before going into the procedures as such, it is useful to detail how the NetLogo implementation of this model represents game trees. Tree stuctures are not a feature of NetLogo by default. 


The comprehensive search procedure represent trees as a set of objects which each contain a path from the root node of a tree to a leaf. Here, the tree as a whole is represented by a a single nested list containing all branches:

```text
[[[<branch 1.1.1> <branch 1.1.2>] [<branch 1.2.1> <branch 1.2.2>]] [[<branch 2.1.1> <branch 2.1.2>] [<branch 2.2.1> <branch 2.2.2> <branch 2.2.3>] [<branch 2.3.1> <branch 2.3.2>]]]
```

where each branch is a sequence of model step results until a stopping condition is reached, e.g.:

```text
[<coalition> <agenda-setter> <coalition-proposal> <proposal-decision> <coalition> ... STOP]
```

A second way of representing trees is used by the comprehensive tree evaluation algorithm, and this decomposes the nested list into an unnested list of branches. Modifying the example for the nested tree, the comprehensive tree evaluation procedure would make it into:

```text
[<branch 1.1.1> <branch 1.1.2> <branch 1.2.1> <branch 1.2.2> <branch 2.1.1> <branch 2.1.2> <branch 2.2.1> <branch 2.2.2> <branch 2.2.3> <branch 2.3.1> <branch 2.3.2>]
```

with the braches being the same as before. I will occasionally refer to this way of representing trees without nesting as a 'bundle of branches' representation.

The third way of representing trees is the one used by the MCTS algorithm. This algorithm represents trees as a set of nodes with one node attribute being the other nodes to which it connects. The tree as a whole then looks like:

```text
[<node 1> <node 2> <node 3> ... <node n>]
```

with each node being a list that looks like:

```
[<node-ID> <X-type-name> <X> <cumulative-payoff> <visits> <children's-IDs-list>]
```

With the structures that the procedures act on clarified, I will now describe the procedures themselves and the kinds of modifications they carry out on the tree representations.


####	Comprehensive tree search
Comprehensive tree search works by first creating (growing) the game tree from the current game state to all possible halting conditions, calculating the payoffs at each halting condition, and then collapsing down the tree from the perspective of a decision-maker to filter out sub-optimal choices and end up with only the nodes an agent should choose remaining to extract the decision to make from it by selecting a random one among them. For example, if an agents has to propose a coalition, it would take itself as an agenda setter for the root node, expand the tree from all coalition proposals possible for this agenda setter to all end states that might follow, and then collapse the tree from these end states by shortening branches when there is no further branching from this branch, eliminating branches that would not be voted towards or that would not be proposed, and averaging across branches when it is a matter of chance which one is realised (i.e., there is a randomly selected agenda setter) until there is just the agenda setter root and branches for coalition proposals whose expected payoff for the agenda setter are maximised. The agenda setter then proposes a random maximum-payoff coalition.

To grow a tree, the model follows the algorithm written down in the flowchart in figure 2.

__Figure 2__
Flowchart of the algorithm for creating a full game tree from a root node
![Flowchart of tree growing algorithm](file:Figures\comprehensive_tree_growing_algorithm.png)

Collapsing the tree follows the algorithm in figure 3.

__Figure 3__
Flowchart of comprehensive tree evaluation algorithm
![Flowchart of comprehensive tree evaluation algorithm](file:Figures\comprehensive_tree_evaluation_algorithm.png)


####	Monte Carlo Tree Search

Monte Carlo Tree Search is a tree exploration algorithm that gradually expands a game tree from a root and evaluates these expansions by looking at the payoffs from random game runs from the leaves of this tree (Coulom, 2007). In this model, this means that agents look through coalitions or position assignments they could propose, and voting decisions they could make on proposals, and explore ways the game could evolve from these proposals. To do so, they gradually map out the decision alternatives and their effects and look at random further trajectories beyond this comprehensive partial map. To select parts of the game tree to focus their limited expansion resources, agents use the Upper Confidence bound applied to Trees statistic (UCT; Kocsis & Szepesvári, 2006):

UCT(node<sub>_i_</sub>, parent-node<sub>_j_</sub>) = Q(node<sub>_i_</sub>) / N(node<sub>_i_</sub>) + _C_ × sqrt(ln(N(parent-node<sub>_j_</sub>))/N(node<sub>_i_</sub>))

Where N(.) is the number of times a trial has entered this node before and Q(.) is the cumulative payoff for the agent that made the last decision at the point the node represents from trial playouts that were done after crossing this node. The constant _C_ needs to be set by the user, and determines how much weight is placed on exploring nodes that are visited relatively rarely compared to exploiting nodes that seem to give high payoffs.

Figure 4 gives a description of the implementation of Monte Carlo Tree Search used in this model. The various pre-defined procedures (backpropagation, playout, and expansion) are detailed further down. Since this algorithm is the most theoretically complex one in the model, but also at the heart of the model, I will give an extensive example of MCTS running in this model's setting after the figure.

__Figure 4__
Flowchart of the general Monte Carlo Tree Search algorithm
![Flowchart of Monte Carlo Tree Search algorithm](file:Figures\MCTS_algorithm.png)

> _MCTS Example_
> As an example of the MCTS evaluation, imagine agent 1 being chosen as the first agenda setter of a game, with the initial and current coalition being `[0 1 2]` (i.e., `[<agent 0> <agent 1> <agent 2>]`) the power levels being `[3 4 2]` (i.e., _&gamma;_<sub>_0_</sub> = 3, _&gamma;_<sub>_1_</sub> = 4, _&gamma;_<sub>_2_</sub> = 2), and agent 0 and agent 1 sharing 30% of their power and 60% of their payoff with each other.
> There is also one position that grants its holder 3 additional power and is held by agent 2. The MCTS algorithm takes agent 1 as agenda setter to create the root node. The constant _C_ in the UCT formula has been set to 1.5. If we look at the first, second, fifth, thirtieth and hundredth trials:
> 
> **Trial 1:** At the start of the first trial, the tree looks like:
> 
> ```
> [["Root" "Agenda setter" 1 0 0 []]]
> ```
> 
> containing just the root node `["Root" "Agenda setter" 1 0 0 []]`. The root node has not been visited at all, so that the algorithm expands this node. Expanding by one step from this node and the other starting conditions show that the possible coalition proposals are `[1]`, 
> `[0 1]`, `[1 2]`, and `[0 1 2]`. New nodes are added for these coalition proposals, resulting in the tree:
> 
> ```
> [
> ["Root" "Agenda setter" 1 0 0 ["0P2PDM3ZQ" "3QZEZGS4Z" "LHO6JWFRJ" "CZ7GV6L5C"]]
> ["0P2PDM3ZQ" "Coalition proposal" [1] 0 0 []]
> ["3QZEZGS4Z" "Coalition proposal" [0 1] 0 0 []]
> ["LHO6JWFRJ" "Coalition proposal" [1 2] 0 0 []]
> ["CZ7GV6L5C" "Coalition proposal" [0 1 2] 0 0 []]
>]
> ```
> 
> The algorithm then plays out a random possible game from a random one among the newly added child nodes. Suppose that it plays out from [1 2] and the playout is as follows:
> 
> ```
> [
> ["Root" "Agenda setter" 1 0 0 ["0P2PDM3ZQ" "3QZEZGS4Z" "LHO6JWFRJ" "CZ7GV6L5C"]]
> ["0P2PDM3ZQ" "Coalition proposal" [1] 0 0 ["OOAP3XN9J"]]
> ["OOAP3XN9J" "Coalition proposal voter" [2 "Against"] 0 0 ["SSYHM5FYR"]]
> ["SSYHM5FYR" "Coalition proposal voter" [1 "Against"] 0 0 ["ZK4CBS7W2"]]
> ["ZK4CBS7W2" "Coalition proposal voter" [0 "In favour"] 0 0 ["OSBBIXIBU"]]
> ["OSBBIXIBU" "Coalition proposal decision" "Rejected" 0 0 ["CS1DD1KPV"]]
> ["CS1DD1KPV" "Coalition" [0 1 2] 0 0 ["PZY983B29"]]
> ["PZY983B29" "Patronage network" [[0 1 0] [1 0 0] [0 0 0]] 0 0 ["ZNWJDRXNF"]]
> ["ZNWJDRXNF" "Power levels (Stage I)" [3.2307696395699996 3.7692303604299995 5] 0 0 ["0V3Q9I8FU"]]
> ["0V3Q9I8FU" "Position assignment proposal" [1] 0 0 ["6LTQBZ689"]]
> ["6LTQBZ689" "Position proposal voter" [0 "Against"] 0 0 ["L1TOUBU62"]]
> ["L1TOUBU62" "Position proposal voter" [2 "In favour"] 0 0 ["6HZDXP8TB"]]
> ["6HZDXP8TB" "Position proposal voter" [1 "Against"] 0 0 ["COQIZW216"]]
> ["COQIZW216" "Position proposal decision" "Rejected" 0 0 ["Z8U64F50B"]]
> ["Z8U64F50B" "Position assignment" [2] 0 0 ["BYHDX65TI"]] ["BYHDX65TI" "Power levels (Stage II)" [3.2307696395699996 3.7692303604299995 5] 0 0 ["CRP2GBYFG"]]
> ["CRP2GBYFG" "Agenda setter" 2 0 0 ["5V70ASGHX"]]
> ["5V70ASGHX" "Coalition proposal" [2] 0 0 ["DS2ASGZKM"]]
> ["DS2ASGZKM" "Coalition proposal voter" [1 "Against"] 0 0 ["655PW53JQ"]]
> ["655PW53JQ" "Coalition proposal voter" [2 "In favour"] 0 0 ["PYAKEF8HR"]]
> ["PYAKEF8HR" "Coalition proposal voter" [0 "In favour"] 0 0 ["RYD5UHQ73"]]
> ["RYD5UHQ73" "Coalition proposal decision" "Accepted" 0 0 ["KC60AQXOY"]]
> ["KC60AQXOY" "Coalition" [2] 0 0 ["H8OEHFQNC"]]
> ["H8OEHFQNC" "Patronage network" [[0 1 0] [1 0 0] [0 0 0]] 0 0 ["C83Z2GJ7K"]] 
> ["C83Z2GJ7K" "Power levels (Stage I)" [3 4 5] 0 0 ["X1ZGTPGN3"]]
> ["X1ZGTPGN3" "Position assignment proposal" [2] 0 0 ["6NDJKI72K"]]
> ["6NDJKI72K" "Position proposal voter" [1 "Not in coalition"] 0 0 ["V8F67ZK78"]]
> ["V8F67ZK78" "Position proposal voter" [0 "Not in coalition"] 0 0 ["JQJ77VA5B"]]
> ["JQJ77VA5B" "Position proposal voter" [2 "Against"] 0 0 ["2SXWC470R"]]
> ["2SXWC470R" "Position proposal decision" "Rejected" 0 0 ["W9RD6PLED"]]
> ["W9RD6PLED" "Position assignment" [2] 0 0 ["AGGEF0NWI"]]
> ["AGGEF0NWI" "Power levels (Stage II)" [3 4 5] 0 0 ["09E6XBBZZ"]]
> ["09E6XBBZZ" "Agenda setter" 2 0 0 ["SLHPT2HFG"]]
> ["SLHPT2HFG" "Coalition proposal" [2] 0 0 ["4F739ZCCW"]]
> ["4F739ZCCW" "Coalition proposal voter" [2 "Against"] 0 0 ["S5LTSY0TE"]]
> ["S5LTSY0TE" "Coalition proposal voter" [1 "Not in coalition"] 0 0 ["39DRXSRCW"]]
> ["39DRXSRCW" "Coalition proposal voter" [0 "Not in coalition"] 0 0 ["FPP9SDTLS"]]
> ["FPP9SDTLS" "Coalition proposal decision" "Rejected" 0 0 ["2VN9C3TS4"]]
> ["2VN9C3TS4" "Coalition" [2] 0 0 ["4GD0G7GO6"]]
> ["4GD0G7GO6" "Patronage network" [[0 1 0] [1 0 0] [0 0 0]] 0 0 ["4Y1SK25J4"]]
> ["4Y1SK25J4" "Power levels (Stage I)" [3 4 5] 0 0 ["V7GQ2Y1JQ"]]
> ["V7GQ2Y1JQ" "Position assignment proposal" [2] 0 0 ["6OL01VOID"]]
> ["6OL01VOID" "Position proposal voter" [1 "Not in coalition"] 0 0 ["MYAOITQ8A"]]
> ["MYAOITQ8A" "Position proposal voter" [0 "Not in coalition"] 0 0 ["Z1MLT7MAS"]]
> ["Z1MLT7MAS" "Position proposal voter" [2 "Against"] 0 0 ["548N7LUP0"]]
> ["548N7LUP0" "Position proposal decision" "Rejected" 0 0 ["SM8PSSKWS"]]
> ["SM8PSSKWS" "Position assignment" [2] 0 0 ["QNWXZVTNW"]]
> ["QNWXZVTNW" "Power levels (Stage II)" [3 4 5] 0 0 ["EQP8NB16T"]]
> ["EQP8NB16T" "Payoff node" [-0.016 -0.016 0.97] 0 0 ["STOP"]]
> ]
> ```
> 
> The algorithm selects the power levels to use for calculating the payoffs as `[3 4 5]` and the payoffs from this branch for the agents as `[-0.016 -0.016 0.97]` (the agents  receive `[-0.1 -0.1 0.97]` by themselves, agent 0 sends -0.06 to agent 1 and agent 1 sends -0.06 to agent 0). This result is then back-propagated.
> 
> **Trial 2:** At the start of the second trial, the tree is the tree expanded one step from the root with back-propagated results from the first round:
> 
> ```
> [
> ["Root" "Agenda setter" 1 0 1 ["0P2PDM3ZQ" "3QZEZGS4Z" "LHO6JWFRJ" "CZ7GV6L5C"]]
> ["0P2PDM3ZQ" "Coalition proposal" [1] -0.016 1 []]
> ["3QZEZGS4Z" "Coalition proposal" [0 1] 0 0 []]
> ["LHO6JWFRJ" "Coalition proposal" [1 2] 0 0 []]
> ["CZ7GV6L5C" "Coalition proposal" [0 1 2] 0 0 []]
> ]
> ```
> 
> Since only one child node to the root has been visited, and the algorithm prioritises visiting unvisited child nodes of whichever node it is at before moving deeper down the tree, the algorithm goes to a random unvisited child node of the root. The algorithm ends up traversing to coalition proposal `[1]`. Since it expands the first unvisited child node it encounters (which, note, was the root in the first trial), it also adds the voting decisions that could follow coalition proposal `[1]`. The tree is then:
> 
> ```
> [
> ["Root" "Agenda setter" 1 0 1 ["0P2PDM3ZQ" "3QZEZGS4Z" "LHO6JWFRJ" "CZ7GV6L5C"]]
> ["0P2PDM3ZQ" "Coalition proposal" [1] -0.016 1 []]
> ["3QZEZGS4Z" "Coalition proposal" [0 1] 0 0 []]
> ["LHO6JWFRJ" "Coalition proposal" [1 2] 0 0 []]
> ["CZ7GV6L5C" "Coalition proposal" [0 1 2] 0 0 ["U5UEW22JY" "HA07Z364R" "UQEAXVJAR" "ZBLPK0KFB" "PXF5GWQ52" "L7IC5ZO61"]]
> ["U5UEW22JY" "Coalition proposal voter" [0 "In favour"] 0.582769285276 1 []]
> ["HA07Z364R" "Coalition proposal voter" [0 "Against"] 0 0 []]
> ["UQEAXVJAR" "Coalition proposal voter" [1 "In favour"] 0 0 []]
> ["ZBLPK0KFB" "Coalition proposal voter" [1 "Against"] 0 0 []]
> ["PXF5GWQ52" "Coalition proposal voter" [2 "In favour"] 0 0 []]
> ["L7IC5ZO61" "Coalition proposal voter" [2 "Against"] 0 0 []]
> ]
> ```
> 
> The algorithm plays out a random game from one of the voting decisions following the expanded coalition proposal:
> 
> ```
> [
> ["Root" "Agenda setter" 1 0 1 ["0P2PDM3ZQ" "3QZEZGS4Z" "LHO6JWFRJ" "CZ7GV6L5C"]]
> ["CZ7GV6L5C" "Coalition proposal" [0 1 2] 0 0 ["U5UEW22JY" "HA07Z364R" "UQEAXVJAR" "ZBLPK0KFB" "PXF5GWQ52" "L7IC5ZO61"]]
> ["U5UEW22JY" "Coalition proposal voter" [0 "In favour"] 0 0 ["EKBLXSSZB"]]
> ["EKBLXSSZB" "Coalition proposal voter" [1 "Against"] 0 0 ["4HWOYCSOT"]]
> ["4HWOYCSOT" "Coalition proposal voter" [2 "Against"] 0 0 ["2DAD5FZR9"]]
> ["2DAD5FZR9" "Coalition proposal decision" "Rejected" 0 0 ["3TXP6VJBT"]]
> ["3TXP6VJBT" "Coalition" [0 1 2] 0 0 ["YWNZNVULO"]]
> ["YWNZNVULO" "Patronage network" [[0 1 0] [1 0 0] [0 0 0]] 0 0 ["56U8GLSEC"]]
> ["56U8GLSEC" "Power levels (Stage I)" [3.2307696395699996 3.7692303604299995 5] 0 0 ["10J7VZUPB"]]
> ["10J7VZUPB" "Position assignment proposal" [0] 0 0 ["QN87SK0V0"]]
> ["QN87SK0V0" "Position proposal voter" [1 "Against"] 0 0 ["J77FG79LP"]]
> ["J77FG79LP" "Position proposal voter" [2 "In favour"] 0 0 ["X52BH40KD"]]
> ["X52BH40KD" "Position proposal voter" [0 "Against"] 0 0 ["JLGXYUK4M"]]
> ["JLGXYUK4M" "Position proposal decision" "Rejected" 0 0 ["QE8L3XLUX"]]
> ["QE8L3XLUX" "Position assignment" [2] 0 0 ["W9CZW89HX"]]
> ["W9CZW89HX" "Power levels (Stage II)" [3.2307696395699996 3.7692303604299995 5] 0 0 ["CV8GVFD8J"]]
> ["CV8GVFD8J" "Agenda setter" 0 0 0 ["FABTG53SL"]]
> ["FABTG53SL" "Coalition proposal" [0 2] 0 0 ["FYDZG3WYW"]]
> ["FYDZG3WYW" "Coalition proposal voter" [2 "In favour"] 0 0 ["5XF6TILYY"]]
> ["5XF6TILYY" "Coalition proposal voter" [0 "Against"] 0 0 ["CI7X2D7PJ"]]
> ["CI7X2D7PJ" "Coalition proposal voter" [1 "Against"] 0 0 ["9RD78P3KG"]]
> ["9RD78P3KG" "Coalition proposal decision" "Rejected" 0 0 ["VUAYC7ALM"]]
> ["VUAYC7ALM" "Coalition" [0 1 2] 0 0 ["HSXRK53X6"]]
> ["HSXRK53X6" "Patronage network" [[0 1 0] [1 0 0] [0 0 0]] 0 0 ["1S6CCH396"]]
> ["1S6CCH396" "Power levels (Stage I)" [3.2307696395699996 3.7692303604299995 5] 0 0 ["M0NXR8CFS"]]
> ["M0NXR8CFS" "Position assignment proposal" [1] 0 0 ["BKFLSUB5K"]]
> ["BKFLSUB5K" "Position proposal voter" [1 "Against"] 0 0 ["J4TX5U3HJ"]]
> ["J4TX5U3HJ" "Position proposal voter" [2 "Against"] 0 0 ["7RLUYLNGD"]]
> ["7RLUYLNGD" "Position proposal voter" [0 "Against"] 0 0 ["GJ3QA3W65"]]
> ["GJ3QA3W65" "Position proposal decision" "Rejected" 0 0 ["K38EK4OV7"]]
> ["K38EK4OV7" "Position assignment" [2] 0 0 ["RAGKFJ7G0"]]
> ["RAGKFJ7G0" "Power levels (Stage II)" [3.2307696395699996 3.7692303604299995 5] 0 0 ["1IZJ8JTIP"]]
> ["1IZJ8JTIP" "Agenda setter" 2 0 0 ["6QXZ26KEO"]]
> ["6QXZ26KEO" "Coalition proposal" [2] 0 0 ["KMZN8E6QI"]]
> ["KMZN8E6QI" "Coalition proposal voter" [0 "In favour"] 0 0 ["DL4I6N19E"]]
> ["DL4I6N19E" "Coalition proposal voter" [1 "Against"] 0 0 ["EMUAB6LHX"]]
> ["EMUAB6LHX" "Coalition proposal voter" [2 "Against"] 0 0 ["GITST6VBV"]]
> ["GITST6VBV" "Coalition proposal decision" "Rejected" 0 0 ["TZ0QMLEHP"]]
> ["TZ0QMLEHP" "Coalition" [0 1 2] 0 0 ["O6SMON8YU"]]
> ["O6SMON8YU" "Patronage network" [[0 1 0] [1 0 0] [0 0 0]] 0 0 ["6610JQELA"]]
> ["6610JQELA" "Power levels (Stage I)" [3.2307696395699996 3.7692303604299995 5] 0 0 ["0FGOGFDF4"]]
> ["0FGOGFDF4" "Position assignment proposal" [1] 0 0 ["K9SQTHTB3"]]
> ["K9SQTHTB3" "Position proposal voter" [1 "In favour"] 0 0 ["SXZM1YAWN"]]
> ["SXZM1YAWN" "Position proposal voter" [0 "In favour"] 0 0 ["5XOH0VZF5"]]
> ["5XOH0VZF5" "Position proposal voter" [2 "In favour"] 0 0 ["QRMNFDRQF"]]
> ["QRMNFDRQF" "Position proposal decision" "Accepted" 0 0 ["BTO02IWMZ"]]
> ["BTO02IWMZ" "Position assignment" [1] 0 0 ["O6ORQD0RB"]]
> ["O6ORQD0RB" "Power levels (Stage II)" [3.92307855828 6.076921441720001 2] 0 0 ["3HG8D1DDF"]]
> ["3HG8D1DDF" "Payoff node" [0.582769285276 0.6545640480573334 0.13666666666666666] 0 0 ["STOP"]]
> ]
> ```
> 
> The power levels that are selected in this case are `[3.92307855828 6.076921441720001 2]` and this implies the payoffs `[0.582769285276 0.6545640480573334 0.13666666666666666]`, which are back-propagated.
> 
> **Trial 5:** Back-propagating the fourth round results gives this tree:
> 
> ```
> [
> ["Root" "Agenda setter" 1 0 4 ["0P2PDM3ZQ" "3QZEZGS4Z" "LHO6JWFRJ" "CZ7GV6L5C"]]
> ["0P2PDM3ZQ" "Coalition proposal" [1] -0.016 1 []]
> ["CZ7GV6L5C" "Coalition proposal" [0 1 2] 0.6545640480573334 1 ["U5UEW22JY" "HA07Z364R" "UQEAXVJAR" "ZBLPK0KFB" "PXF5GWQ52" "L7IC5ZO61"]]
> ["U5UEW22JY" "Coalition proposal voter" [0 "In favour"] 0.582769285276 1 []]
> ["HA07Z364R" "Coalition proposal voter" [0 "Against"] 0 0 []]
> ["UQEAXVJAR" "Coalition proposal voter" [1 "In favour"] 0 0 []]
> ["ZBLPK0KFB" "Coalition proposal voter" [1 "Against"] 0 0 []]
> ["PXF5GWQ52" "Coalition proposal voter" [2 "In favour"] 0 0 []]
> ["L7IC5ZO61" "Coalition proposal voter" [2 "Against"] 0 0 []]
> ["LHO6JWFRJ" "Coalition proposal" [1 2] 0.6705640480573334 1 ["X8KE3ZYVV" "SBNN2RIF3" "I3UGYGE29" "1RORGJYGS" "0CWM5E8DX" "PP8DHS2T4"]]
> ["X8KE3ZYVV" "Coalition proposal voter" [0 "In favour"] 0 0 []]
> ["SBNN2RIF3" "Coalition proposal voter" [0 "Against"] 0.598769285276 1 []]
> ["I3UGYGE29" "Coalition proposal voter" [1 "In favour"] 0 0 []]
> ["1RORGJYGS" "Coalition proposal voter" [1 "Against"] 0 0 []]
> ["0CWM5E8DX" "Coalition proposal voter" [2 "In favour"] 0 0 []]
> ["PP8DHS2T4" "Coalition proposal voter" [2 "Against"] 0 0 []]
> ["3QZEZGS4Z" "Coalition proposal" [0 1] 0.44364101201433326 1 ["8GES1THCC" "X2QTRR855" "ARY4XCJXR" "9TPCLPKK8" "BW7DR0Q90" "5JYYCJ4WA"]]
> ["8GES1THCC" "Coalition proposal voter" [0 "In favour"] 0.4256923213189999 1 []]
> ["X2QTRR855" "Coalition proposal voter" [0 "Against"] 0 0 []]
> ["ARY4XCJXR" "Coalition proposal voter" [1 "In favour"] 0 0 []]
> ["9TPCLPKK8" "Coalition proposal voter" [1 "Against"] 0 0 []]
> ["BW7DR0Q90" "Coalition proposal voter" [2 "In favour"] 0 0 []]
> ["5JYYCJ4WA" "Coalition proposal voter" [2 "Against"] 0 0 []]
> ]
> ```
> 
> At this point, all child nodes of the root have been visited at least once. This means that the algorithm starts traversing the first layer of the tree using the nodes' UCT scores. At this point, the nodes' UCT scores are 2.42 for `[0 1 2]`, 2.44 for `[1 2]`, 2.21 for `[0 1]`, and 1.75 for `[1]`. Based on this, the tree traverses to the node for `[1 2]`. As the next step, one of the unexplored child nodes for `[1 2]` is chosen and this turns out to be node X8KE3ZYVV, representing agent 0 voting in favour of the proposal. After expanding that node, the tree before the playout is:
> 
> ```
> [
> ["Root" "Agenda setter" 1 0 5 ["0P2PDM3ZQ" "3QZEZGS4Z" "LHO6JWFRJ" "CZ7GV6L5C"]]
> ["0P2PDM3ZQ" "Coalition proposal" [1] -0.016 1 []]
> ["CZ7GV6L5C" "Coalition proposal" [0 1 2] 0.6545640480573334 1 ["U5UEW22JY" "HA07Z364R" "UQEAXVJAR" "ZBLPK0KFB" "PXF5GWQ52" "L7IC5ZO61"]]
> ["U5UEW22JY" "Coalition proposal voter" [0 "In favour"] 0.582769285276 1 []]
> ["HA07Z364R" "Coalition proposal voter" [0 "Against"] 0 0 []]
> ["UQEAXVJAR" "Coalition proposal voter" [1 "In favour"] 0 0 []]
> ["ZBLPK0KFB" "Coalition proposal voter" [1 "Against"] 0 0 []]
> ["PXF5GWQ52" "Coalition proposal voter" [2 "In favour"] 0 0 []]
> ["L7IC5ZO61" "Coalition proposal voter" [2 "Against"] 0 0 []]
> ["LHO6JWFRJ" "Coalition proposal" [1 2] 1.3850256192229333 2 ["X8KE3ZYVV" "SBNN2RIF3" "I3UGYGE29" "1RORGJYGS" "0CWM5E8DX" "PP8DHS2T4"]]
> ["SBNN2RIF3" "Coalition proposal voter" [0 "Against"] 0.598769285276 1 []]
> ["I3UGYGE29" "Coalition proposal voter" [1 "In favour"] 0 0 []]
> ["1RORGJYGS" "Coalition proposal voter" [1 "Against"] 0 0 []]
> ["0CWM5E8DX" "Coalition proposal voter" [2 "In favour"] 0 0 []]
> ["PP8DHS2T4" "Coalition proposal voter" [2 "Against"] 0 0 []]
> ["3QZEZGS4Z" "Coalition proposal" [0 1] 0.44364101201433326 1 ["8GES1THCC" "X2QTRR855" "ARY4XCJXR" "9TPCLPKK8" "BW7DR0Q90" "5JYYCJ4WA"]]
> ["8GES1THCC" "Coalition proposal voter" [0 "In favour"] 0.4256923213189999 1 []]
> ["X2QTRR855" "Coalition proposal voter" [0 "Against"] 0 0 []]
> ["ARY4XCJXR" "Coalition proposal voter" [1 "In favour"] 0 0 []]
> ["9TPCLPKK8" "Coalition proposal voter" [1 "Against"] 0 0 []]
> ["BW7DR0Q90" "Coalition proposal voter" [2 "In favour"] 0 0 []]
> ["5JYYCJ4WA" "Coalition proposal voter" [2 "Against"] 0 0 []]
> ["X8KE3ZYVV" "Coalition proposal voter" [0 "In favour"] 0 0 ["SOIE00F8I" "1VVZYU5S9" "NEALQPEU2" "Y1G482T57"]]
> ["SOIE00F8I" "Coalition proposal voter" [1 "In favour"] 0 0 []]
> ["1VVZYU5S9" "Coalition proposal voter" [1 "Against"] 0 0 []]
> ["NEALQPEU2" "Coalition proposal voter" [2 "In favour"] 0 0 []]
> ["Y1G482T57" "Coalition proposal voter" [2 "Against"] 0 0 []]
> ]
> ```
> 
> A playout that might follow the new branch is:
> 
> ```
> [
> ["Root" "Agenda setter" 1 0 4 ["0P2PDM3ZQ" "3QZEZGS4Z" "LHO6JWFRJ" "CZ7GV6L5C"]]
> ["LHO6JWFRJ" "Coalition proposal" [1 2] 0.6705640480573334 1 ["X8KE3ZYVV" "SBNN2RIF3" "I3UGYGE29" "1RORGJYGS" "0CWM5E8DX" "PP8DHS2T4"]]
> ["X8KE3ZYVV" "Coalition proposal voter" [0 "In favour"] 0 0 ["SOIE00F8I" "1VVZYU5S9" "NEALQPEU2" "Y1G482T57"]]
> ["1VVZYU5S9" "Coalition proposal voter" [1 "Against"] 0 0 ["KOMW5M7PT"]]
> ["KOMW5M7PT" "Coalition proposal voter" [2 "Against"] 0 0 ["A1B77E0ST"]]
> ["A1B77E0ST" "Coalition proposal decision" "Rejected" 0 0 ["84ONLM3FC"]]
> ["84ONLM3FC" "Coalition" [0 1 2] 0 0 ["80PF2N6HA"]]
> ["80PF2N6HA" "Patronage network" [[0 1 0] [1 0 0] [0 0 0]] 0 0 ["3CFBC512E"]]
> ["3CFBC512E" "Power levels (Stage I)" [3.2307696395699996 3.7692303604299995 5] 0 0 ["4YUGVXBPK"]]
> ["4YUGVXBPK" "Position assignment proposal" [0] 0 0 ["GNFNGI0R5"]]
> ["GNFNGI0R5" "Position proposal voter" [1 "In favour"] 0 0 ["Q8LB929Y8"]]
> ["Q8LB929Y8" "Position proposal voter" [2 "Against"] 0 0 ["9ICG7B9EV"]]
> ["9ICG7B9EV" "Position proposal voter" [0 "In favour"] 0 0 ["PUDVRRLCW"]]
> ["PUDVRRLCW" "Position proposal decision" "Accepted" 0 0 ["9GZWZBEZI"]]
> ["9GZWZBEZI" "Position assignment" [0] 0 0 ["D3S17DIEQ"]]
> ["D3S17DIEQ" "Power levels (Stage II)" [5.538460720859999 4.461539279139999 2] 0 0 ["XXGCPWT7A"]]
> ["XXGCPWT7A" "Agenda setter" 0 0 0 ["YFXA3QFLI"]]
> ["YFXA3QFLI" "Coalition proposal" [0 1] 0 0 ["GGFR26A94"]]
> ["GGFR26A94" "Coalition proposal voter" [2 "Against"] 0 0 ["0A4K841ZR"]]
> ["0A4K841ZR" "Coalition proposal voter" [0 "In favour"] 0 0 ["D7RDO5DHF"]]
> ["D7RDO5DHF" "Coalition proposal voter" [1 "In favour"] 0 0 ["R2LH1COWZ"]]
> ["R2LH1COWZ" "Coalition proposal decision" "Accepted" 0 0 ["GT6OP7UO3"]]
> ["GT6OP7UO3" "Coalition" [0 1] 0 0 ["N6UOLWEN7"]]
> ["N6UOLWEN7" "Patronage network" [[0 1 0] [1 0 0] [0 0 0]] 0 0 ["EIDOHPWE7"]]
> ["EIDOHPWE7" "Power levels (Stage I)" [5.538460720859999 4.461539279139999 2] 0 0 ["PSZMTUERY"]]
> ["PSZMTUERY" "Position assignment proposal" [1] 0 0 ["XKJOPCCRP"]]
> ["XKJOPCCRP" "Position proposal voter" [1 "Against"] 0 0 ["QWB6C1XWO"]]
> ["QWB6C1XWO" "Position proposal voter" [2 "Not in coalition"] 0 0 ["R8S9SVXY0"]]
> ["R8S9SVXY0" "Position proposal voter" [0 "In favour"] 0 0 ["A5KMCV9MR"]]
> ["A5KMCV9MR" "Position proposal decision" "Accepted" 0 0 ["GZ6VD6L3S"]]
> ["GZ6VD6L3S" "Position assignment" [1] 0 0 ["GYLAVNTU6"]]
> ["GYLAVNTU6" "Power levels (Stage II)" [3.92307855828 6.076921441720001 2] 0 0 ["0O6JW05A6"]]
> ["0O6JW05A6" "Agenda setter" 1 0 0 ["HMXCFKDNS"]]
> ["HMXCFKDNS" "Coalition proposal" [1] 0 0 ["DYGACLNE8"]]
> ["DYGACLNE8" "Coalition proposal voter" [0 "Against"] 0 0 ["12ICCNZTW"]]
> ["12ICCNZTW" "Coalition proposal voter" [1 "Against"] 0 0 ["X3N0DM686"]]
> ["X3N0DM686" "Coalition proposal voter" [2 "Not in coalition"] 0 0 ["8LWS13TJZ"]]
> ["8LWS13TJZ" "Coalition proposal decision" "Rejected" 0 0 ["LESYDZIE5"]]
> ["LESYDZIE5" "Coalition" [0 1] 0 0 ["9LMWMJBYQ"]]
> ["9LMWMJBYQ" "Patronage network" [[0 1 0] [1 0 0] [0 0 0]] 0 0 ["EHG3395TZ"]]
> ["EHG3395TZ" "Power levels (Stage I)" [3.92307855828 6.076921441720001 2] 0 0 ["Y0WI03E2B"]]
> ["Y0WI03E2B" "Position assignment proposal" [0] 0 0 ["0KLE0P6K8"]]
> ["0KLE0P6K8" "Position proposal voter" [1 "Against"] 0 0 ["5NR8RFJK2"]]
> ["5NR8RFJK2" "Position proposal voter" [0 "Against"] 0 0 ["VH83NWCNQ"]]
> ["VH83NWCNQ" "Position proposal voter" [2 "Not in coalition"] 0 0 ["PBUTDPQ95"]]
> ["PBUTDPQ95" "Position proposal decision" "Rejected" 0 0 ["FREEY7UBO"]]
> ["FREEY7UBO" "Position assignment" [1] 0 0 ["9DF0EW74C"]]
> ["9DF0EW74C" "Power levels (Stage II)" [3.92307855828 6.076921441720001 2] 0 0 ["RUKUVMAHY"]]
> ["RUKUVMAHY" "Agenda setter" 0 0 0 ["6HKH8W895"]]
> ["6HKH8W895" "Coalition proposal" [0 1] 0 0 ["26ZS4BQD0"]]
> ["26ZS4BQD0" "Coalition proposal voter" [0 "In favour"] 0 0 ["F934Y58Y9"]]
> ["F934Y58Y9" "Coalition proposal voter" [2 "Not in coalition"] 0 0 ["94I35JU4P"]]
> ["94I35JU4P" "Coalition proposal voter" [1 "Against"] 0 0 ["7HF2E3HHL"]]
> ["7HF2E3HHL" "Coalition proposal decision" "Rejected" 0 0 ["AYI77C7IX"]]
> ["AYI77C7IX" "Coalition" [0 1] 0 0 ["1YE690QLM"]]
> ["1YE690QLM" "Patronage network" [[0 1 0] [1 0 0] [0 0 0]] 0 0 ["EYEFNJRUD"]]
> ["EYEFNJRUD" "Power levels (Stage I)" [3.92307855828 6.076921441720001 2] 0 0 ["V5YU6DWMJ"]]
> ["V5YU6DWMJ" "Position assignment proposal" [0] 0 0 ["TQF8NPX6J"]]
> ["TQF8NPX6J" "Position proposal voter" [1 "In favour"] 0 0 ["A5WDNHAV6"]]
> ["A5WDNHAV6" "Position proposal voter" [2 "Not in coalition"] 0 0 ["CPY3A0OWG"]]
> ["CPY3A0OWG" "Position proposal voter" [0 "In favour"] 0 0 ["QY51LNYJ0"]]
> ["QY51LNYJ0" "Position proposal decision" "Accepted" 0 0 ["T4MRK7TL6"]]
> ["T4MRK7TL6" "Position assignment" [0] 0 0 ["DTSBPMI9F"]]
> ["DTSBPMI9F" "Power levels (Stage II)" [5.538460720859999 4.461539279139999 2] 0 0 ["78AVABWX4"]]
> ["78AVABWX4" "Payoff node" [0.7575384288344 0.7144615711655999 -0.01] 0 0 ["STOP"]]
> ]
> ```
> 
> The power levels selected for calculating the payoffs for this playout are `[5.538460720859999 4.461539279139999 2]`, and with an URC `[0 1]` the actual payoffs that are calculated are `[0.7575384288344 0.7144615711655999 -0.01]`. These results are back-propagated.
> 
> **Trial 30:** At the thirtieth trial, the tree is too large to write out in its entirety. However, the descent to the node to play out and the playout schematically look like:
> 
> __Figure 5__
> Example MCTS tree at iteration 30
> ![Flowchart of MCTS playout algorithm](file:Figures\MCTS_example_thirty_trials.png)
> 
> The playout for this case is:
> 
> ```
> [
> ["Root" "Agenda setter" 1 0 29 ["0P2PDM3ZQ" "3QZEZGS4Z" "LHO6JWFRJ" "CZ7GV6L5C"]]
> ["0P2PDM3ZQ" "Coalition proposal" [1] 1.896 5 ["D1S4YQGAM" "3U67R4I16" "CB10MFD58" "CFZ6URYUO" "CGPZHN8LI" "Z9YHGGX04"]]
> ["D1S4YQGAM" "Coalition proposal voter" [0 "In favour"] 0 0 ["Y9LBTC6W9" "H8QEZ2Q0L" "NX1E73E7N" "NQW2CHSZ9"]]
> ["H8QEZ2Q0L" "Coalition proposal voter" [1 "Against"] 0 0 ["Q0OWW73GL"]]
> ["Q0OWW73GL" "Coalition proposal voter" [2 "Against"] 0 0 ["4W87YS0X5"]]
> ["4W87YS0X5" "Coalition proposal decision" "Rejected" 0 0 ["SH96ZMEK5"]]
> ["SH96ZMEK5" "Coalition" [0 1 2] 0 0 ["VNEKBOK2S"]]
> ["VNEKBOK2S" "Patronage network" [[0 1 0] [1 0 0] [0 0 0]] 0 0 ["D8TPV14DT"]]
> ["D8TPV14DT" "Power levels (Stage I)" [3.2307696395699996 3.7692303604299995 5] 0 0 ["NNL2D89YB"]]
> ["NNL2D89YB" "Position assignment proposal" [0] 0 0 ["BWZMS20ZS"]]
> ["BWZMS20ZS" "Position proposal voter" [2 "In favour"] 0 0 ["TNU72FJVJ"]]
> ["TNU72FJVJ" "Position proposal voter" [0 "Against"] 0 0 ["AA2406JP5"]]
> ["AA2406JP5" "Position proposal voter" [1 "Against"] 0 0 ["XMYMZIV3Z"]]
> ["XMYMZIV3Z" "Position proposal decision" "Rejected" 0 0 ["ALSXIG6LF"]]
> ["ALSXIG6LF" "Position assignment" [2] 0 0 ["VZHIA8U55"]]
> ["VZHIA8U55" "Power levels (Stage II)" [3.2307696395699996 3.7692303604299995 5] 0 0 ["OJTA5KTV5"]]
> ["OJTA5KTV5" "Agenda setter" 2 0 0 ["V6SCB5DUS"]]
> ["V6SCB5DUS" "Coalition proposal" [0 2] 0 0 ["W0RVZMYPX"]]
> ["W0RVZMYPX" "Coalition proposal voter" [2 "Against"] 0 0 ["KOHJET0GL"]]
> ["KOHJET0GL" "Coalition proposal voter" [0 "Against"] 0 0 ["PLPX82SI8"]]
> ["PLPX82SI8" "Coalition proposal voter" [1 "In favour"] 0 0 ["LASIQ09LA"]]
> ["LASIQ09LA" "Coalition proposal decision" "Rejected" 0 0 ["WOI78G109"]]
> ["WOI78G109" "Coalition" [0 1 2] 0 0 ["WVXQ3BGCI"]]
> ["WVXQ3BGCI" "Patronage network" [[0 1 0] [1 0 0] [0 0 0]] 0 0 ["BIRFSK47L"]]
> ["BIRFSK47L" "Power levels (Stage I)" [3.2307696395699996 3.7692303604299995 5] 0 0 ["L67F6IYCR"]]
> ["L67F6IYCR" "Position assignment proposal" [2] 0 0 ["LN40782YP"]]
> ["LN40782YP" "Position proposal voter" [0 "In favour"] 0 0 ["U08P2KWB0"]]
> ["U08P2KWB0" "Position proposal voter" [2 "In favour"] 0 0 ["OM660C2AW"]]
> ["OM660C2AW" "Position proposal voter" [1 "In favour"] 0 0 ["0V6MP08TY"]]
> ["0V6MP08TY" "Position proposal decision" "Accepted" 0 0 ["IQSM11S29"]]
> ["IQSM11S29" "Position assignment" [2] 0 0 ["1MS62M7WR"]]
> ["1MS62M7WR" "Power levels (Stage II)" [3.2307696395699996 3.7692303604299995 5] 0 0 ["2RPFIV1NR"]]
> ["2RPFIV1NR" "Agenda setter" 0 0 0 ["6XAAK9OKK"]]
> ["6XAAK9OKK" "Coalition proposal" [0 1 2] 0 0 ["50PYAOSZV"]]
> ["50PYAOSZV" "Coalition proposal voter" [2 "In favour"] 0 0 ["6RI14PP0J"]]
> ["6RI14PP0J" "Coalition proposal voter" [0 "In favour"] 0 0 ["H6HY8G7TP"]]
> ["H6HY8G7TP" "Coalition proposal voter" [1 "In favour"] 0 0 ["3EL01O6H5"]]
> ["3EL01O6H5" "Coalition proposal decision" "Accepted" 0 0 ["492HI9CA8"]]
> ["492HI9CA8" "Coalition" [0 1 2] 0 0 ["4WD7BMNKV"]]
> ["4WD7BMNKV" "Patronage network" [[0 1 0] [1 0 0] [0 0 0]] 0 0 ["Q0WZEZ0PC"]]
> ["Q0WZEZ0PC" "Power levels (Stage I)" [3.2307696395699996 3.7692303604299995 5] 0 0 ["FUQAQGGOU"]]
> ["FUQAQGGOU" "Position assignment proposal" [0] 0 0 ["ZWM1GY077"]]
> ["ZWM1GY077" "Position proposal voter" [2 "In favour"] 0 0 ["RFMZVDANF"]]
> ["RFMZVDANF" "Position proposal voter" [1 "Against"] 0 0 ["KQ2YWMN1B"]]
> ["KQ2YWMN1B" "Position proposal voter" [0 "Against"] 0 0 ["FLKIX3A50"]]
> ["FLKIX3A50" "Position proposal decision" "Rejected" 0 0 ["20PCFI5SB"]]
> ["20PCFI5SB" "Position assignment" [2] 0 0 ["3MVXVD3I3"]]
> ["3MVXVD3I3" "Power levels (Stage II)" [3.2307696395699996 3.7692303604299995 5] 0 0 ["RGUMPOB3E"]]
> ["RGUMPOB3E" "Payoff node" [0.40969232131899996 0.4276410120143333 0.3866666666666667] 0 0 ["STOP"]]
> ]
> ```
> 
> For this playout, the power levels to use for the calculation of the payoffs are `[3.2307696395699996 3.7692303604299995 5]` and the payoffs that are calculated and back-propagated are `[0.40969232131899996 0.4276410120143333 0.3866666666666667]`. 
> 
> **Trial 100:** Again, at a hundred trials the tree is too large to write out in its entirety. But schematically, descent and playout look like:
> 
> __Figure 6__
> Example MCTS tree at iteration 100
> ![Flowchart of MCTS playout algorithm](file:Figures\MCTS_example_hundred_trials.png)
> 
> The final playout is:
> 
> ```
> [
> ["Root" "Agenda setter" 1 0 99 ["0P2PDM3ZQ" "3QZEZGS4Z" "LHO6JWFRJ" "CZ7GV6L5C"]]
> ["3QZEZGS4Z" "Coalition proposal" [0 1] 17.859794408487332 27 ["8GES1THCC" "X2QTRR855" "ARY4XCJXR" "9TPCLPKK8" "BW7DR0Q90" "5JYYCJ4WA"]]
> ["8GES1THCC" "Coalition proposal voter" [0 "In favour"] 3.0949231777605997 5 ["PARGCEN57" "K43UNC3C3" "AN6NASZZA" "NB8AS4TQ4"]]
> ["PARGCEN57" "Coalition proposal voter" [1 "In favour"] 0.8110768576688 1 ["QK6826GGS" "JF0L4Z111"]]
> ["QK6826GGS" "Coalition proposal voter" [2 "In favour"] 0 0 ["VA4V3U8Z8"]]
> ["VA4V3U8Z8" "Coalition proposal decision" "Accepted" 0 0 ["6W70JUGY7"]]
> ["6W70JUGY7" "Coalition" [0 1] 0 0 ["HPZYO2BYM"]] ["HPZYO2BYM" "Patronage network" [[0 1 0] [1 0 0] [0 0 0]] 0 0 ["J3V30JNFO"]]
> ["J3V30JNFO" "Power levels (Stage I)" [3.2307696395699996 3.7692303604299995 5] 0 0 ["KZ3XWTS1O"]]
> ["KZ3XWTS1O" "Position assignment proposal" [1] 0 0 ["Q8VX2MSKN"]]
> ["Q8VX2MSKN" "Position proposal voter" [1 "Against"] 0 0 ["HL86NPSOI"]]
> ["HL86NPSOI" "Position proposal voter" [2 "Not in coalition"] 0 0 ["SNMMMVLZP"]]
> ["SNMMMVLZP" "Position proposal voter" [0 "In favour"] 0 0 ["M2209BT1O"]]
> ["M2209BT1O" "Position proposal decision" "Rejected" 0 0 ["1IZGI317A"]]
> ["1IZGI317A" "Position assignment" [2] 0 0 ["WML08BFFV"]]
> ["WML08BFFV" "Power levels (Stage II)" [3.2307696395699996 3.7692303604299995 5] 0 0 ["4ECRXAOP8"]]
> ["4ECRXAOP8" "Agenda setter" 0 0 0 ["7QFZA00SA"]]
> ["7QFZA00SA" "Coalition proposal" [0 1] 0 0 ["0NRGUGE26"]]
> ["0NRGUGE26" "Coalition proposal voter" [2 "Not in coalition"] 0 0 ["IZYYSGYKO"]]
> ["IZYYSGYKO" "Coalition proposal voter" [0 "Against"] 0 0 ["1N7D0DF8E"]]
> ["1N7D0DF8E" "Coalition proposal voter" [1 "In favour"] 0 0 ["W0KQOY5B5"]]
> ["W0KQOY5B5" "Coalition proposal decision" "Accepted" 0 0 ["W7G5OS53C"]]
> ["W7G5OS53C" "Coalition" [0 1] 0 0 ["SXYHMFP36"]]
> ["SXYHMFP36" "Patronage network" [[0 1 0] [1 0 0] [0 0 0]] 0 0 ["9UPGQ2SV7"]]
> ["9UPGQ2SV7" "Power levels (Stage I)" [3.2307696395699996 3.7692303604299995 5] 0 0 ["LKY8H27R6"]]
> ["LKY8H27R6" "Position assignment proposal" [1] 0 0 ["KAHDHV86K"]]
> ["KAHDHV86K" "Position proposal voter" [2 "Not in coalition"] 0 0 ["XE1Y8S0YG"]]
> ["XE1Y8S0YG" "Position proposal voter" [1 "In favour"] 0 0 ["Z1V2Q3U2V"]]
> ["Z1V2Q3U2V" "Position proposal voter" [0 "Against"] 0 0 ["NBJB22XI1"]]
> ["NBJB22XI1" "Position proposal decision" "Accepted" 0 0 ["EQVIRF8M3"]]
> ["EQVIRF8M3" "Position assignment" [1] 0 0 ["JK8D3R2R1"]]
> ["JK8D3R2R1" "Power levels (Stage II)" [3.92307855828 6.076921441720001 2] 0 0 ["EQWHIV730"]]
> ["EQWHIV730" "Payoff node" [0.7249231423311999 0.8110768576688 0] 0 0 ["STOP"]]
> ]
> ```
> 
> The final playout has payoffs [0.7249231423311999 0.8110768576688 0]. Now, supposing that agent 1 has to make a decision based on this tree, After the hundredth playout the root node and its children look like:
> ```
> [
> ["Root" "Agenda setter" 1 0 100 ["0P2PDM3ZQ" "3QZEZGS4Z" "LHO6JWFRJ" "CZ7GV6L5C"]]
> ["CZ7GV6L5C" "Coalition proposal" [0 1 2] 10.279572505129712 19 ["U5UEW22JY" "HA07Z364R" "UQEAXVJAR" "ZBLPK0KFB" "PXF5GWQ52" "L7IC5ZO61"]]
> ["LHO6JWFRJ" "Coalition proposal" [1 2] 13.653598230640846 23 ["X8KE3ZYVV" "SBNN2RIF3" "I3UGYGE29" "1RORGJYGS" "0CWM5E8DX" "PP8DHS2T4"]]
> ["3QZEZGS4Z" "Coalition proposal" [0 1] 18.670871266156134 28 ["8GES1THCC" "X2QTRR855" "ARY4XCJXR" "9TPCLPKK8" "BW7DR0Q90" "5JYYCJ4WA"]]
> ["0P2PDM3ZQ" "Coalition proposal" [1] 20.546358881697476 30 ["D1S4YQGAM" "3U67R4I16" "CB10MFD58" "CFZ6URYUO" "CGPZHN8LI" "Z9YHGGX04"]]
> ]
> ```
>
> Of the child nodes, the one that was visited most often was `[1]`. So in this case, agent 1 would propose the subcoalition `[1]`.

####	MCTS Expansion
To expand a game tree, the MCTS algorithm generates all nodes that can follow the current node given the branch it is part of. Figure 7 shows a flowchart for the algorithm.

__Figure 7__
Flowchart of the MCTS stepwise expansion
![Flowchart of MCTS tree expansion algorithm](file:Figures\flowchart_MCTS_expansion_algorithm.png)

####	MCTS Playout
The MCTS playout algorithm is similar to the MCTS expansion algorith. Where MCST expansion systematic lists all possible child nodes, MCTS playout only generates a random one among the potential child nodes but repeats this at each child node until a stopping condition to the game is reached in the branch. Figure 8 shows a flowchart for the algorithm.

__Figure 8__
Flowchart of the MCTS playout algorithm
![Flowchart of MCTS playout algorithm](file:Figures\flowchart_MCTS_playout_algorithm.png)


####	MCTS Back-propagation
Back-propagation in Monte Carlo Tree Search is the updating of all nodes from a leaf to the root with the result of a playout from that leaf. In this implementation, a procedure filters down to the nodes that are contained in an input branch (a newly created playout), and updates their cumulative payoff and number of visits one by one. The flowchart in Figure 9 represents the algorithm in more detail.

__Figure 9__
Flowchart of the MCTS back-propagation algorithm
![Flowchart of MCTS back-propagation algorithm](file:Figures\flowchart_MCTS_back_propagation_algorithm.png)

####	MCTS Argument updating
To ensure that the previous nodes assumed in procedures modifying a branch as input are reliable there are other procedures to update the arguments given to these procedures. The updating prodedures take an identifier for the argument to update, the value assumed as a result of earlier input, and the branch to which the argument applies. It then looks through the branch. For node types whose value can be deduced from a single other node, the argument is updated based on the last of any other nodes of the same type in the branch. Otherwise, the argument remains at the default put into the updating procedure. The single value updating procedure is also able to look back further than the latest node of a type, as this is useful when checking whether the same coalition has been accepted two times in a row (one of the game's two stopping conditions). In pseudocode, the single value updating procedure is:

```text
function update-single-value(tree, current-node, target-type, default, desired-depth):
	counter <- 0
	
	if (type(current-node) = target-type):
		counter <- counter + 1

	while (counter < desired-depth and any?(parents(curent-node))):
		current-node <- parent-of(current-node)
		if (type(current-node) = target-type):
			counter <- counter + 1

	if (any?(parents(curent-node))):
		return(value(current-node))
	else:
		return(default)
```

For updating a list of potential agenda setters, the list depends on the coalition that is current at the point for which the list is updated and the agenda setters that have proposed a coalition since this coalition came into force. To update the list of potential agenda setters, a function takes all members of the current coalition, removes the ones that have proposed a coalition that was rejected since that coalition came into force, and returns the members of the coalition that have not made a rejected coalition proposal since the current coalition came into force. In pseudocode:

```text
function update-potential-agenda-setters(potential-agenda-setters, current-coalition, tree, current-node):
	previous-agenda-setters = list()

	while (!(type(current-node) = "Coalition proposal decision" and value(current-node) = "Accepted") and any?(parents(curent-node))):
		if (type(current-node) = "Agenda setter"):
			previous-agenda-setters <- concat(previous-agenda-setters, value(current-node))
		
		current-node <- parent-of(current-node)

	if (is-root?(current-node) and type(current-node) = "Agenda setter"):
		previous-agenda-setters <- concat(previous-agenda-setters, value(current-node))

	if (type(current-node) = "Coalition proposal decision" and value(current-node) = "Accepted"):
		while(type(current-node) != "Coalition proposal"):
			current-node <- parent(current-node)

		return(setdiff(value(current-node), previous-agenda-setters))
	else:
		return(setdiff(current-coalition, previous-agenda-setters))
```

When updating a list of voters, the procedure is broadly similar to that followed when updating a list of potential agenda setters. All voters that have voted since the last coalition or position assignment proposal are listed, and these are removed from the initial coalition to get the set of agents that still have to be queried for their vote. In pseudocode:

```text
function update-remaining-voters(potential-voters, voter-type, tree, current-node):
	previous-voters <- list()

	while (any?(parents(current-node)) and type(parent-node(current-node)) != "Coalition"):
		if (type(parent-node(current-node)) = voter-type):
			previous-voters <- concat(previous-voters, value(parent-node))
		current-node <- parent-node(current-node)

	return(setdiff(potential-voters, previous-voters))
```

####	Payoff calculation
The formula implemented to calculate agents' payoff at the end of a game is:

Payoff(_i_) = IsolatedPayoff(_i_) + _&delta;_<sub>c &rightarrow; p</sub> sum[ I(**P**<sub>_ij_</sub> = 1) × IsolatedPayoff(_j_), _j_ &in; IC] + _&delta;_<sub>p &rightarrow; c</sub> sum[I(**P**<sub>_ji_</sub> = 1) × IsolatedPayoff(_j_), _j_ &in; IC]

with

IsolatedPayoff(_i_) = _&gamma;_<sub>_i_</sub> / _&gamma;_<sub>URC</sub> - _&epsilon;_ sum[I(_i_ &in; C<sub>k</sub>), 1 &le; _k_ &le; _t_]

with _&gamma;_<sub>i</sub> denoting an agent _i_'s power level in the ultimate ruling coalition, _&gamma;_<sub>URC</sub> being the sum of power levels for all members of the ultimate ruling coalition, _&epsilon;_ being the penalty for transitioning from one coalition to another, _&delta;_<sub>c &rightarrow; p</sub> being the share (between 0 and 1) of an agent's payoff that is transferred from a client to a patron and _&delta;_<sub>p &rightarrow; c</sub> being the share transferred from a patron to a client, **P** being the matrix of patronage ties from patrons to clients, C<sub>k</sub> being the _k_-th ruling coalition, IC being the initial coalition and URC being the ultimate ruling coalition. The variable _t_ is the index of the URC.

There is also an option to give some agents a threshold payoff function. If this is the case, agents that are assigned the "Maximiser" type calculate payoffs as normal, but those that have the "Satisficer" type have a payoff threshold _&omega;_<sub>_i_</sub> that transforms their payoff as follows:

SatisficerPayoff(_i_) = I(Payoff(_i_) > _&omega;_<sub>_i_</sub>) &times; _&omega;_<sub>_i_</sub> + (1 - I[Payoff(_i_) > _&omega;_<sub>_i_</sub>]) &times; Payoff(_i_)

So if a satisficer's payoff is higher than the payoff threshold, it is corrected to be equal to that threshold.

####	Payoff calculation robustness check

As a first robustness check of the model results, payoff transfers can also be handled in the same way as network transfers. In practice, this means that payoffs are calculated as power in the way described in **Network power calculation**, except that matrix **M** has all entries always set to 1, isolated payoffs (IsolatedPayoff(_i_)) replace power levels, _&delta;_ replaces _&zeta;_, and _&tau;_ is set to the minimum of 0.5 and &delta;.

####	Network creation
The model can create patronage networks of eight types: no network, asymmetric and symmetric random networks, asymmetric and symmetric grouped random networks, asymmetric and symmetric star networks, and manually specified networks.

If no network is created, agents are simply arranged in a circle.

To create a random network, all agents are connected to each other, and a random selection covering a certain percentage of these ties is removed. If the network is symmetric, if a tie in one direction is removed the tie in the other direction is also always removed. In pseudocode:

```text
function create_random_network(agents, symmetric?, removal_proportion):
	if (symmetric?):
		foreach (agent in agents):
			create-ties(from = agent, to = setdiff(agents, agent))
		ties_to_remove <- sample(from = ties, howmany = ceiling([removal_proportion / 2] × count(ties)))
		remove(ties_to_remove)
	else:
		foreach (agent in agents):
			create-ties(from = agent, to = setdiff(agents, union(agent, in-neighbours(agent))))
		ties_to_remove <- sample(from = ties, howmany = ceiling(removal_proportion × count(ties)))
		remove(ties_to_remove)
```

Creating a grouped random network follows a similar procedure to creating a random network, but treats each subgroup seperately:

```text
function create_grouped_random_network(agents, symmetric?, removal_proportion, n_groups):
	foreach (g in n_groups):
		group_members <- sample(from = agents, howmany = floor(count(agents) / n_groups), subset = [group = Null])
		if (symmetric?):
			foreach (agent in group_members):
				create-ties(from = agent, to = setdiff(group_members, agent))
			group_ties <- subset(set = ties, filter = [end1 ∈ group_members | end2 ∈ group_members])
			ties_to_remove <- sample(from = group_ties, howmany = ceiling([removal_proportion / 2] × count(group_ties)))
			remove(ties_to_remove)
		else:
			foreach (agent in group_members):
				create-ties(from = agent, to = setdiff(group_members, union(agent, in-neighbours(agent))))
				group_ties <- subset(set = ties, filter = [end1 ∈ group_members | end2 ∈ group_members])
			ties_to_remove <- sample(from = group_ties, howmany = ceiling(removal_proportion × count(group_ties)))
			remove(ties_to_remove)
```

To create a network with a set number of stars, the agents are divided up into the desired number of stars, a random agent within each group is chosen as the center of the star, and that agent makes all other agents in their group its clients. If the stars are symmetric, the center agent also becomes a client to all the other nodes. In pseudocode:

```text
function create_star_network(agents, symmetric?, n_stars):
	foreach (g in n_groups):
		group_members <- sample(from = agents, howmany = floor(count(agents) / n_groups), subset = [group = Null])
		central_agent <- sample(from = group_members, howmany = 1)
		if (symmetric?):
			create-ties(from = central_agent, to = setdiff(group_members, agent))
			create-ties(from = setdiff(group_members, agent), to = central_agent)
		else:
			create-ties(from = central_agent, to = setdiff(group_members, agent))
``` 

Manually specified networks accept two syntaxes: An adjacency matrix-based syntax and an edgelist syntax. The adjacency matrix syntax requires a network to be specified as an adjacency matrix of the format `[[<row1-col1> <row1-col2> ... <row1-colN>] ... [<rowN-col1> <rowN-col2> ... <rowN-colN>]]`, rowwise with each entry being 1 if there is a tie from the row agent to the column agent (from agent 1 to agent 5 in row 1 column 5, for example) and 0 if there is no tie. The edgelist syntax requires a network to be specified as `[[<start-1> <tie-type-1> <end-1>][<start-1> <tie-type-1> <end-1>] ... [<start-N> <tie-type-N> <end-N>]]`, with tie types being "->" (a directed tie from `start` to `end`) or "--" (an undirected tie between `start` and `end`). The procedure goes through the input and creates ties as specified. In pseudocode, the procedure is:

```text
function create_manual_network(input_network, input_type):
	if (input_type = "Matrix"):
		adjacency-matrix <- read-string-as-matrix(string = input_network, rowwise = True)
		foreach (entry in adjacency-matrix):
			if entry = 1:
				create-tie(from = select-agent(who = rownum(entry)), to = select-agemt(who = colnum(entry)))
	if (input-type = "Edgelist"):
		edge-list <- read-string-as-list(string = input-network)
		foreach (entry in edge-list):
			create-tie(from = item(which = 0, of = entry), to = item(which = 2, of = entry))
			if (item(which = 1, of = entry) = "--"):
				create-tie(from = item(which = 2, of = entry), to = item(which = 0, of = entry))			
```

####	Network power calculation
To calculate network power, the model uses an iterative approximation algorithm in which agents divide their initial power and any power they receive into a share that they transder to their connections and a share they keep for themselves. Since it works with shares, the transfers would never quite reduce to zero (a<sup>b</sup> × c > 0 &forall; 0 < a < 1, b < &infin;, c > 0), but the error this induces can be controlled and made arbitrarily small. Specifically, starting from power levels &gamma;<sup>NonNet.</sup>, a square matrix **M** with dimensions equal to the size of the initial coalition, _n_, with each entry denoting whether both the row index agent and the column index agent are in the current coalition so that there could be an active tie between them (_m_<sub>_ij_</sub> = 1), or not (_m_<sub>_ij_</sub> = 0), a patronage network matrix **P** whose entry at row _i_, column _j_ is 1 if agents _i_ and _j_ are connected and 0 otherwise, a parameter &zeta; &in; [0, 1) giving the maximum share of power that can be sent over one tie and another parameter &tau; &in; [0, 1) that describes the maximum share of power that can be sent to by one agent to all its connections, and an error tolerance parameter &eta; &in; (0, 1]. The algorithm then runs as follows:
  1. For each agent, determine how many agents in the current coalition are connected to it. For an agent _i_, this is equal to _c_<sub>_i_</sub> = &Sum;<sub>_j_ = 1</sub><sup>_n_</sup> (**M** &odot; **P**)<sub>_i,j_</sub>)
  2. For each agent, determine what share of transferable power each agent will share with each of its connections. This is a vector _s_<span>&#8407;</span> with entries _s_<sub>_i_</sub> = min(_c_<sub>_i_</sub><sup>-1</sup>, 1);
  3. For each agent, also decide what share of any power it has or receives it saves for itself. This is another vector _r_<span>&#8407;</span>, with entries _r_<sub>_i_</sub> = 1 - min(&zeta; &times; _c_<sub>_i_</sub>, &tau;);
  4. Calculate the initial stored and transferable power vectors, with the stored power vector being _&gamma;_<span>&#8407;</span><sub>0</sub><sup>Stored</sup> = _r_<span>&#8407;</span> &odot; _&gamma;_<span>&#8407;</span><sup>NonNet.</sup>, the element-wise multiplication of _r_<span>&#8407;</span> and _&gamma;_<span>&#8407;</span><sup>NonNet.</sup>, and the transferable power vector being _&gamma;_<span>&#8407;</span><sub>0,2</sub><sup>Transferable</sup> = (1 - _r_<span>&#8407;</span>) &odot; _&gamma;_<span>&#8407;</span><sup>NonNet.</sup>.
 5. Then, as long as the sum of the entries in _&gamma;_<span>&#8407;</span><sub>t,2</sub><sup>Transferable</sup> are more than &eta; of the sum of the pre-transfer power vector _&gamma;_<span>&#8407;</span><sup>NonNet.</sup>:
    5.1. Increment _t_ by one;
    5.2. Update the transferable power vector to show received transfers: _&gamma;_<span>&#8407;</span><sub>_t_,1</sub><sup>Transferable</sup> = (**M** &odot; **P**)(_s_<span>&#8407;</span> &odot; _&gamma;_<span>&#8407;</span><sub>(_t_-1),2</sub><sup>Transferable</sup>
    5.3. Allow each agent to take a share of the power it received: _&gamma;_<span>&#8407;</span><sub>_t_</sub><sup>Stored</sup> = _&gamma;_<span>&#8407;</span><sub>_t_-1</sub><sup>Stored</sup> + _r_<span>&#8407;</span> &odot; _&gamma;_<span>&#8407;</span><sub>_t_,1</sub><sup>Transferable</sup>
    5.4. Update the transferable power vector to show power that might be transferred at the next interation: _&gamma;_<span>&#8407;</span><sub>_t_,2</sub><sup>Transferable</sup> = (1 - _r_<span>&#8407;</span>) &odot; _&gamma;_<span>&#8407;</span><sub>_t_,1</sub><sup>Transferable</sup>$$
  6. Once _&gamma;_<span>&#8407;</span><sub>_t_,2</sub><sup>Transferable</sup> &le; &eta; &Sum;<sub>_i_ = 1</sub><sup>n</sup> &gamma;<sub>_i_</sub><sup>NonNet.</sup> (i.e., the stopping condition above has been reached), assign the power remaining in _&gamma;_<span>&#8407;</span><sub>_t_,2</sub><sup>Transferable</sup> to whichever agent is holding it at that moment (_&gamma;_<span>&#8407;</span><sup>Net.</sup> = _&gamma;_<span>&#8407;</span><sub>_t_</sub><sup>Stored</sup> + _&gamma;_<span>&#8407;</span><sub>_t_,2</sub><sup>Transferable</sup>);
  7. Return _&gamma;_<span>&#8407;</span><sup>Net</sup>.

####	Network power robustness check
As a robustness check for network power calculation, an alternative algorithm only allows agents to send power to direct neighbours in a non-iterative way. This means that Point 5. in the list above is reduced to point 5.2. without iteration until the share of transferable power in total power is small enough. So points 1. to 4. remain the same, but the end of the algorithm looks like:
  5. Update the transferable power vector to show received transfers: _&gamma;_<span>&#8407;</span><sub>_t_,1</sub><sup>Transferable</sup> = (**M** &odot; **P**)(_s_<span>&#8407;</span> &odot; _&gamma;_<span>&#8407;</span><sub>_t_,2</sub><sup>Transferable</sup>
  6.Assign the power remaining in _&gamma;_<span>&#8407;</span><sub>_t_,2</sub><sup>Transferable</sup> to whichever agent is holding it at that moment (_&gamma;_<span>&#8407;</span><sup>Net.</sup> = _&gamma;_<span>&#8407;</span><sub>_t_</sub><sup>Stored</sup> + _&gamma;_<span>&#8407;</span><sub>_t_,2</sub><sup>Transferable</sup>);
  7. Return _&gamma;_<span>&#8407;</span><sup>Net</sup>.

####	Position assignment list generation
Since one agent can hold multiple positions, generating a list of possible position assignment proposals can use drawing with replacement. Using the number of positions to assign and the set of players they can be assigned to, the algorithm in the pseudocode below generated the list of possible position assingments by iteratively replacing incomplete lists with sets of lists with one added assigned position and a new list for each agent that might be proposed there.

```text
function generate_lists_with_replacement(size, sample_space, possible_samples):
	if (length(possible_samples) = 0):
		foreach (sam_spa in sample_space):
			possible_samples <- concat(possible_samples, sam_spa)
	else:
		if (length(all(possible_samples)) != size):
			foreach (pos_sam in possible_samples):
				list_to_expand <- pos_sam
				possible_samples <- remove_item(list_to_expand, possible_samples)
				foreach (sam_spa in sample_space):		
					possible_samples <- concat(possible_samples, concat(list_to_expand, sam_spa))
```

####	Subcoalition list generation
To generate a list of all possible subcoalitions, I use a _k_-combinations algorithm. The algorithm is summarised as a flowchart in Figure 10. At any point, the procedure has a coalition of _n_ it looks at, a subcoalition of _k_ players within that coalition, and a list of all coalitions it has looked at. The algorithm starts at a subcoalition with players 1 to _k_ according to some index (in NetLogo, the who numbers) and an empty list of subcoalitions it has visitied. It also generates a subcoalition with maximal index values _n_ - _k_ to _n_. The procedure starts by adding its given subcoalition to the list of all coalitions it has looked at. It then seens if this subcoalition is the subcoalition with maximal index values. If that is so, it stops and returns the list of subcoalitions it has already looked at. Otherwise, the prodecure uses a meta-index that accesses one of the players in the supplied _k_-player subcoalition. The procedure tries whether it can increment the index of the player accessed by the meta-index by one without exceeding the maximum for that index. If it can, it does so. If it cannot, it goes to the nearest element to the left of it that it is not at its maximum value, increments that by one, and sets the elements to the right of that element to the numbers that follow it. After incrementing in one of the two ways, the procedure calls itself using the changed subcoalition as the subcoalition it starts with and the expanded list of subcoalitions it has already looked at.

__Figure 10__
Flowchart of the combination generating algorithm
![Flowchart of combination generator](file:Figures\flowchart_combinatorics_algorithm.png)
_Note:_ |coalition| denotes the size (cardinality) of the coalition (set) here.


##	References
Acemoglu, D., Egorov, G., & Sonin, K. (2008). Coalition-formation in non-democracies. _Review of Economic Studies_, _75_(4), 987-1009. https://doi.org/10.1111/j.1467-937X.2008.00503.x

Acemoglu, D., Egorov, G., & Sonin, K. (2009). Do Juntas Lead to Personal Rule? _American Economic Review_, _99_(2), 298–303. https://doi.org/10.1257/aer.99.2.298

Baturo, A., & Elkink, J. A. (2017). On the importance of personal sources of power in politics: Comparative perspectives and research agenda. _French Politics_, _15_(4), 505–525. https://doi.org/10.1057/s41253-017-0033-x

Baturo, A., & Elkink, J. A. (2021). _The new Kremlinology: Understanding regime personalization in Russia_ (First edition). Oxford University Press.

Coulom, R. (2007). Efficient Selectivity and Backup Operators in Monte-Carlo Tree Search. In H. J. Van Den Herik, P. Ciancarini, & H. H. L. M. Donkers (Eds.), _Computers and Games_ (Vol. 4630, pp. 72–83). Springer Berlin Heidelberg. https://doi.org/10.1007/978-3-540-75538-8_7

Francois, P., Trebbi, F., & Xiao, K. (2023). Factions in Nondemocracies: Theory and Evidence from the Chinese Communist Party. _Econometrica_, _91_(2), 565–603. https://doi.org/10.3982/ECTA19274

Fruchterman, T. M. J., & Reingold, E. M. (1991). Graph drawing by force-directed placement. _Software: Practice and Experience_, _21_(11), 1129–1164. https://doi.org/10.1002/spe.4380211102

Geddes, B., Wright, J., & Frantz, E. (2018). _How Dictatorships Work: Power, Personalization, and Collapse_ (1st ed.). Cambridge University Press. https://doi.org/10.1017/9781316336182

Gill, G. J. (2021). _Bridling dictators: Rules and authoritarian politics_. Oxford University Press.

Kocsis, L., & Szepesvári, C. (2006). Bandit Based Monte-Carlo Planning. In J. Fürnkranz, T. Scheffer, & M. Spiliopoulou (Eds.), _Machine Learning: ECML 2006_ (Vol. 4212, pp. 282–293). Springer Berlin Heidelberg. https://doi.org/10.1007/11871842_29

Newson, A., & Trebbi, F. (2018). Authoritarian elites. _Canadian Journal of Economics/Revue Canadienne d’économique_, _51_(4), 1088–1117. https://doi.org/10.1111/caje.12362
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
NetLogo 6.4.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="MCTS_parameter_exploration_V2" repetitions="100" runMetricsEveryStep="true">
    <setup>setup
reset-timer</setup>
    <go>go</go>
    <metric>sort [who] of turtles with [in-coalition?]</metric>
    <metric>current-random-seed</metric>
    <metric>timer</metric>
    <metric>map [a -&gt; [residual-power] of a] sort-on [who] turtles</metric>
    <metric>map [a -&gt; [network-power] of a] sort-on [who] turtles</metric>
    <metric>map [a -&gt; [positional-power] of a] sort-on [who] turtles</metric>
    <metric>map [a -&gt; [isolated-payoff] of a] sort-on [who] turtles</metric>
    <metric>map [a -&gt; [payoff] of a] sort-on [who] turtles</metric>
    <enumeratedValueSet variable="number-of-factions">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patron-to-client-payoff-transfer">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print-decisions?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MCTS-tree-search?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="one-step-power-transfers?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="constant-sum-payoff-transfers?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-user-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="client-to-patron-payoff-transfer">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manually-specify-power?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patronage-power-transfer">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-patronage-power-transfer">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="UCT-constant">
      <value value="0.0625"/>
      <value value="0.125"/>
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="1"/>
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="winning-coalition-size">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="penalty">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percentage-rewired">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="debug-display?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patronage-network-structure">
      <value value="&quot;None&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="positions-power-string">
      <value value="&quot;[0]&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-decision-trees-to-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manual-agent-type-input?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manual-agent-types">
      <value value="&quot;[]&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manual-reward-thresholds">
      <value value="&quot;[]&quot;"/>
    </enumeratedValueSet>
    <subExperiment>
      <steppedValueSet variable="MCTS-trials" first="10" step="15" last="100"/>
      <enumeratedValueSet variable="initial-coalition-size">
        <value value="3"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="manually-specified-power">
        <value value="&quot;[3 4 5]&quot;"/>
      </enumeratedValueSet>
    </subExperiment>
    <subExperiment>
      <steppedValueSet variable="MCTS-trials" first="100" step="50" last="250"/>
      <enumeratedValueSet variable="initial-coalition-size">
        <value value="4"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="manually-specified-power">
        <value value="&quot;[3 4 5 10]&quot;"/>
      </enumeratedValueSet>
    </subExperiment>
    <subExperiment>
      <steppedValueSet variable="MCTS-trials" first="200" step="100" last="600"/>
      <enumeratedValueSet variable="initial-coalition-size">
        <value value="5"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="manually-specified-power">
        <value value="&quot;[3 4 5 10 20]&quot;"/>
      </enumeratedValueSet>
    </subExperiment>
  </experiment>
  <experiment name="triad_experiment" repetitions="100" runMetricsEveryStep="true">
    <setup>reset-timer
setup</setup>
    <go>go</go>
    <metric>sort [who] of turtles with [in-coalition?]</metric>
    <metric>timer</metric>
    <metric>current-random-seed</metric>
    <metric>map [a -&gt; [residual-power] of a] sort-on [who] turtles</metric>
    <metric>map [a -&gt; [network-power] of a] sort-on [who] turtles</metric>
    <metric>map [a -&gt; [positional-power] of a] sort-on [who] turtles</metric>
    <metric>map [a -&gt; [isolated-payoff] of a] sort-on [who] turtles</metric>
    <metric>map [a -&gt; [payoff] of a] sort-on [who] turtles</metric>
    <enumeratedValueSet variable="initial-coalition-size">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="constant-sum-payoff-transfers?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="one-step-power-transfers?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-factions">
      <value value="3"/>
    </enumeratedValueSet>
    <steppedValueSet variable="patron-to-client-payoff-transfer" first="0" step="0.1" last="1"/>
    <enumeratedValueSet variable="print-decisions?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-decision-trees-to-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MCTS-tree-search?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-user-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="client-to-patron-payoff-transfer">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manually-specify-power?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manually-assign-positions?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="positions-assignment-string">
      <value value="&quot;[2]&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="UCT-constant">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MCTS-trials">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="current-random-seed">
      <value value="56307012"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="penalty">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="winning-coalition-size">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percentage-rewired">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="debug-display?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patronage-network-structure">
      <value value="&quot;Manual&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manual-network-input-method">
      <value value="&quot;Edgelist&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manual-patronage-network-input">
      <value value="&quot;[\n[0 \&quot;--\&quot; 1]\n]&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manual-agent-type-input?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manual-agent-types">
      <value value="&quot;[]&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manual-reward-thresholds">
      <value value="&quot;[]&quot;"/>
    </enumeratedValueSet>
    <subExperiment>
      <steppedValueSet variable="patronage-power-transfer" first="0" step="0.1" last="0.5"/>
      <enumeratedValueSet variable="max-patronage-power-transfer">
        <value value="0.5"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="manually-specified-power">
        <value value="&quot;[3 4 5]&quot;"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="positions-power-string">
        <value value="&quot;[0]&quot;"/>
      </enumeratedValueSet>
    </subExperiment>
    <subExperiment>
      <enumeratedValueSet variable="patronage-power-transfer">
        <value value="0.6"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="max-patronage-power-transfer">
        <value value="0.6"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="manually-specified-power">
        <value value="&quot;[3 4 5]&quot;"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="positions-power-string">
        <value value="&quot;[0]&quot;"/>
      </enumeratedValueSet>
    </subExperiment>
    <subExperiment>
      <enumeratedValueSet variable="patronage-power-transfer">
        <value value="0.7"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="max-patronage-power-transfer">
        <value value="0.7"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="manually-specified-power">
        <value value="&quot;[3 4 5]&quot;"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="positions-power-string">
        <value value="&quot;[0]&quot;"/>
      </enumeratedValueSet>
    </subExperiment>
    <subExperiment>
      <enumeratedValueSet variable="patronage-power-transfer">
        <value value="0.8"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="max-patronage-power-transfer">
        <value value="0.8"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="manually-specified-power">
        <value value="&quot;[3 4 5]&quot;"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="positions-power-string">
        <value value="&quot;[0]&quot;"/>
      </enumeratedValueSet>
    </subExperiment>
    <subExperiment>
      <enumeratedValueSet variable="patronage-power-transfer">
        <value value="0.9"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="max-patronage-power-transfer">
        <value value="0.9"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="manually-specified-power">
        <value value="&quot;[3 4 5]&quot;"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="positions-power-string">
        <value value="&quot;[0]&quot;"/>
      </enumeratedValueSet>
    </subExperiment>
    <subExperiment>
      <steppedValueSet variable="patronage-power-transfer" first="0" step="0.1" last="0.5"/>
      <enumeratedValueSet variable="max-patronage-power-transfer">
        <value value="0.5"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="manually-specified-power">
        <value value="&quot;[3 4 2]&quot;"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="positions-power-string">
        <value value="&quot;[3]&quot;"/>
      </enumeratedValueSet>
    </subExperiment>
    <subExperiment>
      <enumeratedValueSet variable="patronage-power-transfer">
        <value value="0.6"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="max-patronage-power-transfer">
        <value value="0.6"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="manually-specified-power">
        <value value="&quot;[3 4 2]&quot;"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="positions-power-string">
        <value value="&quot;[3]&quot;"/>
      </enumeratedValueSet>
    </subExperiment>
    <subExperiment>
      <enumeratedValueSet variable="patronage-power-transfer">
        <value value="0.7"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="max-patronage-power-transfer">
        <value value="0.7"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="manually-specified-power">
        <value value="&quot;[3 4 2]&quot;"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="positions-power-string">
        <value value="&quot;[3]&quot;"/>
      </enumeratedValueSet>
    </subExperiment>
    <subExperiment>
      <enumeratedValueSet variable="patronage-power-transfer">
        <value value="0.8"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="max-patronage-power-transfer">
        <value value="0.8"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="manually-specified-power">
        <value value="&quot;[3 4 2]&quot;"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="positions-power-string">
        <value value="&quot;[3]&quot;"/>
      </enumeratedValueSet>
    </subExperiment>
    <subExperiment>
      <enumeratedValueSet variable="patronage-power-transfer">
        <value value="0.9"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="max-patronage-power-transfer">
        <value value="0.9"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="manually-specified-power">
        <value value="&quot;[3 4 2]&quot;"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="positions-power-string">
        <value value="&quot;[3]&quot;"/>
      </enumeratedValueSet>
    </subExperiment>
  </experiment>
  <experiment name="galtung_experiment" repetitions="100" runMetricsEveryStep="true">
    <setup>reset-timer
setup</setup>
    <go>go</go>
    <metric>sort [who] of turtles with [in-coalition?]</metric>
    <metric>timer</metric>
    <metric>current-random-seed</metric>
    <metric>map [a -&gt; [residual-power] of a] sort-on [who] turtles</metric>
    <metric>map [a -&gt; [network-power] of a] sort-on [who] turtles</metric>
    <metric>map [a -&gt; [positional-power] of a] sort-on [who] turtles</metric>
    <metric>map [a -&gt; [isolated-payoff] of a] sort-on [who] turtles</metric>
    <metric>map [a -&gt; [payoff] of a] sort-on [who] turtles</metric>
    <enumeratedValueSet variable="initial-coalition-size">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-factions">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print-decisions?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-decision-trees-to-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="constant-sum-payoff-transfers?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="one-step-power-transfers?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-user-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MCTS-tree-search?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="client-to-patron-payoff-transfer">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manually-assign-positions?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manually-specified-power">
      <value value="&quot;[3 4 2]&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manually-specify-power?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="positions-assignment-string">
      <value value="&quot;[0]&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="UCT-constant">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="winning-coalition-size">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="current-random-seed">
      <value value="-1664174774"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MCTS-trials">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="penalty">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="debug-display?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percentage-rewired">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="positions-power-string">
      <value value="&quot;[0]&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manual-network-input-method">
      <value value="&quot;Edgelist&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manual-agent-type-input?">
      <value value="false"/>
    </enumeratedValueSet>
    <subExperiment>
      <enumeratedValueSet variable="patronage-network-structure">
        <value value="&quot;None&quot;"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="manual-patronage-network-input">
        <value value="&quot;&quot;"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="patron-to-client-payoff-transfer">
        <value value="0"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="patronage-power-transfer">
        <value value="0"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="max-patronage-power-transfer">
        <value value="0"/>
      </enumeratedValueSet>
    </subExperiment>
    <subExperiment>
      <enumeratedValueSet variable="patronage-network-structure">
        <value value="&quot;Manual&quot;"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="manual-patronage-network-input">
        <value value="&quot;[\n[0 \&quot;--\&quot; 1]\n[0 \&quot;--\&quot; 3]\n[0 \&quot;--\&quot; 4]\n[0 \&quot;--\&quot; 5]\n[0 \&quot;--\&quot; 6]\n[1 \&quot;--\&quot; 2]\n[1 \&quot;--\&quot; 7]\n[1 \&quot;--\&quot; 8]\n[1 \&quot;--\&quot; 9]\n[2 \&quot;--\&quot; 3]\n[2 \&quot;--\&quot; 10]\n[2 \&quot;--\&quot; 11]\n[2 \&quot;--\&quot; 12]\n[3 \&quot;--\&quot; 13]\n[3 \&quot;--\&quot; 14]\n[3 \&quot;--\&quot; 15]\n]\n&quot;"/>
        <value value="&quot;[\n[0 \&quot;--\&quot; 1]\n[0 \&quot;--\&quot; 2]\n[0 \&quot;--\&quot; 3]\n[0 \&quot;--\&quot; 4]\n[0 \&quot;--\&quot; 5]\n[0 \&quot;--\&quot; 6]\n[1 \&quot;--\&quot; 2]\n[1 \&quot;--\&quot; 3]\n[1 \&quot;--\&quot; 7]\n[1 \&quot;--\&quot; 8]\n[1 \&quot;--\&quot; 9]\n[2 \&quot;--\&quot; 3]\n[2 \&quot;--\&quot; 10]\n[2 \&quot;--\&quot; 11]\n[2 \&quot;--\&quot; 12]\n[3 \&quot;--\&quot; 13]\n[3 \&quot;--\&quot; 14]\n[3 \&quot;--\&quot; 15]\n]\n&quot;"/>
        <value value="&quot;[\n[0 \&quot;--\&quot; 1]\n[0 \&quot;--\&quot; 2]\n[0 \&quot;--\&quot; 3]\n[0 \&quot;--\&quot; 4]\n[0 \&quot;--\&quot; 5]\n[0 \&quot;--\&quot; 6]\n[1 \&quot;--\&quot; 2]\n[1 \&quot;--\&quot; 3]\n[1 \&quot;--\&quot; 7]\n[1 \&quot;--\&quot; 8]\n[1 \&quot;--\&quot; 9]\n[2 \&quot;--\&quot; 3]\n[2 \&quot;--\&quot; 10]\n[2 \&quot;--\&quot; 11]\n[2 \&quot;--\&quot; 12]\n[3 \&quot;--\&quot; 13]\n[3 \&quot;--\&quot; 14]\n[3 \&quot;--\&quot; 15]\n]\n[6 \&quot;--\&quot; 7]\n]\n&quot;"/>
        <value value="&quot;[\n[0 \&quot;--\&quot; 1]\n[0 \&quot;--\&quot; 2]\n[0 \&quot;--\&quot; 3]\n[0 \&quot;--\&quot; 4]\n[0 \&quot;--\&quot; 5]\n[0 \&quot;--\&quot; 6]\n[1 \&quot;--\&quot; 2]\n[1 \&quot;--\&quot; 3]\n[1 \&quot;--\&quot; 7]\n[1 \&quot;--\&quot; 8]\n[1 \&quot;--\&quot; 9]\n[2 \&quot;--\&quot; 3]\n[2 \&quot;--\&quot; 10]\n[2 \&quot;--\&quot; 11]\n[2 \&quot;--\&quot; 12]\n[3 \&quot;--\&quot; 13]\n[3 \&quot;--\&quot; 14]\n[3 \&quot;--\&quot; 15]\n]\n[6 \&quot;--\&quot; 8]\n]\n&quot;"/>
        <value value="&quot;[\n[0 \&quot;--\&quot; 1]\n[0 \&quot;--\&quot; 2]\n[0 \&quot;--\&quot; 3]\n[0 \&quot;--\&quot; 4]\n[0 \&quot;--\&quot; 5]\n[0 \&quot;--\&quot; 6]\n[1 \&quot;--\&quot; 2]\n[1 \&quot;--\&quot; 3]\n[1 \&quot;--\&quot; 7]\n[1 \&quot;--\&quot; 8]\n[1 \&quot;--\&quot; 9]\n[2 \&quot;--\&quot; 3]\n[2 \&quot;--\&quot; 10]\n[2 \&quot;--\&quot; 11]\n[2 \&quot;--\&quot; 12]\n[3 \&quot;--\&quot; 13]\n[3 \&quot;--\&quot; 14]\n[3 \&quot;--\&quot; 15]\n]\n[3 \&quot;--\&quot; 12]\n]\n&quot;"/>
        <value value="&quot;[\n[0 \&quot;--\&quot; 1]\n[0 \&quot;--\&quot; 2]\n[0 \&quot;--\&quot; 3]\n[0 \&quot;--\&quot; 4]\n[0 \&quot;--\&quot; 5]\n[0 \&quot;--\&quot; 6]\n[1 \&quot;--\&quot; 2]\n[1 \&quot;--\&quot; 3]\n[1 \&quot;--\&quot; 7]\n[1 \&quot;--\&quot; 8]\n[1 \&quot;--\&quot; 9]\n[2 \&quot;--\&quot; 3]\n[2 \&quot;--\&quot; 10]\n[2 \&quot;--\&quot; 11]\n[2 \&quot;--\&quot; 12]\n[3 \&quot;--\&quot; 13]\n[3 \&quot;--\&quot; 14]\n[3 \&quot;--\&quot; 15]\n]\n[6 \&quot;--\&quot; 7]\n[6 \&quot;--\&quot; 8]\n&quot;"/>
        <value value="&quot;[\n[0 \&quot;--\&quot; 1]\n[0 \&quot;--\&quot; 2]\n[0 \&quot;--\&quot; 3]\n[0 \&quot;--\&quot; 4]\n[0 \&quot;--\&quot; 5]\n[0 \&quot;--\&quot; 6]\n[1 \&quot;--\&quot; 2]\n[1 \&quot;--\&quot; 3]\n[1 \&quot;--\&quot; 7]\n[1 \&quot;--\&quot; 8]\n[1 \&quot;--\&quot; 9]\n[2 \&quot;--\&quot; 3]\n[2 \&quot;--\&quot; 10]\n[2 \&quot;--\&quot; 11]\n[2 \&quot;--\&quot; 12]\n[3 \&quot;--\&quot; 13]\n[3 \&quot;--\&quot; 14]\n[3 \&quot;--\&quot; 15]\n]\n[6 \&quot;--\&quot; 7]\n[3 \&quot;--\&quot; 12]\n&quot;"/>
        <value value="&quot;[\n[0 \&quot;--\&quot; 1]\n[0 \&quot;--\&quot; 2]\n[0 \&quot;--\&quot; 3]\n[0 \&quot;--\&quot; 4]\n[0 \&quot;--\&quot; 5]\n[0 \&quot;--\&quot; 6]\n[1 \&quot;--\&quot; 2]\n[1 \&quot;--\&quot; 3]\n[1 \&quot;--\&quot; 7]\n[1 \&quot;--\&quot; 8]\n[1 \&quot;--\&quot; 9]\n[2 \&quot;--\&quot; 3]\n[2 \&quot;--\&quot; 10]\n[2 \&quot;--\&quot; 11]\n[2 \&quot;--\&quot; 12]\n[3 \&quot;--\&quot; 13]\n[3 \&quot;--\&quot; 14]\n[3 \&quot;--\&quot; 15]\n]\n[6 \&quot;--\&quot; 8]\n[3 \&quot;--\&quot; 12]\n&quot;"/>
        <value value="&quot;[\n[0 \&quot;--\&quot; 1]\n[0 \&quot;--\&quot; 2]\n[0 \&quot;--\&quot; 3]\n[0 \&quot;--\&quot; 4]\n[0 \&quot;--\&quot; 5]\n[0 \&quot;--\&quot; 6]\n[1 \&quot;--\&quot; 2]\n[1 \&quot;--\&quot; 3]\n[1 \&quot;--\&quot; 7]\n[1 \&quot;--\&quot; 8]\n[1 \&quot;--\&quot; 9]\n[2 \&quot;--\&quot; 3]\n[2 \&quot;--\&quot; 10]\n[2 \&quot;--\&quot; 11]\n[2 \&quot;--\&quot; 12]\n[3 \&quot;--\&quot; 13]\n[3 \&quot;--\&quot; 14]\n[3 \&quot;--\&quot; 15]\n]\n[6 \&quot;--\&quot; 7]\n[6 \&quot;--\&quot; 8]\n[3 \&quot;--\&quot; 12]\n&quot;"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="patron-to-client-payoff-transfer">
        <value value="0.6"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="patronage-power-transfer">
        <value value="0.3"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="max-patronage-power-transfer">
        <value value="0.5"/>
      </enumeratedValueSet>
    </subExperiment>
  </experiment>
  <experiment name="star_experiment" repetitions="100" runMetricsEveryStep="true">
    <setup>reset-timer
setup</setup>
    <go>go</go>
    <metric>sort [who] of turtles with [in-coalition?]</metric>
    <metric>timer</metric>
    <metric>current-random-seed</metric>
    <metric>map [a -&gt; [residual-power] of a] sort-on [who] turtles</metric>
    <metric>map [a -&gt; [network-power] of a] sort-on [who] turtles</metric>
    <metric>map [a -&gt; [positional-power] of a] sort-on [who] turtles</metric>
    <metric>map [a -&gt; [isolated-payoff] of a] sort-on [who] turtles</metric>
    <metric>map [a -&gt; [payoff] of a] sort-on [who] turtles</metric>
    <metric>out-network-matrix</metric>
    <enumeratedValueSet variable="manual-patronage-network-input">
      <value value="&quot;[\n[0 \&quot;--\&quot; 1]\n[0 \&quot;--\&quot; 2]\n[0 \&quot;--\&quot; 3]\n[0 \&quot;--\&quot; 4]\n]&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-factions">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patron-to-client-payoff-transfer">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print-decisions?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="constant-sum-payoff-transfers?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="one-step-power-transfers?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-user-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MCTS-tree-search?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="client-to-patron-payoff-transfer">
      <value value="0"/>
      <value value="0.3"/>
      <value value="0.6"/>
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manually-assign-positions?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manually-specified-power">
      <value value="&quot;[4.6 4.9 4.2 5.3 5.4]&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manually-specify-power?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="positions-assignment-string">
      <value value="&quot;[0]&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="UCT-constant">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="current-random-seed">
      <value value="389656894"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MCTS-trials">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="penalty">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="winning-coalition-size">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-decision-trees-to-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="debug-display?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percentage-rewired">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patronage-network-structure">
      <value value="&quot;Manual&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="positions-power-string">
      <value value="&quot;[0]&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manual-network-input-method">
      <value value="&quot;Edgelist&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-coalition-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manual-agent-type-input?">
      <value value="false"/>
    </enumeratedValueSet>
    <subExperiment>
      <enumeratedValueSet variable="patronage-power-transfer">
        <value value="0"/>
        <value value="0.3"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="max-patronage-power-transfer">
        <value value="0.5"/>
      </enumeratedValueSet>
    </subExperiment>
    <subExperiment>
      <enumeratedValueSet variable="patronage-power-transfer">
        <value value="0.6"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="max-patronage-power-transfer">
        <value value="0.6"/>
      </enumeratedValueSet>
    </subExperiment>
    <subExperiment>
      <enumeratedValueSet variable="patronage-power-transfer">
        <value value="0.9"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="max-patronage-power-transfer">
        <value value="0.9"/>
      </enumeratedValueSet>
    </subExperiment>
  </experiment>
  <experiment name="variable_cohesion_small_experiment" repetitions="100" runMetricsEveryStep="true">
    <setup>reset-timer
setup</setup>
    <go>go</go>
    <metric>sort [who] of turtles with [in-coalition?]</metric>
    <metric>timer</metric>
    <metric>current-random-seed</metric>
    <metric>map [a -&gt; [residual-power] of a] sort-on [who] turtles</metric>
    <metric>map [a -&gt; [network-power] of a] sort-on [who] turtles</metric>
    <metric>map [a -&gt; [positional-power] of a] sort-on [who] turtles</metric>
    <metric>map [a -&gt; [isolated-payoff] of a] sort-on [who] turtles</metric>
    <metric>map [a -&gt; [payoff] of a] sort-on [who] turtles</metric>
    <metric>out-network-matrix</metric>
    <enumeratedValueSet variable="patron-to-client-payoff-transfer">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print-decisions?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MCTS-tree-search?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="constant-sum-payoff-transfers?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="one-step-power-transfers?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-factions">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-user-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manually-assign-positions?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manually-specified-power">
      <value value="&quot;[3 4 5 10]&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manually-specify-power?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="positions-assignment-string">
      <value value="&quot;[0]&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="UCT-constant">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="current-random-seed">
      <value value="-49548211"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="winning-coalition-size">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="penalty">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MCTS-trials">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-decision-trees-to-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="debug-display?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patronage-network-structure">
      <value value="&quot;Manual&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="positions-power-string">
      <value value="&quot;[0]&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manual-network-input-method">
      <value value="&quot;Edgelist&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percentage-rewired">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-coalition-size">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manual-agent-type-input?">
      <value value="false"/>
    </enumeratedValueSet>
    <subExperiment>
      <enumeratedValueSet variable="client-to-patron-payoff-transfer">
        <value value="0"/>
        <value value="0.3"/>
        <value value="0.6"/>
        <value value="0.9"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="patronage-power-transfer">
        <value value="0"/>
        <value value="0.3"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="max-patronage-power-transfer">
        <value value="0.5"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="manual-patronage-network-input">
        <value value="&quot;[[0 \&quot;--\&quot; 1][0 \&quot;--\&quot; 2][0 \&quot;--\&quot; 3][1 \&quot;--\&quot; 2][1 \&quot;--\&quot; 3][2 \&quot;--\&quot; 3]]&quot;"/>
        <value value="&quot;[[0 \&quot;--\&quot; 1][0 \&quot;--\&quot; 3][1 \&quot;--\&quot; 2][2 \&quot;--\&quot; 3]]&quot;"/>
        <value value="&quot;[[0 \&quot;--\&quot; 1][2 \&quot;--\&quot; 3]]&quot;"/>
      </enumeratedValueSet>
    </subExperiment>
    <subExperiment>
      <enumeratedValueSet variable="client-to-patron-payoff-transfer">
        <value value="0"/>
        <value value="0.3"/>
        <value value="0.6"/>
        <value value="0.9"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="patronage-power-transfer">
        <value value="0.6"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="max-patronage-power-transfer">
        <value value="0.6"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="manual-patronage-network-input">
        <value value="&quot;[[0 \&quot;--\&quot; 1][0 \&quot;--\&quot; 2][0 \&quot;--\&quot; 3][1 \&quot;--\&quot; 2][1 \&quot;--\&quot; 3][2 \&quot;--\&quot; 3]]&quot;"/>
        <value value="&quot;[[0 \&quot;--\&quot; 1][0 \&quot;--\&quot; 3][1 \&quot;--\&quot; 2][2 \&quot;--\&quot; 3]]&quot;"/>
        <value value="&quot;[[0 \&quot;--\&quot; 1][2 \&quot;--\&quot; 3]]&quot;"/>
      </enumeratedValueSet>
    </subExperiment>
    <subExperiment>
      <enumeratedValueSet variable="client-to-patron-payoff-transfer">
        <value value="0"/>
        <value value="0.3"/>
        <value value="0.6"/>
        <value value="0.9"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="patronage-power-transfer">
        <value value="0.9"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="max-patronage-power-transfer">
        <value value="0.9"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="manual-patronage-network-input">
        <value value="&quot;[[0 \&quot;--\&quot; 1][0 \&quot;--\&quot; 2][0 \&quot;--\&quot; 3][1 \&quot;--\&quot; 2][1 \&quot;--\&quot; 3][2 \&quot;--\&quot; 3]]&quot;"/>
        <value value="&quot;[[0 \&quot;--\&quot; 1][0 \&quot;--\&quot; 3][1 \&quot;--\&quot; 2][2 \&quot;--\&quot; 3]]&quot;"/>
        <value value="&quot;[[0 \&quot;--\&quot; 1][2 \&quot;--\&quot; 3]]&quot;"/>
      </enumeratedValueSet>
    </subExperiment>
  </experiment>
  <experiment name="variable_cohesion_factions_experiment" repetitions="100" runMetricsEveryStep="true">
    <setup>reset-timer
setup</setup>
    <go>go</go>
    <metric>sort [who] of turtles with [in-coalition?]</metric>
    <metric>timer</metric>
    <metric>current-random-seed</metric>
    <metric>map [a -&gt; [residual-power] of a] sort-on [who] turtles</metric>
    <metric>map [a -&gt; [network-power] of a] sort-on [who] turtles</metric>
    <metric>map [a -&gt; [positional-power] of a] sort-on [who] turtles</metric>
    <metric>map [a -&gt; [isolated-payoff] of a] sort-on [who] turtles</metric>
    <metric>map [a -&gt; [payoff] of a] sort-on [who] turtles</metric>
    <metric>out-network-matrix</metric>
    <enumeratedValueSet variable="patron-to-client-payoff-transfer">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print-decisions?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MCTS-tree-search?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-user-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="constant-sum-payoff-transfers?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="one-step-power-transfers?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manually-assign-positions?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manually-specified-power">
      <value value="&quot;[4.91 4.90 4.17 4.77 4.66 4.93 4.61 4.04]&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manually-specify-power?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="positions-assignment-string">
      <value value="&quot;[0]&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="UCT-constant">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="current-random-seed">
      <value value="1044470964"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="winning-coalition-size">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="penalty">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MCTS-trials">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-decision-trees-to-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="debug-display?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patronage-network-structure">
      <value value="&quot;Manual&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="positions-power-string">
      <value value="&quot;[0]&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manual-network-input-method">
      <value value="&quot;Edgelist&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-coalition-size">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manual-agent-type-input?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-factions">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percentage-rewired">
      <value value="0"/>
    </enumeratedValueSet>
    <subExperiment>
      <enumeratedValueSet variable="client-to-patron-payoff-transfer">
        <value value="0"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="patronage-power-transfer">
        <value value="0"/>
        <value value="0.3"/>
        <value value="0.6"/>
        <value value="0.9"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="max-patronage-power-transfer">
        <value value="0"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="manual-patronage-network-input">
        <value value="&quot;[[0 \&quot;--\&quot; 1][0 \&quot;--\&quot; 2][0 \&quot;--\&quot;3][0 \&quot;--\&quot; 4][0 \&quot;--\&quot; 5][0 \&quot;--\&quot; 6][0 \&quot;--\&quot; 7][1 \&quot;--\&quot; 2][1 \&quot;--\&quot; 3][1 \&quot;--\&quot; 4][1 \&quot;--\&quot; 5][1 \&quot;--\&quot; 6][1 \&quot;--\&quot; 7][2 \&quot;--\&quot; 3][2 \&quot;--\&quot; 4][2 \&quot;--\&quot; 5][2 \&quot;--\&quot; 6][2 \&quot;--\&quot; 7][3 \&quot;--\&quot; 4][3 \&quot;--\&quot; 5][3 \&quot;--\&quot; 6][3 \&quot;--\&quot; 7][4 \&quot;--\&quot; 5][4 \&quot;--\&quot; 6][4 \&quot;--\&quot; 7][5 \&quot;--\&quot; 6][5 \&quot;--\&quot; 7][6 \&quot;--\&quot; 7]]&quot;"/>
        <value value="&quot;[[0 \&quot;--\&quot; 1][0 \&quot;--\&quot; 2][0 \&quot;--\&quot; 4][1 \&quot;--\&quot; 2][1 \&quot;--\&quot; 4][1 \&quot;--\&quot; 6][2 \&quot;--\&quot; 3][2 \&quot;--\&quot; 4][2 \&quot;--\&quot; 5][3 \&quot;--\&quot; 4][3 \&quot;--\&quot; 5][3 \&quot;--\&quot; 6][3 \&quot;--\&quot; 7][4 \&quot;--\&quot; 5][4 \&quot;--\&quot; 6][4 \&quot;--\&quot; 7][5 \&quot;--\&quot; 7][6 \&quot;--\&quot; 7]]&quot;"/>
        <value value="&quot;[[0 \&quot;--\&quot; 1][0 \&quot;--\&quot; 4][1 \&quot;--\&quot; 2][2 \&quot;--\&quot; 3][3 \&quot;--\&quot; 4][3 \&quot;--\&quot; 5][3 \&quot;--\&quot; 6][3 \&quot;--\&quot; 7][6 \&quot;--\&quot; 7]]&quot;"/>
        <value value="&quot;[[0 \&quot;--\&quot; 1][0 \&quot;--\&quot; 2][0 \&quot;--\&quot;3][1 \&quot;--\&quot; 2][1 \&quot;--\&quot; 3][2 \&quot;--\&quot; 3][4 \&quot;--\&quot; 5][4 \&quot;--\&quot; 6][4 \&quot;--\&quot; 7][5 \&quot;--\&quot; 6][5 \&quot;--\&quot; 7][6 \&quot;--\&quot; 7]]&quot;"/>
        <value value="&quot;[[0 \&quot;--\&quot; 1][0 \&quot;--\&quot; 2][0 \&quot;--\&quot;3][2 \&quot;--\&quot; 3][4 \&quot;--\&quot; 5][4 \&quot;--\&quot; 6][4 \&quot;--\&quot; 7][6 \&quot;--\&quot; 7]]&quot;"/>
        <value value="&quot;[[0 \&quot;--\&quot; 1][0 \&quot;--\&quot;3][4 \&quot;--\&quot; 7][6 \&quot;--\&quot; 7]]&quot;"/>
      </enumeratedValueSet>
    </subExperiment>
    <subExperiment>
      <enumeratedValueSet variable="client-to-patron-payoff-transfer">
        <value value="0.3"/>
        <value value="0.6"/>
        <value value="0.9"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="patronage-power-transfer">
        <value value="0"/>
        <value value="0.3"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="max-patronage-power-transfer">
        <value value="0.5"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="manual-patronage-network-input">
        <value value="&quot;[[0 \&quot;--\&quot; 1][0 \&quot;--\&quot; 2][0 \&quot;--\&quot;3][0 \&quot;--\&quot; 4][0 \&quot;--\&quot; 5][0 \&quot;--\&quot; 6][0 \&quot;--\&quot; 7][1 \&quot;--\&quot; 2][1 \&quot;--\&quot; 3][1 \&quot;--\&quot; 4][1 \&quot;--\&quot; 5][1 \&quot;--\&quot; 6][1 \&quot;--\&quot; 7][2 \&quot;--\&quot; 3][2 \&quot;--\&quot; 4][2 \&quot;--\&quot; 5][2 \&quot;--\&quot; 6][2 \&quot;--\&quot; 7][3 \&quot;--\&quot; 4][3 \&quot;--\&quot; 5][3 \&quot;--\&quot; 6][3 \&quot;--\&quot; 7][4 \&quot;--\&quot; 5][4 \&quot;--\&quot; 6][4 \&quot;--\&quot; 7][5 \&quot;--\&quot; 6][5 \&quot;--\&quot; 7][6 \&quot;--\&quot; 7]]&quot;"/>
        <value value="&quot;[[0 \&quot;--\&quot; 1][0 \&quot;--\&quot; 2][0 \&quot;--\&quot; 4][1 \&quot;--\&quot; 2][1 \&quot;--\&quot; 4][1 \&quot;--\&quot; 6][2 \&quot;--\&quot; 3][2 \&quot;--\&quot; 4][2 \&quot;--\&quot; 5][3 \&quot;--\&quot; 4][3 \&quot;--\&quot; 5][3 \&quot;--\&quot; 6][3 \&quot;--\&quot; 7][4 \&quot;--\&quot; 5][4 \&quot;--\&quot; 6][4 \&quot;--\&quot; 7][5 \&quot;--\&quot; 7][6 \&quot;--\&quot; 7]]&quot;"/>
        <value value="&quot;[[0 \&quot;--\&quot; 1][0 \&quot;--\&quot; 4][1 \&quot;--\&quot; 2][2 \&quot;--\&quot; 3][3 \&quot;--\&quot; 4][3 \&quot;--\&quot; 5][3 \&quot;--\&quot; 6][3 \&quot;--\&quot; 7][6 \&quot;--\&quot; 7]]&quot;"/>
        <value value="&quot;[[0 \&quot;--\&quot; 1][0 \&quot;--\&quot; 2][0 \&quot;--\&quot;3][1 \&quot;--\&quot; 2][1 \&quot;--\&quot; 3][2 \&quot;--\&quot; 3][4 \&quot;--\&quot; 5][4 \&quot;--\&quot; 6][4 \&quot;--\&quot; 7][5 \&quot;--\&quot; 6][5 \&quot;--\&quot; 7][6 \&quot;--\&quot; 7]]&quot;"/>
        <value value="&quot;[[0 \&quot;--\&quot; 1][0 \&quot;--\&quot; 2][0 \&quot;--\&quot;3][2 \&quot;--\&quot; 3][4 \&quot;--\&quot; 5][4 \&quot;--\&quot; 6][4 \&quot;--\&quot; 7][6 \&quot;--\&quot; 7]]&quot;"/>
        <value value="&quot;[[0 \&quot;--\&quot; 1][0 \&quot;--\&quot;3][4 \&quot;--\&quot; 7][6 \&quot;--\&quot; 7]]&quot;"/>
      </enumeratedValueSet>
    </subExperiment>
    <subExperiment>
      <enumeratedValueSet variable="client-to-patron-payoff-transfer">
        <value value="0.3"/>
        <value value="0.6"/>
        <value value="0.9"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="patronage-power-transfer">
        <value value="0.6"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="max-patronage-power-transfer">
        <value value="0.6"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="manual-patronage-network-input">
        <value value="&quot;[[0 \&quot;--\&quot; 1][0 \&quot;--\&quot; 2][0 \&quot;--\&quot;3][0 \&quot;--\&quot; 4][0 \&quot;--\&quot; 5][0 \&quot;--\&quot; 6][0 \&quot;--\&quot; 7][1 \&quot;--\&quot; 2][1 \&quot;--\&quot; 3][1 \&quot;--\&quot; 4][1 \&quot;--\&quot; 5][1 \&quot;--\&quot; 6][1 \&quot;--\&quot; 7][2 \&quot;--\&quot; 3][2 \&quot;--\&quot; 4][2 \&quot;--\&quot; 5][2 \&quot;--\&quot; 6][2 \&quot;--\&quot; 7][3 \&quot;--\&quot; 4][3 \&quot;--\&quot; 5][3 \&quot;--\&quot; 6][3 \&quot;--\&quot; 7][4 \&quot;--\&quot; 5][4 \&quot;--\&quot; 6][4 \&quot;--\&quot; 7][5 \&quot;--\&quot; 6][5 \&quot;--\&quot; 7][6 \&quot;--\&quot; 7]]&quot;"/>
        <value value="&quot;[[0 \&quot;--\&quot; 1][0 \&quot;--\&quot; 2][0 \&quot;--\&quot; 4][1 \&quot;--\&quot; 2][1 \&quot;--\&quot; 4][1 \&quot;--\&quot; 6][2 \&quot;--\&quot; 3][2 \&quot;--\&quot; 4][2 \&quot;--\&quot; 5][3 \&quot;--\&quot; 4][3 \&quot;--\&quot; 5][3 \&quot;--\&quot; 6][3 \&quot;--\&quot; 7][4 \&quot;--\&quot; 5][4 \&quot;--\&quot; 6][4 \&quot;--\&quot; 7][5 \&quot;--\&quot; 7][6 \&quot;--\&quot; 7]]&quot;"/>
        <value value="&quot;[[0 \&quot;--\&quot; 1][0 \&quot;--\&quot; 4][1 \&quot;--\&quot; 2][2 \&quot;--\&quot; 3][3 \&quot;--\&quot; 4][3 \&quot;--\&quot; 5][3 \&quot;--\&quot; 6][3 \&quot;--\&quot; 7][6 \&quot;--\&quot; 7]]&quot;"/>
        <value value="&quot;[[0 \&quot;--\&quot; 1][0 \&quot;--\&quot; 2][0 \&quot;--\&quot;3][1 \&quot;--\&quot; 2][1 \&quot;--\&quot; 3][2 \&quot;--\&quot; 3][4 \&quot;--\&quot; 5][4 \&quot;--\&quot; 6][4 \&quot;--\&quot; 7][5 \&quot;--\&quot; 6][5 \&quot;--\&quot; 7][6 \&quot;--\&quot; 7]]&quot;"/>
        <value value="&quot;[[0 \&quot;--\&quot; 1][0 \&quot;--\&quot; 2][0 \&quot;--\&quot;3][2 \&quot;--\&quot; 3][4 \&quot;--\&quot; 5][4 \&quot;--\&quot; 6][4 \&quot;--\&quot; 7][6 \&quot;--\&quot; 7]]&quot;"/>
        <value value="&quot;[[0 \&quot;--\&quot; 1][0 \&quot;--\&quot;3][4 \&quot;--\&quot; 7][6 \&quot;--\&quot; 7]]&quot;"/>
      </enumeratedValueSet>
    </subExperiment>
    <subExperiment>
      <enumeratedValueSet variable="client-to-patron-payoff-transfer">
        <value value="0.3"/>
        <value value="0.6"/>
        <value value="0.9"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="patronage-power-transfer">
        <value value="0.9"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="max-patronage-power-transfer">
        <value value="0.9"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="manual-patronage-network-input">
        <value value="&quot;[[0 \&quot;--\&quot; 1][0 \&quot;--\&quot; 2][0 \&quot;--\&quot;3][0 \&quot;--\&quot; 4][0 \&quot;--\&quot; 5][0 \&quot;--\&quot; 6][0 \&quot;--\&quot; 7][1 \&quot;--\&quot; 2][1 \&quot;--\&quot; 3][1 \&quot;--\&quot; 4][1 \&quot;--\&quot; 5][1 \&quot;--\&quot; 6][1 \&quot;--\&quot; 7][2 \&quot;--\&quot; 3][2 \&quot;--\&quot; 4][2 \&quot;--\&quot; 5][2 \&quot;--\&quot; 6][2 \&quot;--\&quot; 7][3 \&quot;--\&quot; 4][3 \&quot;--\&quot; 5][3 \&quot;--\&quot; 6][3 \&quot;--\&quot; 7][4 \&quot;--\&quot; 5][4 \&quot;--\&quot; 6][4 \&quot;--\&quot; 7][5 \&quot;--\&quot; 6][5 \&quot;--\&quot; 7][6 \&quot;--\&quot; 7]]&quot;"/>
        <value value="&quot;[[0 \&quot;--\&quot; 1][0 \&quot;--\&quot; 2][0 \&quot;--\&quot; 4][1 \&quot;--\&quot; 2][1 \&quot;--\&quot; 4][1 \&quot;--\&quot; 6][2 \&quot;--\&quot; 3][2 \&quot;--\&quot; 4][2 \&quot;--\&quot; 5][3 \&quot;--\&quot; 4][3 \&quot;--\&quot; 5][3 \&quot;--\&quot; 6][3 \&quot;--\&quot; 7][4 \&quot;--\&quot; 5][4 \&quot;--\&quot; 6][4 \&quot;--\&quot; 7][5 \&quot;--\&quot; 7][6 \&quot;--\&quot; 7]]&quot;"/>
        <value value="&quot;[[0 \&quot;--\&quot; 1][0 \&quot;--\&quot; 4][1 \&quot;--\&quot; 2][2 \&quot;--\&quot; 3][3 \&quot;--\&quot; 4][3 \&quot;--\&quot; 5][3 \&quot;--\&quot; 6][3 \&quot;--\&quot; 7][6 \&quot;--\&quot; 7]]&quot;"/>
        <value value="&quot;[[0 \&quot;--\&quot; 1][0 \&quot;--\&quot; 2][0 \&quot;--\&quot;3][1 \&quot;--\&quot; 2][1 \&quot;--\&quot; 3][2 \&quot;--\&quot; 3][4 \&quot;--\&quot; 5][4 \&quot;--\&quot; 6][4 \&quot;--\&quot; 7][5 \&quot;--\&quot; 6][5 \&quot;--\&quot; 7][6 \&quot;--\&quot; 7]]&quot;"/>
        <value value="&quot;[[0 \&quot;--\&quot; 1][0 \&quot;--\&quot; 2][0 \&quot;--\&quot;3][2 \&quot;--\&quot; 3][4 \&quot;--\&quot; 5][4 \&quot;--\&quot; 6][4 \&quot;--\&quot; 7][6 \&quot;--\&quot; 7]]&quot;"/>
        <value value="&quot;[[0 \&quot;--\&quot; 1][0 \&quot;--\&quot;3][4 \&quot;--\&quot; 7][6 \&quot;--\&quot; 7]]&quot;"/>
      </enumeratedValueSet>
    </subExperiment>
  </experiment>
  <experiment name="old_guard_V2_experiment" repetitions="100" runMetricsEveryStep="true">
    <setup>reset-timer
setup</setup>
    <go>go</go>
    <metric>sort [who] of turtles with [in-coalition?]</metric>
    <metric>timer</metric>
    <metric>current-random-seed</metric>
    <metric>map [a -&gt; [residual-power] of a] sort-on [who] turtles</metric>
    <metric>map [a -&gt; [network-power] of a] sort-on [who] turtles</metric>
    <metric>map [a -&gt; [positional-power] of a] sort-on [who] turtles</metric>
    <metric>map [a -&gt; [isolated-payoff] of a] sort-on [who] turtles</metric>
    <metric>map [a -&gt; [payoff] of a] sort-on [who] turtles</metric>
    <metric>out-network-matrix</metric>
    <enumeratedValueSet variable="manual-patronage-network-input">
      <value value="&quot;[\n[0 \&quot;--\&quot; 4]\n[0 \&quot;--\&quot; 5]\n[0 \&quot;--\&quot; 8]\n[1 \&quot;--\&quot; 2]\n[1 \&quot;--\&quot; 3]\n[1 \&quot;--\&quot; 4]\n[2 \&quot;--\&quot; 3]\n[2 \&quot;--\&quot; 7]\n[3 \&quot;--\&quot; 5]\n[3 \&quot;--\&quot; 6]\n]&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patron-to-client-payoff-transfer">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print-decisions?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-user-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="constant-sum-payoff-transfers?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="one-step-power-transfers?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-factions">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="client-to-patron-payoff-transfer">
      <value value="0"/>
      <value value="0.3"/>
      <value value="0.6"/>
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manually-assign-positions?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="positions-assignment-string">
      <value value="&quot;[0]&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="UCT-constant">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="penalty">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="winning-coalition-size">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-decision-trees-to-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="debug-display?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="positions-power-string">
      <value value="&quot;[5]&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percentage-rewired">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MCTS-tree-search?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manually-specified-power">
      <value value="&quot;[5 4 5.33333 5.2 4.6 4.7 4.2 4.5 4.1]&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manually-specify-power?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MCTS-trials">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="power-transfer-error">
      <value value="1.0E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="current-random-seed">
      <value value="-244371526"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patronage-network-structure">
      <value value="&quot;Manual&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manual-network-input-method">
      <value value="&quot;Edgelist&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decision-tree-file">
      <value value="&quot;trees_file.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-coalition-size">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manual-agent-type-input?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manual-agent-types">
      <value value="&quot;[\&quot;Maximiser\&quot; \&quot;Satisficer\&quot; \&quot;Satisficer\&quot; \&quot;Satisficer\&quot; \&quot;Maximiser\&quot; \&quot;Maximiser\&quot; \&quot;Maximiser\&quot; \&quot;Maximiser\&quot; \&quot;Maximiser\&quot;]&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manual-reward-thresholds">
      <value value="&quot;[0.20 0.08 0.11 0.11 0.09 0.10 0.09 0.09 0.08]&quot;"/>
    </enumeratedValueSet>
    <subExperiment>
      <enumeratedValueSet variable="patronage-power-transfer">
        <value value="0"/>
        <value value="0.3"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="max-patronage-power-transfer">
        <value value="0.5"/>
      </enumeratedValueSet>
    </subExperiment>
    <subExperiment>
      <enumeratedValueSet variable="patronage-power-transfer">
        <value value="0.6"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="max-patronage-power-transfer">
        <value value="0.6"/>
      </enumeratedValueSet>
    </subExperiment>
    <subExperiment>
      <enumeratedValueSet variable="patronage-power-transfer">
        <value value="0.9"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="max-patronage-power-transfer">
        <value value="0.9"/>
      </enumeratedValueSet>
    </subExperiment>
  </experiment>
  <experiment name="old_guard_600runs_experiment" repetitions="100" runMetricsEveryStep="true">
    <setup>reset-timer
setup</setup>
    <go>go</go>
    <metric>sort [who] of turtles with [in-coalition?]</metric>
    <metric>timer</metric>
    <metric>current-random-seed</metric>
    <metric>map [a -&gt; [residual-power] of a] sort-on [who] turtles</metric>
    <metric>map [a -&gt; [network-power] of a] sort-on [who] turtles</metric>
    <metric>map [a -&gt; [positional-power] of a] sort-on [who] turtles</metric>
    <metric>map [a -&gt; [isolated-payoff] of a] sort-on [who] turtles</metric>
    <metric>map [a -&gt; [payoff] of a] sort-on [who] turtles</metric>
    <metric>out-network-matrix</metric>
    <enumeratedValueSet variable="manual-patronage-network-input">
      <value value="&quot;[\n[0 \&quot;--\&quot; 4]\n[0 \&quot;--\&quot; 5]\n[0 \&quot;--\&quot; 8]\n[1 \&quot;--\&quot; 2]\n[1 \&quot;--\&quot; 3]\n[1 \&quot;--\&quot; 4]\n[2 \&quot;--\&quot; 3]\n[2 \&quot;--\&quot; 7]\n[3 \&quot;--\&quot; 5]\n[3 \&quot;--\&quot; 6]\n]&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patron-to-client-payoff-transfer">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print-decisions?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="constant-sum-payoff-transfers?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="one-step-power-transfers?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-user-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-factions">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="client-to-patron-payoff-transfer">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manually-assign-positions?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="positions-assignment-string">
      <value value="&quot;[0]&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="UCT-constant">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="penalty">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="winning-coalition-size">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-decision-trees-to-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="debug-display?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="positions-power-string">
      <value value="&quot;[5]&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percentage-rewired">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MCTS-tree-search?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manually-specified-power">
      <value value="&quot;[5 4 5.33333 5.2 4.6 4.7 4.2 4.5 4.1]&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manually-specify-power?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MCTS-trials">
      <value value="600"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="power-transfer-error">
      <value value="1.0E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decision-tree-file-directory">
      <value value="&quot;C:\\\\Users\\\\Siebren Kooistra\\\\Documents\\\\Opleiding\\\\Linköpings Universitet\\\\Master Thesis\\\\Thesis Simulation Model Folder&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="current-random-seed">
      <value value="-244371526"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patronage-network-structure">
      <value value="&quot;Manual&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manual-network-input-method">
      <value value="&quot;Edgelist&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decision-tree-file">
      <value value="&quot;trees_file.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-coalition-size">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manual-agent-type-input?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manual-agent-types">
      <value value="&quot;[\&quot;Maximiser\&quot; \&quot;Satisficer\&quot; \&quot;Satisficer\&quot; \&quot;Satisficer\&quot; \&quot;Maximiser\&quot; \&quot;Maximiser\&quot; \&quot;Maximiser\&quot; \&quot;Maximiser\&quot; \&quot;Maximiser\&quot;]&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manual-reward-thresholds">
      <value value="&quot;[0.20 0.08 0.11 0.11 0.09 0.10 0.09 0.09 0.08]&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patronage-power-transfer">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-patronage-power-transfer">
      <value value="0.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="star_experiment_robustness_test" repetitions="100" runMetricsEveryStep="true">
    <setup>reset-timer
setup</setup>
    <go>go</go>
    <metric>sort [who] of turtles with [in-coalition?]</metric>
    <metric>timer</metric>
    <metric>current-random-seed</metric>
    <metric>map [a -&gt; [residual-power] of a] sort-on [who] turtles</metric>
    <metric>map [a -&gt; [network-power] of a] sort-on [who] turtles</metric>
    <metric>map [a -&gt; [positional-power] of a] sort-on [who] turtles</metric>
    <metric>map [a -&gt; [isolated-payoff] of a] sort-on [who] turtles</metric>
    <metric>map [a -&gt; [payoff] of a] sort-on [who] turtles</metric>
    <metric>out-network-matrix</metric>
    <enumeratedValueSet variable="manual-patronage-network-input">
      <value value="&quot;[\n[0 \&quot;--\&quot; 1]\n[0 \&quot;--\&quot; 2]\n[0 \&quot;--\&quot; 3]\n[0 \&quot;--\&quot; 4]\n]&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-factions">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patron-to-client-payoff-transfer">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print-decisions?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-user-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MCTS-tree-search?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="client-to-patron-payoff-transfer">
      <value value="0"/>
      <value value="0.3"/>
      <value value="0.6"/>
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manually-assign-positions?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manually-specified-power">
      <value value="&quot;[4.6 4.9 4.2 5.3 5.4]&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manually-specify-power?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="positions-assignment-string">
      <value value="&quot;[0]&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="UCT-constant">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="current-random-seed">
      <value value="389656894"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MCTS-trials">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="penalty">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="winning-coalition-size">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-decision-trees-to-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="debug-display?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percentage-rewired">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patronage-network-structure">
      <value value="&quot;Manual&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="positions-power-string">
      <value value="&quot;[0]&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manual-network-input-method">
      <value value="&quot;Edgelist&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-coalition-size">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manual-agent-type-input?">
      <value value="false"/>
    </enumeratedValueSet>
    <subExperiment>
      <enumeratedValueSet variable="constant-sum-payoff-transfers?">
        <value value="true"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="one-step-power-transfers?">
        <value value="true"/>
        <value value="false"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="patronage-power-transfer">
        <value value="0"/>
        <value value="0.3"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="max-patronage-power-transfer">
        <value value="0.5"/>
      </enumeratedValueSet>
    </subExperiment>
    <subExperiment>
      <enumeratedValueSet variable="constant-sum-payoff-transfers?">
        <value value="false"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="one-step-power-transfers?">
        <value value="true"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="patronage-power-transfer">
        <value value="0"/>
        <value value="0.3"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="max-patronage-power-transfer">
        <value value="0.5"/>
      </enumeratedValueSet>
    </subExperiment>
    <subExperiment>
      <enumeratedValueSet variable="constant-sum-payoff-transfers?">
        <value value="true"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="one-step-power-transfers?">
        <value value="true"/>
        <value value="false"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="patronage-power-transfer">
        <value value="0.6"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="max-patronage-power-transfer">
        <value value="0.6"/>
      </enumeratedValueSet>
    </subExperiment>
    <subExperiment>
      <enumeratedValueSet variable="constant-sum-payoff-transfers?">
        <value value="false"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="one-step-power-transfers?">
        <value value="true"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="patronage-power-transfer">
        <value value="0.6"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="max-patronage-power-transfer">
        <value value="0.6"/>
      </enumeratedValueSet>
    </subExperiment>
    <subExperiment>
      <enumeratedValueSet variable="constant-sum-payoff-transfers?">
        <value value="true"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="one-step-power-transfers?">
        <value value="true"/>
        <value value="false"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="patronage-power-transfer">
        <value value="0.9"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="max-patronage-power-transfer">
        <value value="0.9"/>
      </enumeratedValueSet>
    </subExperiment>
    <subExperiment>
      <enumeratedValueSet variable="constant-sum-payoff-transfers?">
        <value value="false"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="one-step-power-transfers?">
        <value value="true"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="patronage-power-transfer">
        <value value="0.9"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="max-patronage-power-transfer">
        <value value="0.9"/>
      </enumeratedValueSet>
    </subExperiment>
  </experiment>
  <experiment name="triad_experiment_robustness_test" repetitions="100" runMetricsEveryStep="true">
    <setup>reset-timer
setup</setup>
    <go>go</go>
    <metric>sort [who] of turtles with [in-coalition?]</metric>
    <metric>timer</metric>
    <metric>current-random-seed</metric>
    <metric>map [a -&gt; [residual-power] of a] sort-on [who] turtles</metric>
    <metric>map [a -&gt; [network-power] of a] sort-on [who] turtles</metric>
    <metric>map [a -&gt; [positional-power] of a] sort-on [who] turtles</metric>
    <metric>map [a -&gt; [isolated-payoff] of a] sort-on [who] turtles</metric>
    <metric>map [a -&gt; [payoff] of a] sort-on [who] turtles</metric>
    <enumeratedValueSet variable="initial-coalition-size">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="constant-sum-payoff-transfers?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="one-step-power-transfers?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-factions">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patron-to-client-payoff-transfer">
      <value value="0"/>
      <value value="0.3"/>
      <value value="0.6"/>
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print-decisions?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-decision-trees-to-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MCTS-tree-search?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-user-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="client-to-patron-payoff-transfer">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manually-specify-power?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manually-assign-positions?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="positions-assignment-string">
      <value value="&quot;[2]&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="UCT-constant">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MCTS-trials">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="current-random-seed">
      <value value="56307012"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="penalty">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="winning-coalition-size">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percentage-rewired">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="debug-display?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patronage-network-structure">
      <value value="&quot;Manual&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manual-network-input-method">
      <value value="&quot;Edgelist&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manual-patronage-network-input">
      <value value="&quot;[\n[0 \&quot;--\&quot; 1]\n]&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manual-agent-type-input?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manual-agent-types">
      <value value="&quot;[]&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manual-reward-thresholds">
      <value value="&quot;[]&quot;"/>
    </enumeratedValueSet>
    <subExperiment>
      <enumeratedValueSet variable="patronage-power-transfer">
        <value value="0"/>
        <value value="0.3"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="max-patronage-power-transfer">
        <value value="0.5"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="manually-specified-power">
        <value value="&quot;[3 4 5]&quot;"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="positions-power-string">
        <value value="&quot;[0]&quot;"/>
      </enumeratedValueSet>
    </subExperiment>
    <subExperiment>
      <enumeratedValueSet variable="patronage-power-transfer">
        <value value="0.6"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="max-patronage-power-transfer">
        <value value="0.6"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="manually-specified-power">
        <value value="&quot;[3 4 5]&quot;"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="positions-power-string">
        <value value="&quot;[0]&quot;"/>
      </enumeratedValueSet>
    </subExperiment>
    <subExperiment>
      <enumeratedValueSet variable="patronage-power-transfer">
        <value value="0.9"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="max-patronage-power-transfer">
        <value value="0.9"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="manually-specified-power">
        <value value="&quot;[3 4 5]&quot;"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="positions-power-string">
        <value value="&quot;[0]&quot;"/>
      </enumeratedValueSet>
    </subExperiment>
    <subExperiment>
      <enumeratedValueSet variable="patronage-power-transfer">
        <value value="0"/>
        <value value="0.3"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="max-patronage-power-transfer">
        <value value="0.5"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="manually-specified-power">
        <value value="&quot;[3 4 2]&quot;"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="positions-power-string">
        <value value="&quot;[3]&quot;"/>
      </enumeratedValueSet>
    </subExperiment>
    <subExperiment>
      <enumeratedValueSet variable="patronage-power-transfer">
        <value value="0.6"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="max-patronage-power-transfer">
        <value value="0.6"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="manually-specified-power">
        <value value="&quot;[3 4 2]&quot;"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="positions-power-string">
        <value value="&quot;[3]&quot;"/>
      </enumeratedValueSet>
    </subExperiment>
    <subExperiment>
      <enumeratedValueSet variable="patronage-power-transfer">
        <value value="0.9"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="max-patronage-power-transfer">
        <value value="0.9"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="manually-specified-power">
        <value value="&quot;[3 4 2]&quot;"/>
      </enumeratedValueSet>
      <enumeratedValueSet variable="positions-power-string">
        <value value="&quot;[3]&quot;"/>
      </enumeratedValueSet>
    </subExperiment>
  </experiment>
  <experiment name="old_guard_noA_experiment" repetitions="100" runMetricsEveryStep="true">
    <setup>reset-timer
setup</setup>
    <go>go</go>
    <metric>sort [who] of turtles with [in-coalition?]</metric>
    <metric>timer</metric>
    <metric>current-random-seed</metric>
    <metric>map [a -&gt; [residual-power] of a] sort-on [who] turtles</metric>
    <metric>map [a -&gt; [network-power] of a] sort-on [who] turtles</metric>
    <metric>map [a -&gt; [positional-power] of a] sort-on [who] turtles</metric>
    <metric>map [a -&gt; [isolated-payoff] of a] sort-on [who] turtles</metric>
    <metric>map [a -&gt; [payoff] of a] sort-on [who] turtles</metric>
    <metric>out-network-matrix</metric>
    <enumeratedValueSet variable="manual-patronage-network-input">
      <value value="&quot;[\n[0\&quot;--\&quot; 2]\n[1 \&quot;--\&quot; 3]\n[1 \&quot;--\&quot; 4]\n[2 \&quot;--\&quot; 3]\n[2 \&quot;--\&quot; 7]\n[3 \&quot;--\&quot; 5]\n[3 \&quot;--\&quot; 6]\n]&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patron-to-client-payoff-transfer">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print-decisions?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-user-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="constant-sum-payoff-transfers?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="one-step-power-transfers?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-factions">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="client-to-patron-payoff-transfer">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manually-assign-positions?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="positions-assignment-string">
      <value value="&quot;[0]&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="UCT-constant">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="penalty">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="winning-coalition-size">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-decision-trees-to-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="debug-display?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="positions-power-string">
      <value value="&quot;[5]&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percentage-rewired">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MCTS-tree-search?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manually-specified-power">
      <value value="&quot;[4 5.33333 5.2 4.6 4.7 4.2 4.5 4.1]&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manually-specify-power?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MCTS-trials">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="power-transfer-error">
      <value value="1.0E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="current-random-seed">
      <value value="-244371526"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patronage-network-structure">
      <value value="&quot;Manual&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manual-network-input-method">
      <value value="&quot;Edgelist&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decision-tree-file">
      <value value="&quot;trees_file.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-coalition-size">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manual-agent-type-input?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manual-agent-types">
      <value value="&quot;[\&quot;Satisficer\&quot; \&quot;Satisficer\&quot; \&quot;Satisficer\&quot; \&quot;Maximiser\&quot; \&quot;Maximiser\&quot; \&quot;Maximiser\&quot; \&quot;Maximiser\&quot; \&quot;Maximiser\&quot;]&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manual-reward-thresholds">
      <value value="&quot;[0.08 0.11 0.11 0.09 0.10 0.09 0.09 0.08]&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patronage-power-transfer">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-patronage-power-transfer">
      <value value="0.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="old_guard_baseline_experiment" repetitions="100" runMetricsEveryStep="true">
    <setup>reset-timer
setup</setup>
    <go>go</go>
    <metric>sort [who] of turtles with [in-coalition?]</metric>
    <metric>timer</metric>
    <metric>current-random-seed</metric>
    <metric>map [a -&gt; [residual-power] of a] sort-on [who] turtles</metric>
    <metric>map [a -&gt; [network-power] of a] sort-on [who] turtles</metric>
    <metric>map [a -&gt; [positional-power] of a] sort-on [who] turtles</metric>
    <metric>map [a -&gt; [isolated-payoff] of a] sort-on [who] turtles</metric>
    <metric>map [a -&gt; [payoff] of a] sort-on [who] turtles</metric>
    <metric>out-network-matrix</metric>
    <enumeratedValueSet variable="manual-patronage-network-input">
      <value value="&quot;[\n[0 \&quot;--\&quot; 4]\n[0 \&quot;--\&quot; 5]\n[0 \&quot;--\&quot; 8]\n[1 \&quot;--\&quot; 2]\n[1 \&quot;--\&quot; 3]\n[1 \&quot;--\&quot; 4]\n[2 \&quot;--\&quot; 3]\n[2 \&quot;--\&quot; 7]\n[3 \&quot;--\&quot; 5]\n[3 \&quot;--\&quot; 6]\n]&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patron-to-client-payoff-transfer">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print-decisions?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-user-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="constant-sum-payoff-transfers?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="one-step-power-transfers?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-factions">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="client-to-patron-payoff-transfer">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manually-assign-positions?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="positions-assignment-string">
      <value value="&quot;[0]&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="UCT-constant">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="penalty">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="winning-coalition-size">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-decision-trees-to-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="debug-display?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="positions-power-string">
      <value value="&quot;[0]&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percentage-rewired">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MCTS-tree-search?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manually-specified-power">
      <value value="&quot;[10 4 5.33333 5.2 4.6 4.7 4.2 4.5 4.1]&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manually-specify-power?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MCTS-trials">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="power-transfer-error">
      <value value="1.0E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="current-random-seed">
      <value value="-244371526"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patronage-network-structure">
      <value value="&quot;Manual&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manual-network-input-method">
      <value value="&quot;Edgelist&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decision-tree-file">
      <value value="&quot;trees_file.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-coalition-size">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manual-agent-type-input?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manual-agent-types">
      <value value="&quot;[\&quot;Maximiser\&quot; \&quot;Satisficer\&quot; \&quot;Satisficer\&quot; \&quot;Satisficer\&quot; \&quot;Maximiser\&quot; \&quot;Maximiser\&quot; \&quot;Maximiser\&quot; \&quot;Maximiser\&quot; \&quot;Maximiser\&quot;]&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manual-reward-thresholds">
      <value value="&quot;[0.20 0.08 0.11 0.11 0.09 0.10 0.09 0.09 0.08]&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patronage-power-transfer">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-patronage-power-transfer">
      <value value="0.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="triad_precision_experiment" repetitions="100" runMetricsEveryStep="true">
    <setup>reset-timer
setup</setup>
    <go>go</go>
    <metric>sort [who] of turtles with [in-coalition?]</metric>
    <metric>timer</metric>
    <metric>current-random-seed</metric>
    <metric>map [a -&gt; [residual-power] of a] sort-on [who] turtles</metric>
    <metric>map [a -&gt; [network-power] of a] sort-on [who] turtles</metric>
    <metric>map [a -&gt; [positional-power] of a] sort-on [who] turtles</metric>
    <metric>map [a -&gt; [isolated-payoff] of a] sort-on [who] turtles</metric>
    <metric>map [a -&gt; [payoff] of a] sort-on [who] turtles</metric>
    <enumeratedValueSet variable="manual-patronage-network-input">
      <value value="&quot;[\n[0 \&quot;--\&quot; 1]\n]&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manual-agent-type-input?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patron-to-client-payoff-transfer">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print-decisions?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="use-user-seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="constant-sum-payoff-transfers?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-factions">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="client-to-patron-payoff-transfer">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manually-assign-positions?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="one-step-power-transfers?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="positions-assignment-string">
      <value value="&quot;[0]&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="UCT-constant">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="winning-coalition-size">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="penalty">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manual-reward-thresholds">
      <value value="&quot;[0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5]&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="write-decision-trees-to-file?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="debug-display?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="positions-power-string">
      <value value="&quot;[0]&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="power-transfer-error">
      <value value="1.0E-6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percentage-rewired">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MCTS-tree-search?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-patronage-power-transfer">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manually-specified-power">
      <value value="&quot;[3 4 5]&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manually-specify-power?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="patronage-power-transfer" first="0.8" step="0.01" last="0.9"/>
    <enumeratedValueSet variable="current-random-seed">
      <value value="962846764"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MCTS-trials">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manual-agent-types">
      <value value="&quot;[\&quot;Maximiser\&quot; \&quot;Satisficer\&quot; \&quot;Satisficer\&quot; \&quot;Satisficer\&quot; \&quot;Maximiser\&quot; \&quot;Maximiser\&quot; \&quot;Maximiser\&quot; \&quot;Maximiser\&quot; \&quot;Maximiser\&quot;]&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patronage-network-structure">
      <value value="&quot;Manual&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manual-network-input-method">
      <value value="&quot;Edgelist&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decision-tree-file">
      <value value="&quot;29_04_2024_three_person_trees_0.9_10.txt&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-coalition-size">
      <value value="3"/>
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
