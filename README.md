# Intelligent Control System For Hybrid Power Units

## Abstract
At the present time manufactures of hybrid vehicles implement rather diverse philosophies into the management modules of their power units. In many occasions these control units would cycle on a predefined set of pragmas: such as minimising deployment from the internal combustion engine until a certain power requirement is met; start a recharging procedure when some conditions about the battery charge level are reached; or initiate an harvesting mode on liftoffs, coasting or braking occasions. Some other hybrid control units leave to the driver the choosing of predefined deployment or harvesting programs. In most occasions such implementations seem to be making greedy optimisation decisions based on the current state of the vehicle, hence ignoring valuable information regarding the itinerary being undertaken. Motivated by today’s diversity in the two engines diversity in running costs and rage, this project aims at evaluating the possibility of providing better fuel economy for hybrid vehicles, by intelligently make use of the information about the itinerary a vehicle must complete. This is extra knowledge is utilised to compose a schedule of power unit modes to be applied at various stages of the journey.

## Results
A system capable of improving fuel economy of hybrid power units by utilising information of the journey about to be undertaken was presented. The power factors imposed by the journey on the vehicle were modelled with a theoretical approach, which enabled for fine-grain estimation of the power unit’s requirements. Two supervised machine learning of the engine’s efficiency curves. The first, a KNN-Regression with density point control and weighed average; the second a multivariate polynomial regression. Both approaches were tested on dynamometer telemetry of a: Fiat 500, Ford Fusion V6, Nissan Leaf and BMW i3, created by Argonne National Laboratory. The use of dynamic programming for the task of computing the optimal sequence of power unit modes for a given itinerary, was discussed to be inefficient given the lack of overlapping subproblems. The computation of the problem search space was formalised by the introduction of a producer-consumer implementation of Depth First Search. It was however a genetic search to be deeply evaluated in this setting. Single and multiple point crossover were compared in a number of scenarios, as was generation size and mutation chance. A procedure for the generation of random but on-target first generation genes was motivated and formally described, as was a deterministic gene repair function. Finally, the cost effectiveness of the system was compared to the cost of a solution which mimics what it is understood to be today’s management logics of hybrid vehicles. A grammar for the composition of itineraries was also introduced.
Results confirmed it is possible to improve the fuel economy of hybrid drivetrains by adapt- ing to a journey the utilisation of thermic and induction engines. Although in the scenarios tested, an average gain of 7% was recorded, it is important to note that the cost effectiveness of the system can be larger. In fact, this varies upon the uniformity of the power requirement of the journey, hence preferring itineraries with combined conditions.
As this approach is capable (on a simulation level) to obtain better fuel economy whilst not scarifying a journey’s desired velocities, accelerations rates, travel times or average speed, it seems appropriate to consider how its effectiveness could be further consolidated. One area of future work may be the ability to recalculate the solution whenever the expected efficiency is breached during the execution of the itinerary. The effectiveness of such feature however cannot be guaranteed at this stage. In fact, an engine’s efficiency is not expected to deviate largely compared a substantial amount of its telemetry. The system could also be adapted to evaluate solutions in which refuelling/recharging is allowed at certain stages of the jour- ney. Lastly, the simulation of the environment could be enriched to account for power unit operations which might want to be avoided for reasons such to components’ wear and limits of operation.

## Development Environment
The development of the system was carried out in Haskell, and organised by the Haskell Stack build tool. External libraries utilised in the project are listed under every ‘build − depends :’ tag in the ‘.cabal’ file attached to this report. No library was utilised for the any modelling aspects, nor machine learning, nor searching aspects of the project; rather these consisted of: generic data structures, such as HashMap, Heap, HashSet, Matrix, Array, Text, Vector; 2D and 3D graph plotting tools; CSV parser interface; monadic random generators and selectors (utilised in the selection of genetic searching routine); shared resources management; descendants of Unix’s Lec and YACC, Alex and Happy, were utilised for the definition of token and grammar of the Itinerary files (these are part of the nowadays Haskell Platform). Python and libraries ‘matplotlib’ and ‘pandas’ were used for the plotting of all search related graphs of the report only.
The general purpose libraries utilised in the Haskell implementation were pulled from the standard open source package manager for Haskell project Hackage (https://hackage.haskell. org). As of 11/May/2020 the resources utilised are found at:
- Generic Data Structures (hashing and non hashing based)
  - https://hackage.haskell.org/package/array
  - https://hackage.haskell.org/package/vector
  - https://hackage.haskell.org/package/containers
  - https://hackage.haskell.org/package/unordered-containers
  - https://hackage.haskell.org/package/heap
  - https://hackage.haskell.org/package/matrix
  - https://hackage.haskell.org/package/hashable
  - https://hackage.haskell.org/package/scientific

- Random Generators and Shufflers
    - https://hackage.haskell.org/package/random
    - https://hackage.haskell.org/package/random-shuffle
    - https://hackage.haskell.org/package/MonadRandom
- Text formats manipulation and CSV parser interface
    - https://hackage.haskell.org/package/text
    - https://hackage.haskell.org/package/bytestring
    - https://hackage.haskell.org/package/utf8-string
    - https://hackage.haskell.org/package/cassava
- Graph plotting support
    - https://hackage.haskell.org/package/plotlyhs
    - https://hackage.haskell.org/package/aeson
    - https://hackage.haskell.org/package/lucid
    - https://hackage.haskell.org/package/microlens
- Generic multithreading support
    - https://hackage.haskell.org/package/parallel
    - https://hackage.haskell.org/package/rio
    - https://hackage.haskell.org/package/deepseq
- Others
    - https://hackage.haskell.org/package/time
    - https://hackage.haskell.org/package/ansi-terminal
    - https://hackage.haskell.org/package/directory
