# splc23-action-based-artifact

## Overview

The code in this replication package constructs and executes the four scenarios presented in the paper. The core actions and their execution specific to the scenarios are respectively in files `core.pl` and `examples.pl`. The replicator should expect the code to run within less than 10 minutes if she has a working docker setup.

## Data Availability and Provenance Statements

- [X] This paper does not involve analysis of external data (i.e., no data are used or the only data are generated by the authors via simulation in their code).


## Computational requirements

### Software Requirements

- Run within a container :
  - Docker (code was run with Docker 23.0.4)
- Run directly on computer :
  - Swipl (code was run with Prolog 8.4.2 and 9.1.8)

Portions of the code use bash scripting, which may require Linux.

## Description of programs/code

- The program `core.pl` will populate the environment with the action described in the section 3 of the paper.

- The program `examples.pl` will populate the environment with the high-level operations corresponding to the scenarios described in the section 2.

- The file `Dockerfile` is used by the `build.sh` script to run the `docker build command`to create the image running the scenarios.

### License for Code

The code is licensed under a LGPL license. See [LICENSE.txt](LICENSE.txt) for details.

## Instructions to Replicators

*Preamble: As seen below, two warning messages occur: at the beginning of the scenarios in a dockerized run and when loading the core file (`['core'].`) in a native run. These messages are expected and do not affect the execution.*

### How to run the scenarios using Docker

- If you are on a unix system :
   - The script `build_and_run.sh` will execute `build.sh` to create the docker image then `run_scenarios.sh` to execute all the scenarios. 
- else :
   - Build the docker image by running this command from folder root : `docker build -t demo-splc23 .`
   - Execute the scenarios with `docker run --rm demo-splc23`

The output should be like the following :

```
Warning: /app/core.pl:65:
Warning:    Singleton variables: [RefToObs]
Warning: /app/core.pl:75:
Warning:    Singleton variables: [Criteria,Subjects]
========> First step : create a template named grassesTemplate that contains all the standard information required for a grasses trial which are Species Plot Yield
createProduct([grassesTemplate1,"grassesTemplate"])
createObservation([obs1,[],[]])
addObservation([grassesTemplate1,obs1])
setValue([val1,obs1,"species"])
createObservation([obs2,[],[]])
addObservation([grassesTemplate1,obs2])
setValue([val2,obs2,"plot"])
createObservation([obs3,[],[]])
addObservation([grassesTemplate1,obs3])
setValue([val3,obs3,"yield"])
createRequirement([obs3,[obs1,obs2]])

[...]

----------------- Result of check Product : 
p4
obs50- criterion : temperature subjects : 9,20/05, value : 16
obs49- criterion : temperature subjects : 4,08/05, value : 17
obs48- criterion : yield subjects : Sorghum Bicolor,9,08/05, value : 5
obs47- criterion : yield subjects : Andropogon Minarum,4,08/05, value : 6
obs46- criterion : harvestDate subjects :  value : 20/05
obs45- criterion : harvestDate subjects :  value : 08/05
obs44- criterion : plot subjects :  value : 9
obs43- criterion : plot subjects :  value : 4
obs42- criterion : species subjects :  value : Sorghum Bicolor
obs41- criterion : species subjects :  value : Andropogon Minarum
obs40- criterion :  isCriterion subjects :  value : temperature
obs39- criterion :  isCriterion subjects :  value : harvestDate
obs38- criterion :  isCriterion subjects :  value : species
obs37- criterion :  isCriterion subjects :  value : plot
obs36- criterion :  isCriterion subjects :  value : yield
```

### How to run the scenarios without Docker

- Start the prolog environment from the folder root : `swipl`
- Load the core actions : `['core'].`
- Load the scenarios : `['examples'].`
- Execute the scenarios : `createSPLC23Example(P1,P2,Template,P3, P4).`

The output should be like the following :

```prolog
?- ['core'].
Warning: /home/galih/dev/splc23-action-based-artifact/core.pl:65:
Warning:    Singleton variables: [RefToObs]
Warning: /home/galih/dev/splc23-action-based-artifact/core.pl:75:
Warning:    Singleton variables: [Criteria,Subjects]
true.

?- ['examples'].
true.

?- createSPLC23Example(P1,P2,Template,P3, P4).
========> First step : create a template named grassesTemplate that contains all the standard information required for a grasses trial which are Species Plot Yield
createProduct([grassesTemplate1,"grassesTemplate"])
createObservation([obs1,[],[]])
addObservation([grassesTemplate1,obs1])
setValue([val1,obs1,"species"])
createObservation([obs2,[],[]])
addObservation([grassesTemplate1,obs2])
setValue([val2,obs2,"plot"])
createObservation([obs3,[],[]])
addObservation([grassesTemplate1,obs3])
setValue([val3,obs3,"yield"])
createRequirement([obs3,[obs1,obs2]])

[...]

----------------- Result of check Product : 
p4
obs50- criterion : temperature subjects : 9,20/05, value : 16
obs49- criterion : temperature subjects : 4,08/05, value : 17
obs48- criterion : yield subjects : Sorghum Bicolor,9,08/05, value : 5
obs47- criterion : yield subjects : Andropogon Minarum,4,08/05, value : 6
obs46- criterion : harvestDate subjects :  value : 20/05
obs45- criterion : harvestDate subjects :  value : 08/05
obs44- criterion : plot subjects :  value : 9
obs43- criterion : plot subjects :  value : 4
obs42- criterion : species subjects :  value : Sorghum Bicolor
obs41- criterion : species subjects :  value : Andropogon Minarum
obs40- criterion :  isCriterion subjects :  value : temperature
obs39- criterion :  isCriterion subjects :  value : harvestDate
obs38- criterion :  isCriterion subjects :  value : species
obs37- criterion :  isCriterion subjects :  value : plot
obs36- criterion :  isCriterion subjects :  value : yield
```