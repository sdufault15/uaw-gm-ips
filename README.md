# The Impact of Job Loss on Self-Injury Mortality in a Cohort of Autoworkers: Application of a Novel Causal Approach

This repository holds all of the code necessary to recreate the analysis described in [The Impact of Job Loss on Self-Injury Mortality in a Cohort of Autoworkers: Application of a Novel Causal Approach](https://doi.org/10.1097/EDE.0000000000001461) by Suzanne M. Dufault, Kevin T. Chen, Sally Picciotto, Andreas M. Neophytou, and Ellen A. Eisen. This paper was published in *Epidemiology* in May 2022. The code has not been optimized, but should accurately return the results described in the paper. 

## Organization of this Repository

This repository is organized as follows. Note: the raw data is not available in this repository. 

+ `munge` Contains all of the scripts necessary to clean the data, subset the cohort, and run the analysis. 
+ `graphs` Contains separate files for each of the figures presented in the manuscript
+ `lib` Contains three helper functions for 1) the specification of self-injury events from the mortality data, and 2) generating the time-varying dataset for analysis.
+ `reports` Contains the R files and docx files for generating the figures and tables in the manuscript

There is also a change log provided in `change_log.md`.




