# Turkish Phonotactic Acceptability

This repo contains the code used to generate, run and analyze a phonotactic
acceptability judgment task done on Turkish. The task is described in 

Mayer, C. (in press). Reconciling gradient and categorical models of phonotactics. 
*Proceedings of the Society for Computation in Linguistics.*

The data are also used in

Mayer, C., Kondur, A., and Sundara, M. (under review). The UCI Phonotactic 
Calculator: An online tool for computing phonotactic metrics. 
*Behavior Research Methods.*

If you use this data, please cite one or both of these papers.

The repo is organized as follows:
* `data`: this folder is a bit of a mess, but it contains the various files that
were used to construct the stimuli for the experiment. `data/turkish_phonotactic_judgments/candidates_ortho_v4.csv` is the final stimuli set.
* `figs`: contains the figures used in Mayer (2025).
* `results`: contains the results from the study. Because these are formatted in
Gorilla output form, they're unlikely to be useful without `src/analyze_results.R`.
* `src`: all the code for generating and synthesizing the stimuli and analyzing
the results. Each file has a brief description of its function at the top.
* `telldata`: the TELL corpus data used to train the phonotactic models.
