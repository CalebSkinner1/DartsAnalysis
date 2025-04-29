## DartsAnalysis GitHub Page

This GitHub Page hosts the results and analysis of a dartboard competition played by Rice University Graduate Students in Maxfield Hall. The rules are simple. There are three rounds in a game. In each round, three darts are thrown by each participant at a magnetic dartboard. Players earn 1 point for landing the dart on the board, 2 points for landing the dart within the large circle, 3 points for landing the dart within the smaller circle, and 5 points for a bulls-eye. The aggregate scores for each round are recorded. This GitHub page hosts the results, a Metropolis-Hastings sampler to estimate a player's probability of each throw outcome, and analysis.

## MCMC.R
This R Script contains a likelihood function and a Metropolis Hasting algorithm that samples from the posterior distribution of interest.

## Simple Model.R
This R Script actualizes the functions in MCMC.R and plots results for an early model attempt. This model places a dirichlet prior on the throw probabilities, and models each player's probabilities independently. It also does not take into account potential impactful covariates. In the future, I will advance these methods to more complex models.

## Maxfield - Darts.xlsx
This excel document contains the data from each game and a few quick analyses.

