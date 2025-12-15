## DartsAnalysis GitHub Page 🎯

This GitHub Page hosts the results and analysis of a dartboard competition played by Rice University Graduate Students in Maxfield Hall. The rules are simple. There are three rounds in a game. In each round, three darts are thrown by each participant at a magnetic dartboard. Players earn 1 point for landing the dart on the board, 2 points for landing the dart within the large circle, 3 points for landing the dart within the smaller circle, and 5 points for a bulls-eye. The aggregate scores for each round are recorded. This GitHub page hosts the results, a Metropolis-Hastings sampler to estimate a player's probability of each throw outcome, and analysis.

## MCMC.R
This R Script contains a likelihood function and a Metropolis Hasting algorithm that samples from the posterior distribution of interest. Metropolis Hastings is required in this setting, because the aggregate scores are recorded but the distribution of an individual throw is desired. Leveraging the individual throws to model the aggregate scores provides a much stronger fit, because the aggregate scores are not necessarily unimodal or monotonic like a typical discrete parametric distribution. I use a dirichlet random walk to propose a new distribution in the MH algorithm.

## Simple Model.R
I actualize the functions in MCMC.R and plots results for an early model attempt. This model places a dirichlet prior on the throw probabilities, and models each player's probabilities independently. This page contains MCMC diagnostic traceplots and histograms. In the future, I hope to account for potential impactful covariates and acknowledge the potential dependence of successive throws and rounds.

## Win Probability.R
Here, I leverage the score-per round-distribution for each player to compute in-game win probabilities. These win probabilities are specific to the current score and the players involved. I also produce functions for easily charting the win probability over an entire game.

## Maxfield - Darts.xlsx
This excel document contains the data from each game and a few quick analyses.

