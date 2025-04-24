
library("gtools")  # for rdirichlet
library("tidyverse")
library("mcmcr")

model_likelihood <- function(y, p) {
  y <- as.list(y)
  n <- 3 #3 throws
  
  prob <- map_dbl(y, ~{
    total_prob <- 0
    for (x1 in 0:n) { # sum over x1
      for (x2 in 0:(n - x1)) { # sum over x2
        for (x3 in 0:(n - x1 - x2)) { # sum over x3
          x0 <- n - (x1 + x2 + x3) # x0 is deterministic now
          y_value <- x1 + 2 * x2 + 3 * x3 # compute y
          
          if (y_value == .x) {
            prob <- dmultinom(c(x0, x1, x2, x3), size = n, prob = p) # use multinom formula
            
            total_prob <- total_prob + prob
          }
        }
      }
    }
    total_prob
  })
  log_likelihood <- sum(log(prob))
  
  
  return(exp(log_likelihood))
}

# example
# model_likelihood(y = c(4, 5), p = c(.1, .2, .2, .1))

mh_sampler <- function(y, alpha = c(1, 1, 1, 1), burn_in = 1000, n_iter = 5000, proposal_sd = 0.1) {
  samples <- matrix(NA, nrow = n_iter, ncol = 4)
  p_current <- rdirichlet(1, alpha)
  lik_current <- model_likelihood(y, p_current)
  
  accept <- 0
  for (i in 1:n_iter) {
    proposal <- p_current + rnorm(4, mean = 0, sd = proposal_sd) %>%
      abs()  # stay positive
    proposal <- proposal / sum(proposal)  # project onto simplex
    lik_proposal <- model_likelihood(y, proposal)
    
    prior_current <- ddirichlet(p_current, alpha)
    prior_proposal <- ddirichlet(proposal, alpha)
    
    r <- (lik_proposal * prior_proposal) / (lik_current * prior_current)
    if (runif(1) < min(1, r)) {
      p_current <- proposal
      lik_current <- lik_proposal
      accept <- accept + 1
    }
    samples[i, ] <- p_current
  }
  colnames(samples) <- paste0("p", 0:3)
  samples <- samples[(burn_in+1):n_iter,] %>% as.mcmc()
  acceptance_rate <- accept/n_iter
  
  return(list(samples, acceptance_rate))
}

# ex_samples <- mh_sampler(y = c(2, 3, 2, 3, 3, 3, 0, 6, 2), alpha = c(1, 1, 1, 1))

# ex_samples[[1]] %>% summary()
# 
# ex_samples[[1]] %>% plot()