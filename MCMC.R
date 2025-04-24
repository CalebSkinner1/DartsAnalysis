
library("gtools")  # for rdirichlet


multinomial_pmf <- function(y, n = 3, p) {
  total_prob <- 0
  
  for (x1 in 0:n) { # sum over x1
    for (x2 in 0:(n - x1)) { # sum over x2
      for (x3 in 0:(n - x1 - x2)) { # sum over x3
        x0 <- n - (x1 + x2 + x3) # x0 is deterministic now
        y_value <- x1 + 2 * x2 + 3 * x3 # compute y
        
        if (y_value == y) {
          prob <- dmultinom(c(x0, x1, x2, x3), size = n, prob = p) # use multinom formula
          
          total_prob <- total_prob + prob
        }
      }
    }
  }
  return(total_prob)
}

# example
multinomial_pmf(y = 4, p = c(.1, .2, .3, .4))

mh_sampler <- function(y, n, alpha, n_iter = 5000, proposal_sd = 0.1) {
  samples <- matrix(NA, nrow = n_iter, ncol = 4)
  p_current <- rdirichlet(1, alpha)
  lik_current <- multinomial_pmf(y, n, p_current)
  for (i in 1:n_iter) {
    proposal <- p_current + rnorm(4, mean = 0, sd = proposal_sd)
    proposal <- abs(proposal)  # stay positive
    proposal <- proposal / sum(proposal)  # project onto simplex
    lik_proposal <- likelihood_Y_given_p(y, n, proposal)
    
    prior_current <- ddirichlet(p_current, alpha)
    prior_proposal <- ddirichlet(proposal, alpha)
    
    r <- (lik_proposal * prior_proposal) / (lik_current * prior_current)
    if (runif(1) < min(1, r)) {
      p_current <- proposal
      lik_current <- lik_proposal
    }
    samples[i, ] <- p_current
  }
  colnames(samples) <- paste0("p", 0:3)
  return(samples)
}
