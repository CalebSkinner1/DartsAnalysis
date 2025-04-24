# This script runs mcmc sampler for p without gaining information from other players
library("readxl")
library("janitor")
library("tictoc")

source("DartsAnalysis/MCMC.R")

# read data from excel and draw data
data <- read_excel("DartsAnalysis/Maxfield - Darts.xlsx") %>%
  select(angle:Veronica) %>%
  filter(!is.na(Date)) %>%
  clean_names()

# compute samples for Caleb - this is definitely not linear in n
tic()
caleb_samples <- data %>% select(caleb) %>% drop_na() %>% pull() %>%
  mh_sampler(proposal_sd = .02)
toc()

# acceptance rate ~25%, need to make this adaptive
caleb_samples[[2]]

# summary statistics
caleb_samples[[1]] %>% summary()

caleb_samples[[1]] %>% plot()

# compute samples for Joshua - this is definitely not linear in n
joshua_data <- data %>% select(joshua) %>% drop_na() %>% pull()
joshua_samples <- mh_sampler(joshua_data, proposal_sd = .005)

# acceptance rate ~25%, need to make this adaptive
joshua_samples[[2]]

# summary statistics
joshua_samples[[1]] %>% summary()

joshua_samples[[1]] %>% plot()






