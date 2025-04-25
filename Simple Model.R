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
caleb_data <- data %>% select(caleb) %>% drop_na() %>% pull()
caleb_samples <- mh_sampler(caleb_data, proposal_change = 1.4, c(5, 3, 2, 2))
toc()

# acceptance rate ~27%, need to make this adaptive
caleb_samples[[2]]

# summary statistics
caleb_samples[[1]] %>% summary()

caleb_samples[[1]] %>% plot()

freq_fun <- function(df, name){
  df %>% group_by(y) %>%
    summarize(count = n()) %>%
    mutate(prop = count/sum(count)) %>%
    select(y, prop) %>%
    rename_with(~paste0(name), starts_with("prop"))
}

tibble(y = caleb_samples[[1]][,5]) %>%
  freq_fun("sampled") %>%
  left_join(tibble(y = caleb_data) %>% freq_fun("observed"), by = join_by(y)) %>%
  ggplot() +
  geom_line(aes(x = y, sampled), color = "cadetblue4") +
  geom_line(aes(x = y, observed), color = "indianred3")

tibble(samples = caleb_samples[[1]][,5])

# compute samples for Joshua - this is definitely not linear in n
joshua_data <- data %>% select(joshua) %>% drop_na() %>% pull()
joshua_samples <- mh_sampler(joshua_data, proposal_change = 1.4, alpha = c(5, 3, 2, 2))

# acceptance rate ~28%, need to make this adaptive
joshua_samples[[2]]

# summary statistics
joshua_samples[[1]] %>% summary()

joshua_samples[[1]] %>% plot()

tibble(y = joshua_samples[[1]][,5]) %>%
  freq_fun("sampled") %>%
  left_join(tibble(y = joshua_data) %>% freq_fun("observed"), by = join_by(y)) %>%
  ggplot() +
  geom_line(aes(x = y, sampled), color = "cadetblue4") +
  geom_line(aes(x = y, observed), color = "indianred3")

# compute samples for Quadri - this is definitely not linear in n
quadri_data <- data %>% select(quadri) %>% drop_na() %>% pull()
quadri_samples <- mh_sampler(quadri_data, proposal_change = 2.5, alpha = c(5, 3, 2, 2))

# acceptance rate ~33%, need to make this adaptive
quadri_samples[[2]]

# summary statistics
quadri_samples[[1]] %>% summary()

quadri_samples[[1]] %>% plot()


