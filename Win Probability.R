# Compute Win Probability
library("readxl")
library("janitor")
library("tictoc")

source("DartsAnalysis/MCMC.R")

# read data from excel and draw data
data <- read_excel("DartsAnalysis/Maxfield - Darts.xlsx") %>%
  clean_names() %>%
  select(angle:joseph) %>%
  filter(!is.na(date)) %>%
  filter(base != "bad magnet", angle == "vertical")

compute_player_probabilities <- function(player, data, height){
  player <- str_to_lower(player)
  
  past_scores <- data %>% filter(height == height) %>% pull(any_of(player)) %>% na.omit() %>% as.numeric()
  
  samples <- mh_sampler(past_scores)
  
  samples$new_y %>% table() %>% as_tibble() %>%
    rename("score" = ".") %>%
    mutate(perc = n/length(samples$new_y)) %>%
    select(score, perc)
}

# HERE

# game_tibble <- tibble(round = c(1, 2, 3), )

# compute_win_probability <- function(game_tibble, data, height){
#   
# }


compute_player_probabilities("Caleb", data, "mid")
