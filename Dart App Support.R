# Dart Support Functions and files. These functions support the app

source("DartsAnalysis/Win Probability Support.R")

data <- read_excel("DartsAnalysis/Maxfield - Darts.xlsx") %>%
  clean_names() %>%
  select(angle:joseph) %>%
  filter(!is.na(date)) %>%
  filter(base != "bad magnet", angle == "vertical")

load("DartsAnalysis/Densities/density_mid.RData")
load("DartsAnalysis/Densities/density_high.RData")
load("DartsAnalysis/Densities/density_low.RData")
load("DartsAnalysis/Densities/density_254.RData")

names <- c("caleb", "joshua", "quadri", "joseph", "daniel")

density_list <- list("mid" = density_mid, "high" = density_high,
                     "low" = density_low, "254" = density_254)

instant_win_probability <- function(game_tibble, data, )

compute_win_probability(game_tibble, data, height, player_probabilities)