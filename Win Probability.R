# Win Probability Page

source("Win Probability Support.R")

# read data from excel and draw data
data <- read_excel("Maxfield - Darts.xlsx") %>%
  clean_names() %>%
  select(angle:joseph) %>%
  filter(!is.na(date)) %>%
  filter(base != "bad magnet", angle == "vertical")


# Load Win Probabilities for each height ----------------------------------

names <- c("caleb", "joshua", "quadri", "joseph", "daniel")

tic()
density_mid <- names %>% compute_player_probabilities(data, "mid")
density_high <- names %>% compute_player_probabilities(data, "high")
density_low <- names %>% compute_player_probabilities(data, "low")
density_254 <- names %>% compute_player_probabilities(data, "254")
toc()

save(density_mid, file = "Densities/density_mid.RData")
save(density_high, file = "Densities/density_high.RData")
save(density_low, file = "Densities/density_low.RData")
save(density_254, file = "Densities/density_254.RData")

density_list <- list("mid" = density_mid, "high" = density_high, "low" = density_low, "254" = density_254)

# examples
# tic()
# game_chart_wrapper(192, data, density_list)
# p <- game_chart_wrapper(79, data, density_list)
# toc()





