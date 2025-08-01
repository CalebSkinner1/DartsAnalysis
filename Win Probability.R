# Win Probability Page

source("DartsAnalysis/Win Probability Support.R")

# read data from excel and draw data
data <- read_excel("DartsAnalysis/Maxfield - Darts.xlsx") %>%
  clean_names() %>%
  select(angle:joseph) %>%
  filter(!is.na(date)) %>%
  filter(base != "bad magnet", angle == "vertical")

tic()
player_probabilities <- c("caleb", "joshua", "quadri", "joseph", "daniel") %>% compute_player_probabilities(data, "mid")
toc()

tic()
game_chart_wrapper(192, data, player_probabilities)
toc()

tic()
game_chart_wrapper(193, data, player_probabilities)
toc()