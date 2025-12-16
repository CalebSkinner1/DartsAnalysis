# Page for running updates

source("Win Probability Support.R")

# read data from excel and draw data
data <- read_excel("Maxfield - Darts.xlsx") %>%
  clean_names() %>%
  select(angle:theo) %>%
  filter(!is.na(date)) %>%
  filter(base != "bad magnet", angle == "vertical")


# Load Win Probabilities for each height ----------------------------------

names <- c("caleb", "joshua", "quadri", "joseph", "daniel")

tic()
# density_mid <- names %>% compute_player_probabilities(data, "mid")
# density_high <- names %>% compute_player_probabilities(data, "high")
# density_low <- names %>% compute_player_probabilities(data, "low")
load("Densities/density_mid.RData")
load("Densities/density_high.RData")
load("Densities/density_low.RData")
density_256 <- names %>% compute_player_probabilities(data, "256")
toc()

density_list <- list("mid" = density_mid, "high" = density_high, "low" = density_low, "256" = density_256)

# Download win probability tables for each game ----------------------------------
cleaned_data <- data |> select(-qianzi, -kenny, -veronica, -theo) |>
  mutate(
    order = str_remove(order, "Y|K|V|T")
  )


last_game_id <- read_csv("last_game_id.csv")
win_prob_tables <- read_csv("win_prob_tables.csv")
game_ids <- filter(cleaned_data, game > last_game_id$value[1], game < 241) |> pull(game) |> unique()

win_prob_tables <- bind_rows(win_prob_tables, map_dfr(game_ids, ~game_prob_wrapper(.x, cleaned_data, density_list) |>
  mutate(game_id = .x)))

write_csv(win_prob_tables, "win_prob_tables.csv")
# separate out, so we don't have to run each game table every time
max(game_ids) |> as_tibble() |> write_csv("last_game_id.csv")

# to do:
# 1. run win_prob_tables
