# Update Dart Board

source("Win Probability Support.R")

# read data from excel and draw data
data <- read_excel("Maxfield - Darts.xlsx") %>%
  clean_names() %>%
  select(angle:theo) %>%
  filter(!is.na(date)) %>%
  filter(base != "bad magnet", angle == "vertical")

# Load Win Probabilities for each height ----------------------------------

# load old game counts and compute new
old_game_counts <- read_csv("game_counts.csv")
new_game_counts <- data |> select(game, height) |>
  distinct() |> group_by(height) |>
  summarize(count = n())
combined_counts <- old_game_counts |> left_join(new_game_counts, by = join_by(height)) |>
  mutate(diff = count.y - count.x)
# need to update posterior?
update_density <- setNames(as.list(combined_counts$diff > 0), combined_counts$height)

names <- c("caleb", "joshua", "quadri", "joseph", "daniel")

tic()
if(update_density$mid){
  density_mid <- names %>% compute_player_probabilities(data, "mid")
}else{
  load("Densities/density_mid.RData")
}
if(update_density$high){
  density_high <- names %>% compute_player_probabilities(data, "high")
}else{
  load("Densities/density_high.RData")
}
if(update_density$low){
  density_low <- names %>% compute_player_probabilities(data, "low")
}else{
  load("Densities/density_low.RData")
}
if(update_density$`256`){
  density_256 <- names %>% compute_player_probabilities(data, "256")
}else{
  load("Densities/density_256.RData")
}
toc()

density_list <- list("mid" = density_mid, "high" = density_high, "low" = density_low, "256" = density_256)

write_csv(new_game_counts, "game_counts.csv")

# Download win probability tables for each game ----------------------------------
cleaned_data <- data |> select(-qianzi, -kenny, -veronica, -theo) |>
  mutate(
    order = str_remove_all(order, "Y|K|V|T")
  )

last_game_id <- read_csv("last_game_id.csv")
win_prob_tables <- read_csv("win_prob_tables.csv")
game_ids <- filter(cleaned_data, game > last_game_id$value[1]) |> pull(game) |> unique()

win_prob_tables <- bind_rows(win_prob_tables, map_dfr(game_ids, ~game_prob_wrapper(.x, cleaned_data, density_list) |>
  mutate(game_id = .x)))

write_csv(win_prob_tables, "win_prob_tables.csv")
# separate out, so we don't have to run each game table every time
max(win_prob_tables$game_id) |> as_tibble() |> write_csv("last_game_id.csv")

rsconnect::deployApp(appName = "darts-win-probability",
                     account = "calebskinner",
                     forceUpdate = TRUE,
                     appPrimaryDoc = "Dart App.R",
                     appFiles = NULL)

