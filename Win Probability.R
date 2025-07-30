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

sum_mult_rounds <- function(probs_table, n) {
  reduce(
    rep(list(probs_table), n),
    function(a, b) {
      merge(a, b, by = NULL) %>%
        transmute(
          score = as.numeric(score.x) + as.numeric(score.y),
          perc = perc.x * perc.y
        ) %>%
        group_by(score) %>%
        summarise(perc = sum(perc), .groups = "drop")
    }
  )
}

compare_distributions <- function(probs_table, names = NULL) {
  # dist_list: list of tibbles with columns outcome & prob
  n <- length(probs_table)
  if (is.null(names)) names <- paste0("player", seq_len(n))
  
  # 1. Build full grid of outcomes
  grids <- probs_table %>% 
    set_names(names)
  
  # find possibilities and probability
  full <- map2_dfc(expand_grid(!!!grids), names, function(g, n){rename_with(g, ~str_c(n, "_", .x))}) %>%
    rowwise() %>%
    mutate(
      perc = prod(c_across(contains("perc"))),
      top_result = max(c_across(contains("score")))) %>%
    ungroup() %>%
    mutate(across(contains("score"), ~if_else(top_result == .x, "win", "loss"))) %>%
    rowwise() %>%
    mutate(winners = sum(c_across(contains("score")) == "win")) %>%
    ungroup() %>%
    mutate(across(contains("score"), ~if_else(winners > 1 & .x == "win", "tie", .x))) %>%
    select(contains("score"), perc)
  
  win_table <- map_dfr(names, ~{
    lookup <- paste0(.x, "_score")
    
    full %>% filter(!!sym(lookup) == "win") %>% summarize(win = sum(perc)) %>% mutate(player = .x)})
  
  
  ties_names <- map(seq_along(2:n), ~combn(names, .x, simplify = FALSE)) %>% flatten()
  
  tie_table <- map_dfr(ties_names, ~{
    lookup <- paste0(.x, "_score")
    
    # HERE
    full %>% filter(!!sym(lookup[1]) == "tie", ) %>% summarize(win = sum(perc)) %>% mutate(player = .x)})
  
  # 3. Compute win/tie
  full <- full %>%
    mutate(
      joint_prob = reduce(map(seq_len(n),
                              ~pull(full, paste0("p", .x))),
                          `*`),
      max_val = pmax(!!!syms(names)),
      # which ones tie for max?
      is_max = map(names, ~.[[.x]] == max_val),
      tie_group = map_chr(is_max, ~ paste(names[.x][.], collapse = "_"))
    )
  
  # 4. Aggregate
  summary <- full %>%
    group_by(tie_group) %>%
    summarise(prob = sum(joint_prob), .groups = "drop") %>%
    arrange(desc(prob))
  
  list(detail = full, summary = summary)
}

# HERE

game_tibble <- tibble(round = c(1, 2, 3), caleb = c(0, 0, NA), joshua = c(1, 2, NA))

compute_win_probability <- function(game_tibble, data, height){
  game_table <- game_tibble %>% select(-round)
  
  players <- game_table %>% colnames()
  
  rounds_left <- game_table %>% group_by() %>% summarize(across(everything(), ~sum(is.na(.x))))
  
  current_score <- game_table %>% group_by() %>% summarize(across(everything(), ~sum(.x, na.rm = TRUE)))
  
  tic()
  player_probabilities <- map(players, ~compute_player_probabilities(.x, data, height))
  toc()
  
  remaining_rounds_table <- pmap(list(player_probabilities, rounds_left, current_score),
                                 function(prob_table, rounds, current_score){
                                   sum_mult_rounds(prob_table, rounds) %>%
                                     mutate(score = as.numeric(score) + current_score)})
  

}


compute_player_probabilities(game_tibble, data = data, height = "mid")
