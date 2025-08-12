# Compute Win Probability
library("readxl")
library("janitor")
library("tictoc")
library("plotly")

source("MCMC.R")

compute_player_probabilities <- function(players, data, height){
  probs_table <- map(players, ~{
    player <- str_to_lower(.x)
    
    past_scores <- data %>% filter(height == height) %>% pull(any_of(player)) %>% na.omit() %>% as.numeric()
    
    samples <- mh_sampler(past_scores)
    
    samples$new_y %>% table() %>% as_tibble() %>%
      rename("score" = ".") %>%
      mutate(perc = n/length(samples$new_y)) %>%
      select(score, perc)
  })
  names(probs_table) <- players
  
  probs_table
}

# sum multiple rounds together (r is number of rounds)
sum_mult_rounds <- function(probs_table, r) {
  if(r == 0){
    tibble(score = 0, perc = 1)
  }else{
    reduce(
      rep(list(probs_table), r),
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
}

# find win rates from distributions
compare_distributions <- function(probs_table, names = NULL) {
  # dist_list: list of tibbles with columns outcome & prob
  n <- length(probs_table)
  if (is.null(names)){
    if(is.null(names(probs_table))){
      names <- paste0("player", seq_len(n))
    }else{
      names <- names(probs_table)
    }}
  
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
  
  # straight up win
  win_table <- map_dfr(names, ~{
    lookup <- paste0(.x, "_score")
    
    full %>% filter(!!sym(lookup) == "win") %>% summarize(result = sum(perc)) %>% mutate(player = .x, type = "win")})
  
  
  ties_names <- map(c(2:n), ~combn(names, .x, simplify = FALSE)) %>% flatten()
  
  tie_table <- map_dfr(ties_names, ~{
    lookup <- paste0(.x, "_score")
    temp_ties_df <- full
    
    for(i in seq_along(lookup)){
      temp_ties_df <- temp_ties_df %>%
        filter(!!sym(lookup[i]) == "tie")
    }
    temp_ties_df %>%
      rowwise() %>%
      mutate(ties_num = sum(c_across(contains("score")) == "tie")) %>%
      filter(ties_num == length(lookup)) %>%
      ungroup() %>%
      summarize(result = sum(perc)) %>%
      mutate(player = str_flatten_comma(.x), type = "overtime")})
  
  # aggregate
  bind_rows(win_table, tie_table)
}

# find win rates from overtime
compute_overtime <- function(probs_table, win_rates){
  overtimes_table <- win_rates %>% filter(type == "overtime")
  
  ties_names <- overtimes_table %>% pull(player)
  
  weights <- overtimes_table %>% pull(result)
  
  map2_dfr(ties_names, weights, ~probs_table[str_split(.x, ", ")[[1]]] %>%
        compare_distributions() %>%
        filter(type == "win") %>%
        mutate(result = result/sum(result)*.y))
}

compute_win_probability <- function(game_tibble, data, height, player_probabilities){
  game_table <- game_tibble %>% select(-round)
  
  players <- game_table %>% colnames()
  
  rounds_left <- game_tibble %>%
    rowwise() %>%
    mutate(na = sum(is.na(c_across(all_of(players))))) %>%
    ungroup() %>%
    mutate(
      prev_na = if_else(cumsum(na) == na, 0, 1),
      remove = if_else(str_detect(round, "OT") & prev_na, 1, 0)) %>%
    filter(remove != 1) %>%
    group_by() %>% summarize(across(all_of(players), ~sum(is.na(.x))))
  
  current_score <- game_table %>% group_by() %>% summarize(across(everything(), ~sum(.x, na.rm = TRUE)))
  
  if(missing(player_probabilities)){
    tic()
    player_probabilities <- compute_player_probabilities(players, data, height)
    names(player_probabilities) <- players
    toc()
  }else{
    player_probabilities <- player_probabilities[players]
  }
  
  # convert player probabilities to remaining rounds
  remaining_rounds_table <- pmap(list(player_probabilities, rounds_left, current_score),
                                 function(prob_table, rounds, current_score){
                                   sum_mult_rounds(prob_table, rounds) %>%
                                     mutate(score = as.numeric(score) + current_score)})
  
  # win rate before overtime
  win_rates <- compare_distributions(remaining_rounds_table)

  # overtime
  win_rates %>% filter(type == "win") %>%
    bind_rows(compute_overtime(player_probabilities, win_rates)) %>%
    group_by(player) %>%
    summarize(win_prob = sum(result))
}

# examples
# can compute this ahead of time (if computing multiple game probabilities) or in compute_win_probability function

# game_tibble <- tibble(round = c(1, 2, 3), caleb = c(0, 0, NA), joshua = c(1, 2, NA))
# compute_win_probability(game_tibble, data = data, height = "mid", player_probabilities)
# 
# game_tibble <- tibble(round = c(1, 2, 3), caleb = c(NA, NA, NA), joshua = c(NA, NA, NA), quadri = c(1, NA, NA))
# compute_win_probability(game_tibble, data = data, height = "mid", player_probabilities)
# 
# full_game_tibble <- tibble(round = c(1, 2, 3), quadri = c(4, 1, 3), joshua = c(0, 4, 1))

# finds player name from abbreviated letter
convert_name <- function(letter){
  case_when(
    letter == "C" ~ "caleb",
    letter == "J" ~ "joshua",
    letter == "Q" ~ "quadri",
    letter == "B" ~ "joseph",
    letter == "D" ~ "daniel",
    letter == "V" ~ "veronica",
    letter == "K" ~ "kenny",
    letter == "Y" ~ "qianzi",
    .default = NA)
  
}

# converts order abbreviation into list of players
convert_order <- function(order){
  map_chr(seq_len(str_length(order)), ~str_sub(order, start = .x, end = .x) %>% convert_name())
}

# charts win probability of game
chart_game_probability <- function(full_game_tibble, order, data, height, player_probabilities){
  players <- convert_order(order)
  
  game_long <- full_game_tibble %>% pivot_longer(cols = all_of(players), values_to = "score", names_to = "player") %>%
    mutate(player = factor(player, levels = players)) %>%
    arrange(round, player)
  
  win_prob_data <- map_dfr(0:nrow(game_long), ~{
    game_long %>% mutate(index = row_number(),
                  score = if_else(index > .x, NA, score)) %>%
      select(-index) %>%
      pivot_wider(names_from = "player", values_from = "score") %>%
      compute_win_probability(data = data, height = height, player_probabilities = player_probabilities) %>%
      pivot_wider(names_from = "player", values_from = "win_prob") %>%
      mutate(round = .x/length(players) + 1)
    }) %>%
    pivot_longer(cols = all_of(players), values_to = "win_prob", names_to = "player")
  
  win_prob_data %>%
    ggplot() +
    geom_line(aes(x = round, y = win_prob, color = player)) +
    labs(x = "Round", y = "Win Probability")
}

game_chart_wrapper <- function(game_id, data, player_probabilities_list){
  specific_game <- data %>% filter(game == game_id)
  
  order <- specific_game %>% slice(1) %>% pull(order)
  
  height <- specific_game %>% slice(1) %>% pull(height)
  
  player_probabilities <- player_probabilities_list[[height]]
  
  game_tibble <- specific_game %>% select(round, all_of(convert_order(order))) %>%
    mutate(across(all_of(convert_order(order)), ~as.numeric(.x)))
  
  p <- chart_game_probability(full_game_tibble = game_tibble, order = order, data = data, height = height, player_probabilities = player_probabilities) +
    labs(title = str_c("Game ", game_id, " Win Probability", y = ""))
  
  ggplotly(p)
}



