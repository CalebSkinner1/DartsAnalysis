# This script runs mcmc sampler for p without gaining information from other players
library("readxl")
library("janitor")
library("tictoc")

source("DartsAnalysis/MCMC.R")

# read data from excel and draw data
data <- read_excel("DartsAnalysis/Maxfield - Darts.xlsx") %>%
  clean_names() %>%
  select(angle:joseph) %>%
  filter(!is.na(date))

# compute samples for Caleb
tic()
caleb_data <- data %>% filter(angle == "vertical", height == "mid", base == "magnet") %>% pull(caleb) %>% na.omit() %>% as.numeric()
caleb_samples <- mh_sampler(caleb_data)
toc()

# acceptance rate
caleb_samples$acc_rate %>% mean()

# summary statistics
caleb_samples$rho %>% as.mcmc() %>% summary()

caleb_samples$rho %>% as.mcmc() %>% plot()

caleb_samples$new_y %>% summary()

caleb_samples$new_y %>% as.mcmc() %>% plot()

freq_fun <- function(df, name){
  df %>% group_by(y) %>%
    summarize(count = n()) %>%
    mutate(prop = count/sum(count)) %>%
    select(y, prop) %>%
    rename_with(~paste0(name), starts_with("prop"))
}

tibble(y = caleb_samples$new_y) %>%
  freq_fun("sampled") %>%
  left_join(tibble(y = caleb_data) %>% freq_fun("observed"), by = join_by(y)) %>%
  ggplot() +
  geom_line(aes(x = y, sampled), color = "cadetblue4") +
  geom_line(aes(x = y, observed), color = "indianred3")

# compute samples for Joshua
joshua_data <- data %>% filter(angle == "vertical", height == "mid", base == "magnet") %>% pull(joshua) %>% na.omit() %>% as.numeric()
joshua_samples <- mh_sampler(joshua_data)

# acceptance rate
joshua_samples$acc_rate %>% mean()

# summary statistics
joshua_samples$rho %>% as.mcmc() %>% summary()

joshua_samples$rho %>% as.mcmc() %>% plot()

tibble(y = joshua_samples$new_y) %>%
  freq_fun("sampled") %>%
  left_join(tibble(y = joshua_data) %>% freq_fun("observed"), by = join_by(y)) %>%
  ggplot() +
  geom_line(aes(x = y, sampled), color = "cadetblue4") +
  geom_line(aes(x = y, observed), color = "indianred3")

# compute samples for Quadri
quadri_data <- data %>% filter(angle == "vertical", height == "mid", base == "magnet") %>% pull(quadri) %>% na.omit() %>% as.numeric()
quadri_samples <- mh_sampler(quadri_data)

# acceptance rate
quadri_samples$acc_rate %>% mean()

# summary statistics
quadri_samples$rho %>% as.mcmc() %>% summary()

quadri_samples$rho %>% as.mcmc() %>% plot()


