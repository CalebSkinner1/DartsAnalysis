# Dart Support Functions and files. These functions support the app

source("Win Probability Support.R")

data <- read_excel("Maxfield - Darts.xlsx") %>%
  clean_names() %>%
  select(angle:joseph) %>%
  filter(!is.na(date)) %>%
  filter(base != "bad magnet", angle == "vertical")

load("Densities/density_mid.RData")
load("Densities/density_high.RData")
load("Densities/density_low.RData")
load("Densities/density_256.RData")

names <- c("caleb", "joshua", "quadri", "joseph", "daniel")

density_list <- list("mid" = density_mid, "high" = density_high,
                     "low" = density_low, "256" = density_256)