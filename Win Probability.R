# Win Probability Page

source("Win Probability Support.R")

# read data from excel and draw data
data <- read_excel("Maxfield - Darts.xlsx") %>%
  clean_names() %>%
  select(angle:joseph) %>%
  filter(!is.na(date)) %>%
  filter(base != "bad magnet", angle == "vertical")

# Load Win Probabilities for each height ----------------------------------

load("Densities/density_mid.RData")
load("Densities/density_high.RData")
load("Densities/density_low.RData")
load("Densities/density_256.RData")

density_list <- list("mid" = density_mid, "high" = density_high, "low" = density_low, "256" = density_256)

# examples
tic()
game_chart_wrapper(192, data, density_list)
p <- game_chart_wrapper(79, data, density_list)
toc()




