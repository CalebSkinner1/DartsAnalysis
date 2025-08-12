# Update Dart Board
# this page allows one to run the app and update the densities if desired

# update densities
source("Win Probability.R")

if (interactive()) {
  setwd(("DartsAnalysis"))
}

rsconnect::deployApp(appName = "darts-win-probability",
                     account = "calebskinner",
                     forceUpdate = TRUE,
                     appPrimaryDoc = "Dart App.R",
                     appFiles = NULL)
