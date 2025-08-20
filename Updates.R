# Update Dart Board
# this page allows one to run the app and update the densities if desired

if (interactive()) {
  setwd(("DartsAnalysis"))
}

# update densities
source("Win Probability.R")

rsconnect::deployApp(appName = "darts-win-probability",
                     account = "calebskinner",
                     forceUpdate = TRUE,
                     appPrimaryDoc = "Dart App.R",
                     appFiles = NULL)

