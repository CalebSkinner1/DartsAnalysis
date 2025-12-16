# run online app that can predict matches live

library("shiny")
library("shinydashboard")
library("DT")

if (interactive()) {
  # setwd(here::here("DartsAnalysis"))
}

source("Dart App Support.R")

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "DartBoard App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Current Game", tabName = "current", icon = icon("arrow-trend-up")),
      menuItem("Past Games", tabName = "past", icon = icon("calendar-days")))
  ),
  dashboardBody(
    tabItems(
      tabItem(
        # Home Tab
        tabName = "home",
        titlePanel("Dart Board Win Probability App"),
        withMathJax(),
        p("This website is intended to provide quick win probability estimates for in-game Darts Games."),
        p(strong("Rules: "),
        "A single game is composed of three rounds, each consisting of three dart throws each. Players earn
        1 point for landing the dart on the board, 2 points for landing the dart within the large circle,
        3 points for landing the dart within the smaller circle, and 5 points for a bullseye. We record
        the aggregate scores for each round. In the event of a tie for first place after three rounds, the tied players
        proceed to overtime rounds until the tie is broken."),
        p("This aim of this project is to estimate the probability distribution for each player's score in a given round,
        enabling inference and live win probability estimation. A naive approach models the score per round directly with
        any number of discrete parametric distributions. However, this ignores the natural constructions and limitations of the game.
        Each round consists of three throws, each taking one of five possible values. As a result, aggregate round scores are
        unevenly distributed, with certain values occurring far more frequently than others.
        For example, becauseplayers are more likely to throw a 3 than a 2, they are correspondingly more
        likely to throw a 3 or a 6 in a round than a 2 or a 5."),
        p("To address this issue, I model individual throws directly. The rules imply that each throw
        follows a categorical distribution over the five possible outcomes. I assume that throws are independent and identically distributed
        across the entire game-an assumption that is imperfect but not severely restrictive in practice. Formally, we write:
        $$x_{ji} \\sim \\text{Categorical}(\\mathbf{p}),$$
        where \\(i = 1, 2, 3\\) indexes the throw within a round,
        \\(j = 1, \\ldots, J\\) indexes the round, and $\\(\\mathbf{p} = (p_0, p_1, p_2, p_3, p_5)\\) gives the probability of scoring
        \\(0, 1, 2, 3,\\) or \\(5\\) points on a single throw. These probabilities satisfy \\(\\sum_{k=0}^5 p_k = 1\\).
        I place a conjugate Dirichlet prior on \\(\\mathbf{p}\\):
        $$\\mathbf{p} \\sim \\text{Dirichlet}(\\boldsymbol{\\alpha}),$$
        where \\(\\boldsymbol{\\alpha} > 0\\) encodes the prior beliefs about each outcome."),
        p("Posterior sampling of \\(\\mathbf{p}\\) would be straightforward using a sample Gibbs Sampler. However,
        we do not directly observe the individual throws \\(x_{j i}\\). Instead, we observe only
        the round-level totals \\(y_j = \\sum_{i=1}^3 x_{j i}.\\) This missing-data structure complicates inference,
        but a simple Metropolis–Hastings step facilitates efficient sampling by proposing latent row vectors and
        accepting or rejecting them based on the resulting change in posterior probability. This approach closely resembles
        the logic of the EM algorithm commonly used for missing data problems. The full sampler implementation is available on ",
        a("GitHub", href = "https://github.com/CalebSkinner1/DartsAnalysis/blob/main/MCMC.R", target = "_blank"),
        "."),
        p("Leveraging posterior samples, inference on in-game win probabilities becomes straightforward.
        For now, I assume that each player's throws are independent of those of other competitors. In future work,
        this framework could be extended hierarchically to borrow strength across players, which
        would substantially improve estimation for players with limited data."),
      ),
      tabItem(
        # Current Game
        tabName = "current",
        titlePanel("Live Match Win Probability"),
        
        p("Enter the players competing in the match and the current round. The order of which
          you enter the players is assumed to be the order of play. This will take a moment to load."),
        fluidRow(
          column(4,
                 selectizeInput(
                   inputId = "height", 
                   label = "Enter the Height of the Dart Board", 
                   choices = sort(unique(data$height)),
                   options = list(
                     placeholder = "Start typing...",
                     maxOptions = 4  # Limit the number of suggestions shown
                   ))),
          column(4,
                  selectizeInput(
                    inputId = "players", 
                    label = "Enter Multiple Players", 
                    choices = names,
                    multiple = TRUE,
                    options = list(
                      placeholder = "Start typing...",
                      maxOptions = 5  # Limit the number of suggestions shown
                    ))),
          column(4,
                  selectizeInput(
                    inputId = "round", 
                    label = "Enter Current Round of Play", 
                    choices = c(1, 2, 3, 4, 5),
                    options = list(
                      placeholder = "Start typing...",
                      maxOptions = 3  # Limit the number of suggestions shown
                    )))),
        
        uiOutput("numeric_inputs"),
        
        actionButton("compute", "Compute"),
        
        uiOutput("win_probability_title"),
        
        plotOutput("win_probability"), height = "400px", width = "100%"),
      
      tabItem(
        # Past Games
        tabName = "past",
        titlePanel("Past Match Win Probability"),
        
        p("Select any Game ID to see the corresponding win probability graph."),
        selectizeInput(
          inputId = "game_id", 
          label = "Enter a Game ID", 
          choices = unique(data$game),
          options = list(
            placeholder = "Start typing...",
            maxOptions = 5  # Limit the number of suggestions shown
          )),
        
        plotlyOutput("plot_past_game_win_probability", height = "400px", width = "100%"),
        
      )
    )
  )
)

server <- function(input, output, session) {

  # Reactivity for Current Game
  
  output$numeric_inputs <- renderUI({
    req(input$players, input$round)
    
    # Dynamically generate numeric inputs in columns
    tagList(
      lapply(seq_len(input$round), function(row) {
        fluidRow(
          h4(paste("Round", row)),
          
          lapply(seq_along(input$players), function(i) {
            column(
              width = max(12 / length(input$players), 3),  # distribute evenly
              numericInput(
                inputId = paste0("row_", row, "_player_", i),
                label = input$players[i],
                value = NA,
                min = 0,
                max = 11
              )
            )
          })
        )
      }
    )
    )
  })
  
  organize_tibble <- eventReactive(input$compute, {
    req(input$players, input$round)
    
    expand_grid(
      round = seq_len(input$round),
      player_index = seq_along(input$players)) %>% 
      mutate(
        player = input$players[player_index],
        value = map2_dbl(round, player_index, ~{
          val <- input[[paste0("row_", .x, "_player_", .y)]]
          if(is.null(val)) NA_real_ else val})) %>%
      select(-player_index) %>%
      pivot_wider(names_from = player, values_from = value)
  })
  
  output$win_probability_title <- renderUI({
    h3("Win Probability Graph")
  })
  
  output$win_probability <- renderPlot({
    this_game <- organize_tibble()
    
    if(nrow(this_game) == 1){
      this_game <- this_game %>% add_row(round = 2) %>% add_row(round = 3)
    }else if(nrow(this_game) == 2){
      this_game <- this_game %>% add_row(round = 3)
    }
    
    throws <- sum(!is.na(this_game[-1,]))

    density <- density_list[[input$height]]
    
    order <- str_sub(input$players, start = 1, end = 1) |> str_to_upper() |> str_flatten(collapse = "")

    p <- compute_game_probability_tibble(this_game, order, data, input$height, density) |>
      slice(1:(throws+str_length(order))) |>
      ggplot() +
      geom_line(aes(x = round, y = win_prob, color = player)) +
      labs(x = "Round", y = "Win Probability")
    
    ggplotly(p)
  })
  
  # Reactivity for Past Games
  
  output$plot_past_game_win_probability <- renderPlotly({ #first plot
    req(input$game_id)
    win_prob_tables |>
      filter(game_id == input$game_id) |>
      mutate(
        round = round(round, digits = 2),
        win_prob = round(win_prob, digits = 4)) |>
      ggplot() +
      geom_line(aes(x = round, y = win_prob, color = player)) +
      labs(x = "Round", y = "Win Probability", title = str_c("Game ", input$game_id))
  })
}

shinyApp(ui, server)
