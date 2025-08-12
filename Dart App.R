# run online app that can predict matches live

library("shiny")
library("shinydashboard")
library("DT")

if (interactive()) {
  setwd(here::here("DartsAnalysis"))
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
        p("This website is intended to provide quick win probability estimates for in-game Darts Games.
          More details to come - if I feel inclined.")
        ),
      
      tabItem(
        # Current Game
        tabName = "current",
        titlePanel("Live Match Win Probability"),
        
        p("Enter the players competiting in the match and the current round. The order of which
          you enter the players is assumed to be the order of play."),
        fluidRow(
          column(4,
                 selectizeInput(
                   inputId = "height", 
                   label = "Enter the Height of the Dart Board", 
                   choices = unique(data$height),
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
        
        p("Select any Game ID to see the corresponding win probability graph. The graph will take a moment to load."),
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
    
    density <- density_list[[input$height]]
    
    p <- compute_win_probability(this_game, data, input$height, density) %>%
      ggplot(aes(x = player, y = win_prob)) +
      geom_bar(aes(fill = player), stat = "identity") +
      geom_label(aes(label = scales::percent(win_prob))) +
      labs(x = "", y = "Win Probability") +
      scale_y_continuous(labels = scales::percent) +
      theme(legend.position = "none")
    plot(p)
  })
  
  # Reactivity for Past Games
  
  output$plot_past_game_win_probability <- renderPlotly({ #first plot
    req(input$game_id)
    game_chart_wrapper(as.numeric(input$game_id), data, density_list)
  })
  
}

shinyApp(ui, server)
