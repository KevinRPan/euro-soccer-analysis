##  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##    UI
##
##      This file is the User Interface for the app.
##      This is where the text for each page happens,
##        and where the structure of the app is defined.
##
##  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(shiny)
library(shinythemes)

shinyUI(
  navbarPage(
    title = "Euro Football Performance Analysis",
    theme = shinytheme("flatly"),

    #### Rankings table ----------------------------------------------------
    tabPanel(
      "Current rankings",
      fluidRow(column(8,
                      h4(
                        "2016 Euro football rankings"
                      ))),
      selectInput(
        "league_rank",
        "League:",
        names(league_games),
        selected = NULL,
        multiple = FALSE,
        selectize = TRUE,
        width = NULL,
        size = NULL
      ),
      # submitButton("Go"),
      DT::dataTableOutput("rank_table")
    ),

    #### Expectancies table ----------------------------------------------------
    tabPanel(
      "Outcome expectancies",
      fluidRow(column(
        12,
        h4("Expected chance of player match-ups"),
        p(
          "These player odds are calculated by the",
          tags$a(href = "http://www.eloratings.net/system.html", "difference in ratings"),
          " method. The more blue the column, the better the odds of winning!"
        ),
        # p("Read across: Row Player's chance of beating Column Player")
        # p(""),
        p("Try highlighting a column to see your favorite team's chances.")
      )),
      selectInput(
        "league_odds",
        "League:",
        names(league_games),
        selected = NULL,
        multiple = FALSE,
        selectize = TRUE,
        width = NULL,
        size = NULL
      ),
      # submitButton("Go"),
      # uiOutput("ui_player_expectancy")
      plotlyOutput('player_expectancy_ggp')
    ),

    ## 2017 rankings graph --------------------------------------------------
    tabPanel(
      "Rankings Graphs",
      fluidRow(column(8,
                      h4(
                        "Tracking team rankings"
                      ))),

      fixedRow(
        column(4, selectInput(
            "league_rank_graph",
            "League:",
            names(league_games),
            selected = NULL,
            multiple = FALSE,
            selectize = TRUE,
            width = NULL,
            size = NULL
          )
        ),

        column(4, radioButtons(
          "includePointsNew",
          "Include Points?",
          c("Yes" = TRUE, "No" = FALSE)
        )),
        column(4, radioButtons(
          "includeLinesNew", "Include Lines?",
          c("Yes" = TRUE, "No" = FALSE)
        ))
      ),
      plotlyOutput("rank_plot_2017")
    ),

    #### Games Network  ---------------------------------------------------------
    tabPanel(
      "League Networks",
      fluidRow(column(
        8,
        h4("Game network"),
        p("Who's played who?")
      )),
      selectInput(
        "league_network",
        "League:",
        names(league_games),
        selected = NULL,
        multiple = FALSE,
        selectize = TRUE,
        width = NULL,
        size = NULL
      ),
      forceNetworkOutput("force")
    ),

    #### Current games ---------------------------------------------------------
    tabPanel(
      "Match records",
      fluidRow(column(
        8,
        h4("Game records"),
        p("Find your favorite team?")
      )),
      selectInput(
        "league_records",
        "League:",
        names(league_games),
        selected = NULL,
        multiple = FALSE,
        selectize = TRUE,
        width = NULL,
        size = NULL
      ),
      DT::dataTableOutput("games_record")
    )
  )
)
