##  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##    Server
##
##    This file describes what objects are being rendered.
##    For example, there are formatting options for the tables.
##
##  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(shiny)
library(DT)

shinyServer(function(input, output) {

  #### Rank table ------------------------------------------------------------

  output$rank_table <- renderDataTable({
    league_games[[input$league_rank]]$singles_elo %>% CreateRankTable(use_paging = TRUE)
  })

  #### Player expectancy ------------------------------------------------------

  ## Wrapper for heatmap
  output$ui_player_expectancy <- renderUI({
    d3heatmapOutput('player_expectancy')
  })
  output$player_expectancy <- renderD3heatmap({
    league_games[[input$league_odds]]$singles_elo %>% PlotCoWMatrix
  })

  output$player_expectancy_ggp <- renderPlotly({
    league_games[[input$league_odds]]$singles_elo %>% PlotCoWMatrix(use_d3 = FALSE)
  })

  # renderTable(player_odds_df %>% round(3) * 100,
  #             rownames = TRUE,
  #             digits = 1
  # )

  #### Games table ------------------------------------------------------------

  output$games_record <- DT::renderDataTable({
    DT::datatable(all_matches %>%
                    filter(league_name == input$league_records) %>%
                    mutate(date = as.Date(date),
                           winner = ifelse(t1_score>t2_score,
                                           t1,
                                           t2)) %>%
                    select("Game Number" = game_num,
                           Date = date,
                           "Team 1" = t1,
                           "Team 1 Score" = t1_score,
                           "Team 2" = t2,
                           "Team 2 Score" = t2_score,
                           'Winner' = winner)
    )
  })


  #### Games Network --------------------------------------------------

  output$force <- renderForceNetwork({
    PlotGamesNetwork(singles_elo = league_games[[input$league_network]]$singles_elo,
                     singles_games = no_tie_matches %>%
                       filter(league_name == input$league_network))
  })

  #### 2017 Rank plot --------------------------------------------------
  output$rank_plot_2017 <- renderPlotly({
    league_games[[input$league_rank_graph]]$elo_tracker %>%
      PlotElo(input$includePointsNew, input$includeLinesNew)
  })
})
