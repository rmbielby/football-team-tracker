function(input, output) {

  player_position_stats <- reactive({
    if(input$season == "All time"){
      player_match_positions <- team_data |> 
        tidyr::separate_longer_delim(position, delim = ",") |>
        dplyr::summarise(count_positions = dplyr::n(), .by = c(date, name))
      team_data |> 
        tidyr::separate_longer_delim(position, delim = ",") |>
        dplyr::left_join(player_match_positions, by = dplyr::join_by(date, name)) |>
        dplyr::summarise(count = sum(1./count_positions), .by = c(name, position, season)) |>
        dplyr::arrange(name, count)
    } else {
      player_match_positions <- team_data |> 
        filter(season == input$season) |>
        tidyr::separate_longer_delim(position, delim = ",") |>
        dplyr::summarise(count_positions = dplyr::n(), .by = c(date, name))
      team_data |> 
        filter(season == input$season) |>
        tidyr::separate_longer_delim(position, delim = ",") |>
        dplyr::left_join(player_match_positions, by = dplyr::join_by(date, name)) |>
        dplyr::summarise(count = sum(1./count_positions), .by = c(name, position, season)) |>
        dplyr::arrange(name, count)
    }
  })
  
  summary_stats <- reactive({
    if(input$season == "All time"){
      team_data |> 
        summarise(
          Apps = n(),
          Goals = sum(goals, na.rm = TRUE), 
          Assists = sum(assists, na.rm = TRUE), 
          MotM = sum(motm, na.rm = TRUE), 
          .by=name) |>
        left_join(
          player_position_stats() |> arrange(name, -count) |>
            group_by(name) %>% 
            mutate(Positions = paste0(position, collapse = ", ")) |> select(name, Positions) |> distinct(),
          by = "name"
        ) |>
        select(name, Positions, Apps, Goals, Assists, MotM)
    } else {
      team_data |> 
        filter(season == input$season) |>
        summarise(
          Apps = n(),
          Goals = sum(goals, na.rm = TRUE), 
          Assists = sum(assists, na.rm = TRUE), 
          MotM = sum(motm, na.rm = TRUE), 
          .by=name) |>
        left_join(
          player_position_stats() |> 
            arrange(name, -count) |>
            group_by(name) %>% 
            mutate(Positions = paste0(position, collapse = ", ")) |> select(name, Positions) |> distinct(),
          by = "name"
        ) |>
        select(name, Positions, Apps, Goals, Assists, MotM)
    }
  })
    
  output$player_summary_stats <- renderReactable({
      reactable(
        summary_stats() |> 
          filter(name == input$player)  |>
          sjmisc::rotate_df() |>
          rename(Info = V1)
      ) 
  }
  )

  output$top_apps <- renderReactable({
    reactable(
      summary_stats() |> 
        select(name, Apps)  |>
        arrange(-Apps, name) |>
        top_n(5)
    ) 
  }  )
  
  output$top_goals <- renderReactable({
    reactable(
      summary_stats() |> 
        select(name, Goals)  |>
        filter(Goals > 0) |>
        arrange(-Goals, name) |>
        top_n(5)
    ) 
  }  )
  
  output$top_assists <- renderReactable({
    reactable(
      summary_stats() |> 
        select(name, Assists)  |>
        arrange(-Assists, name) |>
        top_n(5)
    ) 
  }  )
  
  output$top_motm <- renderReactable({
    reactable(
      summary_stats() |> 
        select(name, MotM)  |>
        arrange(-MotM, name) |>
        top_n(5)
    ) 
  }  )
  
      
    output$pitch <- renderPlot(
      player_position_stats() |>
        dplyr::filter(name == input$player) |>
        plot_field()
    )
  }
