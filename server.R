function(input, output) {
  team_data_season <- reactive(
    if (input$season == "All time") {
      team_data
    } else {
      team_data |>
        filter(season == input$season)
    }
  )

  result_stats <- reactive({
    team_data |>
          dplyr::filter(
          Competition %in% input$competition
        ) |>
      summarise(
        goals = sum(goals, na.rm = TRUE),
        conceded = sum(conceded, na.rm = TRUE),
        .by = c(season, date, opposition, venue, Competition)
      ) |>
      summarise(
    Played = n(),
        Wins = sum(goals > conceded),
        Draws = sum(goals == conceded),
        Losses = sum(goals < conceded),
        `Goals for` = sum(goals, na.rm = TRUE),
        `Goals against` = sum(conceded, na.rm = TRUE),
        .by = c(season, Competition)
      ) |>
      as.data.frame()
  })

  player_position_stats <- reactive({
    if (input$season == "All time") {
      player_match_positions <- team_data |>
        tidyr::separate_longer_delim(position, delim = ",") |>
        dplyr::summarise(count_positions = dplyr::n(), .by = c(date, name))
      team_data |>
        tidyr::separate_longer_delim(position, delim = ",") |>
        dplyr::left_join(player_match_positions, by = dplyr::join_by(date, name)) |>
        dplyr::summarise(count = sum(1. / count_positions), .by = c(name, position)) |>
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
        dplyr::summarise(count = sum(1. / count_positions), .by = c(name, position)) |>
        dplyr::arrange(name, count)
    }
  })

  summary_stats <- reactive({
    if (input$season == "All time") {
      team_data |>
        dplyr::filter(
          Competition %in% input$competition
        ) |>
        summarise(
          Apps = n(),
          Goals = sum(goals, na.rm = TRUE),
          Assists = sum(assists, na.rm = TRUE),
          MotM = sum(motm, na.rm = TRUE),
          .by = name
        ) |>
        left_join(
          player_position_stats() |> arrange(name, -count) |>
            group_by(name) %>%
            mutate(Positions = paste0(position, collapse = ", ")) |> select(name, Positions) |> distinct(),
          by = "name"
        ) |>
        select(name, Positions, Apps, Goals, Assists, MotM)
    } else {
      team_data |>
        filter(
          season == input$season,
          Competition %in% input$competition
        ) |>
        summarise(
          Apps = n(),
          Goals = sum(goals, na.rm = TRUE),
          Assists = sum(assists, na.rm = TRUE),
          MotM = sum(motm, na.rm = TRUE),
          .by = name
        ) |>
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

  output$seasons_table <- renderReactable({
    reactable(
      result_stats()
    )
  })

  output$player_summary_stats <- renderReactable({
    reactable(
      summary_stats() |>
        filter(name == input$player) |>
        sjmisc::rotate_df() |>
        rename(Info = V1)
    )
  })

  output$top_apps <- renderReactable({
    reactable(
      summary_stats() |>
        select(` ` = name, Apps) |>
        arrange(-Apps, ` `),
      defaultPageSize = 20  
    )
  })

  output$top_goals <- renderReactable({
    reactable(
      summary_stats() |>
        select(` ` = name, Goals) |>
        filter(Goals > 0) |>
        arrange(-Goals, ` `),
      defaultPageSize = 20    

    )
  })

  output$top_assists <- renderReactable({
    reactable(
      summary_stats() |>
        select(` ` = name, Assists) |>
        filter(Assists > 0) |>
        arrange(-Assists, ` `),
      defaultPageSize = 20    
    )
  })

  output$top_motm <- renderReactable({
    reactable(
      summary_stats() |>
        select(` ` = name, MotM) |>
        filter(` ` != "Own goal") |>
        arrange(-MotM, ` `),
      defaultPageSize = 20    )
  })


  output$most_played_team <- renderReactable({
    reactable(
      summary_stats() |>
        select(name, Positions, Apps, Goals, Assists, MotM) |>
        arrange(-Apps, name)
    )
  })

  output$pitch <- renderPlot(
    player_position_stats() |>
      dplyr::filter(name == input$player) |>
      plot_field()
  )

  match_results <- reactive({
    team_data_season() |>
      summarise(
        `Goals for` = sum(goals, na.rm = TRUE),
        `Goals against` = sum(conceded, na.rm = TRUE),
        .by = c(season, date, opposition, Competition, venue)
      ) |>
      mutate(
        result = case_when(
          `Goals for` > `Goals against` ~ "Win",
          `Goals for` < `Goals against` ~ "Loss",
          .default = "Draw"
        )
      )
  })

  output$player_matches <- renderReactable({
    print(team_data_season() |>
      filter(name == input$player))
    print(match_results())
    reactable(
      team_data_season() |>
        filter(name == input$player) |>
        left_join(match_results()) |>
        select(
          season, date, opposition, Competition, venue,
          result, `Goals for`, `Goals against`,
          Start, position, goals, assists, motm
        ),
      defaultPageSize = 25
    )
  })
}
