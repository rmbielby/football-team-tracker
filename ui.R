ui <- page_navbar(
  title = "BCFC Blues",
  sidebar = sidebar(
    selectInput(
      "season",
      label = "Season",
      choices = c("All time", team_data |> dplyr::pull("season") |> unique() |> sort()),
      selected = max(team_data |> dplyr::pull("season"))
    ),
    checkboxGroupInput(
      "competition",
      "Competition",
      c(team_data |> dplyr::pull("Competition") |> unique() |> sort())
    )
  ),
  nav_panel(
    "Team stats",
    bslib::layout_column_wrap(
      reactableOutput("top_apps"),
      reactableOutput("top_motm"),
      reactableOutput("top_goals"),
      reactableOutput("top_assists"),
      min_height = "860px"
    ),
    bslib::layout_column_wrap(
      reactableOutput("seasons_table")
    ),
    bslib::layout_column_wrap(
      reactableOutput("most_played_team")
    )
  ),
  nav_panel(
    "Player stats",
    bslib::layout_column_wrap(
      selectInput(
        "player",
        label = "Player",
        choices = team_data |> dplyr::pull("name") |> unique() |> sort()
      )
    ),
    bslib::layout_columns(
      reactable::reactableOutput("player_summary_stats"),
      reactable::reactableOutput("player_matches"),
      plotOutput("pitch"),
      col_widths = c(12, 6, 6)
    )
  )
)
