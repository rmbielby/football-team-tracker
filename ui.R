ui <- page_navbar(
  title = "BCFC Blues",
  sidebar = sidebar(
    selectInput(
      "season",
      label = "Season",
      choices = c("All time",team_data |> dplyr::pull("season") |> unique() |> sort()),
      selected = max(team_data |> dplyr::pull("season"))
    )
  ),
  nav_panel(
    "Team stats",
    bslib::layout_column_wrap(
      reactableOutput("top_apps"),
      reactableOutput("top_goals"),
      reactableOutput("top_assists"),
      reactableOutput("top_motm"),
    )
  ),
  nav_panel(
    "Player stats",
  bslib::layout_column_wrap(
    selectInput(
    "player",
    label = "Player",
    choices = team_data |> dplyr::pull("name") |> unique() |>sort()
  )
  ),
  bslib::layout_column_wrap(
    reactable::reactableOutput("player_summary_stats"),
    plotOutput("pitch")
  )
)
)
