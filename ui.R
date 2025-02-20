ui <- page_sidebar(
  title = "Player stats",
  sidebar = "Sidebar",
  "Main content area",
  bslib::layout_column_wrap(
    selectInput(
    "player",
    label = "Player",
    choices = team_data |> dplyr::pull("name") |> unique() |>sort()
  ),
  selectInput(
    "season",
    label = "Season",
    choices = c("All time",team_data |> dplyr::pull("season") |> unique() |> sort()),
    selected = max(team_data |> dplyr::pull("season"))
  )
  ),
  bslib::layout_column_wrap(
    reactable::reactableOutput("player_summary_stats"),
    plotOutput("pitch")
  )
)
