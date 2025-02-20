library(bslib)
library(shiny)
library(dplyr)
library(sjmisc)
library(reactable)
library(gitcreds)

team_data <- read_team_data()

player_match_positions <- team_data |> 
  tidyr::separate_longer_delim(position, delim = ",") |>
  dplyr::summarise(count_positions = dplyr::n(), .by = c(date, name))

player_position_stats <- team_data |> 
  tidyr::separate_longer_delim(position, delim = ",") |>
  dplyr::left_join(player_match_positions, by = dplyr::join_by(date, name)) |>
  dplyr::summarise(count = sum(1./count_positions), .by = c(name, position)) |>
  dplyr::arrange(name, count)


summary_stats <- team_data |> 
  summarise(
    Apps = n(),
    Goals = sum(goals, na.rm = TRUE), 
    Assists = sum(assists, na.rm = TRUE), 
    MotM = sum(motm, na.rm = TRUE), 
    .by=name) |>
  left_join(
    player_position_stats |> arrange(name, -count) |>
      group_by(name) %>% 
      mutate(Positions = paste0(position, collapse = ", ")) |> select(name, Positions) |> distinct(),
    by = "name"
  ) |>
  select(name, Positions, Apps, Goals, Assists, MotM)

