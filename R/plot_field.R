library(ggplot2)

position_coords <- readr::read_csv("data/positions.csv")

plot_field <- function(positions){
  positions <- positions |>
    dplyr::left_join(position_coords, by = "position")
  if(!("count" %in% names(positions))){
    positions <- positions |>
      dplyr::mutate(count = 1)
  }
  ggplot(positions, aes(x=x, y=y, label=position, size=count)) +
    geom_point(color = "#d8d8d8") +
    ggrepel::geom_text_repel() +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "#3f9b0b"),
      legend.position = "none",
      axis.text=element_blank(),
      axis.ticks=element_blank()
  ) +
    xlab("") +
    ylab("") +
    scale_y_continuous(
      breaks = seq(0, 1, by = 1),
      minor_breaks = seq(0, 1, by = 0.5),
      limits = c(0,1)
    ) +
    scale_x_continuous(
      breaks = seq(0, 1, by = 1),
      minor_breaks = seq(0, 1, by = 1),
      limits = c(0,1)
    ) + 
    scale_size_continuous(range = c(4,8))
}

