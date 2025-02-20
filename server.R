function(input, output) {
  
  output$player_summary_stats <- renderReactable({
      reactable(
        summary_stats |> 
          filter(name == input$player)  |>
          sjmisc::rotate_df() |>
          rename(Info = V1)
      ) 
  }
  )
  
    output$pitch <- renderPlot(
      player_position_stats |>
        dplyr::filter(name == input$player) |>
        plot_field()
      
    )
  }
