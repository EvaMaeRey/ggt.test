compute_layer_diff_mean_segment <- function(data, ...){
  
  data |> 
    summarise(mean = mean(x),
              .by = PANEL) |> 
    select(mean, PANEL) |>
    pivot_wider(values_from = mean, names_from = PANEL, names_prefix = "V") |>
    rename(x = V1, xend = V2) |> 
    mutate(y = 0, yend = 0) |> 
    crossing(data.frame(PANEL = 1:2))
  
}


compute_layer_diff_mean_segment_label <- function(data, ...){
  
  data |> 
    summarise(mean = mean(x),
              .by = PANEL) |> 
    select(mean, PANEL) |>
    pivot_wider(values_from = mean, names_from = PANEL, names_prefix = "V") |>
    rename(x = V1, xend = V2) |> 
    mutate(y = 0, yend = 0) |> 
    crossing(data.frame(PANEL = 1:2)) |> 
    mutate(difference = c((x - xend)) |> round(2)) |>
    mutate(label = paste0("Difference: \n", difference)) |> 
    mutate(x = I(c(.2, -5)), y = I(.8))
  
}

geom_mean_diff <- function(...){

  qlayer(stat = compute_layer_diff_mean_segment |> qstat_layer(), 
         geom = GeomSegment |> 
           qproto_update(aes(color = from_theme(accent),
                   linewidth = from_theme(linewidth*3))), 
         ...)
  
}

geom_mean_diff_label <- function(...){
  
    qlayer(
      geom = GeomLabel |> qproto_update(aes(color = from_theme(colour %||% accent),
                                            fill  = from_theme(colour %||% paper))),
      stat = compute_layer_diff_mean_segment_label |> qstat_layer(),
      ...
      ) 
  
}
