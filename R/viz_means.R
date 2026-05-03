
compute_xmean_at_y0 <- function(data, scales){
  
  data %>% 
    dplyr::summarise(x = mean(x),
              y = 0, 
              label = "^",
              xend = mean(x),
              yend = Inf) 
  
}

#' @export
geom_mean <- function(...){
  list(
    # balancing point
    qlayer(geom = GeomText |> 
             qproto_update(default_aes_update = 
                             aes(size = from_theme(pointsize*4),
                                 vjust = 1,
                                 color = from_theme(colour %||% accent))),
         stat = qstat_panel(compute_xmean_at_y0),
         ...),
    # vline
    qlayer(geom = GeomSegment |> 
             qproto_update(default_aes_update = 
                             aes(linetype = "dashed",
                                 linewidth = from_theme(linewidth),
                                 vjust = 1,
                                 color = from_theme(colour %||% accent))), 
           stat = qstat_panel(compute_xmean_at_y0),
           )
  )
  }



# 5. layer add balancing point value label
compute_xmean_at_y0_label <- function(data, scales){
  
  data %>% 
    dplyr::summarise(x = mean(x),
              y = 0, 
              label = after_stat(round(x, 2))) 
  
}

#' @export
geom_mean_label <- function(...){ 
  qlayer(geom = qproto_update(ggplot2::GeomLabel, 
                              ggplot2::aes(fill = ggplot2::from_theme(colour %||% paper), label.size = NA, vjust = 0, color = ggplot2::from_theme(colour %||% accent)) ),
         stat = qstat_panel(compute_xmean_at_y0_label), 
         ...) 
  }


# 6. Add 'point' for asserted balancing point (null)
compute_panel_mean_asserted <- function(data, scales, value = 0){
  
  # stamp type layer - so ignore input data
  data.frame(y = 0, 
             x = value,
             label = "^",
              xend = value,
              yend = Inf
             )
  
}

#' @export
stamp_mean <- function(value, ...){ 
  
  list(
  qlayer(geom = qproto_update(ggplot2::GeomText, 
                              ggplot2::aes(size = from_theme(pointsize*4),
                                           vjust = 1)),
         stat = qstat_panel(compute_panel_mean_asserted), inherit.aes = F, value = value,
         ...),
    ## vline
    qlayer(geom = GeomSegment |>
             qproto_update(default_aes_update =
                             aes(linetype = "dashed",
                                 vjust = 1)),
           stat = qstat_panel(compute_panel_mean_asserted), inherit.aes = F, value = value, ...
           )
  )
  
  }


# 6. Add label for asserted balancing point (null)
compute_panel_mean_asserted_label <- function(data, scales, value = 0){
  
  # stamp type layer - so ignore input data
  data.frame(y = 0, 
             x = value,
             label = round(value, 2)
             )
  
}

  
#' @export  
stamp_mean_label <- function(value, ...){  
  qlayer(geom = qproto_update(ggplot2::GeomLabel, 
                              ggplot2::aes(fill = ggplot2::from_theme(colour %||% paper), 
                                  label.size = NA, vjust = 0
                                  )),
         stat = qstat_panel(compute_panel_mean_asserted_label), 
         inherit.aes = F, value = value,
         ...)
  }


