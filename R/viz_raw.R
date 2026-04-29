

compute_balance <- function(data, scales){
  
  data %>% 
    dplyr::summarise(min_x = min(x, na.rm = T),
              xend = max(x, na.rm = T),
              y = 0,
              yend = 0) %>% 
    dplyr::rename(x = min_x)
  
}

#' @export
geom_support <- function(...){
  
  qlayer(geom = GeomSegment, 
         stat = qstat(compute_balance),
         ...)
  
}


compute_stacks <- function(data, scales){
  
  data |> 
    StatBin$compute_group(scales) |> 
    mutate(row = row_number())
  
}

#' @export
geom_stacks <- function(...){
  
  # qlayer(geom = GeomBar |> qproto_update(aes(color = from_theme("ink"))),
  #        stat = StatBin,
  #          # qstat(compute_stacks, aes(group = after_stat(row))),
  #        ...)
  
  stat_bin(geom = GeomBar |> 
             qproto_update(aes(color = from_theme(scales::col_mix(paper,ink)),
                               fill = from_theme(scales::col_mix(paper, ink, .2)))), ...
           )
  
}

