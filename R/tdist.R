# 7. normal distribution based on null and n
compute_dist_t <- function (data, scales, value = 3.5, height = NULL, tails = c("none", "two-sided", "both","lower", "upper") )
{
  
  tails <- tails[1]
  
  mean_x <- mean(data$x)
  diff <- mean_x - value
  mirrored <- value - diff
  upper <- max(mean_x, mirrored)
  lower <- min(mean_x, mirrored)
  
  height = height %||% 3*nrow(data)/30
  
  out <- seq(-5, 5, 0.01) %>% 
    tibble(x = .) %>% 
    mutate(y_density = dt(x, df = length(data$x) - 
        1)) %>%
    mutate(y = height*y_density/max(y_density)) %>% 
    mutate(x = x * (sd(data$x)/sqrt(length(data$x))) + 
        value) |> 
    mutate(upper_t = x > mean(data$x),
           lower_t = x < mean(data$x),
           two_tail = (x > upper) | (x < lower))
  
  out$tails_logical <- F
  out$tails_logical <- if(tails == "none"){FALSE}else{out$tails_logical}
  out$tails_logical <- if(tails == "upper"){out$upper_t}else{out$tails_logical}
  out$tails_logical <- if(tails == "lower"){out$upper_t}else{out$tails_logical}
  out$tails_logical <- if(tails == "two-sided"| tails == "both"){out$two_tail}else{out$tails_logical}

  out
  
}



#' @export
scale_fill_logical <- function(...){
  scale_fill_manual(values = c(scales::col_mix(theme_get()$geom@ink, 
                                               theme_get()$geom@paper, .6),
                               scales::col_mix(theme_get()$geom@accent, 
                                               theme_get()$geom@paper, .2)), 
                    breaks = c(F,T),
                    guide = "none")
                    }



#' @export
geom_tdist_null <- function(value, ..., tails = NULL){
  
  # aes_upper <- aes(fill = after_stat(upper_tail))
  # aes_lower <- aes(fill = after_stat(lower_tail))
  # aes_none <- StatIdentity$default_aes
  # aes_both <- aes(fill = after_stat(two_tail))
  # 
  # # if(is.null(tails)d_aes = aes_none
  # if(tails == "two-sided"){d_aes = aes_both}
  # if(tails == "upper"){d_aes = aes_upper}
  # if(tails == "lower"){d_aes = aes_lower}
  
  list(
  qlayer(geom = qproto_update(ggplot2::GeomArea, 
                              ggplot2::aes(alpha = .66)),
         stat = qstat(compute_dist_t,
                      default_aes = aes(fill = after_stat(tails_logical))), 
         value = value, 
         tails = tails,
         ...),
        scale_fill_logical()
                    
  
  )
  
  }

# # 8. normal distribution mean and sds based on null and n
# compute_dnorm_prop_sds <- function(data, scales, null, dist_sds = -4:4){
#   
#   n <- data |> dplyr::count(.by = x) |> dplyr::pull(n) |> sum()
#   
#   sd = sqrt(null * (1 - null)/n) # sd of the null distribution
#   
#   q <- dist_sds * sd + null
#   
#   data.frame(x = q) %>%
#     dplyr::mutate(height = dnorm(q, sd = sd, mean = null)) %>%
#     dplyr::mutate(height_max = dnorm(0, sd = sd, mean = 0)) %>%
#     dplyr::mutate(y = .55*n*height/height_max) %>% # This is a bit fragile...
#     dplyr::mutate(xend = x,
#            yend = 0)
# 
# }  

# 
# #' @export
# geom_normal_prop_null_sds <- function(...){
#    qlayer(geom = qproto_update(ggplot2::GeomSegment, ggplot2::aes(linetype = "dotted")),
#           stat = qstat_panel(compute_dnorm_prop_sds), 
#           ...)
#   }


