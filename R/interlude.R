#' @export
data_add_synth <- function(data, var, mean, num_trials = 1){
  
  observed <- data |> pull({{var}})
  
 
    tibble(.trial = 1:num_trials,
           data = rep(list(data), num_trials)) |>
      unnest(data) |> 
    mutate(synthetic = rnorm(
      nrow(data)*num_trials, mean = mean, sd = sd(observed)
      ))
  
}


