data_shuffle_var <- function(data, var){
  
  data |> 
    mutate(shuffled = sample({{var}}, replace = F))
  
}
