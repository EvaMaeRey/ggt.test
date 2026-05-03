facet_align <- function(var, ...){

  facet_wrap(facets = vars({{var}}), ncol = 1,  ...)
  
}
