# Functions for confidence intervals
lwr_conf <- function(vectors){
  meanci <- MeanCI(vectors, na.rm = T)
  return(nth(meanci, 2))
}

upr_conf <- function(vectors){
  meanci <- MeanCI(vectors, na.rm = T)
  return(nth(meanci, 3))
}