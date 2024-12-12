#function to find the minimum number in a vector, and return NA if all values are NA
min_safe<- function(vec){
  ifelse(length(vec[!is.na(vec)]) == 0, NA_real_, min(vec,na.rm=TRUE))
}
