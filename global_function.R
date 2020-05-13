between_fun <- function(first_value, second_value, multiplier){
  value <- (second_value - first_value) /multiplier
  result <- c()
  for (i in 1:multiplier-1) {
    between_value <- first_value + ( value * i)
    result[i] <- between_value
  }
  return(result)
}
seq_fun <- function(start_value, end_value, multiplier){
  rst <- c()
  val <- start_value
  while (val <= end_value) {
    #print(val)
    rst <- append(rst,val)
    next_val <- val * 2
    in_between <- between_fun(val , next_val, multiplier)
    in_between <- in_between[in_between < end_value]
    rst <- append(rst,in_between)
    val <- next_val
  }
  return(rst)
}
