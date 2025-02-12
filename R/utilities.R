#' format_pids
#' A function that attempts to shorten the display of a vector of process id's by
#' splitting them into a prefix and unique trailers
#' 
#' @param x
#' A vector of integers, each between 0 and 99999
#' @details
#' Finds common leading integer and a vector of uniwue integers
#' 
#' 
#' @returns
#' A string, containing the integers in a compact way
#'  
format_pids = function(x) {
  ## splits the digits in integer vector x into a same and diff components, returning a two-component list
  ## with common part and different parts
  x = unlist(x)
  for(i in 1:5) {
    u = unique(x %/% 10^i)
    if(length(u) == 1) break
  }
  formatC(x, width = 8, format = "d", flag = "0")
  paste(paste0("(", u, ")"),
        paste(formatC(x - u*10^i, width = i, format = "d", flag = "0"), collapse = " "))
}
