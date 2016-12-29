#' Concatenate
#'
#' This function concatenates two strings with
#' a separating space
#' @param s1 First string, no trailing spaces.
#' @param s2 Second string. no leading spaces.
#' @keywords Helper, string
#' @export
#' @return Concatenated strings with a separating space
#' @examples 
#' > s1 <- "a"
#' > s2 <- "b"
#' > concatenate(s1,s2)
#' [1] "a b"
concatenate <- function (s1,s2){
  return(paste(s1,s2,sep=" "))
}


#' Concatenate Operator
#'
#' This function wraps concatentate
#' @param s1 First string, no trailing spaces.
#' @param s2 Second string. no leading spaces.
#' @keywords Helper, string
#' @export
#' @return Concatenated strings with a separating space
#' @examples 
#' > s1 <- "a"
#' > s2 <- "b"
#' > s1 %+% s2
#' [1] "a b"
`%++%` <- function (s1,s2)concatenate(s1,s2)