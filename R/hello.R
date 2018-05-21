# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

hello <- function() {
  print("Hello, world!")
}


#' Get Nth Element
#'
#' Returns the nth element of a string split on a character
#' @param string the string to split
#' @param n which element of the split string to return
#' @param delim character to split on, default is underscore
#' @keywords strings
#' @export
#' @examples
#' getNthElement()

getNthElement <- function(string, n, delim='_') {
  return(strsplit(string, delim)[[1]][n])
}
