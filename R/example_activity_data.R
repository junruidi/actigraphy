#' @title Example Activity/Wear Data from Multiple People
#'
#' @description A list of two data.frames containing the counts and the weartime
#' for multiple participants, with a column for the ID and a column
#' for the visit.
#'
#' @format A list of two \code{data.frame}s with 1442 columns, which are in the following order:
#' \describe{
#' \item{ID}{identifier of the person}
#' \item{Day}{indicator of which day of activity it is, can be a numeric vector of sequence 1,2,... or a string of date}
#' \item{MIN1-MIN1440}{counts of activity}
#' }
"example_activity_data"