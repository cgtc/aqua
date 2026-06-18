#' Table of accuracy
#'
#' @param data Data to calculate accuracy for
#' @param ref_value Reference value to compare against. If not provided, relative accuracy compared to group 1 average is used.
#' @param limits Limits to compare against as an acceptance criteria
#'
#' @returns A table of accuracy
#' @export
#'
#' @examples
#' table_accuracy(rnorm(10, 0, 4))
#' table_accuracy(rnorm(10, 0, 4), 0)
table_accuracy <- function(data, ref_value = NULL, limits = c(0.8, 1.25)) {

}


#' Comparison between methods
#'
#' @param x Method 1 data
#' @param y Method 2 data
#'
#' @returns A table of comparisons between the two methods.
#' @export
#'
#' @examples
#' table_comparison(rnorm(10, 0, 4), rnorm(10, 0, 4))
table_comparison <- function(x, y) {

}


#' Table of repeatability
#'
#' @param data Data to calculate repeatability from
#' @param groups which groups to include in the repeatability assessment
#' @param limit The limit to compare against
#'
#' @returns A table of repeatability
#' @export
table_repeatability <- function(data, groups = NULL, limit = .25) {


}


#' Table of intermediate precision
#'
#' @param data Data to calculate intermediate precision from
#' @param groups which groups to include in the intermediate precision assessment
#' @param limit The limit to compare against
#'
#' @returns A table of intermediate precision
#' @export
table_intermediate_precision <- function(data, groups = NULL, limit = .3) {


}


#' Table for specificity recovery
#'
#' @param data Data to calculate specificity recovery from
#' @param ref_value Reference value to compare against. If not provided, relative accuracy compared to group 1 average is used.
#' @param limits Limits to compare against as an acceptance criteria
#'
#' @returns A table of specificity recovery
#' @export
table_specificity_recovery <- function(data, ref_value = NULL, limits = c(0.8, 1.25)) {


}


#' Table for the Linearity Assessment
#'
#' @param data Data to calculate linearity from.
#' @param averaged Whether to average the data or not
#' @param .log.y Whether to log-transform the y variable
#' @param .log.x Whether to log-transform the x variable
#'
#' @returns A table of linearity
#' @export
table_linearity <- function(data, averaged = TRUE, .log.y = TRUE, .log.x = FALSE) {


}


#' Table of Limits
#'
#' @param data Data to calculate baseline from
#' @param variance_data Variance data to use. If NULL, the data is used instead
#' @param limit The named list of limits to calculate (e.g. 3.3 for LOD, 10 for LOQ)
#'
#' @returns A table of limits
#' @export
#'
#' @examples
#' table_limits(rnorm(10, 0, 4))
#' table_limits(rnorm(10, 0, 4), rnorm(10, 0, 4))
#' table_limits(rnorm(10, 0, 4), rnorm(10, 0, 4), list("LoD" = 3.3, "LoQ" = 10))
table_limits <- function(data, variance_data = NULL, limit = list("LoD" = 3.3, "LoQ" = 10)) {


}

