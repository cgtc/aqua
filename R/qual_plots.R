#' Create accuracy plot
#'
#' @param data Data to plot
#' @param ref_value Reference value to compare against
#' @param limits Limits to plot
#' @param .log Whether to log-transform the y variable (default = TRUE)
#'
#' @returns ggplot2 object with accuracy evaluation
#' @export
plot_accuracy <- function(data, ref_value = NULL, limits = c(0.8, 1.25), .log = TRUE) {

}


#' Create a comparison Bland-Altman plot
#'
#' @param x Method 1 data
#' @param y Method 2 data
#' @param .log Whether to log-transform the y variable and present the result as percent difference (default = TRUE)
#'
#' @returns ggplot2 object with Bland-Altman plot
#' @export
plot_comparison <- function(x, y, .log = TRUE) {


}


#' Create a repeatability plot
#'
#' @param data Data to plot
#' @param type Type of repeatability to plot, "A" = repeatability heatmap, "B" = traditional plot.
#' @param limit The acceptance criteria limit
#'
#' @returns ggplot2 object with repeatability evaluation
#' @export
plot_repeatability <- function(data, type = "A", limit = .25) {


}


#' Create an intermediate precision plot
#'
#' @param data Data to plot
#' @param type Type of intermediate precision to plot, "A" = intermediate precision heatmap, "B" = traditional plot.
#' @param limit The acceptance criteria limit
#'
#' @returns ggplot2 object with intermediate precision evaluation
#' @export
plot_intermediate_precision <- function(data, type = "A", limit = .3) {


}


#' Create a specificity recovery plot
#'
#' @param data Data to plot
#' @param ref_value Reference value to compare against
#' @param limits The acceptance criteria limits
#' @param .log.y Whether to log-transform the y variable (default = TRUE)
#'
#' @returns ggplot2 object with specificity recovery evaluation
#' @export
plot_specificity_recovery <- function(data, ref_value = NULL, limits = c(0.8, 1.25), .log.y = TRUE) {


}


#' Create a linearity plot
#'
#' @param data Data to plot
#' @param averaged Whether to fit the model for the ungrouped data or not
#' @param .x.offset Offset for the linear model on the x-axis (default = 0)
#' @param .log.y Whether to log-transform the y variable (default = TRUE)
#' @param .log.x Whether to log-transform the x variable (default = FALSE)
#'
#' @returns ggplot2 object with linearity evaluation
#' @export
plot_linearity <- function(data, averaged = TRUE, .x.offset = 0, .log.y = TRUE, .log.x = FALSE) {


}


#' Create a LoD/LoQ plot
#'
#' @param data Data to plot
#' @param limit Set the limit of the plot
#' @param .setcolour Set the colour of the plot limit lines
#' @param .log.y Whether to log-transform the y variable (default = TRUE)
#'
#' @returns ggplot2 object with LoD/LoQ evaluation
#' @export
plot_limit <- function(data, limit = NULL, .setcolour = NULL, .log.y = TRUE) {


}

