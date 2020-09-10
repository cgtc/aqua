#' Mixed effects model correlation structure investigation
#'
#' @param object A Mixed-Effects model as given by lmer (lme4 package)
#' @param num_timepoints Number of timepoints in the longitudinal dataset
#' @param intercept_only Boolean value indicating whether the model is random intercept-only (default = TRUE) or has a random slope too
#' @importFrom lme4 VarCorr
#' @importFrom Matrix cov2cor
#' @export
corr_structure <- function(object, num_timepoints, intercept_only = TRUE) {
  variance <- VarCorr(object)
  if(intercept_only) {
    random_matrix <- as.matrix(object@pp$X[1:num_timepoints, 1])
    var_cor <- random_matrix %*% variance[[1]][1] %*% t(random_matrix) +
      diag(attr(variance, "sc")^2, nrow = num_timepoints, ncol = num_timepoints)
  } else {
    random_matrix <- as.matrix(object@pp$X[1:num_timepoints, ])
    var_cor <- random_matrix %*% variance[[1]][1:2, 1:2] %*% t(random_matrix) +
      diag(attr(variance, "sc")^2, nrow = num_timepoints, ncol = num_timepoints)
  }
  cov2cor(var_cor)
}

#' Re-range some numbers to stretch them so that they fit a scale from a to b
#'
#' @param x A numerical vector
#' @param a New minimum for the scale (numeric)
#' @param b New maximum for the scale (numeric)
#' @param na.rm Whether to remove NAs or not (default = TRUE)
#' @return A stretched version of x that goes from a to b
#' @examples
#' rescale(1:10, 1, 100)
#' @export
rescale <- function(x, a = 0, b = 1, na.rm = T) ((b-a)*(x - min(x, na.rm = na.rm)) / ((max(x, na.rm = na.rm) - min(x, na.rm = na.rm)))) + a

#' Get pretty p-values (sometimes)
#'
#' @param x A vector of p-values
#' @param signif Significant digits (default = 4) to keep
#' @return x, if x > x^(-signif), otherwise "<1^(-signif)" in non-scientific notation
#' @examples
#' pvl(c(0.0000003, 0.06), 2) # returns "<0.01" "0.06"
#' @export
pvl <- function(x, signif = 4) ifelse(abs(x) < 10^(0 - signif),
                                      paste0("<0.", paste0(rep("0", signif - 1), collapse = ""), 1, collapse = ""),
                                      sub(x = format(round(x, signif), scientific = FALSE),
                                                 pattern = "0+$", replacement = ""))

#' Turn kable output into a pretty kable output
#'
#' @param x An html-styled table, as produced by kable(format = "html")
#' @param full Boolean value indicating whether the table should span the entire width of the screen (TRUE) or not (default, FALSE)
#' @return A pretty kable output, to use in markdown reports; requires kableExtra
#' @importFrom kableExtra kable_styling row_spec
#' @importFrom knitr kable
#' @examples
#' \dontrun{
#' mtcars %>% kable() %>% kable_style()
#' }
#' @export
kable_style <- function(x, full = F) x %>%
  kable_styling(position = "center", full_width = full, fixed_thead = T,
                bootstrap_options = c("striped", "hover", "compressed")) %>%
  row_spec(row = 0, align = "c", bold = T, hline_after = T,
           extra_css = "vertical-align: middle;")

bigcolour <- "#400000"
hicolour  <- "#A02020"
locolour  <- "#20A020"
