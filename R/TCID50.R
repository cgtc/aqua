#' TCID50 Computing Functions - logTCID50
#'
#' @param x A vector containing proportion of positive samples
#' @param d Log-Dilution factor difference, 1 for 1:10 dilutions
#' @param log_dilution the log-dilution of the assay
#' @export
get_logTCID <- function(x, d = 1, log_dilution) {
  a = 0
  for (i in 1:(length(x) - 1)) {
    a <- a + (x[i] + x[i + 1])
  }
  return(log_dilution - (0.5 * d * a))
}

#' TCID50 Computing Functions - logTCID50 Standard Error
#'
#' @param x A vector containing proportion of positive samples
#' @param d Log-Dilution factor difference, 1 for 1:10 dilutions
#' @param total Number of technical replicates (samples for a given dilution)
#' @export
get_logTCID_SE <- function(x, d = 1, total) {
  return(sqrt(sum(x * (1 - x) / total)) * d)
}

#' TCID50 Computing Functions - TCID50 Level
#'
#' @param x A vector containing proportion of positive samples
#' @param d Log-Dilution factor difference, 1 for 1:10 dilutions
#' @param log_dilution A vector of log10-dilutions (-1 for 1/10, -2 for 1/100, etc.)
#' @param vol_inoculum Volume Inoculum (in ml)
#' @export
get_TCID <- function(x, d = 1, log_dilution, vol_inoculum) {
  a <- get_logTCID(x, d, log_dilution)
  return(10^(-a)/vol_inoculum)
}

#' TCID50 Computing Functions - TCID50 Standard Error
#'
#' @param x A vector containing proportion of positive samples
#' @param d Log-Dilution factor difference, 1 for 1:10 dilutions
#' @param log_dilution A vector of log10-dilutions (-1 for 1/10, -2 for 1/100, etc.)
#' @param vol_inoculum Volume Inoculum (in ml)
#' @param total Number of technical replicates (samples for a given dilution)
#' @export
get_TCID_SE <- function(x, d = 1, log_dilution, vol_inoculum, total) {
  a <- get_TCID(x, d, log_dilution, vol_inoculum)
  b <- get_logTCID(x, d, log_dilution)
  c <- get_logTCID_SE(x, d, total)
  return(a * log(-b) * c)
}

#' TCID50 Computing Functions - TCID50 Coefficient of Variation
#'
#' @param x A vector containing proportion of positive samples
#' @param d Log-Dilution factor difference, 1 for 1:10 dilutions
#' @param log_dilution A vector of log10-dilutions (-1 for 1/10, -2 for 1/100, etc.)
#' @param vol_inoculum Volume Inoculum (in ml)
#' @param total Number of technical replicates (samples for a given dilution)
#' @export
get_TCID_CV <- function(x, d = 1, log_dilution, vol_inoculum, total) {
  a <- get_TCID_SE(x, d, log_dilution, vol_inoculum, total)
  b <- get_TCID(x, d, log_dilution, vol_inoculum)
  return(a/b)
}

#' TCID50 Computing Functions - TCID50 Confidence Interval
#'
#' @param x A vector containing proportion of positive samples
#' @param d Log-Dilution factor difference, 1 for 1:10 dilutions
#' @param log_dilution A vector of log10-dilutions (-1 for 1/10, -2 for 1/100, etc.)
#' @param vol_inoculum Volume Inoculum (in ml)
#' @param total Number of technical replicates (samples for a given dilution)
#' @param alpha Significance threshold (0.05 alpha corresponds to 95\% CI)
#' @importFrom stats qnorm
#' @export
get_TCID_CI <- function(x, d = 1, log_dilution, vol_inoculum, total, alpha = 0.05) {
  low <- get_logTCID(x, d, log_dilution) + (qnorm(1 - (alpha/2), 0, 1) * get_logTCID_SE(x, d, total))
  high <- get_logTCID(x, d, log_dilution) - (qnorm(1 - (alpha/2), 0, 1) * get_logTCID_SE(x, d, total))
  return(list(lower_bound = 10^-low/vol_inoculum,
              upper_bound = 10^-high/vol_inoculum))
}

#' TCID50 Computing Functions - P:I Ratio
#'
#' @param x A vector containing proportion of positive samples
#' @param d Log-Dilution factor difference, 1 for 1:10 dilutions
#' @param log_dilution A vector of log10-dilutions (-1 for 1/10, -2 for 1/100, etc.)
#' @param vol_inoculum Volume Inoculum (in ml)
#' @param vec_concentration Vector concentration (GC/ml)
#' @export
get_PI_ratio <- function(x, d = 1, log_dilution, vol_inoculum, vec_concentration) {
  a <- get_TCID(x, d, log_dilution, vol_inoculum)
  return(vec_concentration/a)
}

#' TCID50 Computing Functions - P:I Ratio Confidence Intervals
#'
#' @param x A vector containing proportion of positive samples
#' @param d Log-Dilution factor difference, 1 for 1:10 dilutions
#' @param log_dilution A vector of log10-dilutions (-1 for 1/10, -2 for 1/100, etc.)
#' @param vol_inoculum Volume Inoculum (in ml)
#' @param vec_concentration Vector Concentration (GC/ml)
#' @param total Number of technical replicates (samples for a given dilution)
#' @param alpha Significance threshold (0.05 alpha corresponds to 95\% CI)
#' @export
get_TCID_CI_PI <- function(x, d = 1, log_dilution, vol_inoculum, vec_concentration, total, alpha = 0.05) {
  CI <- get_TCID_CI(x, d, log_dilution, vol_inoculum, total, alpha)
  return(list(lower_bound = vec_concentration / CI[["lower_bound"]],
              upper_bound = vec_concentration / CI[["upper_bound"]]))
}
