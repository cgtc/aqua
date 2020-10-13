#' Get assay repeatability CV measure
#'
#' Returns the repeatability based on the residual variance in the model.
#' Works for any lme4 object, but make sure it's only got a fixed intercepts and no other fixed terms.
#' Random ones can be anything as long as they describe the model hierarchy properly.
#'
#' @param model A random-effects model
#'
#' @return The repeatability (as percent RSD)
#' @export
#' @importFrom lme4 fixef VarCorr
#' @importFrom dplyr summarize mutate select
#' @importFrom tidyr replace_na
get_repeat <- function(model) {
  grandMean <- fixef(model)[[1]]
  varianceFactors <- as.data.frame(VarCorr(model))
  varianceFactors %>%
    mutate(var1 = replace_na(var1, "Residual")) %>%
    filter(var1 == "Residual") %>%
    summarize(DEV = sdcor) %>%
    mutate(CV = (DEV/grandMean * 100)) %>%
    mutate(var1 = c("Repeatability")) %>%
    select(var1, CV) %>%
    set_names(c("Imprecision<br>Level", "CV %")) -> ret
  return(ret)
}

#' Get assay intermediate precision factors CV measure
#'
#' Returns the added relative standard deviation based on the variances of the random intercepts in the model.
#' Works for any lme4 object, but make sure it's only got a fixed intercepts and no other fixed terms.
#' Random ones can be anything, as long as they describe the model hierarchy properly.
#'
#' @param model A random-effects model
#'
#' @return The components for the intermediate precision (as percent RSD)
#' @export
#' @importFrom lme4 fixef VarCorr
#' @importFrom dplyr summarize mutate select filter
get_intermed_levels <- function(model) {
  grandMean <- fixef(model)[[1]]
  varianceFactors <- as.data.frame(VarCorr(model))
  varianceFactors %>%
    filter(grp != "Residual") %>%
    mutate(CV = sdcor/c(rep(grandMean, nrow(varianceFactors) - 1)) * 100) %>%
    select(-var1, -vcov) %>%
    select(grp, CV) %>%
    set_names(c("Error term", "CV %")) -> ret
  return(ret)
}

#' Get assay intermediate precision CV measure
#'
#' Returns the intermediate precision based on the sum of all the variances of the random intercepts.
#' Works for any lme4 object, but make sure it's only got a fixed intercepts and no other fixed terms.
#' Random ones can be anything, as long as they describe the model hierarchy properly.
#'
#' @param model A random-effects model
#' @return The intermediate precision (as percent RSD)
#' @export
#' @importFrom lme4 fixef VarCorr
#' @importFrom dplyr summarize mutate select
get_intermed <- function(model) {
  grandMean <- fixef(model)[[1]]
  varianceFactors <- as.data.frame(VarCorr(model))
  varianceFactors %>%
    summarize(DEV = sqrt(sum(vcov))) %>%
    mutate(CV = DEV/grandMean * 100) %>%
    mutate(var1 = c("Intermediate Precision")) %>%
    select(var1, CV) %>%
    set_names(c("Imprecision<br>Level", "CV %")) -> ret
  return(ret)
}

#' Unbiased Standard Deviation
#'
#' Unbiased Standard Deviation. Very helpful when sample size is small, especially less than 6,
#' which is very common in assay qualification or validation runs. Regular sample standard deviation
#' can be up to 20\% biased when the sample size is 3, for example. This one isn't.
#'
#' @param x A vector of (normally-distributed) numbers
#' @return The corrected/unbiased standard deviation of the input vector, based on a chi-distributed correction factor (c4)
#' @export
#' @importFrom stats sd
#' @examples
#' sd.u(rnorm(10, 0, 4))
sd.u <- function(x) {
  c4 <- function(n) {
    if (n <= 1) {
      return(NA)
    } else if (n <= 343) {  # gamma(172) overflows the max floating point number,
                            # also don't need to correct for bias when n is that high anyway.
      sqrt(2/(n - 1)) * gamma(n/2) / gamma((n - 1)/2)
    } else {
      return(1)
    }
  }
  if (length(x) <= 1) return(NA)
  return(sd(x, na.rm = T) / c4(length(x) - sum(is.na(x))))
}

#' Coefficient of Variation for Log-transformed Data
#'
#' A less known fact is that for log-transformed data you can't just take 100% * σ / μ and call it a day.
#' This function implements the correct way of getting a CV when you apply it on log-transformed data.
#' It does a method of moments based estimation of it, to check full maths and everything, read the paper published by Nelson W. in Applied Life Data Analysis. USA: John Wiley & Sons Inc; 2003.
#'
#' @param x A vector of log-normally distributed data
#' @param na.rm Whether or not to ignore NAs
#' @export
#' @importFrom stats var
#' @examples
#' logCV(c(0.8, 0.9, 0.78))
logCV <- function(x, na.rm = F) {
  return(100 * sqrt(exp(log(10)^2 * var(x, na.rm = na.rm)) - 1))
}
