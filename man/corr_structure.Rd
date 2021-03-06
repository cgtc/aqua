% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/general.R
\name{corr_structure}
\alias{corr_structure}
\title{Mixed effects model correlation structure investigation}
\usage{
corr_structure(object, num_timepoints, intercept_only = TRUE)
}
\arguments{
\item{object}{A Mixed-Effects model as given by lmer (lme4 package)}

\item{num_timepoints}{Number of timepoints in the longitudinal dataset}

\item{intercept_only}{Boolean value indicating whether the model is random intercept-only (default = TRUE) or has a random slope too}
}
\description{
Mixed effects model correlation structure investigation
}
