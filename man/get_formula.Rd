% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_formula.R
\name{get_formula}
\alias{get_formula}
\title{Get formula to be passed to the model based on list of specified covariates and outcome}
\usage{
get_formula(
  outcome,
  covariates,
  rcs_covariate = NULL,
  rcs_df = 5,
  time_var = NULL,
  time_var2 = NULL,
  weights = NULL,
  bayesian = FALSE,
  string = FALSE
)
}
\arguments{
\item{outcome:}{the model outcome name (dependent variable). If survival analysis ("time_var" specified) this is the event column}

\item{covariates:}{vector of model covariate names}

\item{rcs_covariate:}{the variable to be used as RCS (optional)}

\item{rcs_df:}{if rcs_covariate is specified the degrees of freedom to use for RCS transformation (optional, default: 5)}
}
\value{
A as.formula() object to be passed to regression models
}
\description{
This function ease the creation of the formula to be passed to the model simply
passing covariates (as a vector) and outcome.
It can handle Restricted Cubic Spline transformation for a specific variable.
}
