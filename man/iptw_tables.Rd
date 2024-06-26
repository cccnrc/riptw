% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iptw_tables.R
\name{iptw_tables}
\alias{iptw_tables}
\title{Returns two table objects that indicate each covariate stratification (and p-val) with the grouping variable in unadjusted and adjusted analysis}
\usage{
iptw_tables(data, weighted, covariates, outcome, print = FALSE)
}
\arguments{
\item{data:}{the data.frame object for the analysis}

\item{weighted:}{the survey::svydesign() design object which is used to incorporate weights in the adjusted analysis ("weighted" output part of balance_cov())}

\item{covariates:}{list of variables to be compared}

\item{outcome:}{grouping variable}

\item{print:}{resumen to stdout? (default: FALSE)}
}
\value{
A list with unadjusted and adjusted comparison of variables based on outcome group and the design for weighted analysis (weighted)
}
\description{
This function returns a list with unadjusted and adjusted tables of covariates stratification (and p-val)
with the grouping variable (passed as outcome). Adjusted is corrected for weights
specified in survey::svydesign() in balance_cov
}
