#' Returns two table objects that indicate each covariate stratification (and p-val) with the grouping variable in unadjusted and adjusted analysis
#'
#' This function returns a list with unadjusted and adjusted tables of covariates stratification (and p-val)
#' with the grouping variable (passed as outcome). Adjusted is corrected for weights
#' specified in survey::svydesign() in balance_cov
#'
#'
#' @param data: the data.frame object for the analysis
#' @param weighted: the survey::svydesign() design object which is used to incorporate weights in the adjusted analysis ("weighted" output part of balance_cov())
#' @param covariates: list of variables to be compared
#' @param outcome: grouping variable
#' @param print: resumen to stdout? (default: FALSE)
#' @return A list with unadjusted and adjusted comparison of variables based on outcome group and the design for weighted analysis (weighted)
#' @export
iptw_tables <- function( data, weighted, covariates, outcome, print = FALSE )
{
  unadjusted <- tableone::kableone(tableone::CreateTableOne( data = data,vars = covariates, strata = outcome))
  adjusted <- tableone::kableone(tableone::svyCreateTableOne( data = weighted, vars = covariates, strata = outcome ))
  if ( isTRUE(print) ) {
    cat('\n')
    cat('----------------------- IPTW normalization -----------------------\n')
    cat('\n')
    cat('  --> covariates stratification with ', outcome, ' before IPTW: \n', sep = '' )
    print( unadjusted )
    cat('\n')
    cat('  --> covariates stratification with ', outcome, ' after IPTW: \n', sep = '' )
    print( adjusted )
    cat('----------------------------------------------------------------\n')
    cat('\n')
  }
  OUT_LIST <- list( 'unadjusted' = unadjusted, 'adjusted' = adjusted )
}
