#' Calculate unadjusted and IPTW-adjusted Standardised Mean Difference (SMD) for each variable based on the grouping factor
#'
#' This function calculates SMD for both the unadjusted and the IPTW-adjusted model
#' returning both models variable SMD and the model design used for adjusted (weighted)
#'
#' @param data: the data.frame object for the analysis
#' @param db_iptw: the data.frame object for the analysis, output of normalize_IPTW()
#' @param covariates: list of variables to be compared
#' @param outcome: grouping variable
#' @return A list with unadjusted and adjusted comparison of variables based on outcome group and the design for weighted analysis (weighted)
balance_cov <- function( data, db_iptw, covariates, outcome, round_factor = 3, print = FALSE )
{
  ### calculates SMD for each variable unadjusted model
  Unadjusted <- tableone::CreateTableOne( data = data, vars = covariates, strata = outcome)
  ### specify corrected model design (use weights)
  iptwsvy <- survey::svydesign(ids = ~ 1, data = db_iptw, weights = ~ sw)
  ### calculates SMD for each variable adjusted model, based on weighted design
  iptw <- tableone::svyCreateTableOne(vars = covariates, strata = outcome, data = iptwsvy)
  if ( isTRUE(print) ) {
    ### stdout SMD differences reduction
    UT <- tableone::ExtractSmd(Unadjusted)
    AT <- tableone::ExtractSmd(iptw)
    UT_VEC <- vector()
    AT_VEC <- vector()
    cat('----------------------- IPTW normalization -----------------------\n')
    cat('- variables standardised mean difference before and after IPTW:\n')
    for (i in 1:length(covariates))
    {
      cat( '   - var:\t', covariates[i], '\t', round( UT[i], round_factor ), ' -->\t', round( AT[i], round_factor ), '\n', sep = '' )
    }
    cat('- ideally, after normalization all values should be <0.1\n')
    cat('------------------------------------------------------------------\n')
  }
  OUT_LIST <- list(
    'unadjusted' = Unadjusted,
    'adjusted' = iptw,
    'weighted' = iptwsvy
  )
  return(OUT_LIST)
}
