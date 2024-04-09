#' Get formula to be passed to the model based on list of specified covariates and outcome
#'
#' This function ease the creation of the formula to be passed to the model simply
#' passing covariates (as a vector) and outcome.
#' It can handle Restricted Cubic Spline transformation for a specific variable.
#'
#' @param outcome: the model outcome name (dependent variable)
#' @param covariates: vector of model covariate names
#' @param rcs_covariate: the variable to be used as RCS (optional)
#' @param rcs_df: if rcs_covariate is specified the degrees of freedom to use for RCS transformation (optional, default: 5)
#' @return A as.formula() object to be passed to regression models
#' @export
get_formula <- function( outcome, covariates, rcs_covariate = NULL, rcs_df = 5 )
{
  formula_outcome <- paste( outcome, '~', sep = ' ')
  if ( is.null( rcs_covariate ) ) {
    model_formula <- as.formula( paste( formula_outcome
                                , paste( covariates, collapse='+' ) ) )
  } else {
    formula_covariates <- covariates
    formula_rcs <- vector()
    for (i in 1:length(rcs_covariate))
    {
      if ( length(rcs_df) > 1 ) {
        if ( length(rcs_df) != length(rcs_covariate) ) {
          stop('rcs_df must be length 1 or same as rcs_covariate')
        }
      }
      ### remove covariate to be inserted as RCS from the list of all covariates
      rcs_index <- which( formula_covariates == rcs_covariate[i] )
      formula_covariates <- formula_covariates[-rcs_index]
      if ( length(rcs_df) > 1 ) {
        formula_rcs <- c( formula_rcs, paste( 'rms::rcs(', rcs_covariate[i],',', rcs_df[i], ')', sep = ' ' ))
      } else {
        formula_rcs <- c( formula_rcs, paste( 'rms::rcs(', rcs_covariate[i],',', rcs_df, ')', sep = ' ' ))
      }
    }
    ### prepare model formula
    model_formula <- as.formula( paste( formula_outcome
                                , paste(c(formula_covariates, formula_rcs),collapse='+') ))
  }
  return( model_formula )
}
