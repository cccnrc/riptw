#' Get formula to be passed to the model based on list of specified covariates and outcome
#'
#' This function ease the creation of the formula to be passed to the model simply
#' passing covariates (as a vector) and outcome.
#' It can handle Restricted Cubic Spline transformation for a specific variable.
#'
#' @param outcome: the model outcome name (dependent variable). If survival analysis ("time_var" specified) this is the event column
#' @param covariates: vector of model covariate names
#' @param rcs_covariate: the variable to be used as RCS (optional)
#' @param rcs_df: if rcs_covariate is specified the degrees of freedom to use for RCS transformation (optional, default: 5)
#' @return A as.formula() object to be passed to regression models
#' @export
get_formula <- function(
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
{
  ### check wether a time variable was passed
  if ( ! base::is.null(time_var) ) {
    ### check wether a single time or an interval is passed
    if ( base::is.null(time_var2) ) {
      formula_surv <- base::paste( 'survival::Surv( time = ', time_var, ', event = ', outcome, ') ~', sep = '' )
    } else {
      formula_surv <- base::paste( 'survival::Surv( time = ', time_var, ', time2 = ', time_var2, ', event = ', outcome, ') ~', sep = '' )
    }
    ### check if user asked for a brmsformula cens() time
    if ( base::isTRUE(bayesian) ) {
      if ( ! base::is.null(weights) ) {
        if ( base::is.null(time_var2) ) {
          formula_surv <- base::paste( time_var, '|', 'cens( ', outcome, ' ) + weights( ', weights, ' ) ~ ', sep = ' ' )
        } else {
          formula_surv <- base::paste( time_var, '|', 'cens( ', outcome, ', ', time_var2, ' ) + weights( ', weights, ' ) ~ ', sep = ' ' )
        }
      } else {
        if ( base::is.null(time_var2) ) {
          formula_surv <- base::paste( time_var, '|', 'cens( ', outcome, ' ) ~ ', sep = ' ' )
        } else {
          formula_surv <- base::paste( time_var, '|', 'cens( ', outcome, ', ', time_var2, ' ) ~ ', sep = ' ' )
        }
      }
    } else {
      if ( ! base::is.null(weights) ) {
        base::stop( ' weights can be incorporated in bayesian survival analysis (brmsformula()) but not directly in survival::Surv() analysis. Use survey::svydesign()' )
      }
    }
    ### replace formula_outcome with formula_surv
    formula_outcome <- formula_surv
  ### if user did not pass time_var (thus not asking for survival)
  } else {
    if ( base::is.null(weights) ) {
      formula_outcome <- base::paste( outcome, '~', sep = ' ')
    ### if user asked for weighted analysis
    } else {
      ### if this is to be passed to brms::brm()
      if ( base::isTRUE(bayesian) ) {
        formula_outcome <- base::paste( outcome, ' | weights( ', weights, ' ) ~ ', sep = ' ' )
      ### if this is for a non-brms::brm() weighted model
      } else {
        ### TODO: integrate svydesign into regression models
        formula_outcome <- base::paste( outcome, '~', sep = ' ')
        base::warning( ' \n weighted regression can be specified in formula only for Bayesian brms::brm() models \n the output formula will be a simple regression formula \n' )
      }
    }
  }
  if ( base::is.null( rcs_covariate ) ) {
    model_formula <- stats::as.formula( base::paste( formula_outcome
                                , base::paste( covariates, collapse='+' ) ) )
  } else {
    formula_covariates <- covariates
    formula_rcs <- base::vector()
    for (i in 1:base::length(rcs_covariate))
    {
      if ( base::length(rcs_df) > 1 ) {
        if ( base::length(rcs_df) != base::length(rcs_covariate) ) {
          base::stop('rcs_df must be length 1 or same as rcs_covariate')
        }
      }
      ### remove covariate to be inserted as RCS from the list of all covariates
      rcs_index <- base::which( formula_covariates == rcs_covariate[i] )
      formula_covariates <- formula_covariates[-rcs_index]
      if ( base::length(rcs_df) > 1 ) {
        formula_rcs <- c( formula_rcs, base::paste( 'rms::rcs(', rcs_covariate[i],',', rcs_df[i], ')', sep = ' ' ))
      } else {
        formula_rcs <- c( formula_rcs, base::paste( 'rms::rcs(', rcs_covariate[i],',', rcs_df, ')', sep = ' ' ))
      }
    }
    ### prepare model formula
    model_formula <- stats::as.formula( base::paste( formula_outcome
                                , base::paste(c(formula_covariates, formula_rcs),collapse='+') ))
  }
  if ( base::isTRUE(string) ) {
    model_formula <- base::Reduce( paste, base::deparse(model_formula))
  }
  return( model_formula )
}
