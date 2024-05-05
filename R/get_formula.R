#' Get formula to be passed to the model based on list of specified covariates and outcome
#'
#' This function ease the creation of the formula to be passed to the model simply
#' passing covariates (as a vector) and outcome.
#' It can handle Restricted Cubic Spline transformation for a specific variable.
#'
#' @param outcome: the model outcome name (dependent variable). If survival analysis ("time_var" specified) this is the event column
#' @param covariates: vector of model covariate names
#' @param random_covariate: the variable to be used as random effects (optional)
#' @param rcs_covariate: the variable to be used as RCS (optional)
#' @param rcs_df: if rcs_covariate is specified the degrees of freedom to use for RCS transformation (optional, default: 5)
#' @return A as.formula() object to be passed to regression models
#' @export
get_formula <- function(
                  outcome,
                  covariates,
                  random_covariates = NULL,
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
  ### update random covariates if asked for
  if ( ! base::is.null( random_covariates ) ) {
    random_covariates_vector <- base::vector()
    for ( rci in 1:length(random_covariates) )
    {
      if ( random_covariates[rci] %in% covariates ) {
        random_index <- base::which( covariates == random_covariates[rci] )
        covariates <- covariates[-random_index]
      }
      random_covariate_term <- base::paste( ' ( 1 |', random_covariates[rci], ' ) ', sep = '' )
      random_covariates_vector <- c( random_covariates_vector, random_covariate_term )
    }
    covariates <- c( covariates, random_covariates_vector )
  }
  ### update RCS covariates (if asked)
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
      ### remove covariate to be inserted as RCS from the list of all covariates (if present)
      if ( rcs_covariate[i] %in% formula_covariates ) {
        rcs_index <- base::which( formula_covariates == rcs_covariate[i] )
        formula_covariates <- formula_covariates[-rcs_index]
      }
      ### add RCS covariate term
      if ( base::length(rcs_df) > 1 ) {
        formula_rcs <- c( formula_rcs, base::paste( 'rms::rcs(', rcs_covariate[i],',', rcs_df[i], ')', sep = ' ' ))
      } else {
        formula_rcs <- c( formula_rcs, base::paste( 'rms::rcs(', rcs_covariate[i],',', rcs_df, ')', sep = ' ' ))
      }
    }
    ### prepare model formula
    model_formula <- stats::as.formula( base::paste( formula_outcome,
      base::paste(c(formula_covariates, formula_rcs ),collapse='+') )
    )
  }
  ### if asked, return as a simple string
  if ( base::isTRUE(string) ) {
    model_formula <- base::Reduce( paste, base::deparse(model_formula))
  }
  return( model_formula )
}
