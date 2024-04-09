#' Calculate normalized Inverse Probability of Treatment Weighting (IPTW) weights based on a set of covariates and the treatment variable specified
#'
#' This function calculates normalized Inverse Probability of Treatment Weighting (IPTW)
#' for each sample and add propensity score column (for each tratment variable group, "PS_" prefix),
#' IPTW ("iptw"), and normalized-IPTW ("sw") column to the original input data.frame.
#' It allows for both model formula direct specification or simply passing outcome and
#' covariates as a vector. It also allows for Restricted Cubic Spline (RCS) transformation
#' of some covariates. It also output unadjusted vs. adjusted Standardised Mean Differenc (SMD)
#' plot to compare SMD reduction for each covariate using IPTW, and unadjusted and adjusted
#' stratification tables for each covariate with the treatment variable groups (with p-values).
#'
#' @param data: the data.frame object for the analysis
#' @param outcome: the column name of the treatment variable
#' @param formula: the regression model formula as.formula() object (optional, you can pass outcome and covariates as vector)
#' @param outcome_levels: the levels of the treatment variable (optional: levels() of the column, it will be converted to factor() in case is necessary)
#' @param covariates: vector of model covariate names
#' @param rcs_covariate: the variable(s) to be used as restricted cubic spline (optional)
#' @param rcs_df: if rcs_covariate is specified the degrees of freedom to use for RCS transformation (optional, default: 5)
#' @param ps_prefix: the propensity score columns prefix to use for the output data.frame (default: "PS_")
#' @param stdout: print summaries for each function to stdout (default: FALSE)
#' @param model_family: the family of regression to be used in vglm (default: "multinomial")
#' @param iptw: the name of the added column with IPTW values in output data.frame (default: "iptw")
#' @param round_norm: if stdout = TRUE selected, the number of decimal inputs to round the normalize_IPTW() stdout (default: 2)
#' @param round_bc: if stdout = TRUE selected, the number of decimal inputs to round the balance_cov() stdout (default: 3)
#' @param iptw: the name of the added column with IPTW values in output data.frame (default: "iptw")
#' @param plot_font_family: the family to use as output plot font (default: sans-serif)
#' @param plot_font_size: the size to use as output plot font (default: 18)
#' @return A list object with 'data': the original data.frame with PS, IPTW, and weights columns added for each sample - 'plot': the SMD reduction plot - 'unadjusted': the covariates stratification table (before IPTW-adjustment) - 'adjusted': the covariates stratification table (after IPTW-adjustment)
#' @export
riptw <- function( data,
                  outcome,
                  formula = NULL,
                  outcome_levels = NULL,
                  covariates,
                  rcs_covariate = NULL,
                  rcs_df = 5,
                  ps_prefix = "PS_",
                  stdout = FALSE,
                  model_family = VGAM::multinomial(parallel = FALSE),
                  iptw = 'iptw',
                  round_norm = 2,
                  round_bc = 3,
                  plot_font_family = "sans-serif",
                  plot_font_size = 18
                 )
{
  if ( is.null(formula) ) {
    ### elaborate model formula
    model_formula <- get_formula( outcome = outcome,
                                  covariates = covariates,
                                  rcs_covariate = rcs_covariate,
                                  rcs_df = rcs_df )
  } else {
    model_formula <- formula
  }
  ### add propensity score column to the original database
  data_ps <- add_PS( data = data,
                      formula = model_formula,
                      prefix = ps_prefix,
                      family = model_family )
  ### add IPTW column to the original database
  data_iptw <- add_IPTW( data = data_ps,
                          tx_name = outcome,
                          tx_levels = outcome_levels,
                          iptw = iptw )
  ### add normalized IPTW column - based on the number of samples in each group
  data_sw <- normalize_IPTW( data = data_iptw,
                              tx_name = outcome,
                              tx_levels = outcome_levels,
                              round_factor = round_norm,
                              print = stdout )
  ### check SMD reduction after normalized IPTW adjustement for each variable
  balanced_cov <- balance_cov( data = data,
                                db_iptw = data_sw,
                                covariates = covariates,
                                outcome = outcome,
                                round_factor = round_bc,
                                print = stdout )
  ### plot SMD reduction after normalized IPTW adjustement for each variable
  iplot <- iptw_plot( unadjusted = balanced_cov[['unadjusted']],
                      adjusted = balanced_cov[['adjusted']],
                      font_family = plot_font_family,
                      font_size = plot_font_size )
  ### create stratification tables for SMD before and normalized IPTW adjustement
  itables <- iptw_tables( data = data,
                          weighted = balanced_cov[['weighted']],
                          covariates = covariates,
                          outcome = outcome,
                          print = stdout )
  ### create the return object
  out_list <- list(
    'plot' = iplot,
    'unadjusted' = itables[['unadjusted']],
    'adjusted' = itables[['adjusted']],
    'data' = data_sw
  )
}
