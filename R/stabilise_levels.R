#' Multiply each sample IPTW for their specific treatment group percentage on the whole cohort
#'
#' This function multiply each sample IPTW value (as per add_IPTW()) for
#' the percentage of cases on that specific treatment variable group
#'
#' @param data: the data.frame object for the analysis, output of add_IPTW()
#' @param outcome: the column name of the treatment variable
#' @param outcome_levels: the levels of the treatment variable (optional: levels() of the column, it will be converted to factor() in case is necessary)
#' @param iptw: the column name of the IPTW variable (output of add_IPTW())
#' @return A vector object where IPTW is corrected for the percentage of cases in that specific tratment group for each sample
stabilise_levels <- function( data, outcome, outcome_levels = NULL, iptw = 'iptw' )
{
  ### if tx_levels are not specified it assign to the levels of the passed variable
  if (is.null(outcome_levels)) {
    if (is.factor(data[[outcome]])) {
      outcome_levels <- levels( data[[outcome]] )
    } else {
      outcome_levels <- levels( factor(data[[outcome]]) )
    }
  }
  ### get percentage of patients within each treatment variable group
  perc_vector <- vector()
  for ( i in 1:length( outcome_levels ) )
  {
    perc <- length( which( data[[outcome]] == outcome_levels[i] ) ) / nrow( data )
    perc_vector <- c( perc_vector, perc )
  }
  ### multiply each sample IPTW * percentage of cases in that group
  stabilised_IPTW <- vector()
  for ( i in 1:nrow(data) )
  {
    iptw_i <- data[[iptw]][i]
    level_index_i <- which( outcome_levels == data[[outcome]][i] )
    iptw_i_stabilised <- iptw_i * perc_vector[level_index_i]
    stabilised_IPTW <- c( stabilised_IPTW, iptw_i_stabilised )
  }
  return( stabilised_IPTW )
}
