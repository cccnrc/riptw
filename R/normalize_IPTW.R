#' Add weights based on IPTW normalization to a dataframe
#'
#' This function add an 'sw' column to the input data.frame containing weights
#' calculated on normalized IPTW on the number of samples within each group
#'
#' @param data: the data.frame object for the analysis, output of add_IPTW()
#' @param iptw: the column name of the IPTW variable (output of add_IPTW())
#' @param tx_name: the column name of the treatment variable
#' @param tx_levels: the levels of the treatment variable (default: levels() of the column, it will be converted to factor() if is not)
#' @param round_factor: number of decimal inputs to round the stdout (default: 2)
#' @param print: resumen of mean comparison to stdout? (default: FALSE)
#' @return A data.frame object which is the original data.frame with a 'sw' column added with weights based on IPTW
normalize_IPTW <- function( data, iptw = 'iptw', tx_name, tx_levels = NULL, round_factor = 2, print = FALSE )
{
  NIPTW_COLNAME <- 'sw'
  ### if tx_levels are not specified it assign to the levels of the passed variable
  if (is.null(tx_levels)) {
    if (is.factor(data[[tx_name]])) {
      tx_levels <- levels( data[[tx_name]] )
    } else {
      tx_levels <- levels( factor(data[[tx_name]]) )
    }
  }
  ### calculate stabilised IPTW based on the actual number of samples with that specific treatment level
  stabilisedIPTW <- stabilise_levels( data = data, outcome = tx_name, outcome_levels = tx_levels, iptw = iptw )
  data[[ NIPTW_COLNAME ]] <- stabilisedIPTW
  ### see how that this reduced the differences:
  IPTW_MEAN <- mean( data[[iptw]], na.rm = TRUE )
  IPTW_SD <- sd( data[[iptw]], na.rm = TRUE )
  SIPTW_MEAN <- mean( data[[NIPTW_COLNAME]], na.rm = TRUE )
  SIPTW_SD <- sd( data[[NIPTW_COLNAME]], na.rm = TRUE )
  if ( isTRUE(print) ) {
    cat('----------------------- IPTW normalization -----------------------\n')
    cat('  --> IPTW mean before normalization:\t', round(IPTW_MEAN, round_factor), ' (sd: ', round(IPTW_SD, round_factor), ')\n', sep = '' )
    cat('  --> IPTW mean after normalization:\t', round(SIPTW_MEAN, round_factor), ' (sd: ', round(SIPTW_SD, round_factor), ')\n', sep = '' )
    cat('------------------------------------------------------------------\n')
  }
  return( data )
}
