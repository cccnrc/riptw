#' Check all passed variables are in dataframe and type (factor vs numeric)
#'
#' This function check that all passed variables are present in data.frame
#' colnames and return a stdout of type of variable too (if requested)
#'
#' @param data: the data.frame input onject
#' @param covariates: the vector of variables to be analyzed
#' @param stdout: print summaries for each variable passed (default: TRUE)
#' @return A stdout value of each variable type (numeric vs. factor)
#' @export
numeric_variable_summary <- function( NUMERIC_VARIABLE_VECTOR )
{
  COV_TOT <- length(NUMERIC_VARIABLE_VECTOR)
  COV_MEAN <- round(mean( NUMERIC_VARIABLE_VECTOR, na.rm = TRUE ),2)
  COV_SD <- round(sd( NUMERIC_VARIABLE_VECTOR, na.rm = TRUE ),2)
  COV_MEDIAN <- round(median( NUMERIC_VARIABLE_VECTOR, na.rm = TRUE ),2)
  COV_Q1 <- round(quantile( NUMERIC_VARIABLE_VECTOR, 0.25, na.rm = TRUE ),2)
  COV_Q3 <- round(quantile( NUMERIC_VARIABLE_VECTOR, 0.75, na.rm = TRUE ),2)
  COV_MIN <- round(min( NUMERIC_VARIABLE_VECTOR, na.rm = TRUE ),2)
  COV_MAX <- round(max( NUMERIC_VARIABLE_VECTOR, na.rm = TRUE ),2)
  COV_NA <- length(which(is.na(NUMERIC_VARIABLE_VECTOR)))
  COV_NA_PERC <- round(COV_NA / COV_TOT * 100, 2)
  COV_SUMMARY <- paste( 'num: ', COV_TOT, ' -> mean: ', COV_MEAN, ' (', COV_SD, ') - median: ', COV_MEDIAN, ' [', COV_Q1, ' - ', COV_Q3, '] - min: ', COV_MIN, ', max: ', COV_MAX, ' - NA: ', COV_NA, ' (', COV_NA_PERC, '%)', sep = '' )
  return(COV_SUMMARY)
}


check_covariates <- function( data, covariates, stdout = TRUE )
{
  ### check all covariates are in the passed DB
  if (isFALSE(all(covariates %in% colnames(data)))){
    # cat ('  ========>  Exiting ...\n')
    stop('some variable are not in input data.frame column names!!!')
  }
  if ( isTRUE( stdout) ) {
    cat('\n')
    cat('---------------------------------------------------------------------------------------------\n')
    cat('----------------------------------> Variables Description <----------------------------------\n')
    cat('---------------------------------------------------------------------------------------------\n')
    ### explain each covariate type
    for ( COV in covariates )
    {
      ### get NA
      COV_TOT <- length(data[[COV]])
      COV_NA <- length(which(is.na(data[[COV]])))
      COV_NA_PERC <- round(COV_NA / COV_TOT * 100, 2)
      COV_NA_STRING <- paste('num: ', COV_TOT, ' - NA: ', COV_NA, ' (', COV_NA_PERC, '%)', sep = '')
      if (is.factor(data[[COV]])){
        ### get levels and table summary
        NUM_LEV_TABLE <- length(table(data[[COV]]))
        NUM_LEV <- length(levels(data[[COV]]))
        if ( NUM_LEV_TABLE < 2 ){
          cat ('  ====> variable:\t', COV, ' is FACTOR with <2 levels !!!\n')
        } else {
          if ( NUM_LEV_TABLE != NUM_LEV ) {
            cat ('  ==> variable:\t', COV, ' is FACTOR with ', NUM_LEV, ' levels, but actual levels: ', NUM_LEV_TABLE, ' !!!\n')
          } else {
            cat ('  --> variable:\t', COV, ' is FACTOR with ', NUM_LEV, ' levels -> ', COV_NA_STRING, '\n')
          }
        }
      } else if (is.numeric(data[[COV]])) {
        cat('  --> variable:\t', COV, ' is NUMERIC -> ', COV_NA_STRING, '\n' )
        cat('     -> ', numeric_variable_summary(data[[COV]]), '\n', sep = '')
      } else {
        cat('  ======> variable:\t', COV, ' is UNRECOGNIZED !!!\n')
      }
    }
    cat('---------------------------------------------------------------------------------------------\n')
    cat('---------------------------------------------------------------------------------------------\n')
    cat('\n')
  }
}
