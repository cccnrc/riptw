#' Check all passed variables are in dataframe and type (factor vs numeric)
#'
#' This function check that all passed variables are present in data.frame
#' colnames and return a stdout of type of variable too (if requested)
#'
#' @param data: the data.frame input onject
#' @param covariates: the vector of variables to be analyzed
#' @param stdout: print summaries for each variable passed (default: TRUE)
#' @param convert: automatically converts characters and numeric with 0 and 1 to factors (default: FALSE)
#' @return A stdout value of each variable type (numeric vs. factor)
#' @export
numeric_variable_summary <- function( NUMERIC_VARIABLE_VECTOR )
{
  COV_TOT <- base::length(NUMERIC_VARIABLE_VECTOR)
  COV_MEAN <- base::round(base::mean( NUMERIC_VARIABLE_VECTOR, na.rm = TRUE ),2)
  COV_SD <- base::round(stats::sd( NUMERIC_VARIABLE_VECTOR, na.rm = TRUE ),2)
  COV_MEDIAN <- base::round(stats::median( NUMERIC_VARIABLE_VECTOR, na.rm = TRUE ),2)
  COV_Q1 <- base::round(stats::quantile( NUMERIC_VARIABLE_VECTOR, 0.25, na.rm = TRUE ),2)
  COV_Q3 <- base::round(stats::quantile( NUMERIC_VARIABLE_VECTOR, 0.75, na.rm = TRUE ),2)
  COV_MIN <- base::round(base::min( NUMERIC_VARIABLE_VECTOR, na.rm = TRUE ),2)
  COV_MAX <- base::round(base::max( NUMERIC_VARIABLE_VECTOR, na.rm = TRUE ),2)
  COV_NA <- base::length(base::which(base::is.na(NUMERIC_VARIABLE_VECTOR)))
  COV_NA_PERC <- base::round(COV_NA / COV_TOT * 100, 2)
  COV_SUMMARY <- base::paste( 'num: ', COV_TOT, ' -> mean: ', COV_MEAN, ' (', COV_SD, ') - median: ', COV_MEDIAN, ' [', COV_Q1, ' - ', COV_Q3, '] - min: ', COV_MIN, ', max: ', COV_MAX, ' - NA: ', COV_NA, ' (', COV_NA_PERC, '%)', sep = '' )
  return(COV_SUMMARY)
}


check_covariates <- function( data, covariates, stdout = TRUE, convert = FALSE )
{
  ### check all covariates are in the passed DB
  if (base::isFALSE(base::all(covariates %in% base::colnames(data)))){
    # cat ('  ========>  Exiting ...\n')
    base::stop('some variable are not in input data.frame column names!!!')
  }
  if ( base::isTRUE( stdout) ) {
    base::cat('\n')
    base::cat('---------------------------------------------------------------------------------------------\n')
    base::cat('----------------------------------> Variables Description <----------------------------------\n')
    base::cat('---------------------------------------------------------------------------------------------\n')
    ### explain each covariate type
    for ( COV in covariates )
    {
      ### get NA
      COV_TOT <- base::length(data[[COV]])
      COV_NA <- base::length(base::which(base::is.na(data[[COV]])))
      COV_NA_PERC <- base::round(COV_NA / COV_TOT * 100, 2)
      COV_NA_STRING <- base::paste('num: ', COV_TOT, ' - NA: ', COV_NA, ' (', COV_NA_PERC, '%)', sep = '')
      if (base::is.factor(data[[COV]])){
        ### get levels and table summary
        NUM_LEV_TABLE <- base::length(base::table(data[[COV]]))
        NUM_LEV <- base::length(base::levels(data[[COV]]))
        if ( NUM_LEV_TABLE < 2 ){
          base::cat('  ====> variable:\t', COV, ' is FACTOR with <2 levels !!!\n')
        } else {
          if ( NUM_LEV_TABLE != NUM_LEV ) {
            base::cat('  ==> variable:\t', COV, ' is FACTOR with ', NUM_LEV, ' levels, but actual levels: ', NUM_LEV_TABLE, ' !!!\n')
          } else {
            base::cat('  --> variable:\t', COV, ' is FACTOR with ', NUM_LEV, ' levels -> ', COV_NA_STRING, '\n')
          }
        }
      } else if (base::is.numeric(data[[COV]])) {
        base::cat('  --> variable:\t', COV, ' is NUMERIC -> ', COV_NA_STRING, '\n' )
        base::cat('     -> ', numeric_variable_summary(data[[COV]]), '\n', sep = '')
        cov_unique_values <- base::unique( data[[COV]] )
        if ( base::setequal( cov_unique_values, c(0,1) ) ) {
          if ( base::isFALSE(convert) ) {
            base::cat('       -> unique values are 0 and 1, you sure this is not a factor? \n', sep = '')
          } else {
            base::cat('       -> unique values are 0 and 1, converted into factor \n', sep = '')
            data[[COV]] <- base::factor( data[[COV]] )
          }
        }
      } else if (base::is.character(data[[COV]])) {
        base::cat('  --> variable:\t', COV, ' is CHARACTER -> ', COV_NA_STRING, '\n' )
        if ( base::isFALSE(convert) ) {
          base::cat('    --> tip: convert to factor to ensure analysis precision\n' )
        } else {
          base::cat('       -> converted into factor \n', sep = '' )
          data[[COV]] <- base::factor( data[[COV]] )
        }
      } else {
        base::cat('  ======> variable:\t', COV, ' is UNRECOGNIZED !!!\n')
      }
    }
    base::cat('---------------------------------------------------------------------------------------------\n')
    base::cat(' ------------> make sure they correspond to how you want to analyze them <-------------------\n')
    base::cat('---------------------------------------------------------------------------------------------\n')
    base::cat('\n')
  }
  return( data )
}
