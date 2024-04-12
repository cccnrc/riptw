#' Split passed formula into outcome and covariates
#'
#' This function splits the passed formula into outcome and covariates
#'
#' @param formula: the model formula passed
#' @return A list() object with oucome and covariates splitted
#' @export
split_formula <- function( formula )
{
  ### check the object is actually as.formula()
  if ( rlang::is_formula( formula ) ) {
    F0 <- as.character( formula )
  } else {
    F00 <- as.formula( formula )
    F0 <- as.character( formula )
  }
  ### check as.character properly splitted it
  if ( length(F0) < 3 ) {
    stop( 'Length of formula is wrong, ensure you passed an as.formula() object of form Y ~ X1 + X2 + X3 ...' )
  }
  if ( '~' %in% F0 ) {
    outcome <- F0[2]
    outcome_trim <- gsub("[[:space:]]", "", outcome )
    covariate_string <- F0[length(F0)]
  } else {
    stop( '~ not found in formula, ensure you passed an as.formula() object of form Y ~ X1 + X2 + X3 ...' )
  }
  ### split covariates
  covariate_vector <- strsplit( covariate_string, split = "+", fixed=TRUE )[[1]]
  covariate_vector_trim <- vector()
  for ( i in 1:length(covariate_vector) )
  {
    x_trim <- gsub("[[:space:]]", "", covariate_vector[i])
    ### check if some have been passed transformed
    if (grepl( '(', x_trim, fixed = TRUE) ) {
      x_trim_par <- gsub( "\\(([^()]*)\\)|.", "\\1", x_trim )
      x_trim_par_comma <- unlist(strsplit( x_trim_par, ","))[1]
      x_trim_par_comma_trim <- gsub("[[:space:]]", "", x_trim_par_comma )
      x_trim <- x_trim_par_comma_trim
    }
    covariate_vector_trim <- c( covariate_vector_trim, x_trim )
  }
  out_list <- list(
    'outcome' = outcome_trim,
    'covariates' = covariate_vector_trim
  )
  return( out_list )
}
