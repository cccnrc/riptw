#' Plot IPTW correction effect on Standardised Mean Difference (SMD) of variables passed as covariates
#'
#' This function plots the result of Standardised Mean Difference (SMD) reduction
#' after adjustment for normalized IPTW. Cut-off 0.1 is highligted, unadjusted and
#' adjusted variable SMD are plotted as dotted and continuous lines respectively.
#'
#' @param unadjusted: the unadjusted SMD stratification (output of balance_cov())
#' @param adjusted: the adjusted SMD stratification (output of balance_cov())
#' @param font_family: the family to use as plot font
#' @param font_size: the size to use as plot font
#' @return A ggplot2 object that represent effect of IPTW normalization on SMD for each covariate
iptw_plot <- function( unadjusted, adjusted, font_family = "sans-serif", font_size = 18 )
{
  dataPlot <- data.frame(variable   = rownames(tableone::ExtractSmd(unadjusted)),
                         unadjusted = tableone::ExtractSmd(unadjusted),
                         Weighted   = tableone::ExtractSmd(adjusted))
  ### if more than 2 groups in treatment variable dataPlot needs to be restricted to average columns
  if ( 'unadjusted.average' %in% names(dataPlot) ) {
    dataPlot <- dataPlot[ , c( 'variable', 'unadjusted.average', 'Weighted.average' ) ]
  }
  names(dataPlot)<-c("variable", 'unadjusted', 'weighted')
  dataPlotMelt <- reshape2::melt(data          = dataPlot,
                                 id.vars       = "variable",
                                 variable.name = "method",
                                 value.name    = "SMD")
  varsOrderedBySmd <- rownames(dataPlot)[order(dataPlot[,"unadjusted"])]
  dataPlotMelt$variable <- factor(dataPlotMelt$variable,
                                  levels = varsOrderedBySmd)
  dataPlotMelt$method <- factor(dataPlotMelt$method,
                                levels = c("weighted","unadjusted"))
  # print(dataPlotMelt$method)
  P0 <- ggplot2::ggplot(data = dataPlotMelt, mapping = ggplot2::aes(x = variable, y = SMD, group = method, linetype = method)) +
                  ggplot2::geom_line() +
                  ggplot2::geom_point() +
                  ggplot2::geom_hline( yintercept = 0, size = 0.3 ) +
                  ggplot2::geom_hline( yintercept = 0.1, size = 0.1, color = 'red') +
                  ggplot2::coord_flip() +
                  ggplot2::theme_classic() +
                  ggplot2::theme(legend.key = ggplot2::element_blank(),
                                 text=ggplot2::element_text(size=font_size,  family=font_family)) +
                  ggplot2::labs( y = "Standardised Mean Difference", x = "Variable", title = "Covariate Balance" )
  return(P0)
}
