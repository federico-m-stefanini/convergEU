#' Graphical representation based on sigma convergence
#'
#' A ggplot of the standard deviation and the coefficient of variation based on the results obtained for sigma-convergence
#'
#' @param sigmaconvOut  the output obtained from sigma_conv function.
#' @param time_0 starting time.
#' @param time_t ending time.
#' @param aggregation the name of the set of member states for which the sigma-convergence is calculated.
#' @param x_angle axis orientation for time labels, default 45.
#' @return  a ggplot object to be displayed of saved using ggsave.
#'
#'
#' @references{\url{https://local.disia.unifi.it/stefanini/RESEARCH/coneu/tutorial-conv.html}}
#'
#' @examples
#'
#' # Example 1
#' # Sigma convergence for the emp_20_64_MS Eurofound dataset in the period 2002-2006:
#' data(emp_20_64_MS)
#' reSigConv <- sigma_conv(emp_20_64_MS, timeName = "time", time_0 = 2002,time_t = 2006)
#'
#' # Graphical plot based on the results for sigma-convergence
#' reSiggraph<-sigma_conv_graph(reSigConv,2002,2006,aggregation = 'EU27')
#'
#' # Example 2
#' # Sigma-convergence for the emp_20_64_MS Eurofound dataset in the period 2008-2016:
#' reSigConv1 <- sigma_conv(emp_20_64_MS, timeName = "time", time_0 = 2008,time_t = 2016)
#'
#' # Graphical plot based on the results for sigma-convergence
#' reSiggraph1<-sigma_conv_graph(reSigConv1,2008,2016,aggregation = 'EU27')
#'
#' # Select different time windows, e.g. 2012-2016 and change x_angle:
#' reSiggraph2<-sigma_conv_graph(reSigConv1,2012,2016,aggregation = 'EU27', x_angle=90)
#'
#' @export
#'
#' @importFrom rlang .data
#'
sigma_conv_graph <- function(sigmaconvOut,
                             time_0 = NA,
                             time_t = NA,
                             aggregation=NA,
                             x_angle=45
)
{
  ptime_0<-time_0
  ptime_t<-time_t
  worktb<-  dplyr::filter(sigmaconvOut$res,.data$time >= ptime_0,
                          .data$time <= ptime_t)
  plot2sig <- ggplot2::qplot(worktb$time,worktb$stdDev,
                    xlab= paste("Years "),
                    ylab=aggregation) +
    ggplot2::geom_line()  +
    ggplot2::scale_x_continuous(
      breaks = seq(ptime_0,ptime_t),
      labels = seq(ptime_0,ptime_t)) +
    ggplot2::theme(
      axis.text.x=ggplot2::element_text(
        angle = x_angle ,
        vjust = 1,
        hjust=1)) +
    ggplot2::ggtitle(paste("Standard Deviation"))


  plot3sig <- ggplot2::qplot( worktb$time,worktb$CV*100,
                     xlab= paste("Years "),
                     ylab=aggregation
                     ) +
    ggplot2::geom_line()  +
    ggplot2::scale_x_continuous(
      breaks = seq(ptime_0,ptime_t),
      labels = seq(ptime_0,ptime_t)) +
    ggplot2::theme(
      axis.text.x=ggplot2::element_text(
        angle = x_angle ,
        vjust = 1,
        hjust=1)) +
    ggplot2::ggtitle(paste("Coefficient of variation"))

  sigma_gr <-  ggpubr::ggarrange(
    plot2sig,plot3sig)
  return(sigma_gr)
}
