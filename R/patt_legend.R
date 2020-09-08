#' Graphical legend about time patterns
#'
#' A 4 by 4 plot showing patterns of change along time is made
#' and returned as a list of ggplot objects.
#'
#' @param   indiType a string equal to "highBest" or "lowBest"
#'          to select a type of indicator.
#' @return a list of ggplot objects to be plotted using grid.arrange()
#'         function.
#'
#'
#' @references{\url{https://local.disia.unifi.it/stefanini/RESEARCH/coneu/tutorial-conv.html}}
#'
#'
#' @examples
#'
#' require(gridExtra)
#' refGGpat2 <- patt_legend(indiType="lowBest")
#'
#' refGGpat3 <- patt_legend(indiType="highBest")
#'
#'
#'
#' @export
#'
patt_legend <- function(indiType = "highBest"){
  time<-measureEU<-NULL # to pass CRAN check
  wmyTB_1 <- structure(list(pattern_HB = c("Catching up (1)", "Catching up (1)",
     "Flattening (2)", "Flattening (2)", "Inversion (3)", "Inversion (3)",
     "Outperforming (4)", "Outperforming (4)", "Slower pace (5)",
     "Slower pace (5)", "Diving (6)", "Diving (6)", "Defending better (7)",
     "Defending better (7)", "Escaping (8)", "Escaping (8)", "Falling away (9)",
     "Falling away (9)", "Underperforming (10)", "Underperforming (10)",
     "Recovering (11)", "Recovering (11)", "Reacting better (12)",
     "Reacting better (12)",
     "Parallel-better-over (13)",
     "Parallel-better-over (13)",
     "Parallel-equal-over (14)",
     "Parallel-equal-over (14)",
     "Parallel-worse-over (15)",
     "Parallel-worse-over (15)",
     "Crossing (19)", "Crossing (19)", "Crossing reversed (20)",
     "Crossing reversed (20)")
    , measureEU = c(23,24,21,23,21,23,
                    21,22,23,25,23,25,
                    24,21,22,21,24,23,
                    22,21,25,23,25,22,
                    21,23,22,22,23,21,21,24,24,21),
    measureM = c(21,23,23,24,25,24,
                 22,25,22,23,22,21,
                 25,24,23,25,23,21,
                 25,22,21,22,22,21,
                 23,25,24,24,25,23,
                 25,22,21,25),
    time = c(2001, 2002, 2001, 2002, 2001, 2002,
    2001, 2002, 2001, 2002, 2001, 2002, 2001, 2002, 2001, 2002, 2001,
    2002, 2001, 2002, 2001, 2002, 2001, 2002, 2001, 2002, 2001, 2002,
    2001, 2002, 2001, 2002, 2001, 2002),
    pattern_LB = c(
      "Underperforming (10)", "Underperforming (10)",
      "Reacting better (12)", "Reacting better (12)",
    "Recovering (11)", "Recovering (11)",
    "Falling away (9)", "Falling away (9)",
    "Defending better (7)", "Defending better (7)",
    "Escaping (8)",  "Escaping (8)",
    "Slower pace (5)", "Slower pace (5)",
    "Diving (6)",    "Diving (6)",
    "Outperforming (4)", "Outperforming (4)",
    "Catching up (1)","Catching up (1)",
    "Inversion (3)", "Inversion (3)",
    "Flattening (2)", "Flattening (2)",
    "Parallel-worse-under (16)",    "Parallel-worse-under (16)",
    "Parallel-equal-under (17)", "Parallel-equal-under (17)",
    "Parallel-better-under (18)","Parallel-better-under (18)",
    "Crossing reversed (20)", "Crossing reversed (20)",
    "Crossing (19)", "Crossing (19)")), class = "data.frame", row.names = c(NA,
    -34L))

  wmyTB_2 <-  dplyr::tribble(
  ~pattern_HB, ~measureEU, ~measureM, ~time,             ~pattern_LB,
  "Parallel-better-under (18)",     23,  21,       2001,    "Parallel-worse-over (15)",
  "Parallel-better-under (18)",     25,  23,       2002,    "Parallel-worse-over (15)",
  "Parallel-equal-under (17)",      24,  22,        2001,   "Parallel-equal-over (14)",
  "Parallel-equal-under (17)",      24,  22,        2002,   "Parallel-equal-over (14)",
  "Parallel-worse-under (16)",      25,  23,        2001,   "Parallel-better-over (13)",
  "Parallel-worse-under (16)",      23,  21,        2002,   "Parallel-better-over (13)"
  )
  #
  oneTB <- dplyr::bind_rows(wmyTB_1 ,wmyTB_2  )
  selez_LB <- c(19,20,23,24,21,22,17,18,13:16,9:12,7,8,1,2,5,6,3,4,
                39,40,37,38,35,36,25:30,33,34,31,32)
  selez_HB <- c(1:30,39,40,37,38,35,36,31,32,33,34)

  pattern_HB <- pattern_LB <- NULL
  if(indiType == "highBest"){
    oneTB <- oneTB[selez_HB,]
    oneTB <- dplyr::mutate(oneTB,pattern = pattern_HB )
    oneTB$pattern <- factor(oneTB$pattern,
                            levels=unique(oneTB[["pattern_HB"]]))
  }else if(indiType == "lowBest"){
    oneTB <- oneTB[selez_LB,]
    oneTB <- dplyr::mutate(oneTB,pattern = pattern_LB )
    oneTB$pattern <- factor(oneTB$pattern,
                    levels=unique(oneTB[["pattern"]]))
  }else{
     stop("Error: wrong type of indicator selected!")
  }

    myGGout <- ggplot2::ggplot(oneTB,
                  ggplot2::aes(x = time,
                               y = measureEU)) +
              ggplot2::facet_wrap(~pattern,ncol=4) +#ncol=1,nrow=1)+
              ggplot2::geom_line() +
              ggplot2::geom_line(ggplot2::aes(x = time,
                                              y = measureM),
                                 colour = "blue",linetype = "3313") +
              ggplot2::scale_y_continuous(
                breaks = 19:26) +
              # alternatives
              ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                             axis.ticks.x = ggplot2::element_blank(),
                             axis.title.x.bottom = ggplot2::element_blank(),
                             axis.title.y = ggplot2::element_blank() )

  return( myGGout )
}

utils::globalVariables(c("measureM","time","measureEU"))
