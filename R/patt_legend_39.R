#' Graphical legend about time patterns
#'
#' A plot showing patterns of change along time is made
#' and returned as a list of ggplot objects.
#'

#' @return a list of 2 ggplot objects 20 plus 19 patterns.
#'
#'
#' @references{\url{https://local.disia.unifi.it/stefanini/RESEARCH/coneu/tutorial-conv.html}}
#'
#'
#' @examples
#'
#' refGGpat <- patt_legend_39()
#'
#'
#'
#' @export
#'
patt_legend_39 <- function(
                        #indiType = "highBest"
                        ){
  time<-measureEU<-NULL # to pass CRAN check
      
  #### first pannel
  myTB2 <- structure(list(Y1 = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 
  2, 2, 2, 2, 2, 2, 2), EUT1 = c(10, 10, 10, 10, 10, 10, 10, 10, 
  10, 10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), Pat = c(1, 2, 3, 4, 5, 
  6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10), Indic = c(12, 
  12, 12, 12, 16, -12, -2, 4, 6, 8, 22, 12, 6, 2, 2, -2, -2, -2, 
  -4, -6)), row.names = c(NA, -20L), class = c("tbl_df", "tbl", "data.frame"))
  
  myTB20 <- dplyr::select(dplyr::mutate(myTB2, Time=Y1,
                   EU= EUT1,
                   Grp=Pat,
                   MS=Indic),Time,EU,Grp,MS)
  
  myTB2fin <-structure(list(Pat = c("EUT1", "EUT1", "EUT1", "EUT1", "EUT1", 
  "EUT1", "EUT1", "EUT1", "EUT1", "EUT1", "EUT2", "EUT2", "EUT2", 
  "EUT2", "EUT2", "EUT2", "EUT2", "EUT2", "EUT2", "EUT2"), EU = c(0, 
  0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 10, 10, 10, 10, 10, 10, 10, 10, 
  10), Time = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 
  2, 2, 2, 2), MS = c(18, 12, 6, 2, 1, -8, -2, -2, -4, -2, 12, 
  12, 12, 12, 17, 9, 8, 3, -4, -16), PatMS = c("M1", "M1", "M1", 
  "M1", "M1", "M1", "M1", "M1", "M1", "M1", "M2", "M2", "M2", "M2", 
  "M2", "M2", "M2", "M2", "M2", "M2"), Grp = c(11L, 12L, 13L, 14L, 
  15L, 16L, 17L, 18L, 19L, 20L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 
  18L, 19L, 20L)), row.names = c(NA, -20L), class = c("tbl_df", "tbl", "data.frame"))                                                   
  
   tbG01<- dplyr::select(myTB2fin, Time,EU,Grp,MS)
   tbG10<- dplyr::bind_rows(myTB20,tbG01)
  
   outG1 <- 
    ggplot(tbG10,
           ggplot2::aes(x=Time, y=EU))+  
     ggplot2::geom_line()+
     ggplot2::geom_point() +
     ggplot2::facet_wrap(vars(Grp),ncol = 7)+
     ggplot2::geom_line(
       ggplot2::aes(x=Time,y=MS),color="red",linetype = "dashed")+
     ggplot2::geom_point(
       ggplot2::aes(x=Time,y=MS),color="red") +
     ggplot2::theme(axis.text.x = ggplot2::element_text(
        #face = "bold", color = "#993333",size = 12, 
        angle = 90))+
   ggplot2::scale_y_continuous(
     breaks = seq(-20,25,5)) +
   # alternatives
   ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                  axis.ticks.x = ggplot2::element_blank(),
                  axis.title.x.bottom = ggplot2::element_blank(),
                  axis.title.y = ggplot2::element_blank() )

  
 ## second panel
myTB33fin <- structure(list(Pat = c("EUT1", "EUT1", "EUT1", "EUT1", "EUT1", 
  "EUT1", "EUT1", "EUT1", "EUT2", "EUT2", "EUT2", "EUT2", "EUT2", 
  "EUT2", "EUT2", "EUT2"), EU = c(10, 10, 10, 10, 10, 10, 10, 10, 
  10, 10, 10, 10, 10, 10, 10, 10), Time = c(1, 1, 1, 1, 1, 1, 1, 
  1, 2, 2, 2, 2, 2, 2, 2, 2), MS = c(6, 13, 13, 18, 18, 7, 7, 1, 
  18, 18, 13, 13, 6, 1, 7, 8), PatMS = c("M1", "M1", "M1", "M1", 
  "M1", "M1", "M1", "M1", "M2", "M2", "M2", "M2", "M2", "M2", "M2", 
  "M2"), Grp = c(21L, 22L, 23L, 24L, 25L, 26L, 27L, 28L, 21L, 22L, 
  23L, 24L, 25L, 26L, 27L, 28L)), row.names = c(NA, -16L), 
  class = c("tbl_df","tbl", "data.frame"))
  
  myTB44fin <-
  structure(list(Pat = c("EUT1", "EUT1", "EUT1", "EUT1", "EUT1", 
  "EUT1", "EUT1", "EUT1", "EUT2", "EUT2", "EUT2", "EUT2", "EUT2", 
  "EUT2", "EUT2", "EUT2"), EU = c(10, 10, 10, 10, 0, 0, 0, 0, 0, 
  0, 0, 0, 10, 10, 10, 10), Time = c(1, 1, 1, 1, 1, 1, 1, 1, 2, 
  2, 2, 2, 2, 2, 2, 2), MS = c(13, 7, 5, 5, 18, 7, 4, -2, -3, 3, 
  5, 14, 7, 7, 7, 12), PatMS = c("M1", "M1", "M1", "M1", "M1", 
  "M1", "M1", "M1", "M2", "M2", "M2", "M2", "M2", "M2", "M2", "M2"
  ), Grp = c(29L, 30L, 31L, 32L, 33L, 34L, 35L, 36L, 29L, 30L, 
  31L, 32L, 33L, 34L, 35L, 36L)), 
  row.names = c(NA, -16L), 
  class = c("tbl_df", "tbl", "data.frame"))
  
  myTBIIfin <- structure(list(Pat = c("EUT1", "EUT1", "EUT1", "EUT2", "EUT2", 
  "EUT2"), EU = c(0, 5, 10, 10, 5, 0), Time = c(1, 1, 1, 2, 2, 
  2), MS = c(0, 5, 10, 10, 5, 0), PatMS = c("M1", "M1", "M1", "M2", 
  "M2", "M2"), Grp = c(37L, 38L, 39L, 37L, 38L, 39L)), 
  row.names = c(NA, -6L), 
  class = c("tbl_df", "tbl", "data.frame"))  

 
 tbG2<- dplyr::bind_rows(myTB33fin,myTB44fin,myTBIIfin)
 outG2<- ggplot2::ggplot(tbG2,
   ggplot2::aes(x=Time, y=EU))+  
   ggplot2::geom_line()+
   ggplot2::geom_point() +
   ggplot2::facet_wrap(vars(Grp),ncol = 7)+
   ggplot2::geom_line(ggplot2::aes(x=Time,y=MS),color="red",linetype = "dashed")+
   ggplot2::geom_point(ggplot2::aes(x=Time,y=MS),color="red")+
   ggplot2::theme(axis.text.x = element_text(
     #face = "bold", color = "#993333",size = 12, 
     angle = 90))+
   ggplot2::scale_y_continuous(
     breaks = seq(-20,25,5)) +
   # alternatives
   ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                  axis.ticks.x = ggplot2::element_blank(),
                  axis.title.x.bottom = ggplot2::element_blank(),
                  axis.title.y = ggplot2::element_blank() )
 
  return( list( 
    imgPat1 = outG1,
    imgPat2 = outG2
    ))
}

utils::globalVariables(c("measureM","time","measureEU"))
