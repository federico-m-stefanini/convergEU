#' Member state dynamics
#'
#' A ggplot object time by countries where coloured rectangles show the departure
#' from the mean after partitioning into intervals (-Inf, m-1 s, m-0.5 s, m+0.5 s, m+1 s, Inf).
#' Note that the following convention is adopted where the colour of labels changes depending on the type of indicator,
#' i.e. "lowBest" or "highBest":
#'
#' * (-Inf, m -1 s] is labelled as -1; it is coloured in dark green for "lowBest" type of indicator
#'    and in red for "highBest" type of indicator;
#' * (m -1 s, m -0.5 s] is labelled as -0.5; it is coloured in pale green for "lowBest" type of indicator
#'    and in yellow (ocra) for "highBest" type of indicator;
#' * (m -0.5 s,m +0.5 s ] is labelled as 0; it is coloured in pale yellow for both "lowBest" and "highBest
#'    types of indicators;
#' * (m +0.5 s, m +1 s] is labelled as 0.5; it is coloured in  yellow (ocra) for "lowBest" type of indicator
#'   and in pale green for "highBest" type of indicator;
#' * (m +1 s, Inf] is labelled as 1; it is coloured in red for "lowBest" type of indicator
#'    and in dark green for "highBest" type of indicator.
#'
#'
#' @param  myTB  dataset time by countries.
#' @param timeName a string, name of the time variable.
#' @param displace rectangle half height.
#' @param displaceh rectangle half base.
#' @param dimeFontNum size of font.
#' @param myfont_scale axes magnification.
#' @param x_angle  angle of x axis labels.
#' @param axis_name_y  name of y axis.
#' @param axis_name_x name of x axis.
#' @param alpha_color transparency.
#' @param indiType is a string: "highBest" or "lowBest" to define the type
#'        of indicator.
#' @return  a ggplot object to be displayed or saved using ggsave.
#'
#'
#' @references{\url{https://local.disia.unifi.it/stefanini/RESEARCH/coneu/tutorial-conv.html}}
#'
#'
#' @examples
#'
#' \donttest{
#'
#' # Example 1: "lowBest" type of indicator:
#' # Dataset in the format time by countries:
#' require(tibble)
#' testTB <- dplyr::tribble(
#'     ~time, ~countryA ,  ~countryB,  ~countryC,
#'     2000,     0.8,   2.7,    3.9,
#'     2001,     1.2,   3.2,    4.2,
#'     2002,     0.9,   2.9,    4.1,
#'     2003,     1.3,   2.9,    4.0,
#'     2004,     1.2,   3.1,    4.1,
#'     2005,     1.2,   3.0,    4.0
#'     )
#'
#' # Calculate scoreboards for countries:
#' res<-scoreb_yrs(testTB, timeName = "time")
#'
#' # Extract the component "sco_level_num" from "res"
#' resTB<-res$res$sco_level_num
#'
#' # Plot the departures from the mean for each country:
#' ms_dynam ( resTB,
#'     timeName = "time",
#'     displace = 0.25,
#'     displaceh = 0.45,
#'     dimeFontNum = 5,
#'     myfont_scale = 1.35,
#'     x_angle = 45,
#'     axis_name_y = "Countries",
#'     axis_name_x = "Time",
#'     alpha_color = 0.9,
#'     indiType = "lowBest")
#'
#' # Plot the departures from the mean for some years only:
#' # Extract results from sco_level_num" for some years only:
#' estrattore <- resTB[["time"]] >= 2001 & resTB[["time"]] <= 2004
#' scobelvl <- dplyr::filter(resTB, estrattore)
#'
#' # Plot the countries dynamics
#' ms_dynam ( scobelvl,
#'     timeName = "time",
#'     displace = 0.25,
#'     displaceh = 0.45,
#'     dimeFontNum = 5,
#'     myfont_scale = 1.35,
#'     x_angle = 45,
#'     axis_name_y = "Countries",
#'     axis_name_x = "Time",
#'     alpha_color = 0.9,
#'     indiType = "lowBest"
#'     )
#'
#' # Example 2: "highBest" type of indicator:
#' # Scoreboards of Member States for the emp_20_64_MS Eurofound dataset:
#' data(emp_20_64_MS)
#'
#' # Extract the component "sco_level_num
#' sco_lvl <- scoreb_yrs(emp_20_64_MS,timeName = "time")$res$sco_level_num
#'
#' # Extract the results from 2009 to 2016
#' estrattore1 <- sco_lvl[["time"]] >= 2009 & sco_lvl[["time"]] <= 2016
#' scobelvl1 <- dplyr::filter(sco_lvl, estrattore1)
#' # Plot the departures from the mean for the EU Member States:
#' ms_dynam( scobelvl1,
#'     timeName = "time",
#'     displace = 0.25,
#'     displaceh = 0.45,
#'     dimeFontNum = 3,
#'     myfont_scale = 1.35,
#'     x_angle = 45,
#'     axis_name_y = "Countries",
#'     axis_name_x = "Time",
#'     alpha_color = 0.9,
#'     indiType = "highBest")
#'
#' # Extract the results for Member States from 2007 to 2012:
#' estrattore2 <- sco_lvl[["time"]] >= 2007 & sco_lvl[["time"]] <= 2012
#' scobelvl2 <- dplyr::filter(sco_lvl, estrattore2)
#'
#' # Plot the departures from the mean:
#' ms_dynam( scobelvl2,
#'     timeName = "time",
#'     displace = 0.25,
#'     displaceh = 0.45,
#'     dimeFontNum = 3,
#'     myfont_scale = 1.35,
#'     x_angle = 45,
#'     axis_name_y = "Countries",
#'     axis_name_x = "Time",
#'     alpha_color = 0.9,
#'     indiType = "highBest")
#' }
#'
#'
#' @export
#'
#'
ms_dynam <- function( myTB,
                timeName = "time",
                displace = 0.25,
                displaceh = 0.45,
                dimeFontNum = 5,
                myfont_scale = 1.35,
                x_angle = 45,
                axis_name_y = "Countries",
                axis_name_x = "Time",
                alpha_color = 0.9,
                indiType = "highBest"
                ){
# setup
if(indiType == "highBest"){
    color_rect = c("-1"   = "#ff0000",
                   "-0.5" = "#ffc000",
                   "0"    = "#ffff00",
                   "0.5"  = "#92d050",
                   "1"    = "#00b050")
    #
  }else{ # "lowBest"
    color_rect = c("-1"   = "#00b050",
                   "-0.5" = "#92d050",
                   "0"    = "#ffff00",
                   "0.5"  = "#ffc000",
                   "1"    = "#ff0000")
  #
  }
miniT <- min(myTB[,timeName])
maxiT <- max(myTB[,timeName])
breaks_x <- unlist(unique(myTB[,timeName]))
etichY  <-  names(myTB)[-c(1)]
names(etichY) <-  etichY
myTB2 <- tidyr::gather(myTB, key = "MS",
                       value = "profile",etichY)
myTB2 <- dplyr::mutate(myTB2, position = rep(1:length(etichY),
                                         each=nrow(myTB)))

# make a plot
myG <- ggplot2::ggplot(myTB2,
            ggplot2::aes(x = `time`,y = `position`)) +
  ggplot2::scale_y_discrete(
    axis_name_y,
    labels = etichY,
    limits = etichY
  ) +
  ggplot2::theme(
         axis.text.x=ggplot2::element_text(
         size = ggplot2::rel(myfont_scale ),
         angle = x_angle ,
         vjust = 1,
         hjust=1),
    axis.text.y  = ggplot2::element_text(size = ggplot2::rel(myfont_scale)),
    axis.title.x = ggplot2::element_text(size = ggplot2::rel(myfont_scale)),
    axis.title.y = ggplot2::element_text(size = ggplot2::rel(myfont_scale)),
    panel.grid.minor = ggplot2::element_blank()
  )  +
  ggplot2::scale_fill_manual(values = color_rect) +
  ggplot2::geom_rect(data = myTB2,
            mapping = ggplot2::aes(
              xmin = `time` - displaceh,
              xmax = `time` + displaceh,
              ymin = `position` - displace,
              ymax = `position` + displace,
              fill = factor(`profile`)
            ),
            color = "black", alpha=alpha_color
  ) +
  ggplot2::scale_x_continuous(breaks = breaks_x,
                     labels = breaks_x) +
  ggplot2::xlab(axis_name_x) +
  ggplot2::guides(fill = FALSE) +
  ggplot2::geom_text(data=myTB2,
                     ggplot2::aes(x = `time`,
                                  y = `position`,
                                  label = `profile`),
            size = dimeFontNum, colour = "black")


return(myG)
}

utils::globalVariables(c("time","position","profile"))
