#' Graphical representation based on sigma convergence
#'
#' A ggplot object countries by time where coloured rectangles show if in that
#' time unit the indicator is below one standard deviation (-1) from the mean,
#' above one standard deviation (-1) from the mean or within 2 standard
#' deviations around the mean.
#'
#' Note that  calculation of departure must be already performed by invoking
#' \code{\link{departure_mean}}.
#'
#' @param  myTB  the component $res$departure of an object created by
#'               \code{departure_mean()}
#' @param timeName name of the time variable
#' @param indiType  indicator type, one among "highBest" and "lowBest"
#' @param displace rectangle half height
#' @param displaceh rectangle half base
#' @param dimeFontNum size of font
#' @param myfont_scale axes magnification
#' @param x_angle  angle of x axis labels
#' @param color_rect  colors within rectangles; the default for a "highBest" indicator type is red for
#' "-1", grey for "0" and light sky blue for "1"; the default for a "lowBest" indicator type is light
#'  sky blue for "-1", grey for "0" and red for "1"
#' @param axis_name_y  name of y axis
#' @param axis_name_x name of x axis
#' @param alpha_color transparency
#' @return  a list with component $res made by a ggplot object to be displayed
#'          or saved using ggsave function.
#'
#'
#' @references{\url{https://local.disia.unifi.it/stefanini/RESEARCH/coneu/tutorial-conv.html}}
#'
#'
#' @examples
#'
#' \donttest{
#' # Example 1: "lowBest" indicator type:
#' # Dataframe in the format time by countries:
#' require(tibble)
#' testTB <- dplyr::tribble(
#'    ~time, ~countryA ,  ~countryB,  ~countryC,
#'    2000,     0.8,   2.7,    3.9,
#'    2001,     1.2,   3.2,    4.2,
#'    2002,     0.9,   2.9,    4.1,
#'    2003,     1.3,   2.9,    4.0,
#'    2004,     1.2,   3.1,    4.1,
#'    2005,     1.2,   3.0,    4.0
#'    )
#' mySTB <- sigma_conv(testTB)
#' resDM <-  departure_mean(oriTB=testTB, sigmaTB=mySTB$res)
#' myG <- NULL
#' myG <- graph_departure(resDM$res$departures,
#'                        timeName = "time",
#'                        indiType = "lowBest",
#'                        displace = 0.25,
#'                        displaceh = 0.45,
#'                        dimeFontNum = 6,
#'                        myfont_scale = 1.35,
#'                        x_angle = 45,
#'                        axis_name_y = "Countries",
#'                        axis_name_x = "Time",
#'                        alpha_color = 0.9)
#' # Change the colour of rectangles:
#' myGG <- graph_departure(resDM$res$departures,
#'                        timeName = "time",
#'                        indiType = "lowBest",
#'                        displace = 0.25,
#'                        displaceh = 0.45,
#'                        dimeFontNum = 6,
#'                        myfont_scale = 1.35,
#'                        x_angle = 45,
#'                        color_rect = c("-1"='green4', "0"='yellow',"1"='red'),
#'                        axis_name_y = "Countries",
#'                        axis_name_x = "Time",
#'                        alpha_color = 0.9)
#'
#' # Example 2: "highBest" type of indicator:
#' # Graphical plot of sigma convergence for the emp_20_64_MS Eurofound dataset:
#' data(emp_20_64_MS)
#' mySC <- sigma_conv(emp_20_64_MS)
#' resDMeur <- departure_mean(oriTB = emp_20_64_MS, sigmaTB = mySC$res)
#' myG1 <- NULL
#' myG1 <- graph_departure(resDMeur$res$departures,
#'                         timeName = "time",
#'                         indiType = "highBest",
#'                         displace = 0.25,
#'                         displaceh = 0.45,
#'                         dimeFontNum = 6,
#'                         myfont_scale = 1.35,
#'                         x_angle = 45,
#'                         axis_name_y = "Countries",
#'                         axis_name_x = "Time",
#'                         alpha_color = 0.9)
#'
#' # Plot mean departures for selected countries only and change the colour of rectangles:
#' myG2 <- NULL
#' myG2 <- graph_departure(resDMeur$res$departures[,1:8],
#'                         timeName = "time",
#'                         indiType = "highBest",
#'                         displace = 0.25,
#'                         displaceh = 0.45,
#'                         dimeFontNum = 6,
#'                         myfont_scale = 1.35,
#'                         x_angle = 45,
#'                         color_rect = c("-1"='red', "0"='yellow',"1"='green4'),
#'                         axis_name_y = "Countries",
#'                         axis_name_x = "Time",
#'                         alpha_color = 0.9)
#' }
#' @export
#'
#'
graph_departure <- function( myTB,
                timeName = "time",
                indiType =  "highBest",
                displace = 0.25,
                displaceh = 0.45,
                dimeFontNum = 6,
                myfont_scale = 1.35,
                x_angle = 45,
                color_rect = c("-1"='red1', "0"='gray80',"1"='lightskyblue1'),
                axis_name_y = "Countries",
                axis_name_x = "Time",
                alpha_color = 0.9
                ){
    obj_res <- convergEU_glb()$tmpl_out
    if(indiType == "lowBest"){
        newcol <- as.character(color_rect)
        color_rect[1] <-  newcol[c(3)]
        color_rect[3] <-  newcol[c(1)]
    } else if(indiType != "highBest"){
        obj_res$err <- "Wrong indicator type."
        return(obj_res)
    }
    miniT <- min(myTB[[timeName]])
    maxiT <- max(myTB[[timeName]])
    breaks_x <- seq(miniT,maxiT,length=nrow(myTB))
    etichY  <-  names(myTB)[-c(1,2,3,4,5)]
    names(etichY) <-  etichY
    myTB2 <- tidyr::gather(myTB[-c(2,3,4,5)], key = "MS",
                           value = "profile",etichY)
    myTB2 <- dplyr::mutate(myTB2, position = rep(1:length(etichY),
                                             each=nrow(myTB)))

    # make a plot
    myG <- ggplot2::ggplot(myTB2,
                ggplot2::aes(x = `time`, y = `position`)) +
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
                        # xmin = myTB2$`time` - displaceh,
                        # xmax = myTB2$`time` + displaceh,
                        # ymin = myTB2$`position` - displace,
                        # ymax = myTB2$`position` + displace,
                        # fill = factor(myTB2$`profile`)
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
      # ggplot2::geom_text(data=myTB2,
      #                    ggplot2::aes(x = myTB2$`time`,
      #                                 y = myTB2$`position`,
      #                                 label = myTB2$`profile`),
      #           size = dimeFontNum, colour = "black")
      ggplot2::geom_text(data=myTB2,
                        ggplot2::aes(x = `time`,
                                     y = `position`,
                                     label = `profile`),
                        size = dimeFontNum, colour = "black")

    obj_res$res <- myG
return(obj_res)
}


utils::globalVariables(c("time","position","profile"))
