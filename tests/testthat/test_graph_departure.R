
context("Graphical representation from sigma convergence.")


# debug(graph_departure)
# undebug(graph_departure)


test_that("Simplest test", {

  myDF <- dplyr::tibble(
    time = c(2000,2001,2002,2003,2004,2005),
    time2 = c(2000,2001,2002,2003,2004,2005),
    time3 = c(2000,2001,2002,2003,2004,2005),
    time4 = c(2000,2001,2002,2003,2004,2005),
    IT =  base::sample(c(-1,0,1),6,rep=T),
    DE = base::sample(c(-1,0,1),6,rep=T),
    UK = base::sample(c(-1,0,1),6,rep=T)
  )

  # myTB <- tidyr::gather(myDF,key="MS", value= profile, IT,DE,UK)
  # myTB <- tidyr::gather(myDF,key="MS", value= "profile", "IT","DE","UK")
  # myTB <- dplyr::mutate(myTB, position=rep(c(1,2,3),each=6))
  # #myTB
  myG <- NULL
  myG <- graph_departure( tibble::as_tibble(myDF),
      timeName = "time",
      displace = 0.25,
      displaceh = 0.45,
      dimeFontNum = 6,
      myfont_scale = 1.35,
      x_angle = 45,
      color_rect = c("-1"='red1', "0"='gray80',"1"='lightskyblue1'),
      axis_name_y = "Countries",
      axis_name_x = "Time",
      alpha_color = 0.9
    )
  expect_false(is.null(myG))

})



# debug(graph_departure)

test_that("LowBest and highBest", {
      testTB <- dplyr::tribble(
          ~time, ~countryA ,  ~countryB,  ~countryC,
          2000,     0.8,   2.7,    3.9,
          2001,     1.2,   3.2,    4.2,
          2002,     0.9,   2.9,    4.1,
          2003,     1.3,   2.9,    4.0,
          2004,     1.2,   3.1,    4.1,
          2005,     1.2,   3.0,    4.0
      )
      mySTB <- sigma_conv(testTB)
      resDM <-  departure_mean(oriTB=testTB, sigmaTB=mySTB$res)
      myG <- graph_departure(resDM$res$departures,
                             timeName = "time",
                             indiType ="highBest",
                             displace = 0.25,
                             displaceh = 0.45,
                             dimeFontNum = 6,
                             myfont_scale = 1.35,
                             x_angle = 45,
                             color_rect = c("-1"='red1', "0"='gray80',"1"='green2'),
                             axis_name_y = "Countries",
                             axis_name_x = "Time",
                             alpha_color = 0.9)
      #
      expect_null(myG$err)

      myG <- graph_departure(resDM$res$departures,
                             timeName = "time",
                             indiType ="lowBest",
                             displace = 0.25,
                             displaceh = 0.45,
                             dimeFontNum = 6,
                             myfont_scale = 1.35,
                             x_angle = 45,
                             color_rect = c("-1"='red1', "0"='gray80',"1"='green2'),
                             axis_name_y = "Countries",
                             axis_name_x = "Time",
                             alpha_color = 0.9)
      expect_null(myG$err)

})

