
context("Indicator fiche")



#  undebug(go_indica_fi)


test_that("Basic operation", {


#  debug(go_indica_fi)

testest <- function(){


  tmp <- go_indica_fi(
    workDF = NA,#'emp_20_64_MS' ,
    time_0 = 2004,
    time_t = 2018,
    timeName = 'time',
    indicaT = 'emp_20_64',
    indiType = c('highBest','lowBest')[1],
    seleMeasure = 'all',
    seleAggre = 'EU28',
    x_angle =  45,
    data_res_download =  FALSE,
    auth = 'A.Student',
    dataNow =  '2019/01/31',
    outFile = "test_indica-fi-emp_20_64_MS",
    outDir = tempdir(),
    workTB = emp_20_64_MS
    )

    # browseURL( file.path(tempdir(),'test_indica-fi-emp_20_64_MS.html'))


  tmp <- go_indica_fi(
    workDF = NA,#'emp_20_64_MS' ,
    time_0 = 2004,
    time_t = 2018,
    timeName = 'time',
    indicaT = 'emp_20_64',
    indiType = c('highBest','lowBest')[1],
    seleMeasure = 'all',
    seleAggre = 'EU12',
    x_angle =  45,
    data_res_download =  FALSE,
    auth = 'A.Student',
    dataNow =  '2019/01/31',
    outFile = "test_indica-fi-emp_20_64_MS_ONE",
    outDir = tempdir(),
    workTB = emp_20_64_MS,
    selfContained = TRUE
  )

  # browseURL( file.path(tempdir(),'test_indica-fi-emp_20_64_MS_ONE.html'))




  folder_tmp <- tempdir()

  go_indica_fi(
    time_0 = 2004,
    time_t = 2014,
    timeName = 'time',
    workDF = 'emp_20_64_MS' ,
    indicaT = 'emp_20_64',
    indiType = c('highBest','lowBest')[1],
    seleMeasure = 'all',
    seleAggre = 'EU28',
    x_angle =  45,
    data_res_download =  FALSE,
    auth = 'A.Student',
    dataNow =  '2019/01/31',
    outFile = "test_indica-fi-emp_20_64_MS",
    outDir = folder_tmp
  )

  browseURL(file.path(folder_tmp,'test_indica-fi-emp_20_64_MS.html'))




  go_indica_fi(
    time_0 = 2002,
    time_t = 2010,
    timeName = 'time',
    workDF = 'emp_20_64_MS' ,
    indicaT = 'emp_20_64',
    # indiType = 'highBest',
    indiType = 'lowBest',
    seleMeasure = 'all',
    seleAggre = 'EU28',
    x_angle =  45,
    data_res_download =  FALSE,
    auth = 'A.Student',
    dataNow =  '2019/10/16',
    outFile = "newtest_IT-emp_20_64_MS",
    outDir = folder_tmp
    )

    browseURL(file.path(folder_tmp,'newtest_IT-emp_20_64_MS.html'))










go_indica_fi(
    time_0 = 2002,
    time_t = 2010,
    timeName = 'time',
    workDF = 'emp_20_64_MS' ,
    indicaT = 'emp_20_64',
    # indiType = 'highBest',
    indiType = 'lowBest',
    seleMeasure = 'all',
    seleAggre = 'custom',
    x_angle =  45,
    data_res_download =  FALSE,
    auth = 'A.Student',
    dataNow =  '2019/05/16',
    outFile = "newtest_IT-emp_20_64_MS",
    outDir = tempdir()
  )


browseURL(file.path(tempdir(),'newtest_IT-emp_20_64_MS.html'))



  go_indica_fi(
    time_0 = 2002,
    time_t = 2010,
    timeName = 'time',
    workDF = 'emp_20_64_MS' ,
    indicaT = 'emp_20_64',
    indiType = 'highBest',
    #indiType = 'lowBest',
    #    seleMeasure = c(),
    #seleMeasure = c('beta','sigma'),
    #seleMeasure = c('delta','sigma','beta','gamma'),
    #    seleMeasure = c("delta",'gamma')
    #seleMeasure = c("beta","sigma"),
    seleMeasure = c('all'),
    seleAggre = 'EU28',#Eurozone',
    x_angle =  45,
    data_res_download =  FALSE,
    auth = 'A.Student',
    dataNow =  '2019/05/16',
    outFile = "newtest_IT",
    outDir = tempdir
  )

   browseURL(file.path(tempdir(),'newtest_IT.html'))




  # custom aggregation
  myTTB <- emp_20_64_MS
  dim(myTTB)
  names(myTTB)<- c("time",paste("PP",1:28,sep="~"))

  go_indica_fi(
    time_0 = 2005,
    time_t = 2010,
    timeName = 'time',
    workDF = 'myTTB' ,
    indicaT = 'emp_20_64',
    indiType = 'highBest',
    #indiType = 'lowBest',
    #    seleMeasure = c(),
    #seleMeasure = c('beta','sigma'),
    #seleMeasure = c('delta','sigma','beta','gamma'),
    #    seleMeasure = c("delta",'gamma')
    #seleMeasure = c("beta","sigma"),
    seleMeasure = c('all'),
    seleAggre = 'custom',
    x_angle =  45,
    data_res_download =  FALSE,
    auth = 'A.Student',
    dataNow =  '2019/10/16',
    outFile = "newtest_IT",
    outDir = tempdir()
  )

  browseURL(file.path(tempdir(),'newtest_IT.html'))


  # custom aggergation
   myTTB <-   tibble::tribble(
      ~time, ~UK, ~DE, ~IT,
      2005,   10 , 7  , 6,
      2006,   10 , 7    ,  6, #
      2007,    10,  7 ,   6#
    )

  go_indica_fi(
    time_0 = 2005,
    time_t = 2007,
    timeName = 'time',
    workDF = 'myTTB' ,
    indicaT = 'testerIndica',
    indiType = 'highBest',
    #indiType = 'lowBest',
    #    seleMeasure = c(),
    #seleMeasure = c('beta','sigma'),
    #seleMeasure = c('delta','sigma','beta','gamma'),
    #    seleMeasure = c("delta",'gamma')
    #seleMeasure = c("beta","sigma"),
    seleMeasure = c('all'),
    seleAggre = 'custom',
    x_angle =  45,
    data_res_download =  FALSE,
    auth = 'A.Student',
    dataNow =  '2019/12/07',
    outFile = "indica_custom",
    outDir = tempdir()
  )



  # strict converg highBest
  myTTB <-   tibble::tribble(
    ~time, ~UK, ~DE, ~IT,
    2005,   10 , 5  , 2,
    2006,   12 , 9  , 6, #
    2007,   10,  9  , 6#
  )
  # strict diverg highBest
  myTTB <-   tibble::tribble(
    ~time, ~UK, ~DE, ~IT,
    2005,   10 , 5  , 2,
    2006,   12 , 9  , 6, #
    2007,   25,  9  , 10#
  )


  go_indica_fi(
    time_0 = 2005,
    time_t = 2007,
    timeName = 'time',
    workDF = 'myTTB' ,
    indicaT = 'testerIndica',
    indiType = 'highBest',
    #indiType = 'lowBest',
    #    seleMeasure = c(),
    #seleMeasure = c('beta','sigma'),
    #seleMeasure = c('delta','sigma','beta','gamma'),
    #    seleMeasure = c("delta",'gamma')
    #seleMeasure = c("beta","sigma"),
    seleMeasure = c('all'),
    seleAggre = 'custom',
    x_angle =  45,
    data_res_download =  FALSE,
    auth = 'A.Student',
    dataNow =  '2019/12/07',
    outFile = "indica_custom",
    outDir = tempdir()
  )

   browseURL(file.path(tempdir(),'indica_custom.html'))



  # strict converg lowBest
  myTTB <-   tibble::tribble(
    ~time, ~UK, ~DE, ~IT,
    2005,   10 , 5  , 2,  # 3 7
    2006,   12 , 9  , 6, #
    2007,   7,  6  , 4  #  2, 3
  )
  # strict diverg lowBest
  myTTB <-   tibble::tribble(
    ~time, ~UK, ~DE, ~IT,
    2005,   10 , 4  , 2,
    2006,   12 , 5  , 6, #
    2007,   25,  4  , 10#
  )
  #   diverg lowBest
  myTTB <-   tibble::tribble(
    ~time, ~UK, ~DE, ~IT,
    2005,   10 , 14  , 8,
    2006,   12 , 10  , 7, #
    2007,   25,   7  , 6#
  )

  go_indica_fi(
    time_0 = 2005,
    time_t = 2007,
    timeName = 'time',
    workDF = 'myTTB' ,
    indicaT = 'testerIndica',
    indiType = 'lowBest',
    #indiType = 'lowBest',
    #    seleMeasure = c(),
    #seleMeasure = c('beta','sigma'),
    #seleMeasure = c('delta','sigma','beta','gamma'),
    #    seleMeasure = c("delta",'gamma')
    #seleMeasure = c("beta","sigma"),
    seleMeasure = c('all'),
    seleAggre = 'custom',
    x_angle =  45,
    data_res_download =  FALSE,
    auth = 'A.Student',
    dataNow =  '2019/12/07',
    outFile = "indica_custom",
    outDir = tempdir()
  )


  browseURL(file.path(tempdir(),'indica_custom.html'))





     negaTB <- emp_20_64_MS
     #negaTB[4,3]<- -5.18
     #negaTB[1,5]<- -15.18

     #negaTB[4,3]<- 0
     #negaTB[1,5]<- 0

    go_indica_fi(
      time_0 = 2002,
      time_t = 2010,
      timeName = 'time',
      workDF = 'negaTB' ,
      #workDF = 'emp_20_64_MS' ,
      indicaT = 'emp_20_64',
      # indiType = 'highBest',
      indiType = 'lowBest',
      #seleMeasure = c(),
      #seleMeasure = c('beta'),
      #seleMeasure = c('delta','sigma','beta',  'gamma'),
      #seleMeasure = c('gamma'),
      seleMeasure = c('all'),
      seleAggre = 'EA',
      x_angle =  45,
      data_res_download =  FALSE,
      auth = 'A.Student',
      dataNow =  '2019/05/16',
      outFile = "negafake",
      outDir = tempdir()
    )


    browseURL(file.path(tempdir(),'negafake.html'))



  TB <-  emp_20_64_MS
  TB[15,1]<- 2020
  go_indica_fi(time_0=2002,time_t=2020,
               timeName = 'time',
               workDF = "TB",
               indicaT = 'emp_20_64_MS',
               indiType = c("highBest","lowBest")[1],
               seleMeasure = 'all',
               seleAggre ='EU28',
               dataNow = Sys.time(),
               outFile = 'test_EA_indicator fiche',
               #outDir = 'C:/Users/cli/OneDrive - Eurofound/Policy Brief - Pillar of social rights',
               outDir = tempdir(),
               pdf_out = T)

   browseURL(file.path(tempdir(),'test_EA_indicator fiche.html'))





   lbTB <- structure(list(time = c(2005, 2006, 2007, 2008, 2009, 2010, 2011,
2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019), BE = c(15.7,
15.2, 14.7, 13.4, 12.2, 11.9, 11.5, 11, 10.2, 8.7, 8.3, 9.3,
9.8, 8.4, 8), DK = c(8.6, 9, 8.5, 8.7, 6, 5.7, 6.9, 6.4, 6.3,
7.4, 7.8, 6.9, 6.7, 7, 7.2), FR = c(11.8, 11.2, 10.4, 10.1, 9.4,
9.3, 9.5, 8.9, 8.3, 7.5, 7.2, 7.4, 7.8, 7.6, 7.1), DE = c(12.5,
12.2, 12.4, 12.3, 10.9, 10.7, 10.4, 10.5, 9.6, 9.1, 8.7, 8.2,
7.9, 8.1, 8), EL = c(29.6, 28.6, 28.4, 27.5, 25.6, 24.2, 22.1,
19.8, 19.4, 18.3, 18, 19, 19.7, 21, 20), IE = c(20.5, 19.9, 18.4,
16, 10.2, 8.8, 8.7, 8.6, 10.5, 11.8, 12.3, 12.1, 12.1, 12.2,
12.4), IT = c(26.3, 25.8, 25.8, 24.7, 24, 23.2, 22.6, 21, 19.8,
19.4, 20, 20.1, 19.8, 19.8, 19.6), LU = c(21, 19.5, 17.3, 17.1,
17.5, 17.2, 16.2, 14.4, 14.1, 12.9, 11.7, 11, 7.9, 8, 9.1), NL = c(16.5,
15.9, 15.5, 15, 13.7, 12.7, 12, 11.3, 10.5, 11.4, 11.1, 11, 10.5,
10.1, 9.3), PT = c(12.7, 12.9, 12.8, 12.3, 10.3, 9.8, 8.6, 6.8,
6.4, 7.1, 6.7, 6.8, 7.5, 6.8, 7.2), ES = c(24.7, 23.6, 22, 19,
14.2, 12.9, 11.6, 10, 9.6, 10.2, 11.2, 11.5, 11.9, 12.1, 11.9
), AT = c(12.9, 12.9, 13.3, 12.5, 10.5, 10.2, 10, 9.7, 9.1, 8.2,
8.2, 7.8, 8, 9, 8.8), FI = c(4.3, 4.8, 4.7, 5.3, 2.3, 3, 3.7,
3, 2.8, 1.9, 2.1, 3.3, 3.5, 3.7, 2.7), SE = c(5.3, 5.9, 6, 6.3,
5.2, 6.1, 5.6, 5.1, 5, 4.6, 4.2, 3.8, 4, 4.2, 4.7), CY = c(21.7,
20.3, 18.7, 17, 14.5, 12.9, 11.9, 11.3, 10.4, 7.7, 8.3, 9.7,
9.5, 10.4, 11.6), CZ = c(18.8, 18.6, 19.1, 19.5, 18.8, 18.7,
18.2, 17.7, 17.2, 17.5, 16.6, 16, 15.8, 15.2, 15), EE = c(4.9,
7, 8.8, 8.6, 2, 1.9, 5.7, 5.7, 6.6, 7.7, 7.9, 8.2, 7.3, 7.8,
7.7), HU = c(13.6, 14.5, 14.6, 13.9, 12.5, 10.9, 11.7, 11.1,
12.4, 13.3, 13.7, 14, 15.3, 15.3, 15.5), LV = c(9.8, 10, 10.2,                                                                                                                                                                                                                                                    7.4, 0.3, -0.5, 2.2, 3.6, 4.2, 4.6, 4.1, 2.9, 4.3, 4.2, 3.8),
LT = c(8.4, 6.9, 7.5, 6.9, -0.4, -1.5, 0.6, 1.2, 2.6, 2.5,
2.4, 1.9, 1, 2.3, 1.6), MT = c(44.9, 43.9, 41.3, 39.1, 37.5,
36.6, 35.2, 31.4, 28.6, 26.8, 26.8, 25.5, 24.1, 21.9, 20),
PL = c(13.4, 14.2, 14.7, 15.7, 15, 14, 14.7, 14.5, 14.5,
14.2, 13.8, 14.2, 14.6, 14.4, 15.4), SK = c(15.8, 17.1, 17.3,
17.1, 16.4, 14.5, 15.1, 15.5, 14.4, 14.6, 14.7, 14.2, 12.8,
13.7, 13), SI = c(9.6, 9.8, 10.4, 8.9, 7.7, 7.5, 7, 7.2,
8.2, 8, 8.6, 6.6, 7.2, 7.3, 6.8), BG = c(9.7, 9.5, 9.9, 10.7,
9.8, 7.8, 6.2, 5.6, 5.7, 6.1, 6.6, 7.3, 8, 8.2, 8.6), RO = c(13.5,
12.7, 13.1, 14.3, 14.4, 16.6, 15.3, 16.1, 16.3, 16.7, 17.5,
17.6, 17.1, 18.3, 19), HR = c(14.5, 14.1, 16.2, 15.9, 12.5,
11.5, 12.5, 11.1, 8.8, 10, 9.5, 9.6, 10.6, 10.2, 10.5)), row.names = c(NA,
-15L), class = c("tbl_df", "tbl", "data.frame"))


   go_indica_fi(time_0=2005,time_t=2019,
                timeName = 'time',
                workDF = "lbTB",
                indicaT = 'I.02.01.00',
                indiType = c("highBest","lowBest")[2],
                seleMeasure = 'all',
                seleAggre ='EU27',
                dataNow = Sys.time(),
                outFile = 'test_EA_indica_lb',
                #outDir = 'C:/Users/cli/OneDrive - Eurofound/Policy Brief - Pillar of social rights',
                outDir = tempdir(),
                pdf_out = T)

   browseURL(file.path(tempdir(),'test_EA_indica_lb.html'))






} # incapsulator

  expect_equal(1, 1)
  #rm(TB)
})




