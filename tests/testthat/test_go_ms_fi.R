
context("Scoreboard numerical calculations")




#  undebug(go_ms_fish)
#  debug(go_ms_fish)


test_that("Basic operation", {


testest <- function(){


  myOutDir <- tempdir()


  go_ms_fi(
    workDF =NA,#'emp_20_64_MS',
    countryRef ='IT',
    otherCountries = 'NA',#c('DE')",
    time_0 = 2005,
    time_t = 2010,
    tName = 'time',
    indiType = "highBest",
    aggregation= 'EU28',
    x_angle=  45,
    dataNow=  Sys.time(),
    author = 'A.Student',
    outFile = "primoTest",#"test_emp_20_64_MS.html",
    outDir = myOutDir,
    indiName= 'emp_20_64_MS',
    workTB= emp_20_64_MS
    )
  browseURL(paste0(myOutDir,'/primoTest.html'))





  go_ms_fi(
    workDF ='emp_20_64_MS',
    countryRef ='IT',
    otherCountries = 'NA',#c('DE')",
    time_0 = 2005,
    time_t = 2010,
    tName = 'time',
    indiType = "highBest",
    aggregation= 'EU28',
    x_angle=  45,
    dataNow=  Sys.time(),
    author = 'A.Student',
    outFile = "primoTest",#"test_emp_20_64_MS.html",
    outDir = myOutDir,
    indiName= 'emp_20_64_MS',
    workTB=NULL
  )
  browseURL(paste0(myOutDir,'/primoTest.html'))








  go_ms_fi(
    workDF ='emp_20_64_MS',
    countryRef ='IT',
    otherCountries = 'c("DE","UK")',
    time_0 = 2005,
    time_t = 2015,
    tName = 'time',
    aggregation= 'EU28',
    x_angle=  45,
    dataNow=  Sys.time(),
    indiType = "highBest",
    author = 'A.Student',
    outFile = NA,#"test_emp_20_64_MS.html",
    outDir = myOutDir,
    indiName= "emp_20_64_MS")

    # browseURL(  )


  go_ms_fi(
    workDF ='myTB',
    countryRef ='IT',
    otherCountries = 'c("DE","UK","FR")',
    time_0 = 2005,
    time_t = 2015,
    tName = 'time',
    indiType = "highBest",
    aggregation= 'EU28',
    x_angle=  45,
    dataNow=  Sys.time(),
    author = 'A.Student',
    outFile = NA,#"test_emp_20_64_MS.html",
    outDir = myOutDir,
    indiName= 'emp_20_64_MS'
  )


  go_ms_fi(
        workDF ='myTB',
        countryRef ='DE',
        #otherCountries = "c('IT','UK','FR','BE','AT')",
        #otherCountries = "c('IT','FR','RO','MT')",
        otherCountries = 'c("CZ", "DK", "EE", "EL",
        "ES", "FI", "FR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT",
        "NL", "PL", "PT", "RO", "SE", "SI", "SK", "UK")',
        time_0 = 2003,#2002,
        time_t = 2014,#2015,
        tName = 'time',
        indiType =  "lowBest",
        #indiType =  "highBest",#"lowBest",
        aggregation= 'EU28',
        #aggregation= 'EA',
        x_angle=  45,
        dataNow=  Sys.time(),
        author = 'A.Student',
        outFile = 'Counfic-DE-2015',#"test_emp_20_64_MS.html",
        outDir = myOutDir,
        indiName= 'lifeAsRun'
       )





       myTTB <- extract_indicator_EUF(
             indicator_code= "lifesatisf",
             fromTime = 2000,
             toTime = 2018,
             countries =  convergEU_glb()$EU28$memberStates$codeMS)$res
       myTTB <- impute_dataset(myTTB,"HR")$res[,-2]
       go_ms_fi(
         workDF ='myTTB',
         indiName= "lifesatisf",
         countryRef ='DE',
         #otherCountries = "c('IT','UK','FR','BE','AT')",
         #otherCountries = "c('IT','FR')",
         otherCountries = "c(NA,NA)",
         #otherCountries = "c('IT','UK','FR','BE','FI')",
         time_0 = 2003,#2002,
         time_t = 2016,#2015,
         tName = 'time',
         indiType =  "highBest",
         #indiType =  "highBest",#"lowBest",
         #aggregation= 'EU12',
         aggregation= 'EU28',
         x_angle=  45,
         dataNow=  Sys.time(),
         author = 'A.Student',
         outFile = 'lifesta-AT-03_16',#"test_emp_20_64_MS.html",
         outDir = myOutDir
       )




       myTTB <- emp_20_64_MS
       dim(myTTB)
       names(myTTB)<- c("time",paste("PP",1:28,sep="@"))

       go_ms_fi(
         workDF ='myTTB',
         indiName= "lifesatisf",
         countryRef ='PP@21',
         otherCountries = "c('PP@1','PP@24','PP@11','PP@28','PP@13')",
         #otherCountries = "c(NA,NA)",
         time_0 = 2005,#2002,
         time_t = 2010,#2015,
         tName = 'time',
         indiType =  "highBest",
         #indiType =  "highBest",#"lowBest",
         #aggregation= 'EU12',
         aggregation= 'custom',
         x_angle=  45,
         dataNow=  Sys.time(),
         author = 'A.Student',
         outFile = 'country-test-custom',
         outDir = myOutDir
       )


       myTTB <- emp_20_64_MS
       dim(myTTB)
       names(myTTB)<- c("time",paste("PP",1:28,sep="@"))

         go_ms_fi(
           #workDF ='',
           indiName= "lifesatisf",
           countryRef ='PP@21',
           otherCountries = "c('PP@1','PP@24','PP@11','PP@28','PP@13')",
           #otherCountries = "c(NA,NA)",
           time_0 = 2005,#2002,
           time_t = 2010,#2015,
           tName = 'time',
           indiType =  "highBest",
           #indiType =  "highBest",#"lowBest",
           #aggregation= 'EU12',
           aggregation= 'custom',
           x_angle=  45,
           dataNow=  Sys.time(),
           author = 'A.Student',
           outFile = 'country-test-custom',
           outDir = myOutDir,
           workTB = myTTB
         )


} # incapsulator

  expect_equal(1, 1)

})




