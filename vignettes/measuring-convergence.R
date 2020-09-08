## ----setup, include = FALSE---------------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyverse)
library(eurostat)
library(purrr)
library(tibble)
library(tidyr)
library(formattable) 
library(kableExtra)
library(caTools)

library(convergEU)

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)


## ----eval=FALSE---------------------------------------------------------------
#  data("emp_20_64_MS",package = "convergEU")
#  head(emp_20_64_MS)

## ----eval=FALSE---------------------------------------------------------------
#  data(package = "convergEU")

## ----eval=FALSE---------------------------------------------------------------
#  help(emp_20_64_MS)
#  

## -----------------------------------------------------------------------------
data(dbEurofound)
head(dbEurofound)

## -----------------------------------------------------------------------------
names(dbEurofound)

## -----------------------------------------------------------------------------
c(min(dbEurofound$time), max(dbEurofound$time))

## -----------------------------------------------------------------------------
data(dbEUF2018meta)
print(dbEUF2018meta,n=20,width=100)

## -----------------------------------------------------------------------------
convergEU_glb()$EU12$memberStates$codeMS

## -----------------------------------------------------------------------------
myTB <- extract_indicator_EUF(
    indicator_code = "lifesatisf", #Code_in_database
    fromTime=2003,
    toTime=2016,
    gender= c("Total","Females","Males")[2],
    countries= convergEU_glb()$EU12$memberStates$codeMS
    )
  
myTB

## -----------------------------------------------------------------------------
print(dbEUF2018meta,n=20,width=100)
 
names(convergEU_glb())
myTB <- extract_indicator_EUF(
    indicator_code = "JQIintensity_i", #Code_in_database
    fromTime= 1965,
    toTime=2016,
    gender= c("Total","Females","Males")[1],
    countries= convergEU_glb()$EU27_2020$memberStates$codeMS
    )
  
print(myTB$res,n=35,width=250)

## ----out.width="100%"---------------------------------------------------------
myTBinp <- impute_dataset(myTB$res, timeName = "time",
                          countries=convergEU_glb()$EU27_2020$memberStates$codeMS,
                          tailMiss = c("cut", "constant")[2],
                          headMiss = c("cut", "constant")[2]) 
print(myTBinp$res,n=35,width=250)

## -----------------------------------------------------------------------------
check_data(emp_20_64_MS)

## -----------------------------------------------------------------------------
tmp <-  emp_20_64_MS
tmp <-  mutate(tmp, time=factor(emp_20_64_MS$time))
check_data(tmp)

## -----------------------------------------------------------------------------
tmp <-  emp_20_64_MS 
tmp[3:6,1]<- NA
check_data(tmp)

## -----------------------------------------------------------------------------

myTB <- extract_indicator_EUF(
    indicator_code = "exposdiscr_p", #Code_in_database
    fromTime=1966,
    toTime=2016,
    gender= c("Total","Females","Males")[1],
    countries= convergEU_glb()$EU12$memberStates$codeMS
    )

## -----------------------------------------------------------------------------
sapply(myTB$res,function(vx)sum(is.na(vx)))

## -----------------------------------------------------------------------------
set.seed(1999)
myTB2 <- dplyr::bind_rows(myTB$res,myTB$res,myTB$res)
myTB2 <- dplyr::mutate(myTB2, time= seq(1975,2015,5))
for(aux in 3:14){
  myTB2[[aux]] <-   myTB2[[aux]] + c(runif(6,-2.5,2.5),0,0,0)
}

## -----------------------------------------------------------------------------
myTB2[["BE"]][1:2] <-  NA
myTB2[["DE"]][8:9] <-  NA
myTB2[["IT"]][c(3,4, 6,7,8)] <-  NA
myTB2[["DK"]][6] <-  NA
myTB2

## -----------------------------------------------------------------------------
toBeProcessed <- c( "IT","BE", "DE", "DK","UK")
# debug(impute_dataset)

impute_dataset(myTB2, countries=toBeProcessed,
                            timeName = "time",
                            tailMiss = c("cut", "constant")[1],
                            headMiss = c("cut", "constant")[1]) 

impute_dataset(myTB2, countries=toBeProcessed,
                            timeName = "time",
                            tailMiss = c("cut", "constant")[2],
                            headMiss = c("cut", "constant")[1]) 


## -----------------------------------------------------------------------------
#library(ggplot2)
#library(dplyr)
#library(tibble)

testTB <- tribble(
  ~time, ~countryA ,  ~countryB,  ~countryC,
    2000,     0.8,   2.7,    3.9,
    2001,     1.2,   3.2,    4.2,
    2002,     0.9,   2.9,    4.1,
    2003,     1.3,   2.9,    4.0,
    2004,     1.2,   3.1,    4.1,
    2005,     1.2,   3.0,    4.0
  )
 
res <- beta_conv(tavDes = testTB, time_0 = 2002, time_t = 2004, 
                 all_within = TRUE, 
                 timeName = "time")
res

## -----------------------------------------------------------------------------
res <- beta_conv(tavDes = testTB, time_0 = 2002, time_t = 2004, 
                 all_within = FALSE, 
                 timeName = "time")
res


## -----------------------------------------------------------------------------
testTB <- tribble(
  ~time, ~countryA ,  ~countryB,  ~countryC,
    2000,     0.8,   2.7,    3.9,
    2001,     1.2,   3.2,    4.2,
    2002,     0.9,   2.9,    4.1,
    2003,     1.3,   2.9,    4.0,
    2004,     1.2,   3.1,    4.1,
    2005,     1.2,   3.0,    4.0
  )

sigma_conv(testTB,timeName="time")

## -----------------------------------------------------------------------------
sigma_conv(testTB,timeName="time",time_0 = 2002,time_t = 2004)
sigma_conv(testTB,time_0 = 2002,time_t = 2004)


## -----------------------------------------------------------------------------
data(emp_20_64_MS)
mySTB <- sigma_conv(emp_20_64_MS)
mySTB

## -----------------------------------------------------------------------------
res <- departure_mean(oriTB = emp_20_64_MS, sigmaTB = mySTB$res)
names(res$res)
res$res$departures

## -----------------------------------------------------------------------------
res$res$squaredContrib

## -----------------------------------------------------------------------------
##  sigma_conv(testTB,timeName="time",time_0 = 2002,time_t = 2004)
res$res$devianceContrib

## ----eval=T,fig.width=7,fig.height=9------------------------------------------
myGG <- graph_departure(res$res$departures,
                timeName = "time",
                displace = 0.25,
                displaceh = 0.45,
                dimeFontNum = 4,
                myfont_scale = 1.35,
                x_angle = 45,
                color_rect = c("-1"='red1', "0"='gray80',"1"='lightskyblue1'),
                axis_name_y = "Countries",
                axis_name_x = "Time",
                alpha_color = 0.9
                )
myGG


## ----eval=T-------------------------------------------------------------------
#myWW1<- warnings()
myGG <- graph_departure(res$res$departures[1:10],
                timeName = "time",
                displace = 0.25,
                displaceh = 0.45,
                dimeFontNum = 4,
                myfont_scale = 1.35,
                x_angle = 45,
                color_rect = c("-1"='red1', "0"='gray80',"1"='lightskyblue1'),
                axis_name_y = "Countries",
                axis_name_x = "Time",
                alpha_color = 0.29
                )

myGG

## -----------------------------------------------------------------------------
gamma_conv(emp_20_64_MS,2002,2016)

## -----------------------------------------------------------------------------
(timeCounTB <- testTB)

## -----------------------------------------------------------------------------
tmp <- c( 3, 6, 9, 1, 12)
rank(tmp)

## -----------------------------------------------------------------------------
# debug(gamma_conv)
(gamma_conv(timeCounTB,ref=2000,last=2005,timeName = "time"))
(gamma_conv(timeCounTB,ref=2000,last=2004,timeName = "time"))
(gamma_conv(timeCounTB,ref=2000,last=2003,timeName = "time"))
(gamma_conv(timeCounTB,ref=2000,last=2002,timeName = "time"))
(gamma_conv(timeCounTB,ref=2000,last=2001,timeName = "time"))

## -----------------------------------------------------------------------------
(gamma_conv(timeCounTB,ref=2001,last=2005,timeName = "time"))
(gamma_conv(timeCounTB,ref=2002,last=2004,timeName = "time"))

## -----------------------------------------------------------------------------
timeCounTB2 <- timeCounTB
timeCounTB2[2,2:4] <-  timeCounTB[2,4:2]
timeCounTB2[4,2:4] <-  timeCounTB[4,c(4,2,3)]
timeCounTB2

gamma_conv(timeCounTB2,last=2005,ref=2000, timeName = "time",printRanks = T)

## -----------------------------------------------------------------------------
timeCounTB3 <- cbind(timeCounTB[1],t(apply(timeCounTB,1,
                                        function(vet)vet[sample(2:4,3)])))


timeCounTB3
(gamma_conv(timeCounTB3,last=2005,ref=2000, timeName = "time",printRanks = T))

## ----echo=FALSE,eval=FALSE----------------------------------------------------
#  
#  timeCounTB <- tribble(
#    ~time, ~countryA ,  ~countryB,  ~countryC,
#      0,     0.8,   2.7,    3.9,
#      1,     1.2,   3.2,    4.2,
#      2,     0.9,   2.9,    4.1,
#      3,     1.3,   2.9,    4.0,
#      4,     1.2,   3.1,    4.1,
#      5,     1.2,   3.0,    4.0
#    )
#  timeCounTB

## -----------------------------------------------------------------------------
delta_conv(timeCounTB)

## -----------------------------------------------------------------------------
data(emp_20_64_MS)
mySTB <- abso_change(emp_20_64_MS, 
                        time_0 = 2005, 
                        time_t = 2010,
                        all_within=TRUE,
                        timeName = "time")
names(mySTB$res)

## -----------------------------------------------------------------------------
mySTB$res$abso_change

## -----------------------------------------------------------------------------
round(mySTB$res$sum_abs_change,4)

## -----------------------------------------------------------------------------
round(mySTB$res$average_abs_change,4)

## -----------------------------------------------------------------------------
workDF <- extract_indicator_EUF(
  indicator_code ="lifesatisf", #Code_in_database
  fromTime=2000,
  toTime =2018,
  gender= c("Total","Females","Males")[1],
  countries =  convergEU_glb()$EU27_2020$memberStates$codeMS)
workDF

wDF <- workDF$res

## -----------------------------------------------------------------------------
check_data(select(wDF,-sex),timeName="time")

## -----------------------------------------------------------------------------
wDFI <- impute_dataset(select(wDF,-sex),
               countries= names(select(wDF,-sex,-time)),
               timeName = "time",
               tailMiss = c("cut", "constant")[2],
               headMiss = c("cut", "constant")[1])

## -----------------------------------------------------------------------------
check_data(wDFI$res,timeName="time")

## -----------------------------------------------------------------------------
wwTB <- (wDFI$res %>%
   average_clust(timeName="time",cluster="EU27"))$res

wwTB$EU27

## -----------------------------------------------------------------------------
mini_EU <- min(wwTB$EU27)
maxi_EU <- max(wwTB$EU27)

qplot(time, EU27, data=wwTB,
      ylim=c(mini_EU,maxi_EU))+geom_line(colour="navy blue")+
      ylab("lifesatisf")

## -----------------------------------------------------------------------------
betaRes <- beta_conv(wDFI$res,time_0=2007, time_t=2011, all_within=FALSE)
betaRes 

## ----out.width="100%"---------------------------------------------------------
mybetaplot<-beta_conv_graph(betaRes,
                            indiName = 'Mean Life Satisfaction',
                            time_0 = 2007,
                            time_t = 2011)
mybetaplot

## -----------------------------------------------------------------------------
mysigmares<-sigma_conv(wwTB)
#mysigmares

## ----fig.width=5,fig.height=4,out.width="65%"---------------------------------
mysigmaplot<-sigma_conv_graph(sigmaconvOut=mysigmares, 
         time_0 = 2007, 
         time_t = 2011,
        aggregation='EU27_2020')
mysigmaplot

## -----------------------------------------------------------------------------
workDF <- extract_indicator_EUF(
  indicator_code ="lifesatisf", #Code_in_database
  fromTime=2000,
  toTime =2018,
  gender= c("Total","Females","Males")[1],
  countries =  convergEU_glb()$EU27_2020$memberStates$codeMS)
wDFI <- impute_dataset(select(workDF$res,-sex),
               countries= names(select(wDF,-sex,-time)),
               timeName = "time",
               tailMiss = c("cut", "constant")[2],
               headMiss = c("cut", "constant")[1])

check_data(wDFI$res,timeName="time")

## -----------------------------------------------------------------------------
gamma_conv(wDFI$res,ref=2003,last=2016,timeName = "time")

## -----------------------------------------------------------------------------
tmpRes <- gamma_conv(wDFI$res,ref=2007,last=2011,timeName = "time")

## -----------------------------------------------------------------------------
wDFI$res

## -----------------------------------------------------------------------------
gamma_conv_msteps(wDFI$res,
                  startTime=2003, 
                  endTime=2016,
                  timeName = "time")


## -----------------------------------------------------------------------------
delta_conv(wwTB)

## -----------------------------------------------------------------------------
delta_conv(wwTB,"time", extended=TRUE)

## -----------------------------------------------------------------------------
res1<-demea_change(wwTB,
                   timeName="time",
                   time_0 = 2003,
                   time_t = 2016,
                   sele_countries= NA,
                   doplot=TRUE)
res1

## ----fig.width = 6,out.width="100%"-------------------------------------------
plot(res1$res$res_graph)

## -----------------------------------------------------------------------------
convergEU_glb()$Eurozone

## -----------------------------------------------------------------------------
convergEU_glb()$EU27_2020

## -----------------------------------------------------------------------------
testTB <- emp_20_64_MS
average_clust(testTB,timeName = "time",cluster = "EU27")$res[,c(1,30)]

## -----------------------------------------------------------------------------
average_clust(testTB,timeName = "time",cluster = "EU12")$res[,c(1,30)]

## -----------------------------------------------------------------------------
average_clust(testTB,timeName = "TTime",cluster = "EUspirit")

## ----out.width="65%"----------------------------------------------------------
intervalTime <-  c(1999,2000,2001) 
intervalMeasure <- c( 66.5, NA,87.2) 
currentData <- tibble(time= intervalTime, veval= intervalMeasure) 
currentData 
resImputed <- impute_dataset(currentData,
                           countries = "veval",
                           timeName = "time",
                           tailMiss = c("cut", "constant")[2],
                           headMiss = c("cut", "constant")[2]) 
resImputed  

## ----echo=FALSE,out.width="65%"-----------------------------------------------
tmp <-  as.data.frame(currentData[ c(1,3),] )
tmp2 <- as.data.frame(resImputed$res[2,] )
 
myg <- ggplot(as.data.frame(resImputed$res),  mapping=aes(x=time,y=veval)) + 
  geom_point() + 
  geom_line(data=resImputed$res,col="red") + 
  geom_point(data=tmp,mapping=aes(x=time,y=veval), 
              size=4, 
              colour="blue")  + 
  geom_point(data= tmp2, 
             aes(x=time,y=veval),size=4,alpha=1/3,col="black") + 
  xlab("Time") + ylab("Measure / Index") +  
  ggtitle( "Blue points are observed values (grey ones are missing) \n") 
   
myg 

## -----------------------------------------------------------------------------
intervalTime <-  c(1999,2000,2001,2002,2003) 
intervalMeasure <- c( 66.5, NA,NA,NA,87.2) 
currentData <- tibble(time= intervalTime, veval= intervalMeasure) 
currentData
resImputed <- impute_dataset(currentData,
                           countries = "veval",
                           timeName = "time",
                           tailMiss = c("cut", "constant")[2],
                           headMiss = c("cut", "constant")[2]) 
tmp <-  as.data.frame(currentData[ c(1,5),] )
tmp2 <- as.data.frame(resImputed$res[2:4,] )

resImputed  

## ----echo=FALSE,out.width="65%"-----------------------------------------------
myg <- ggplot(as.data.frame(resImputed$res),  mapping=aes(x=time,y=veval)) + 
  geom_point() + 
  geom_line(data=resImputed$res,col="red") + 
  geom_point(data=tmp,mapping=aes(x=time,y=veval), 
              size=4, 
              colour="blue")  + 
  geom_point(data= tmp2, 
             aes(x=time,y=veval),size=4,alpha=1/3,col="black") + 
  xlab("Time") + ylab("Measure / Index") +  
  ggtitle( "Blue points are observed values (grey ones are missing) \n") 
   
myg 


## -----------------------------------------------------------------------------
workTB <- dplyr::select(emp_20_64_MS, time, IT,DE)
check_data(workTB)

## -----------------------------------------------------------------------------
resSM <- smoo_dataset(select(workTB,-time), leadW = 0.149, timeTB= select(workTB,time))
resSM

## -----------------------------------------------------------------------------
tmpSM <- dplyr::rename(dplyr::select(resSM,-time),IT1=IT,DE1=DE)
compaTB <- dplyr::select(bind_cols(workTB, tmpSM), time,IT,IT1,DE,DE1)
compaTB

## ----out.width="70%"----------------------------------------------------------
qplot(time,IT,data=compaTB) + 
  geom_line(colour="navyblue") +
  geom_line(aes(x=time,y=IT1),colour="red") +
  geom_point(aes(x=time,y=IT1),colour="red",shape=8)

## ----out.width="70%"----------------------------------------------------------
qplot(time,DE,data=compaTB) + 
  geom_line(colour="navyblue") +
  geom_line(aes(x=time,y=DE1),colour="red") +
  geom_point(aes(x=time,y=DE1),colour="red",shape=8)


## ----out.width="70%"----------------------------------------------------------
resSM <- smoo_dataset(dplyr::select(workTB,-time), leadW = 1,
                      timeTB= dplyr::select(workTB,time))
resSM <- dplyr::rename(resSM,IT1=IT, DE1=DE)
compaTB <- dplyr::select(dplyr::bind_cols(workTB, 
                     dplyr::select(resSM,-time)), time,IT,IT1,DE,DE1)
qplot(time,IT,data=compaTB) + 
  geom_line(colour="navyblue") +
  geom_line(aes(x=time,y=IT1),colour="red") +
  geom_point(aes(x=time,y=IT1),colour="red",shape=8)

## -----------------------------------------------------------------------------
data(emp_20_64_MS)
cuTB <- dplyr::tibble(ITori =emp_20_64_MS$IT)
cuTB <- dplyr::mutate(cuTB,time =emp_20_64_MS$time)

## -----------------------------------------------------------------------------

cuTB <-  dplyr:: mutate(cuTB, IT_k_3= caTools::runmean(emp_20_64_MS$IT, k=3, 
        alg=c("C", "R", "fast", "exact")[4],
        endrule=c("mean", "NA", "trim", "keep", "constant", "func")[4],
        align = c("center", "left", "right")[1]))

cuTB <-  dplyr:: mutate(cuTB, IT_k_5= caTools::runmean(emp_20_64_MS$IT, k=5, 
        alg=c("C", "R", "fast", "exact")[4],
        endrule=c("mean", "NA", "trim", "keep", "constant", "func")[4],
        align = c("center", "left", "right")[1]))

cuTB <-  dplyr:: mutate(cuTB, IT_k_7= caTools::runmean(emp_20_64_MS$IT, k=7, 
        alg=c("C", "R", "fast", "exact")[4],
        endrule=c("mean", "NA", "trim", "keep", "constant", "func")[4],
        align = c("center", "left", "right")[1]))


## -----------------------------------------------------------------------------
myG <- ggplot(cuTB,aes(x=time,y=ITori))+geom_line()+geom_point()+
       geom_line(aes(x=time,y=IT_k_3),colour="red")+
       geom_point(aes(x=time,y=IT_k_3),colour="red")+
       #
       geom_line(aes(x=time,y=IT_k_5),colour="blue")+
       geom_point(aes(x=time,y=IT_k_5),colour="blue")+
       #
       geom_line(aes(x=time,y=IT_k_7),colour="orange")+
       geom_point(aes(x=time,y=IT_k_7),colour="orange")+
       theme(legend.position = c(.5, .5),
              legend.title = element_text(face = "bold"))

myG

## -----------------------------------------------------------------------------
cuTB <- dplyr::mutate(cuTB, DEori =emp_20_64_MS$DE)

cuTB <-  dplyr:: mutate(cuTB, DE_k_3= runmean(emp_20_64_MS$DE, k=3, 
        alg=c("C", "R", "fast", "exact")[4],
        endrule=c("mean", "NA", "trim", "keep", "constant", "func")[4],
        align = c("center", "left", "right")[1]))

cuTB <-  dplyr:: mutate(cuTB, DE_k_5= runmean(emp_20_64_MS$DE, k=5, 
        alg=c("C", "R", "fast", "exact")[4],
        endrule=c("mean", "NA", "trim", "keep", "constant", "func")[4],
        align = c("center", "left", "right")[1]))

cuTB <-  dplyr:: mutate(cuTB, DE_k_7= runmean(emp_20_64_MS$DE, k=7, 
        alg=c("C", "R", "fast", "exact")[4],
        endrule=c("mean", "NA", "trim", "keep", "constant", "func")[4],
        align = c("center", "left", "right")[1]))


## -----------------------------------------------------------------------------
myG <- ggplot(cuTB,aes(x=time,y=DEori))+geom_line()+geom_point()+
       geom_line(aes(x=time,y=DE_k_3),colour="red")+
       geom_point(aes(x=time,y=DE_k_3),colour="red")+
       #
       geom_line(aes(x=time,y=DE_k_5),colour="blue")+
       geom_point(aes(x=time,y=DE_k_5),colour="blue")+
       #
       geom_line(aes(x=time,y=DE_k_7),colour="orange")+
       geom_point(aes(x=time,y=DE_k_7),colour="orange")+
       theme(legend.position = c(.5, .5),
              legend.title = element_text(face = "bold"))

myG

## -----------------------------------------------------------------------------
cuTB <-  emp_20_64_MS[,c("time","IT","DE")]

ma_dataset(cuTB, kappa=3, timeName= "time")

## -----------------------------------------------------------------------------
data(emp_20_64_MS)
resTB <- scoreb_yrs(emp_20_64_MS,timeName = "time")
resTB

## -----------------------------------------------------------------------------
# library(ggplot2)
data(emp_20_64_MS)
selectedCountry <- "IT"
timeName <-  "time"
myx_angle <-  45

outSig <- sigma_conv(emp_20_64_MS, timeName = timeName,
           time_0=2002,time_t=2016)
miniY <- min(emp_20_64_MS[,- which(names(emp_20_64_MS) == timeName )])
maxiY <-  max(emp_20_64_MS[,- which(names(emp_20_64_MS) == timeName )])
estrattore<-  emp_20_64_MS[[timeName]] >= 2002  &  emp_20_64_MS[[timeName]] <= 2016
ttmp <- cbind(outSig$res, dplyr::select(emp_20_64_MS[estrattore,], -contains(timeName)))

myG2 <- 
  ggplot(ttmp) + ggtitle(
  paste("EU average (black, solid) and country",selectedCountry ," (red, dotted)") )+
  geom_line(aes(x=ttmp[,timeName], y =ttmp[,"mean"]),colour="black") +
  geom_point(aes(x=ttmp[,timeName],y =ttmp[,"mean"]),colour="black") +
#        geom_line()+geom_point()+
    ylim(c(miniY,maxiY)) + xlab("Year") +ylab("Indicator") +
  theme(legend.position = "none")+
  # add countries
  geom_line( aes(x=ttmp[,timeName], y = ttmp[,"IT"],colour="red"),linetype="dotted") + 
  geom_point( aes(x=ttmp[,timeName], y = ttmp[,"IT"],colour="red")) +
  ggplot2::scale_x_continuous(breaks = ttmp[,timeName],
                     labels = ttmp[,timeName]) +
   ggplot2::theme(
         axis.text.x=ggplot2::element_text(
         #size = ggplot2::rel(myfont_scale ),
         angle = myx_angle 
         #vjust = 1,
         #hjust=1
         ))
  
myG2

## ----fig.height=11------------------------------------------------------------
obe_lvl <- scoreb_yrs(emp_20_64_MS,timeName = timeName)$res$sco_level_num
# select subset of time
estrattore <- obe_lvl[[timeName]] >= 2009 & obe_lvl[[timeName]] <= 2016  
scobelvl <- obe_lvl[estrattore,]

my_MSstd <- ms_dynam( scobelvl,
                timeName = "time",
                displace = 0.25,
                displaceh = 0.45,
                dimeFontNum = 3,
                myfont_scale = 1.35,
                x_angle = 45,
                axis_name_y = "Countries",
                axis_name_x = "Time",
                alpha_color = 0.9
                )   

my_MSstd

## -----------------------------------------------------------------------------
setupConvergEU <- convergEU_glb()
names(setupConvergEU)

## -----------------------------------------------------------------------------
print(setupConvergEU$EUcodes,n=30)
print(setupConvergEU$Eurozone)
setupConvergEU$EU12
setupConvergEU$EU15

## -----------------------------------------------------------------------------
print(setupConvergEU$EU25$dates)
print(setupConvergEU$EU25$memberStates,n=30)

print(setupConvergEU$EU27$dates)
print(setupConvergEU$EU27$memberStates,n=30)

print(setupConvergEU$EU27_2020$dates)
print(setupConvergEU$EU27_2020$memberStates,n=30)

