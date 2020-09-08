## ----setup, include = FALSE----------------------------------------------
require(ggplot2)
require(dplyr)
require(tidyverse)
require(convergEU)
require(eurostat)
require(purrr)
require(tibble)
require(tidyr)
require(ggplot2)
require(formattable) 
require(kableExtra)
require(caTools)

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)


## ----eval=FALSE----------------------------------------------------------
#  require(convergEU)
#  require(ggplot2)
#  require(dplyr)
#  require(tidyverse)
#  require(eurostat)
#  require(purrr)
#  require(tibble)
#  require(tidyr)
#  require(ggplot2)
#  require(formattable)
#  require(caTools)

## ----eval=FALSE----------------------------------------------------------
#  library(convergEU)
#  data("emp_20_64_MS",package = "convergEU")
#  head(emp_20_64_MS)

## ----eval=FALSE----------------------------------------------------------
#  data(package = "convergEU")

## ------------------------------------------------------------------------
print(dbEUF2018meta, n=200,width=200)             

## ------------------------------------------------------------------------
require(convergEU)
data(dbEurofound)
head(dbEurofound)

## ------------------------------------------------------------------------
names(dbEurofound)

## ------------------------------------------------------------------------
c(min(dbEurofound$time), max(dbEurofound$time))

## ----eval=FALSE----------------------------------------------------------
#  help(emp_20_64_MS)

## ------------------------------------------------------------------------
convergEU_glb()$EU12

## ------------------------------------------------------------------------
names(convergEU_glb())[c(3:8)]

## ------------------------------------------------------------------------
convergEU_glb()$EU19

## ------------------------------------------------------------------------
head(dbEUF2018meta)
myTB <- extract_indicator_EUF(
    indicator_code = "lifesatisf", #Code_in_database
    fromTime=2003,
    toTime=2016,
    gender= c("Total","Females","Males")[2],
    countries= convergEU_glb()$EU12$memberStates$codeMS
    )
  
myTB

## ----echo=TRUE,eval=FALSE------------------------------------------------
#  ddTB1 <- download_indicator_EUS(
#        indicator_code= convergEU_glb()$metaEUStat$selectorUser[1],
#        fromTime = 2005,
#        toTime = 2015,
#        gender= c(NA,"T","F","M")[2],#c("Total","Females","Males")
#        countries =  convergEU_glb()$EU28$memberStates$codeMS,
#        rawDump=T )
#  ddTB1

## ----echo=FALSE----------------------------------------------------------
sourceFile1 <- system.file("extdata", package = "convergEU")
# save(ddTB1,file=file.path(sourceFile1,"ddTB1.RData"))
load(file.path(sourceFile1,"ddTB1.RData"))
ddTB1

## ----echo=TRUE,eval=FALSE------------------------------------------------
#  ddTB2 <- download_indicator_EUS(
#        indicator_code= convergEU_glb()$metaEUStat$selectorUser[1],
#        fromTime = 2005,
#        toTime = 2015,
#        gender= c(NA,"T","F","M")[1],#c("Total","Females","Males")
#        ageInterv = NA,
#        countries =  convergEU_glb()$EU28$memberStates$codeMS,
#        rawDump=F,
#        uniqueIdentif = 1)
#  
#  convergEU_glb()$metaEUStat$selectorUser[1]
#  ddTB2

## ----echo=FALSE----------------------------------------------------------
convergEU_glb()$metaEUStat$selectorUser[1]
sourceFile1 <- system.file("extdata", package = "convergEU")
# save(ddTB2,file=file.path(sourceFile1,"ddTB2.RData"))
load(file.path(sourceFile1,"ddTB2.RData"))
ddTB2

## ----echo=T,eval=FALSE---------------------------------------------------
#  ddTB3 <- download_indicator_EUS(
#        indicator_code= convergEU_glb()$metaEUStat$selectorUser[1],
#        fromTime = 2005,
#        toTime = 2015,
#        gender= "M",
#        ageInterv = "Y15-64",
#        countries =  convergEU_glb()$EU28$memberStates$codeMS,
#        rawDump=F,
#        uniqueIdentif = 5)
#  ddTB3

## ----echo=FALSE----------------------------------------------------------
sourceFile1 <- system.file("extdata", package = "convergEU")
# save(ddTB3,file=file.path(sourceFile1,"ddTB3.RData"))
load(file.path(sourceFile1,"ddTB3.RData"))
ddTB3

## ----eval=T--------------------------------------------------------------
# print(dbEUF2018meta[11,],n=20,width=100)
t(dbEUF2018meta[11,])

## ----eval=FALSE----------------------------------------------------------
#  ddTB4 <- extract_indicator_EUF(
#      indicator_code = "JQIintensity_i", #Code_in_database
#      fromTime= 1965,
#      toTime=2016,
#      gender= c("Total","Females","Males")[1],
#      countries= convergEU_glb()$EU28$memberStates$codeMS
#      )
#  print(ddTB4$res,n=35,width=250)

## ----echo=FALSE----------------------------------------------------------
sourceFile1 <- system.file("extdata", package = "convergEU")
# save(ddTB4,file=file.path(sourceFile1,"ddTB4.RData"))
load(file.path(sourceFile1,"ddTB4.RData"))
print(ddTB4$res,n=35,width=250)

## ------------------------------------------------------------------------
JQIinte <- ddTB4$res 
dim(JQIinte)

## ------------------------------------------------------------------------
names(JQIinte)

## ------------------------------------------------------------------------
check_data(JQIinte,timeName="time")

## ------------------------------------------------------------------------
JQIinteImp <- impute_dataset(JQIinte, timeName = "time",
                          countries=convergEU_glb()$EU28$memberStates$codeMS,
                          tailMiss = c("cut", "constant")[2],
                          headMiss = c("cut", "constant")[2])$res 
print(JQIinteImp,n=35,width=250)

## ------------------------------------------------------------------------
select(filter(JQIinte, is.na(HR)),time,HR) 

## ------------------------------------------------------------------------
JQIinte %>% 
  filter(is.na(HR)) %>%
  select(time,HR)

## ------------------------------------------------------------------------
check_data(JQIinteImp)

## ------------------------------------------------------------------------
JQIinteFin <- dplyr::select(JQIinteImp,-sex)
check_data(JQIinteFin)

## ------------------------------------------------------------------------
JQIinteFin[, -1] <- round(select(JQIinteFin,- time), digits = 0)
JQIinteFin

## ------------------------------------------------------------------------
intervalTime <-  c(1999,2000,2001,2002,2003) 
intervalMeasure <- c( 66.5, NA,NA,NA,87.2) 
currentData <- tibble(time= intervalTime, veval= intervalMeasure) 
currentData
resImputed <- impute_dataset(currentData,
                           countries = "veval",
                           timeName = "time",
                           tailMiss = c("cut", "constant")[2],
                           headMiss = c("cut", "constant")[2]) 
resImputed$res  

## ----echo=FALSE,fig.width=4,fig.height=3---------------------------------
tmp <-  as.data.frame(currentData[ c(1,5),] )
tmp2 <- as.data.frame(resImputed$res[2:4,] )

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


## ------------------------------------------------------------------------
workTB <- dplyr::select(emp_20_64_MS, time, IT,DE)
check_data(workTB)

## ------------------------------------------------------------------------
resSM <- smoo_dataset(select(workTB,-time), leadW = 0.149, timeTB= select(workTB,time))
resSM

## ------------------------------------------------------------------------
compaTB <- select(bind_cols(workTB, select(resSM,-time)), time,IT,IT1,DE,DE1)
compaTB

## ----fig.width=4,fig.height=3--------------------------------------------
qplot(time,IT,data=compaTB) + 
  geom_line(colour="navyblue") +
  geom_line(aes(x=time,y=IT1),colour="red") +
  geom_point(aes(x=time,y=IT1),colour="red",shape=8)

## ----fig.width=4,fig.height=3--------------------------------------------
qplot(time,DE,data=compaTB) + 
  geom_line(colour="navyblue") +
  geom_line(aes(x=time,y=DE1),colour="red") +
  geom_point(aes(x=time,y=DE1),colour="red",shape=8)


## ----fig.width=4,fig.height=3--------------------------------------------
resSM <- smoo_dataset(select(workTB,-time), leadW = 1, timeTB= select(workTB,time))
compaTB <- select(bind_cols(workTB, select(resSM,-time)), time,IT,IT1,DE,DE1)
qplot(time,IT,data=compaTB) + 
  geom_line(colour="navyblue") +
  geom_line(aes(x=time,y=IT1),colour="red") +
  geom_point(aes(x=time,y=IT1),colour="red",shape=8)

## ------------------------------------------------------------------------
data(emp_20_64_MS)
cuTB <- select(emp_20_64_MS,time)
cuTB <- mutate(cuTB,ITori =emp_20_64_MS$IT)

## ------------------------------------------------------------------------
cuTB <- mutate(cuTB, IT_k_3= runmean(emp_20_64_MS$IT, k=3, 
        alg=c("C", "R", "fast", "exact")[4],
        endrule=c("mean", "NA", "trim", "keep", "constant", "func")[4],
        align = c("center", "left", "right")[1]))

cuTB <-  mutate(cuTB, IT_k_5= runmean(emp_20_64_MS$IT, k=5, 
        alg=c("C", "R", "fast", "exact")[4],
        endrule=c("mean", "NA", "trim", "keep", "constant", "func")[4],
        align = c("center", "left", "right")[1]))

cuTB <-  mutate(cuTB, IT_k_7= runmean(emp_20_64_MS$IT, k=7, 
        alg=c("C", "R", "fast", "exact")[4],
        endrule=c("mean", "NA", "trim", "keep", "constant", "func")[4],
        align = c("center", "left", "right")[1]))

## ------------------------------------------------------------------------
myG <- ggplot(cuTB,aes(x=time,y=ITori))+geom_line()+geom_point()+
       geom_line(aes(x=time,y=IT_k_3),colour="red")+
       geom_point(aes(x=time,y=IT_k_3),colour="red")+
       #
       geom_line(aes(x=time,y=IT_k_5),colour="blue")+
       geom_point(aes(x=time,y=IT_k_5),colour="blue")+
       #
       geom_line(aes(x=time,y=IT_k_7),colour="orange")+
       geom_point(aes(x=time,y=IT_k_7),colour="orange")

myG

## ------------------------------------------------------------------------
cuTB <-  emp_20_64_MS[,c("time","IT","DE")]
ma_dataset(cuTB, kappa=3, timeName= "time")

## ------------------------------------------------------------------------
data(emp_20_64_MS)
mySTB <- abso_change(emp_20_64_MS, 
                        time_0 = 2005, 
                        time_t = 2010,
                        all_within=TRUE,
                        timeName = "time")
names(mySTB$res)

## ------------------------------------------------------------------------
mySTB$res$abso_change

## ------------------------------------------------------------------------
round(dplyr::select(mySTB$res$abso_change,AT:UK), 5)

## ------------------------------------------------------------------------
round(mySTB$res$sum_abs_change,4)

## ------------------------------------------------------------------------
round(mySTB$res$average_abs_change,4)

## ------------------------------------------------------------------------
convergEU_glb()$Eurozone
convergEU_glb()$EU19

## ------------------------------------------------------------------------
convergEU_glb()$EU28

## ------------------------------------------------------------------------
names(convergEU_glb())[3:8]

## ------------------------------------------------------------------------
average_clust(emp_20_64_MS, 
              timeName = "time",
              cluster = "EU28")$res[,c(1,30)]

## ------------------------------------------------------------------------
average_clust(emp_20_64_MS,timeName = "time",cluster = "EU12")$res[,c(1,30)]

## ------------------------------------------------------------------------
average_clust(emp_20_64_MS,timeName = "time",cluster = "EUspirit")

## ------------------------------------------------------------------------
average_clust(emp_20_64_MS,timeName = "TTime",cluster = "EU19")

## ----fig.width=5,fig.height=4--------------------------------------------
wwTB <- average_clust(emp_20_64_MS,timeName = "time",cluster = "EU28")$res[,c(1,30)]
mini_EU <- min(wwTB$EU28)
maxi_EU <- max(wwTB$EU28)

qplot(time, EU28, data=wwTB,
      ylim=c(mini_EU,maxi_EU))+geom_line(colour="navy blue")+
      ylab("emp_20_64")

## ------------------------------------------------------------------------
require(ggplot2)
require(dplyr)
require(tibble)

empBC <- beta_conv(emp_20_64_MS, 
                 time_0 = 2002, 
                 time_t = 2006, 
                 all_within = FALSE, 
                 timeName = "time")
empBC

## ----fig.width=6,fig.height=5--------------------------------------------
qplot(empBC$res$workTB$indic,
      empBC$res$workTB$deltaIndic,
      xlab="log-Indicator",
      ylab="Delta-log-indicator") +
  geom_abline(intercept = as.numeric(empBC$res$summary[1,2]),
              slope = as.numeric(empBC$res$summary[2,2]),
              colour = "red") +
  geom_text(aes(label=empBC$res$workTB$countries),
            hjust=0, vjust=0,colour="blue")

## ------------------------------------------------------------------------
mySTB <- sigma_conv(emp_20_64_MS,timeName="time")
mySTB

## ------------------------------------------------------------------------
sigma_conv(emp_20_64_MS,time_0 = 2002,time_t = 2004)

## ------------------------------------------------------------------------
res <- departure_mean(oriTB = emp_20_64_MS, sigmaTB = mySTB$res)
names(res$res)
res$res$departures

## ------------------------------------------------------------------------
res$res$squaredContrib

## ------------------------------------------------------------------------
res$res$devianceContrib

## ----eval=T,fig.width=6,fig.height=7-------------------------------------
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

## ----eval=T,fig.width=6,fig.height=7-------------------------------------
#myWW1<- warnings()
myGG <- graph_departure(res$res$departures[,1:10],
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

## ------------------------------------------------------------------------
gamma_conv(emp_20_64_MS,last=2016,ref=2002,timeName="time")

## ------------------------------------------------------------------------

delta_conv(emp_20_64_MS,"time")

## ------------------------------------------------------------------------
data(emp_20_64_MS)
resTB <- scoreb_yrs(emp_20_64_MS,timeName = "time")
resTB

## ----fig.height=4,fig.width=6--------------------------------------------
# require(ggplot2)
# data(emp_20_64_MS)
selectedCountry <- "IT"
timeName <-  "time"
myx_angle <-  45

outSig <- sigma_conv(emp_20_64_MS, timeName = timeName,
           time_0=2002,time_t=2016)
miniY <- min(emp_20_64_MS[,- which(names(emp_20_64_MS) == timeName )])
maxiY <-  max(emp_20_64_MS[,- which(names(emp_20_64_MS) == timeName )])
estrattore<-  emp_20_64_MS[,timeName] >= 2002  &  emp_20_64_MS[,timeName] <= 2016
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

## ----fig.height=6,fig.width=6--------------------------------------------
obe_lvl <- scoreb_yrs(emp_20_64_MS,timeName = timeName)$res$sco_level_num
# select subset of time
estrattore <- obe_lvl[,timeName] >= 2009 & obe_lvl[,timeName] <= 2016  
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

