## ----setup,include = FALSE----------------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyverse)
library(convergEU)
library(eurostat)
library(purrr)
library(tibble)
library(tidyr)
library(ggplot2)
library(formattable) 
library(kableExtra)
library(caTools)
library(readxl)

knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  message = FALSE,
  comment = "#>",
  fig.width = 4,
  fig.height = 5,
  dpi=200
)


## ---- eval=FALSE--------------------------------------------------------------
#  help(ms_pattern_ori)

## ---- eval=FALSE--------------------------------------------------------------
#  help(patt_legend)

## ---- fig.width = 6, fig.height = 5,out.width="100%"--------------------------
highind<-patt_legend(indiType="highBest")
highind

## ---- fig.width = 6, fig.height = 5,out.width="100%"--------------------------
lowhind<-patt_legend(indiType="lowBest")
lowhind

## -----------------------------------------------------------------------------
myemp <-ms_pattern_ori(emp_20_64_MS, "time",type="highBest")

## -----------------------------------------------------------------------------
names(myemp)

## -----------------------------------------------------------------------------
mypattemp<-myemp$res$mat_label_tags
mypattempn<-myemp$res$mat_without_summaries
mypattempn
mypattemp

## -----------------------------------------------------------------------------
mypattemp[["2006/2007"]][12]

## ----echo=FALSE,out.width="70%",fig.height=6,fig.width=7----------------------
matRaw1 <- dplyr::select(emp_20_64_MS, -time)
matRawT <- dplyr::select(emp_20_64_MS, time)
EUavemp <- dplyr::bind_cols(matRawT ,EUavempp=apply(matRaw1,1,mean))
EUavemph <- EUavemp[5:6,]
avehu <- emp_20_64_MS[5:6,"FR"]
gFR <- ggplot() + geom_point(aes(x=EUavemph$time,y=EUavemph$EUavempp),color='black') +
  geom_point(aes(x=EUavemph$time,y=avehu$FR),color='blue') +
  geom_line(aes(x=EUavemph$time,y=EUavemph$EUavempp),color='black') +
  geom_line(aes(x=EUavemph$time,y=avehu$FR),color='blue',linetype = 2) + 
  ggtitle("Employment rate indicator: 2006-2007")+
  labs(y="France", x="Time")+
  theme(axis.text.x=element_blank())
gFR

## ----echo=F-------------------------------------------------------------------


## ---- echo=TRUE---------------------------------------------------------------
require(readxl)
file_name <- system.file("vign/une_educ_a.xls", package = "convergEU")
myxls2<-read_excel(file_name,
                   sheet="Data",range = "A12:AP22", na=":")
myxls2 <- dplyr::mutate(myxls2, `TIME/GEO` = as.numeric(`TIME/GEO`))

## ---- echo=TRUE---------------------------------------------------------------
EU27estr<-convergEU_glb()$EU27_2020$memberStates$codeMS
myxls<- dplyr::select(myxls2,`TIME/GEO`,all_of(EU27estr))
check_data(myxls)
myxls3<- dplyr::rename(myxls,time=`TIME/GEO`)
myxlsf <- impute_dataset(myxls3, timeName ="time",
                         countries=convergEU_glb()$EU27_2020$memberStates$codeMS,
                         headMiss = c("cut", "constant")[2],
                         tailMiss = c("cut", "constant")[2])$res
check_data(myxlsf)

## -----------------------------------------------------------------------------
myres <-  ms_pattern_ori(myxlsf, "time",type="lowBest")

## -----------------------------------------------------------------------------
mypattl<-myres$res$mat_label_tags
mypattn<-myres$res$mat_without_summaries
mypattn
mypattl

## -----------------------------------------------------------------------------
mypattl$`2015/2016`[14]

## ----echo=FALSE,out.width="70%",fig.height=5,fig.width=5----------------------
matRaw <- select(myxlsf,-time)
EUave <- cbind( select(myxlsf,time) ,EUave=apply(matRaw,1,mean))
EUave1<-EUave[7:8,]
avef<-matRaw[7:8, "FI"]

gfr<-ggplot() + geom_point(aes(x=EUave1$time,y=EUave1$EUave),color='black') + 
  geom_point(aes(x=EUave1$time,y=avef$FI),color='blue') +
  geom_line(aes(x=EUave1$time,y=EUave1$EUave),color='black') + 
  geom_line(aes(x=EUave1$time,y=avef$FI),color='blue',linetype = 2) + 
  ggtitle("Unemployment rate indicator: 2015-2016")+
  labs(y="Finland", x="Time")+
  theme(axis.text.x=element_blank())
gfr

## -----------------------------------------------------------------------------
mypattemp$`2011/2012`[23]

## ----echo=FALSE,out.width="70%",fig.height=5,fig.width=5----------------------
EUavemph1<-EUavemp[10:11,]
avept<-emp_20_64_MS[10:11,c("PT")]
gpt<-ggplot() + geom_point(aes(x=EUavemph1$time,y=EUavemph1$EUavempp),color='black') +
  geom_point(aes(x=EUavemph1$time,y=avept$PT),color='blue') +
  geom_line(aes(x=EUavemph1$time,y=EUavemph1$EUavempp),color='black') +
  geom_line(aes(x=EUavemph1$time,y=avept$PT),color='blue',linetype = 2) + 
  ggtitle("Employment rate indicator: 2011-2012")+
  labs(y="Portugal", x="Time")+
  theme(axis.text.x=element_blank())
gpt

## ----echo=FALSE,out.width="70%",fig.height=5,fig.width=5----------------------
avelt<-emp_20_64_MS[10:11,c("LT")]
glt<-ggplot() + geom_point(aes(x=EUavemph1$time,y=EUavemph1$EUavempp),color='black') +
  geom_point(aes(x=EUavemph1$time,y=avelt$LT),color='blue') +
  geom_line(aes(x=EUavemph1$time,y=EUavemph1$EUavempp),color='black') +
  geom_line(aes(x=EUavemph1$time,y=avelt$LT),color='blue',linetype = 2) + 
  ggtitle("Employment rate indicator: 2011-2012")+
  labs(y="Lithuania", x="Time")+
  theme(axis.text.x=element_blank())
glt

## ---- eval=FALSE--------------------------------------------------------------
#  help(upDo_CoDi)

## -----------------------------------------------------------------------------
Empconv<-upDo_CoDi(emp_20_64_MS,
              timeName = "time",
              indiType = "highBest",
              time_0 = 2008,
              time_t = 2010,
              heter_fun = "var")

## -----------------------------------------------------------------------------
names(Empconv)
Empconv$msg
Empconv$err

## -----------------------------------------------------------------------------
Empconv$res$declaration_type

## -----------------------------------------------------------------------------
Empconv$res$declaration_strict
Empconv$res$declaration_weak

## -----------------------------------------------------------------------------
Empconv$res$declaration_split$names_incre

## -----------------------------------------------------------------------------
Empconv$res$declaration_split$names_decre

## -----------------------------------------------------------------------------
Empconv$res$diffe_MS

## -----------------------------------------------------------------------------
Empconv$res$diffe_averages

## -----------------------------------------------------------------------------
Empconv$res$dispersions

## -----------------------------------------------------------------------------
Empconvpop<-upDo_CoDi(emp_20_64_MS,
                   timeName = "time",
                   indiType = "highBest",
                   time_0 = 2008,
                   time_t = 2010,
                   heter_fun = "pop_var")

## -----------------------------------------------------------------------------
diffQQmu <-  function(vettore){
 (quantile(vettore,0.75)-quantile(vettore,0.25))/mean(vettore)
  }

## -----------------------------------------------------------------------------
unempconvvar<-upDo_CoDi(myxlsf,
                      timeName = "time",
                      indiType = "lowBest",
                      time_0 = 2009,
                      time_t = 2011,
                      heter_fun = "diffQQmu")
unempconvvar

