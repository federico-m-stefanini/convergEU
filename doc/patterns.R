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
require(knitr)
#library(rvest)
require(eurostat)
require(formattable)
require(kableExtra)


knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)


## ----echo=FALSE----------------------------------------------------------
grid_patt <-  function(time=c(-1,0,1),timeRef=2002){
  tipologia <- tibble(pattern=c("outperforming", "flattening", "inversion", "catching up", 
"slower pace", "diving", "recovering", "better reaction", "falling away", 
"under performing", "defending better", "escaping"))
  taTB <- cbind(tipologia,beu0 = 23, beu1=rep(c(1,-1),c(6,6)),
                alfam=NA, deltam=NA,bem0=NA,bem1=NA,
                measureEU = NA,measureM=NA,time=NA)
  taTB[c(1,4,9,10),4]    <- c(2,-2,-2,2)
  taTB[c(1,4,9,10),5]    <- c(1,1,-1,-1)
  #
  taTB[c(2,3,  5,6,7,8, 11,12),4]    <- c(2,    2,    -2,  -2,    -2, -2,   2,  2)
  taTB[c(2,3,  5,6,7,8, 11,12),5]    <- c(-0.5, -1.5, -0.5, -1.5, +2,  0.5, 0.5, 2)

  # calcolo par retta emma
  taTB$bem0 <-   taTB$beu0 +taTB$alfam 
  taTB$bem1 <-   taTB$beu1 +taTB$deltam
  taTB$measureEU <- map2(taTB$beu0,taTB$beu1,function(b0,b1){b0+b1*time})  
  taTB$measureM <- map2(taTB$bem0,taTB$bem1,function(b0,b1){b0+b1*time}) 
  #taTB$time <- map2(taTB$bem0,taTB$bem1,function(b0,b1){list(time)}) 
  taTB$time <- list(time+timeRef) 
  taTB$country <- factor('EU',levels=c('EU','MS'))
  taTB
}

patTB <- grid_patt(c(-1,0,1))

## ----echo=FALSE,fig.width=7,fig.height=7---------------------------------

myTB <- tidyr::unnest(patTB,measureEU,measureM,time)

ggplot(myTB, aes(x=time,y=measureEU))+facet_wrap(~pattern,ncol=3)+
  geom_line() +
  geom_line(aes(x=time,y=measureM),colour="blue",linetype = "3313")+
  xlab("Time") + ylab("Measure")+
  scale_x_continuous(  breaks = round(seq(min(myTB$time),
              max(myTB$time), 
              by = 1),1)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  #guides(fill=guide_legend(title="EU vs MS"),title.position = "left")+
  ggtitle("MS: dashed blue lines\nEU: solid black lines") 
 


## ----fig.width=4,fig.height=3--------------------------------------------
require(tibble)
myTB <- tribble(
  ~time , ~indic,
   1    ,   25,
   10   ,   5
)

plot(indic ~ time,data=myTB,type="b",
     ylim=c(0,40))

## ------------------------------------------------------------------------
mapInX <- function(veva, refMax=100){
  refMax - veva
}
myTB2 <- dplyr::mutate(myTB,vaV= mapInX(myTB$indic,35))
myTB2

## ----fig.width=4,fig.height=3--------------------------------------------
plot(indic ~ time,data=myTB,type="b",
     ylim=c(0,40))
lines(vaV ~ time,data=myTB2,col=2,lty=3)
points(vaV ~ time,data=myTB2,col=2)

## ----fig.width=4,fig.height=3--------------------------------------------
myTB2 <- dplyr::mutate(myTB,
                       vaV= mapInX(myTB$indic,35),
                       vaV3= mapInX(myTB$indic,30))
myTB2
plot(indic ~ time,data=myTB,type="b",
     ylim=c(0,40))
lines(vaV3 ~ time,data=myTB2,col=4,lty=3)
points(vaV3 ~ time,data=myTB2,col=4)
lines(vaV ~ time,data=myTB2,col=2,lty=3)
points(vaV ~ time,data=myTB2,col=2)

## ------------------------------------------------------------------------
rePP <-  ms_pattern_ori( emp_20_64_MS, "time",type="highBetter")
rePP$res$mat_without_summaries

## ------------------------------------------------------------------------
rePP$res$mat_label_tags 

## ----testDE,echo=F,eval=F------------------------------------------------
#  names(emp_20_64_MS)
#  matRaw <- emp_20_64_MS[,-1]
#  EUave <- cbind(emp_20_64_MS[,1] ,EUave=apply(matRaw,1,mean))
#  emp_20_64_MS[2:3,c("time","DE")]
#  betams <- diff(unlist(emp_20_64_MS[2:3,c("DE")]))
#  betams
#  betaEU <-  diff(c(67.77143,67.90714))
#  betaEU
#  
#  plot(c(2003,2004,2003,2004),c(c(67.77143,67.90714,68.4, 67.9 )),type="p")
#  lines(c(2003,2004), c(68.4, 67.9 ),col=2)
#  lines(c(2003,2004), c(67.77143,67.90714 ),col=4)
#  # > betaEU
#  # [1] 0.13571
#  # > betaEU
#  # [1] 0.13571
#  # equal lines
#  ## coeu_grad(67.90714, 67.77143, 67.9, 68.4, 2004,2003 )

