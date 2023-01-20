![](inst/logoConvergEU9_github.png)  

# R package *convergEU*

Indicators and measures by country and time describe
what happens at economic and social levels. This package provides
functions to calculate several measures of convergence after imputing
missing values. The automated download of Eurostat data,
followed by the production of country fiches and indicator fiches,
makes possible to  automate the production of reports.
This is the development release.  

Some references  are:   

  *  [tutorial-conv.html](https://unimi2013-my.sharepoint.com/:u:/g/personal/federico_stefanini_unimi_it/EW0cVSIgbtZAvLPNbqcxdX8Bfn5VGSRHfAH88hQwc_RIEQ?e=MgtSZu)   
  *  [Eurofound working paper](https://www.eurofound.europa.eu/sites/default/files/wpef20008.pdf) 
  *  [Eurofound research report](https://www.eurofound.europa.eu/sites/default/files/ef_publication/field_ef_document/ef18003en.pdf)       
  *  To prepare fiches without any R code, please visit the Eurofound official   site of the [**convergEU app**](https://www.eurofound.europa.eu/data/convergence-hub/convergeu-app)  

The stable release of the R package is available at https://CRAN.R-project.org/package=convergEU

This project is a joint work with  **Eurofound** developed under **contract &#8470;
 18-3030-42**.<br>
<img src="inst/EF2015_Logo_Colour_rid.png" width="113"  height="75">  


<br>  

You may use  *devtools* to  install the current version under development:   

```
install.packages("devtools")
library(devtools)
install_github(repo="federico-m-stefanini/convergEU",
              build_vignettes= TRUE,
              force= TRUE)
```


