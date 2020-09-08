
context("Scoreboard numerical calculations")



# debug(scoreb_yrs )

# undebug(scoreb_yrs )


test_that("Basic calculations on levels", {
  data("emp_20_64_MS")
  # emp_20_64_MS
  resTB <- scoreb_yrs(emp_20_64_MS,timeName = "time")
  #resTB$res$sco_level[,-c(1:25)]
 # an alternative algorithm that work fine as well
  for(aux in 1:15){
     media <- mean(unlist(emp_20_64_MS[aux,-1]))
     stddev <- pop_var(unlist(emp_20_64_MS[aux,-1]))$popsd
     for(auxCC in names(emp_20_64_MS[,-1])){
      tagnum <-  5
      corrente <- unlist(emp_20_64_MS[aux,auxCC]);
      # sequence very important for the algorithm to work !!!
      if(corrente <= media+stddev) tagnum <-  4
      if(corrente <= media+0.5*stddev) tagnum <-  3
      #if(corrente <= media) tagnum <-  -1
      if(corrente <= media-0.5*stddev) tagnum <-  2
      if(corrente <= media-stddev) tagnum <-  1
      expect_equal(as.numeric(resTB$res$sco_level[aux,auxCC]),
                  tagnum)
      }
    }


})



test_that("Basic calculations on change", {
  data("emp_20_64_MS")
  # emp_20_64_MS
  resTB <- scoreb_yrs(emp_20_64_MS,timeName = "time")
  #resTB$res$sco_level[,-c(1:25)]
  # an alternative algorithm that work fine as well
  tmopTB <-  emp_20_64_MS[-1,]
  for(auxN in names(emp_20_64_MS[,-1])){
    tmopTB[,auxN] <- diff(unlist(emp_20_64_MS[,auxN]))
  }
  for(aux in 1:14){
    media <- mean(unlist(tmopTB[aux,-1]))
    stddev <- pop_var(unlist(tmopTB[aux,-1]))$popsd
    for(auxCC in names(tmopTB[,-1])){
      tagnum <-  5
      corrente <- unlist(tmopTB[aux,auxCC]);
      # sequence very important for the algorithm to work !!!
      if(corrente <= media+stddev) tagnum <-  4
      if(corrente <= media+0.5*stddev) tagnum <-  3
      #if(corrente <= media) tagnum <-  -1
      if(corrente <= media-0.5*stddev) tagnum <-  2
      if(corrente <= media-stddev) tagnum <-  1
      expect_equal(as.numeric(resTB$res$sco_change[aux+1,auxCC]),
                   tagnum)
    }
  }


})



test_that("Basic calculations on levels", {
  myTB <-  dplyr::rename(emp_20_64_MS,tautime = time)
  #
  resTB <- scoreb_yrs(myTB,timeName = "tautime")


  for(aux in 1:15){
    media <- mean(unlist(emp_20_64_MS[aux,-1]))
    stddev <- pop_var(unlist(emp_20_64_MS[aux,-1]))$popsd
    for(auxCC in names(emp_20_64_MS[,-1])){
      tagnum <-  5
      corrente <- unlist(emp_20_64_MS[aux,auxCC]);
      # sequence very important for the algorithm to work !!!
      if(corrente <= media+stddev) tagnum <-  4
      if(corrente <= media+0.5*stddev) tagnum <-  3
      #if(corrente <= media) tagnum <-  -1
      if(corrente <= media-0.5*stddev) tagnum <-  2
      if(corrente <= media-stddev) tagnum <-  1
      expect_equal(as.numeric(resTB$res$sco_level[aux,auxCC]),
                   tagnum)
    }
  }

})

