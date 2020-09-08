
context("Overall convergence")



#  debug(upDo_CoDi)

myhete1999 <<-  function(vettore){
  sd(vettore)*100/mean(vettore)
}

diffQQmu <<-  function(vettore){
  (quantile(vettore,0.75)-quantile(vettore,0.25))/mean(vettore)
  }


# debug(upDo_CoDi)


test_that("Basic calculations", {

  res <-   upDo_CoDi(emp_20_64_MS,
            timeName = "time",
            time_0 =  2010,
            time_t = 2015,
            heter_fun = "pop_var"
            )

  expect_null(res$err)
  expect_null(res$msg)

  # dispersion
  dispe_1 <- pop_var(unlist(emp_20_64_MS[emp_20_64_MS$time == 2010,-1]))$popvar
  dispe_2 <- pop_var(unlist(emp_20_64_MS[emp_20_64_MS$time == 2015,-1]))$popvar
  expect_equal(as.numeric(res$res$dispersions[1]), dispe_1)
  expect_equal(as.numeric(res$res$dispersions[2]), dispe_2)

  # diffe_MS
  differe_MS <- emp_20_64_MS[emp_20_64_MS$time == 2015,-1] -
                emp_20_64_MS[emp_20_64_MS$time == 2010,-1]
  for(aux in 1:length(differe_MS)){
      expect_equal(differe_MS[aux] , res$res$diffe_MS[aux])
    }

  # differnece of means
    diffe_aver <- mean(unlist(emp_20_64_MS[emp_20_64_MS$time == 2015,-1])) -
                  mean(unlist(emp_20_64_MS[emp_20_64_MS$time == 2010,-1]));
    expect_equal(res$res$diffe_averages,diffe_aver);

    # split MS
    negative_subset <-  names(differe_MS)[differe_MS<0]
    notnegative_subset <-  names(differe_MS)[differe_MS>=0]
    resIN <- setdiff(res$res$declaration_split$names_incre,
            notnegative_subset)
    resDEC <- setdiff(res$res$declaration_split$names_decre,
            negative_subset)
    expect_equal( length(resIN) , 0)
    expect_equal( length(resDEC), 0)
})



test_that("Declarations of convergence 2", {

    myTB2  <- tibble::tribble(
      ~years, ~UK, ~DE, ~IT,
        88,   1201,   868, 578,
        91,   1400,  1410, 1399
    )

    res <-   upDo_CoDi(myTB2,
                       timeName = "time",
                       time_0 =  88,
                       time_t = 91,
                       heter_fun = "pop_var"
    )

    expect_equal(res$err,"Error: Time variable not in the dataframe.")
    res <-   upDo_CoDi(myTB2,
                       timeName = "years",
                       time_0 =  88,
                       time_t = 91,
                       heter_fun = "pop_var"
    )
    # strict upward
    expect_equal(res$res$declaration_type , "Convergence")
    expect_equal(res$res$declaration_strict , "Strict upward")
    expect_equal(res$res$declaration_weak , "Weak upward")

})




test_that("Declarations of convergence downward", {

  myTB2  <- tibble::tribble(
    ~years, ~UK, ~DE, ~IT,
    88,   1201,   868, 578,
    91,   1400,  1410, 1399
    )

   myTB2[2,2:4] <- myTB2[2,2:4] - 1000


    # strict downward
    res <-   upDo_CoDi(myTB2,
                       timeName = "years",
                       time_0 =  88,
                       time_t = 91,
                       heter_fun = "pop_var"
                       )

    expect_equal(res$res$declaration_type , "Convergence")
    expect_equal(res$res$declaration_strict , "Strict downward")
    expect_equal(res$res$declaration_weak , "Weak downward")


})




test_that("Declarations of convergence  weak  ", {

  myTB2  <- tibble::tribble(
    ~years, ~UK, ~DE, ~IT,
    88,   1201,   868, 578,
    91,   1400,  1410, 1581
  )

  myTB2[2,2:4] <- myTB2[2,2:4] - 1000
  #myTB2

  # strict downward
  res <-   upDo_CoDi(myTB2,
                     timeName = "years",
                     time_0 =  88,
                     time_t = 91,
                     heter_fun = "pop_var"
  )

  expect_equal(res$res$declaration_type , "Convergence")
  expect_equal(res$res$declaration_strict , "none")
  expect_equal(res$res$declaration_weak , "Weak downward")

})



test_that("Declarations of convergence weak upward", {

  myTB2  <- tibble::tribble(
    ~years, ~UK, ~DE, ~IT,
    88,   1201,   868, 1412,
    91,   1400,  1410, 1410
  )


  # strict downward
  res <-   upDo_CoDi(myTB2,
                     timeName = "years",
                     time_0 =  88,
                     time_t = 91,
                     heter_fun = "pop_var"
  )

  expect_equal(res$res$declaration_type , "Convergence")
  expect_equal(res$res$declaration_strict , "none")
  expect_equal(res$res$declaration_weak , "Weak upward")

})




test_that("Declarations of divergence", {

  myTB2  <- tibble::tribble(
    ~years, ~UK, ~DE, ~IT,
    91,   1201,   868, 1412,
    88,   1400,  1410, 1410
  )


  # strict downward
  res <-   upDo_CoDi(myTB2,
                     timeName = "years",
                     time_0 =  88,
                     time_t = 91,
                     heter_fun = "pop_var"
  )

  expect_equal(res$res$declaration_type , "Divergence")
  expect_equal(res$res$declaration_strict , "none")
  expect_equal(res$res$declaration_weak , "Weak downward")


})






test_that("Different heterogeneity functions", {

  res <-   upDo_CoDi(emp_20_64_MS,
                     timeName = "time",
                     time_0 =  2010,
                     time_t = 2015,
                     heter_fun = "sd"
  )

  # dispersion
  dispe_1 <- sd(unlist(emp_20_64_MS[emp_20_64_MS$time == 2010,-1]))
  dispe_2 <- sd(unlist(emp_20_64_MS[emp_20_64_MS$time == 2015,-1]))
  expect_equal(as.numeric(res$res$dispersions[1]), dispe_1)
  expect_equal(as.numeric(res$res$dispersions[2]), dispe_2)




  res <-   upDo_CoDi(emp_20_64_MS,
                     timeName = "time",
                     time_0 =  2010,
                     time_t = 2015,
                     heter_fun = "myhete1999"
  )

  dispe_1 <- myhete1999(unlist(emp_20_64_MS[emp_20_64_MS$time == 2010,-1]))
  dispe_2 <- myhete1999(unlist(emp_20_64_MS[emp_20_64_MS$time == 2015,-1]))
  expect_equal(as.numeric(res$res$dispersions[1]), dispe_1)
  expect_equal(as.numeric(res$res$dispersions[2]), dispe_2)



  res <-   upDo_CoDi(emp_20_64_MS,
                     timeName = "time",
                     time_0 =  2010,
                     time_t = 2015,
                     heter_fun = "diffQQmu"
  )

  dispe_1 <- diffQQmu(unlist(emp_20_64_MS[emp_20_64_MS$time == 2010,-1]))
  dispe_2 <- diffQQmu(unlist(emp_20_64_MS[emp_20_64_MS$time == 2015,-1]))
  expect_null(res$err)
  expect_null(res$msg)


})





test_that("Declarations for lowBest", {

  myTB2  <- tibble::tribble(
    ~years, ~UK, ~DE, ~IT,
    91,   1201,   868, 1412,
    88,   1400,  1410, 1410
  )


  # strict downward
  res <-   upDo_CoDi(myTB2,
                     timeName = "years",
                     indiType = "lowBest",
                     time_0 =  88,
                     time_t = 91,
                     heter_fun = "pop_var"
  )

  expect_equal(res$res$declaration_type , "Divergence")
  expect_equal(res$res$declaration_strict , "none")
  expect_equal(res$res$declaration_weak , "Weak upward")


  myTB2  <- tibble::tribble(
    ~years, ~UK, ~DE, ~IT,
    91,   1201,   868, 1312,
    88,   1400,  1410, 1410
  )
  res <-   upDo_CoDi(myTB2,
                     timeName = "years",
                     indiType = "lowBest",
                     time_0 =  88,
                     time_t = 91,
                     heter_fun = "pop_var"
  )

  expect_equal(res$res$declaration_type , "Divergence")
  expect_equal(res$res$declaration_strict , "Strict upward")
  expect_equal(res$res$declaration_weak , "Weak upward")


  myTB2  <- tibble::tribble(
    ~years, ~UK, ~DE, ~IT,
    91,   1201,   1228, 1212,
    88,   1200,  1220, 1210
  )
  res <-   upDo_CoDi(myTB2,
                     timeName = "years",
                     indiType = "lowBest",
                     time_0 =  88,
                     time_t = 91,
                     heter_fun = "pop_var"
  )

  expect_equal(res$res$declaration_type , "Divergence")
  expect_equal(res$res$declaration_strict , "Strict downward")
  expect_equal(res$res$declaration_weak , "Weak downward")



    myTB2  <- tibble::tribble(
    ~years, ~UK, ~DE, ~IT,
    91,   1300,   1299, 1302,
    88,   1205,   1300, 1295
  )
  res <-   upDo_CoDi(myTB2,
                     timeName = "years",
                     indiType = "lowBest",
                     time_0 =  88,
                     time_t = 91,
                     heter_fun = "pop_var"
  )

  expect_equal(res$res$declaration_type , "Convergence")
  expect_equal(res$res$declaration_strict , "none")
  expect_equal(res$res$declaration_weak , "Weak downward")




  myTB2  <- tibble::tribble(
    ~years, ~UK, ~DE, ~IT,
    91,   1000,   1005, 1020,
    88,   1020,  1000, 1100
  )
  res <-   upDo_CoDi(myTB2,
                     timeName = "years",
                     indiType = "lowBest",
                     time_0 =  88,
                     time_t = 91,
                     heter_fun = "pop_var"
  )

  expect_equal(res$res$declaration_type , "Convergence")
  expect_equal(res$res$declaration_strict , "none")
  expect_equal(res$res$declaration_weak , "Weak upward")



})


rm(diffQQmu,envir=.GlobalEnv)
rm(myhete1999,envir=.GlobalEnv)

