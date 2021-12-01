test_that("loess_fit works", {
  check1=predict(loess(cars$dist~cars$speed,surface="direct"))
  check2=predict(loess(cars$dist~cars$speed,surface="direct",span=0.5))
  expect_equal(NonParamLines::loess_fit(cars$speed,cars$dist),check1)
  expect_equal(NonParamLines::loess_fit(cars$speed,cars$dist,span=0.5),check2)
  set.seed(1)
  x=seq(0,10,length.out=100)
  y=sin(x)+rnorm(100,0,0.5)
  check1=predict(loess(y~x,surface="direct"))
  check2=predict(loess(y~x,surface="direct",span=0.5))
  expect_equal(NonParamLines::loess_fit(x,y),check1)
  expect_equal(NonParamLines::loess_fit(x,y,span=0.5),check2)
})

test_that("suspmu_fit works", {
  set.seed(1)
  x=seq(0,10,length.out=1000)
  y=sin(x)+rnorm(100,0,0.5)
  check1=supsmu(x,y,span=0.2)$y
  check2=supsmu(x,y,span=0.4)$y
  check3=supsmu(x,y)$y
  check4=supsmu(x,y,bass=4)$y
  expect_equal(NonParamLines::supsmu_fit(x,y,span=0.2),check1)
  expect_equal(NonParamLines::supsmu_fit(x,y,span=0.4),check2)
  expect_equal(NonParamLines::supsmu_fit(x,y),check3)
  expect_equal(NonParamLines::supsmu_fit(x,y,bass=4),check4)
  set.seed(1)
  x=seq(0,10,length.out=1000)
  y=sin(x)+rnorm(1000,0,0.5)
  check1=supsmu(x,y,span=0.2)$y
  check2=supsmu(x,y,span=0.4)$y
  check3=supsmu(x,y)$y
  check4=supsmu(x,y,bass=4)$y
  expect_equal(NonParamLines::supsmu_fit(x,y,span=0.2),check1)
  expect_equal(NonParamLines::supsmu_fit(x,y,span=0.4),check2)
  expect_equal(NonParamLines::supsmu_fit(x,y),check3)
  expect_equal(NonParamLines::supsmu_fit(x,y,bass=4),check4)
})




