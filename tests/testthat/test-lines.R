test_that("neighbors works", {
  expect_equal(NonParamLines::nearest(1:10,0.3,4),c(3,4,5))
  expect_equal(NonParamLines::nearest(1:10,0.1,4),4)
  expect_equal(NonParamLines::nearest(c(0.1,0.4,0.3,0.5),0.5,0.2),c(1,3))
})

test_that("loess_lm_predict works", {
  dist=abs(cars$speed-cars$speed[1])
  w=(1-(dist/max(dist))^3)^3
  check1=predict(lm(cars$dist~cars$speed,weights=w))[1]
  names(check1)=NULL
  check2=predict(lm(cars$dist~cars$speed+I(cars$speed^2),weights=w))[1]
  names(check2)=NULL
  expect_equal(NonParamLines::loess_lm_predict(cars$speed,cars$dist,1,cars$speed[1]),check1)
  expect_equal(NonParamLines::loess_lm_predict(cars$speed,cars$dist,2,cars$speed[1]),check2)
  set.seed(1)
  x=seq(0,10,length.out=100)
  y=sin(x)+rnorm(100,0,0.5)
  dist=abs(x-x[25])
  w=(1-(dist/max(dist))^3)^3
  check1=predict(lm(y~x,weights=w))[25]
  names(check1)=NULL
  check2=predict(lm(y~x+I(x^2),weights=w))[25]
  names(check2)=NULL
  expect_equal(NonParamLines::loess_lm_predict(x,y,1,x[25]),check1)
  expect_equal(NonParamLines::loess_lm_predict(x,y,2,x[25]),check2)
})

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

test_that("supsmu_lm_predict works", {
  set.seed(1)
  x=seq(0,10,length.out=100)
  y=sin(x)+rnorm(100,0,0.5)
  x_bar=mean(x)
  y_bar=mean(y)
  C=sum((x-mean(x))*(y-mean(y)))
  V=sum((x-mean(x))^2)
  check1=predict(lm(y~x))[1]
  check2=predict(lm(y~x))[2]
  names(check1)=NULL
  names(check2)=NULL
  expect_equal(NonParamLines::supsmu_lm_predict(x[1],C,V,x_bar,y_bar),check1)
  expect_equal(NonParamLines::supsmu_lm_predict(x[2],C,V,x_bar,y_bar),check2)
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




