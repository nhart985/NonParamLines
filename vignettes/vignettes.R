## ---- message=F, results='hide'-----------------------------------------------
devtools::install_github("nhart985/NonParamLines")

## ----fig.height=10------------------------------------------------------------
set.seed(1)
data(cars)
speed=cars$speed
distance=cars$dist
par(mfrow=c(2,1))
plot(speed,distance,main="NonParamLines Implementation")
lines(speed,NonParamLines::loess_fit(speed,distance),col=4,lwd=3)
plot(speed,distance,main="Existing Implementation")
lines(speed,predict(stats::loess(distance~speed,surface="direct")),col=4,lwd=3)

## -----------------------------------------------------------------------------
all.equal(NonParamLines::loess_fit(speed,distance),
          predict(stats::loess(distance~speed,surface="direct")))

## -----------------------------------------------------------------------------
NonParamLines_Implementation=function(x,y) {
  return(NonParamLines::loess_fit(speed,distance))
}
Existing_Implementation=function(x,y) {
  return(predict(stats::loess(distance~speed,surface="direct")))
}
x=speed
y=distance
plot(bench::mark(NonParamLines_Implementation(x,y),
     Existing_Implementation(x,y)),type="boxplot")

## ---- fig.height=5------------------------------------------------------------
set.seed(1)
x=seq(0,10,length.out=1000)
y=sin(x)+rnorm(1000,0,0.5)
par(mfrow=c(1,1))
plot(x,y,ylim=c(-4,2.5))
lines(x,NonParamLines::loess_fit(x,y),col=4,lwd=3) #Quadratic, Span=0.75
lines(x,NonParamLines::loess_fit(x,y,degree=1,span=0.5),col=3,lwd=3) #Linear, Span=0.5
legend("bottomright",legend=c("Degree=2, Span=0.75","Degree=1, Span=0.5"),
       col=c(4,3),lwd=c(3,3),lty=c(1,1))

## -----------------------------------------------------------------------------
all.equal(NonParamLines::loess_fit(x,y),
          predict(stats::loess(y~x,surface="direct")))
all.equal(NonParamLines::loess_fit(x,y,degree=1,span=0.5),
          predict(stats::loess(y~x,degree=1,span=0.5,surface="direct")))

## ----fig.height=5-------------------------------------------------------------
set.seed(1)
x=seq(0,10,length.out=1000)
y=sin(x)+rnorm(1000,0,0.5)
plot(x,y,main="NonParamLines Implementation",ylim=c(-4,2.5))
lines(x,NonParamLines::supsmu_fit(x,y,span=0.2),col=4,lwd=3) #Span=0.2
lines(x,NonParamLines::supsmu_fit(x,y,span=0.5),col=3,lwd=3) #Span=0.5
legend("bottomright",legend=c("Span=0.2","Span=0.5"),
       col=c(4,3),lwd=c(3,3),lty=c(1,1))

## -----------------------------------------------------------------------------
all.equal(NonParamLines::supsmu_fit(x,y,span=0.2),supsmu(x,y,span=0.2)$y)
all.equal(NonParamLines::supsmu_fit(x,y,span=0.5),supsmu(x,y,span=0.5)$y)

## -----------------------------------------------------------------------------
NonParamLines_Implementation=function(x,y) {
  return(NonParamLines::supsmu_fit(x,y,span=0.2))
}
Existing_Implementation=function(x,y) {
  return(supsmu(x,y,span=0.2)$y)
}
plot(bench::mark(NonParamLines_Implementation(x,y),
     Existing_Implementation(x,y)),type="boxplot")

## -----------------------------------------------------------------------------
set.seed(1)
x=(1:1000)
y=x+rnorm(1000,5,500)
plot(x,y,ylim=c(-1500,2050))
lines(sort(x),NonParamLines::supsmu_fit(x,y),col=4,lwd=3)

## -----------------------------------------------------------------------------
all.equal(NonParamLines::supsmu_fit(x,y),supsmu(x,y)$y)

## -----------------------------------------------------------------------------
set.seed(1)
x=seq(0,10,length.out=1000)
y=sin(x)+rnorm(1000,0,0.5)
plot(x,y,ylim=c(-4,2.5))
lines(sort(x),NonParamLines::supsmu_fit(x,y,bass=4),col=4,lwd=3) #Bass=4
lines(sort(x),NonParamLines::supsmu_fit(x,y),col=3,lwd=3) #Bass=0
legend("bottomright",legend=c("Bass=0","Bass=4"),
       col=c(3,4),lwd=c(3,3),lty=c(1,1))

## -----------------------------------------------------------------------------
all.equal(NonParamLines::supsmu_fit(x,y,bass=4),supsmu(x,y,bass=4)$y)
all.equal(NonParamLines::supsmu_fit(x,y),supsmu(x,y)$y)

## -----------------------------------------------------------------------------
NonParamLines_Implementation=function(x,y) {
  return(NonParamLines::supsmu_fit(x,y))
}
Existing_Implementation=function(x,y) {
  return(supsmu(x,y)$y)
}
plot(bench::mark(NonParamLines_Implementation(x,y),
     Existing_Implementation(x,y)),type="boxplot")

