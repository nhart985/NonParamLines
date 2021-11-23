
nearest=function(x,span,x_point) {
  N=length(x)
  J=trunc(span*N)
  abs_diff=abs(x-x_point)
  abs_diff_min=sort(abs_diff)[1:J]
  return(which(abs_diff %in% abs_diff_min))
}

loess_lm_predict=function(x,y,degree,x_point) {
  dist=abs(x-x_point)
  w=(1-(dist/max(dist))^3)^3
  W=diag(w)
  N=length(x)
  if(degree==1) {
    X=matrix(c(rep(1,N),x),nrow=N,ncol=2)
    x_index=c(1,x_point)
  }else if (degree==2) {
    X=matrix(c(rep(1,N),x,x^2),nrow=N,ncol=3)
    x_index=c(1,x_point,x_point^2)
  }
  beta=(solve(t(X)%*%W%*%X)%*%(t(X)%*%W%*%y))[,1]
  return((beta%*%x_index)[,1])
}

loess_fit=function(x,y,degree=2,span=0.75) {
  N=length(x)
  yhat=rep(0,N)
  for(i in 1:N) {
    neighbors=nearest(x,span,x[i])
    yhat[i]=loess_lm_predict(x[neighbors],y[neighbors],degree,x[i])
  }
  return(yhat)
}

x=rnorm(100)
y=rnorm(100)
all.equal(predict(loess(y~x,surface="direct")),loess_fit(x,y))
plot(bench::mark(predict(loess(y~x,surface="direct")),loess_fit(x,y)),type="boxplot")

supsmu_lm_predict=function(x_point,C,V,x_bar,y_bar) {
  beta1=(C/V)
  beta0=y_bar-beta1*x_bar
  return(beta0+beta1*x_point)
}

update_lm_predict=function(x,y,J,C,V,x_bar,y_bar,index) {
  N=length(x)
  if(index-(J/2) < 1) {
    x_bar*J
  }
}

supsmu_fit=function(x,y,span=NULL) {
  N=length(x)
  if(is.null(span)) {
    span=0.2
  }
  J=span*N
  x_sub=x[1:J]
  y_sub=y[1:J]
  x_bar=mean(x_sub)
  y_bar=mean(y_sub)
  C=sum(x_sub-x_bar)*(y_sub-y_bar)
  V=sum(x_sub-x_bar)^2
  yhat=rep(0,N)
  yhat[1]=supsmu_lm_predict(x[1],C,V,x_bar,y_bar)
  for(i in 2:N) {
    yhat[i]=update_lm_predict()
  }
}



