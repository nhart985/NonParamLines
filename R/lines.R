
#'nearest
#'
#'Helper function for loess_fit(): Finds the indices of the span*N nearest neighbors to a given x value, where N is the total number of datapoints
#'@param x A numeric vector corresponding to the independent variable
#'@param span The proportion of datapoints to include in the local fit
#'@param x_point The x value that is used to predict a y value
#'
#'@return A vector of indices
#'
#'@examples
#'x=rnorm(100)
#'nearest(x,0.75,x[1])
#'
#'
#'@export
#'
nearest=function(x,span,x_point) {
  N=length(x)
  J=trunc(span*N)
  abs_diff=abs(x-x_point)
  abs_diff_min=sort(abs_diff)[1:J]
  return(which(abs_diff %in% abs_diff_min))
}

#'loess_lm_predict
#'
#'Helper function for loess_fit(): Computes a prediction from the estimated local polynomial regression
#'@param x A numeric vector corresponding to the independent variable
#'@param y A numeric vector corresponding to the dependent variable
#'@param degree The degree of the local polynomial fit (1=linear fit, 2=quadratic fit)
#'@param x_point The x value that is used to predict a y value
#'
#'@return A scalar prediction of the y variable at x_point
#'
#'@details
#'The local polynomial model is fit by weight least squares with a tricubic weight function.
#'When degree=1, only an intercept and linear term for x are included in the model.
#'When degree=2, an intercept, linear term, and quadratic term for x are included in the model.
#'Predictions are made at x_point using the resulting estimates. See Cleveland, 1979 for details.
#'
#'@examples
#'x=rnorm(100)
#'y=x+rnorm(1000,5,500)
#'nearest_indices=nearest(x,0.75,x[1])
#'x_sub=x[nearest_indices]
#'y_sub=y[nearest_indices]
#'loess_lm_predict(x_sub,y_sub,2,x[1])
#'
#'@export
#'
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

#'loess_fit
#'
#'Computes LOESS local polynomial predictions (Cleveland, 1979)
#'@param x A numeric vector corresponding to the independent variable
#'@param y A numeric vector corresponding to the dependent variable
#'@param degree The degree of the local polynomial fit (1=linear fit, 2=quadratic fit)
#'@param span The proportion of datapoints to include in the local fit
#'
#'@return A numeric vector of LOESS predictions for the y variable at each value of the x variable
#'
#'@examples
#'x=(1:1000)
#'y=x+rnorm(1000,5,500)
#'loess_fit(x,y,degree=2,span=0.75)
#'
#'@export
#'
loess_fit=function(x,y,degree=2,span=0.75) {
  N=length(x)
  yhat=rep(0,N)
  for(i in 1:N) {
    neighbors=nearest(x,span,x[i])
    yhat[i]=loess_lm_predict(x[neighbors],y[neighbors],degree,x[i])
  }
  return(yhat)
}

#'supsmu_lm_predict
#'
#'Helper function for supsmu_fit(): Computes a prediction from the estimated local linear regression
#'@param x_point The x value that is used to predict a y value
#'@param C The C term corresponding to the local regression estimation (Friedman, 1984)
#'@param V The V term corresponding to the local regression estimation (Friedman, 1984)
#'@param x_bar The local x average
#'@param y_bar The local y average
#'
#'@return A scalar prediction of the y variable at x_point
#'
#'@examples
#'x=(1:1000)
#'y=x+rnorm(1000,5,500)
#'u=get_inital_values(x,y)
#'supsmu_lm_predict(x[1],u[1],u[2],u[3],u[4])
#'
#'
#'@export
#'
supsmu_lm_predict=function(x_point,C,V,x_bar,y_bar) {
  beta1=(C/V)
  beta0=y_bar-beta1*x_bar
  return(beta0+beta1*x_point)
}

#'update_lm_predict
#'
#'Helper function for supsmu_fit(): Updates the estimates of the previous local linear regression without refitting the model
#'@param x A numeric vector corresponding to the independent variable
#'@param y A numeric vector corresponding to the dependent variable
#'@param J The span value times the total number of datapoints
#'@param C The C term used for regression estimation (Friedman, 1984)
#'@param V The V term used for regression estimation (Friedman, 1984)
#'@param index The index of the current prediction point
#'
#'@return A vector with four elements: (1) C term, (2) V term, (3) Local x average, and (4) Local y average
#'
#'@examples
#'x=(1:1000)
#'y=x+rnorm(1000,5,500)
#'u=get_inital_values(x,y)
#'u=update_lm_predict(x,y,u[1],u[2],u[3],u[4],2)
#'
#'@details
#'See Friedman, 1984 for details.
#'
#'@export
#'
update_lm_predict=function(x,y,J,C,V,x_bar,y_bar,index) {
  J_half=J%/%2
  N=length(x)
  if(index > J_half+1 & index < N-J_half+1) {
    x_bar_temp=((J+1)*x_bar-x[index-J_half-1])/J
    y_bar_temp=((J+1)*y_bar-y[index-J_half-1])/J
    C=C-x[index-J_half-1]*y[index-J_half-1]+(J+1)*x_bar*y_bar-J*x_bar_temp*y_bar_temp
    V=V-x[index-J_half-1]^2+(J+1)*x_bar^2-J*x_bar_temp^2
    x_bar=((J+1)*x_bar+x[index+J_half]-x[index-J_half-1])/(J+1)
    y_bar=((J+1)*y_bar+y[index+J_half]-y[index-J_half-1])/(J+1)
    C=C+((J+1)/J)*(x[index+J_half]-x_bar)*(y[index+J_half]-y_bar)
    V=V+((J+1)/J)*(x[index+J_half]-x_bar)^2
  }
  return(c(C,V,x_bar,y_bar))
}

#'supsmu_fit_span
#'
#'Helper function for supsmu_fit(): Fits a super smoother corresponding to the fixed span value
#'@param x A numeric vector corresponding to the independent variable
#'@param y A numeric vector corresponding to the dependent variable
#'@param span The proportion of datapoints to include in the local fit
#'
#'@return A numeric vector of super smoother predictions for the y variable at each value of the x variable (sorted by the x variable)
#'
#'@examples
#'x=(1:1000)
#'y=x+rnorm(1000,5,500)
#'supsmu_fit_span(x,y,span=0.2)
#'
#'
#'@details
#'This function is the workhorse of supsmu_fit() if a fixed span is requested.
#'Otherwise, it serves as an intermediate helper function in the variable span algorithm (Friedman, 1984).
#'
#'@export
#'
supsmu_fit_span=function(x,y,span=0.2) {
  y=y[order(x)]
  x=sort(x)
  N=length(x)
  J=trunc(span*N)
  x_sub=x[1:(J+1)]
  y_sub=y[1:(J+1)]
  x_bar=mean(x_sub)
  y_bar=mean(y_sub)
  C=sum((x_sub-x_bar)*(y_sub-y_bar))
  V=sum((x_sub-x_bar)^2)
  yhat=rep(0,N)
  yhat[1]=supsmu_lm_predict(x[1],C,V,x_bar,y_bar)
  for(i in 2:N) {
    u=update_lm_predict(x,y,J,C,V,x_bar,y_bar,i)
    C=u[1]
    V=u[2]
    x_bar=u[3]
    y_bar=u[4]
    yhat[i]=supsmu_lm_predict(x[i],C,V,x_bar,y_bar)
  }
  return(yhat)
}

#'get_initial_values
#'
#'Helper function for supsmu_fit(): Computes the terms needed to estimate the first local regression line
#'@param x A numeric vector corresponding to the independent variable
#'@param y A numeric vector corresponding to the dependent variable
#'
#'@return A vector with four elements: (1) C term, (2) V term, (3) Local x average, and (4) Local y average
#'
#'@examples
#'x=(1:1000)
#'y=x+rnorm(1000,5,500)
#'get_inital_values(x,y)
#'
#'
#'@details
#'See Friedman, 1984 for details.
#'
#'@export
#'
get_initial_values=function(x,y) {
  x_bar=mean(x)
  y_bar=mean(y)
  C=sum((x-x_bar)*(y-y_bar))
  V=sum((x-x_bar)^2)
  return(c(C,V,x_bar,y_bar))
}

#'supsmu_fit_three_span
#'
#'Helper function for supsmu_fit(): Fits super smoothers corresponding to spans of 0.5,0.2, and 0.05 with only one loop
#'@param x A numeric vector corresponding to the independent variable
#'@param y A numeric vector corresponding to the dependent variable
#'
#'@return A list with three elements: (1) an Nx3 matrix of predictions for each span,
#'(2) an Nx3 matrix of local x averages for each span, and (3) an Nx3 matrix of V terms for each span (Friedman, 1984).
#'N is the total number of datapoints.
#'
#'@examples
#'x=(1:1000)
#'y=x+rnorm(1000,5,500)
#'supsmu_three_span(x,y)
#'
#'
#'@export
#'
supsmu_fit_three_span=function(x,y) {
  y=y[order(x)]
  x=sort(x)
  N=length(x)
  J1=trunc(0.5*N)
  J2=trunc(0.2*N)
  J3=trunc(0.05*N)
  x_sub1=x[1:(J1+1)]
  y_sub1=y[1:(J1+1)]
  x_sub2=x[1:(J2+1)]
  y_sub2=y[1:(J2+1)]
  x_sub3=x[1:(J3+1)]
  y_sub3=y[1:(J3+1)]
  V=matrix(0,N,3)
  Xbar=matrix(0,N,3)
  u1=get_initial_values(x_sub1,y_sub1)
  u2=get_initial_values(x_sub2,y_sub2)
  u3=get_initial_values(x_sub3,y_sub3)
  V[1,]=c(u1[2],u2[2],u3[2])
  Xbar[1,]=c(u1[3],u2[3],u3[3])
  yhat1=rep(0,N)
  yhat2=rep(0,N)
  yhat3=rep(0,N)
  yhat1[1]=supsmu_lm_predict(x[1],u1[1],u1[2],u1[3],u1[4])
  yhat2[1]=supsmu_lm_predict(x[1],u2[1],u2[2],u2[3],u2[4])
  yhat3[1]=supsmu_lm_predict(x[1],u3[1],u3[2],u3[3],u3[4])
  for(i in 2:N) {
    u1=update_lm_predict(x,y,J1,u1[1],u1[2],u1[3],u1[4],i)
    u2=update_lm_predict(x,y,J2,u2[1],u2[2],u2[3],u2[4],i)
    u3=update_lm_predict(x,y,J3,u3[1],u3[2],u3[3],u3[4],i)
    V[i,]=c(u1[2],u2[2],u3[2])
    Xbar[i,]=c(u1[3],u2[3],u3[3])
    yhat1[i]=supsmu_lm_predict(x[i],u1[1],u1[2],u1[3],u1[4])
    yhat2[i]=supsmu_lm_predict(x[i],u2[1],u2[2],u2[3],u2[4])
    yhat3[i]=supsmu_lm_predict(x[i],u3[1],u3[2],u3[3],u3[4])
  }
  return(list(matrix(c(yhat1,yhat2,yhat3),nrow=N),V,Xbar))
}

#'supsmu_fit
#'
#'Computes local linear predictions using Friedman's super smoother (Friedman, 1984)
#'@param x A numeric vector corresponding to the independent variable
#'@param y A numeric vector corresponding to the dependent variable
#'@param span The proportion of datapoints to include in the local fit (Optional)
#'@param bass A value between 0 and 10 that controls the smoothness of the predictions (See Details)
#'
#'@return A numeric vector of super smoother predictions for the y variable at each value of the x variable (sorted by the x variable)
#'
#'@examples
#'x=(1:1000)
#'y=x+rnorm(1000,5,500)
#'supsmu_fit(x,y,span=0.2)
#'supsmu_fit(x,y,bass=5)
#'supsmu_fit(x,y)
#'
#'@details
#'If the span argument is specified, then a fixed span is used for each x value to
#'estimate the local linear regression line, and the bass argument is ignored. Otherwise,
#'the span is selected for each point via cross-validation, as in Friedman, 1984. For the variable span
#'algorithm, bass values closer to 10 result in a smoother-looking curve. Note that ties in the x variable
#' are treated as distinct points, which is different from the behavior of stats::supsmu()
#' (this function's method for handling ties is undocumented).
#'
#'@export
#'
supsmu_fit=function(x,y,span=NULL,bass=0) {
  if(!is.null(span)) {
    yhat=supsmu_fit_span(x,y,span)
  }
  else {
    y=y[order(x)]
    x=sort(x)
    N=length(x)
    fit=supsmu_fit_three_span(x,y)
    Yhat=fit[[1]]
    V=fit[[2]]
    Xbar=fit[[3]]
    X_diff=(matrix(rep(x,3),nrow=N)-Xbar)^2
    J=c(0.5,0.2,0.05)*N
    den=t(1-(1/(J+1))-t(X_diff/V))
    r=abs(y-Yhat)/den
    fitr1=supsmu_fit_span(x,r[,1],span=0.2)
    fitr2=supsmu_fit_span(x,r[,2],span=0.2)
    fitr3=supsmu_fit_span(x,r[,3],span=0.2)
    fitsr=matrix(c(fitr1,fitr2,fitr3),nrow=N)
    best_spans=J[apply(fitsr,1,which.min)]
    best_rhat=pmin(fitsr[,1],fitsr[,2],fitsr[,3])
    w_rhat=fitsr[,1]
    ratio=best_rhat/w_rhat
    if(bass!=0) {
      best_spans=best_spans+(J[1]-best_spans)*(ratio^(10-bass))
    }
    best_spans=pmin(supsmu_fit_span(x,best_spans,span=0.2),500)
    yhat=Yhat[,2]-Yhat[,2]*(best_spans-J[2])/(J[1]-J[2])+Yhat[,1]*(best_spans-J[2])/(J[1]-J[2])
    yhat[best_spans < J[2]]=Yhat[,3][best_spans < J[2]]*(1-(best_spans[best_spans < J[2]]-J[3])/(J[2]-J[3]))+Yhat[,2][best_spans < J[2]]*((best_spans[best_spans < J[2]]-J[3])/(J[2]-J[3]))
    yhat=supsmu_fit_span(x,yhat,span=0.05)
  }
  return(yhat)
}
