
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

supsmu_lm_predict=function(x_point,C,V,x_bar,y_bar) {
  beta1=(C/V)
  beta0=y_bar-beta1*x_bar
  return(beta0+beta1*x_point)
}

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

get_initial_values=function(x,y) {
  x_bar=mean(x)
  y_bar=mean(y)
  C=sum((x-x_bar)*(y-y_bar))
  V=sum((x-x_bar)^2)
  return(c(C,V,x_bar,y_bar))
}

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
    best_spans=best_spans+(J[1]-best_spans)*(ratio^(10-bass))
    best_spans=pmin(supsmu_fit_span(x,best_spans,span=0.2),500)
    yhat=Yhat[,2]-Yhat[,2]*(best_spans-J[2])/(J[1]-J[2])+Yhat[,1]*(best_spans-J[2])/(J[1]-J[2])
    yhat[best_spans < J[2]]=Yhat[,3]*(1-(best_spans-J[3])/(J[2]-J[3]))+Yhat[,2]*((best_spans-J[3])/(J[2]-J[3]))
    yhat=supsmu_fit_span(x,yhat,span=0.05)
  }
  return(yhat)
}





