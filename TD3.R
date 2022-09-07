

#exo 1
#question 1
x<-g(x)
g(x)<-((x^2)-1)/3
#exo 2
#question 1
#a
ptfixe<-function(x_0,g,e, Nmax){
  x0=x_0
  x1<-g(x0)
  res<-x1
  n<-0
  while(abs(x1-x0)/abs(x1)>e & n<Nmax){
    x0=x1
    x1=g(x0)
    res<-c(res,x1)
    n<-n+1
  }
  return(list(res,n))}
res

#question 3
A<-function(x){
  go<-(x^2)-(2*x)-3=0
  return (go)
}
g1<- function(x){
  y=sqrt((2*x)+3)
  return(y)
}
g2<- function(x){
  y=3/(x/2)
  return(y)
}

g3<- function(x){
  y=((1/2)*((x^2)-3))
  return(y)
}
ptfixe(4,g1,1E-8,100)
ptfixe(4,g2,1E-8,100)
ptfixe(4,g3,1E-8,100)

#question5
gx<-function(x){
  y=sqrt(x+1)
  return(y)
}

res<-ptfixe(1.5,gx,10^8, 100)
plot(ptfixe)
xstar<-(1+sqrt(5))/2

#quetion 6
f<-function(x){x^2-2}
fprime<-function(x){x*2}
x_0=3
#e<-1e-8 

NR<-function(x_0, e,Nmax,f,fprime){
  res<-x_0
  res<-c(res, x_0-(f(x_0)/fprime(x_0)))
  n<-1
  while (abs(res[length(res)]-res[length(res)-1])>e & n<Nmax) {
    res<-c(res,res[length(res)]-(f(res[length(res)])/fprime(res[length(res)])))
           
           n<-n+1
  }
  return(list(res,n))
  }
NR(3,1E-1,20,f,fprime)
f<-function(x){c(3*x[1]^2-x[2]^2, 3*x[1]*x[2]-x[1]^3-1)}
J<-function(x){
  M<-matrix(NA,now=2,ncol = 2)
  M[1,1]<-6*x[1]
  M[1,2]<-(-2)*x[2]
  M[2,1]<-(3*x[2])-(3*x[1]^2)
  M[2,2]<-3*x[1]
  M
  
}
x0<-c(1,1)

NRd2<-function(x_0, f,Nmax,J,e){
  res<-x_0
  res<-rblind(res, x_0-solve(J(x_0),f(x_0)) )
  
  n<-1
  while (dist(res[nrow(res),],res[nrow(res)-1])>e & n<Nmax) {
    res<-rblind(res,res[nrow(res)]-solve(J(res[nrow(res)],),f(res[nrow(res)])))
    n<-n+1
  }
  return(list(res,n))
}
NRd2(0.64,f,Nmax, 0.79)