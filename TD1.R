getwd()
log(-1)
1/0
0/0
log(2, base=23)
round(exp(1),digit=3)
asin(3/11)
#asin == arcinus
# c == concatenation [1:n]
# c ne prendra que un type de primitive à la fois on ne peut pas mélanger string avec des int par exemple contrairment au list
#l'affichage peut se faire sans le print

a=c(10,5,3,6,21)
a[2]
2:6
a[1:3]

b<-matrix(c(15,3,12,2,1))
seq(from=1,to=9,by=2)
length.out=5
nrow(a)
ncol(a)
dim(a)
nrow(b)
ncol(b)
dim(b)

rep(1:3,3)
#rep(x,n) prend un vercteur x et le repète n fois 
rep(1:3,each=3)
#rep(x,each=n) repete chaque valeur n fois 
diag(1:5)
diag(c(1,5,9))
#donne une matrice diagonale
#e[3,2] troisieme ligne et deuxieme colonne
e=c(1,2,3,4,5)
2*a + b + 1
e[3]
cos(a)
exp(a)
a*e
a%*%e
b%*%e
f <- t(b)%*%e
#cbind() crée une matrice a partir de vecteur
cbind(a,b)
sort(x)=x[order(x)]
#sd() ecart type abreviation en en anglais 
apply(X = b, MARGIN = 2, FUN = "sum")
# 
col5<- apply (M1,2,"all")
which(col5==T)#
#mat < 5 donne une matrice de boullean donnne si T ou F <5
apply(MARGIN = 2, FUN = "sum", X=b)
apply(X = b, MARGIN = 2, FUN = "sum")
apply(2,b,"sum")


A<-matrix(data = c(1,0,0,0,1,2,0,3,1),nrow=3, ncol = 3,byrow = TRUE)
B<-c(1,2,1)
A
B
x<-solve(A,B)#resoudre le système linéaire
solve(A)
A%*%x
res<-eigen(A, symmetric=FALSE, only.values = FALSE)
eigen(A)#acces au valeur propre
#  a%*%b le produit scallaire 
#a%%b le nombre a est divisible par b 
res$values #=res[[1]] sauf qu'on va l'appeler par sa valeur 
n<-4
res<-1

F<-function(n){
  res<-1
  for (i in 1:n){
    res<-res*i}res}

G<-function(n){
  if(n<0){
    print("n est négatif")
  }
}
G(-1)

V<-function(K,Q){
  if(K<0|Q<0){return("argument incorrect ")}
  res<-1:K
  res2<--res%%Q
  sum(res2==0)#donne un vecteur de boolean
  res[which(res==0)] # res[...] va aller chercher les valeur correspondantes # which(res==0) donne les corrdonner ici 1256
  return(list(n,res))
  }
#EXO 7 A FAIRE A LA MAISON

#EXO 8
P<-function(x){
  2*x^2-8*x+6
}
d=(8^2-4*6*2)
c1<-(-8-sqrt(d))/(2*a)
c2<-(-8+sqrt(d))/(2*a)
a=2
b=(-8)
c=6
abs<-seq(0,5,0.05)

curve(expr=P,from=0,to=5)
abline(v=2)
abline(h=0)
points(x=c(c1,c2), y=c(0,0), col="red")


plot(abs,P(abs))
M<-matrix(NA,3,4)
M

#tab<-(data,frame(M))
tab$X1<-c("toto","titi","tutu")
Y<-(tab$X1)

as.factors(c("M","M","F"))
R<-function(x){
  return(sin(x)^2+sqrt(|x-3|))
}

#exercice 2 TD2

E<-function (a, b, e, Nmax){

n<-0
an<-a
bn<-b
xn<-(n+bn)/2
while(abs(bn-an)>e & n<Nmax){
  if (f(an)*f(xn) < 0){
      bn <-xn}
  else
  {an <-xn
  }
x<-(an+bn)/2
  n<-n+1}
xx<-xn 
return(xx)
}
E
#question 1
A<-function(x){
  exp(x)-(4*x)
}

#question 2
curve(F,from = -3, to=3)

#question 3

dichotomie<-function(a, b, e,F, Nmax){
  res<-c()
  n<-0
  an<-a
  bn<-b
  xn<-(an+bn)/2
  res<-xn
  while(abs(bn-an)>e & n<Nmax){
    if (A(an)*A(xn) < 0){
      bn <-xn}
    else
    {an <-xn
    }
    n<-n+1
    xn<-(an+bn)/2
    res<-c(res,xn)
    xx<-xn }
  
  return(list(res,n))
}

#question 4
dichotomie(1,2.5,1*10^(-8),,100)
#question 8
#1er méthode : e = |xn-x*| fonction w de lambda
#dire que x* = dernier xn donc res[length(res)]
#x<-res[length(res)]
#2e méthode : ae^x+bx+c=0 => discriminant
library("lamW")
delta<-(-1/4)
x1<--lambertW0(-1/4)
x1
x2<--lambertWm1(-1/4)
x2
#x_n <-dichotomie()
xn <-dichotomie(1,2.5,1*10^(-8),,100)[[1]]
n<-length(xn)

plot(1:n,log(abs(xn-x2)))
xn
#Exercice 3
D<-function(p,k0){k0*(1+1/p)}
O<-function(p,alpha,k1){k1*p^alpha}

X<-seq(0.1,10,0.1)
plot(X,D(X,k0),type="l")
lines(X,O(X,alpha,k1),col="red")
P<-function(p,k0,k1,alpha){O(p,alpha,k1)-P(p,k0)}
P<-function(p){k0<-20
k1<-75
alpha<-3/2
O(p,alpha,k1)-P(p,k0)

}
dichotomie(a=0.1,b=10,e=1E-8,P,Nmax = 100)

