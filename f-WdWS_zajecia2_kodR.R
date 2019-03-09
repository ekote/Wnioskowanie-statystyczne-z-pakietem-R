############ |ZAJÊCIA 2 (wtorek) ###############

# zadanie 2.1
# b)
library("MASS")
dane <- c(423, 705, 2623, 347, 620, 2719, 1035, 482)
lambda_nw <- 1 / mean(dane)
print(lambda_nw)
?fitdistr
lambda_nw_fitdistr <- fitdistr(x = dane,
                densfun = "exponential")$estimate
print(lambda_nw_fitdistr)
#c)
ex_nw <- 1/lambda_nw_fitdistr
print(ex_nw)
p_1000_nw <- exp(-lambda_nw_fitdistr*1000)
print(p_1000_nw)

#ZADANIE 2.3
#Generuje liczby z rozkladu Gamma(2,1)
set.seed(1337) #ustawienie ziarna = powtarzalnosc
Dane100 <- rgamma(n=100,shape=2,scale=1)
Dane1000 <- rgamma(n=1000,shape=2,scale=1)

params100 <-fitdistr(Dane100,"gamma"
                     ,lower=c(0,0))
params1000 <-fitdistr(Dane1000,
                      "gamma",lower=c(0,0))

print(params100)
print(params1000)

#ZADANIE 1.3
library(e1071)
dane_1 <- c( ((-1-sqrt(10))/4), -1/4, -1/4, 
             ((sqrt(10) - 1)/4), 1)
dane_1
skewness(dane_1)

#1.4
quantile(c(5,8,9,3,8,7), 
         probs = c(0.25, 0.5, 0.75))

############################################
# Wykres funkcji y=sin x na przedziale (0,pi/2)
plot(x<-seq(0,pi/2,length.out = 1000),
     y=sin(x))
curve(sin(x),from=0,to=pi/2)

# Niech X ma rozk³ad normalny o sredniej m i
# wariancji sigma^2 (m=1,sigma^2=5)
# Chcemy policzyæ f(2) gdzie f to gêstoœæ X
dnorm(2,mean=1,sd=sqrt(5))
?pnorm
# Liczymy P(X<=7)
pnorm(7,mean=1,sd=sqrt(5))
# P(X>7)=
pnorm(7,mean=1,sd=sqrt(5),lower.tail=F)

# qnorm wyznacza kwantyle
# kwantyl rzêdu 0.7 dla X
qnorm(0.7,mean=1,sd=sqrt(5))
