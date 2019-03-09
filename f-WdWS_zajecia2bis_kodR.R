############## ZAJÊCIA 2 (ŒRODA) ############################

#ZAD 1.4
dane<-c(3,5,7,8,8,9)
summary(dane)
#Med=7.5
#Q1=5.5; Q3=8 

#2.1b)
library(MASS)
czas.pracy <- c(423, 705, 2623, 347, 620, 2719, 1035, 482)
sredni.czas.pracy<-mean(czas.pracy)
estymator <- 1 / sredni.czas.pracy # 0.000893455438909984
print(fitdistr(czas.pracy, "exponential"))
#c esrtymator wartosci oczekiwanej
estymator2<-mean(czas.pracy) #1119.25
#ii estymator prawdopodobieñstwa 
#,¿e bateryjka bêdzie dzia³aæ d³u¿ej ni¿ 1000godzin
estymator3<-exp(-1000/mean(czas.pracy)) 
#estymator prawdopodobieñstwa=0.409

#2.3
?rgamma
set.seed(1)
gamma100<-rgamma(n=100,shape=2,scale=1)
gamma100res<-fitdistr(gamma100,"gamma",lower=c(0,0))
gamma100res

gamma1000<-rgamma(n=1000,shape=2,scale=1)
gamma1000res<-fitdistr(gamma1000,"gamma", lower=c(0,0))
gamma1000res

########## Zad 3.1 #################################
# funkcja licz¹ca gêstoœæ standardowego rozk³adu normalnego 
# dnorm(x)
# kwantyl rzedu 0,001
?dnorm
?qnorm
wektor_prawdopodobienst<-c(0.001,0.999)
wartosci_kwawntyli<-qnorm(wektor_prawdopodobienst)
x<-seq(-3.09,3.09,0.001)
x
plot(x,dnorm(x),cex=0.1)
title("Gauss dla kwantyli {0.001,0.999}")
?dt
lines(x,dt(x,5),col="red")
lines(x,dt(x,10),col="green")
lines(x,dt(x,20),col="blue")
lines(x,dt(x,50),col="darkred")
lines(x,dt(x,100),col="darkgreen")
legend(0.30,col=c("red","green","blue","darkred","darkgreen"),legend=c(5,10,20,50,100))
?legend
