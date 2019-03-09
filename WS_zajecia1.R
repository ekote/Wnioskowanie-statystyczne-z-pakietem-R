####### LABORATORIUM 1 ###################
############# ZAD 1.1
# a).
waga <- c(60,72,57,90,95,72)
wzrost <- c(1.75,1.8,1.65,1.9,1.74,1.91)
waga

dane <- data.frame(waga,wzrost)
dane

dane$waga
attach(dane)
detach(dane)

# b). 
bmi <- waga/(wzrost^2)
bmi

komentarz <- rep("",6)
for (i in 1:6) {
  if (bmi[i]<20.7) komentarz[i] <- "niedowaga"
  else{
    if (bmi[i]>=26.5) komentarz[i] <- "nadwaga"
  else komentarz[i] <- "waga prawidlowa"
  }
}
komentarz

# c) 
mean(waga)
median(waga) # obserwacja srodkowa po ustawieniu obserwacji w kolejnosci rosnacej 
var(waga)
sd(waga)

# d) wspolczynnik korelacji nalezy do przedzialu [-1,1]
# miara sily zaleznosci liniowej
cor(waga,wzrost)

# f) 
par(mfrow=c(1,2))
plot(wzrost,waga,xlab="wzrost",ylab="waga")
plot(wzrost^2,waga,xlab="kwadrat wzrostu",ylab="waga")
?plot

# e) histogram licznosci
par(mfrow=c(1,1))
hist(waga,breaks=3)
# wykres skrzynkowy
boxplot(waga,horizontal = T)$stats

# kartyle
quantile(waga,c(0.25,0.75))

# ZAD 1.2
zbiory <- read.table(file.choose(),header=T)
zbiory$yield
attach(zbiory)
yield

# e).
table(year,site)

# ZAD 1.4

# a)
?rnorm
set.seed(1764)
probka1 <- rnorm(n=500,mean=20,sd=5)
# lub
rnorm(500,20,5)
?runif
probka2 <- runif(500,-1,1)
?rexp
probka3 <- rexp(500,1/5)
?rpois
probka4 <- rpois(500,lambda=3)

# b).

par(mfrow=c(2,2))
qqnorm(probka1)
qqline(probka1)
qqnorm(probka2)
qqline(probka2)
qqnorm(probka3)
qqline(probka3)
qqnorm(probka4)
qqline(probka4)

# c) test Shapiro-Wilka
shapiro.test(probka1)
# H_0: rozk³ad, z ktorego pochodzi proba, jest normalny
# H_1: rozk³ad, z ktorego pochodzi proba, nie jest rozk³adem normalnym
# p-value = 0.8583 > alfa (poziom istotnosci) = 0.05 => nie 
# mamy podstaw do odrzucenia H_0 => mo¿emy przyj¹æ, ¿e próba pochodzi
# z rozk³adu normalnego
shapiro.test(probka3)
# p-value < 2.2e-16 < alfa => odrzucamy H_0 czyli stwierdzamy, ¿e dane 
# nie pochodza z rozk³adu normalnego

# DO DOMU: Zad 1.2 bez e, Zad 1.3