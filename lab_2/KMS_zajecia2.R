############## LABORATORIUM 2 ###########################
########################### ZADANIE 1.2 #######################################

# ZAD 1.2
# a)
library(MASS)
data(Cars93)
attach(Cars93)
Min.Price
?Cars93

# b) 
#

zuzycie.miasto <- ((1/MPG.city) * (380/1.6))
zuzycie.autostrada <- (1/MPG.highway)*(380/1.6)
wagaKg <- Weight * 0.4536
cenaZl <- Min.Price * 3.35

#c) kwantyl rzedu 0.95 dla ceny podstawowej wersji samochodu
kw0.95 <- quantile(cenaZl, 0.95) # =116

#podstawowe statystyki probkowe
mean(cenaZl)
median(cenaZl)
min(cenaZl)
max(cenaZl)
var(cenaZl)
sd(cenaZl)
#kwantyl dolny = 36.18 - 25% najtanszych samochodow w 
#wersji podstawowej jest tanszych niz cena 36.16 tys. z³ 
quantile(cenaZl, 0.25)
quantile(cenaZl, 0.75)

#d).
#ceny samochodów w wersji podst, które s¹ wy¿sze od kwantyla 
#rzedu 0.95
cenaZl[cenaZl>kw0.95]
#modele dla których ceny samochodów w wersji podst s¹ wy¿sze
#od kwantyla 
#rzedu 0.95
Make[cenaZl>kw0.95]
# e)
Type
licznosci <- summary(Type)
licznosci
# wykres slupkowy
barplot(licznosci)
# wykres kolowy
pie(licznosci)
# 14 typu sportowego
licznosci["Sporty"]

# f). 
boxplot(zuzycie.miasto~Origin)$stats

#g)
# wykres rozrutu ceny podstawowej samochodu 
# od jego zu zycia benzyny w mie scie
kor1 = cor(cenaZl, zuzycie.miasto)
plot(zuzycie.miasto, cenaZl, main = kor1)

# wykres rozrutu zuzycia benzyny w miescie 
# od jego zu zycia benzyny na autostradzie
kor2 = cor(zuzycie.miasto, zuzycie.autostrada)
plot(zuzycie.autostrada, zuzycie.miasto, main = kor2)

# w jednym oknie oba wykresy
par(mfrow=c(1,2)) # dwa w wierszu
plot(zuzycie.miasto, cenaZl, main = kor1)
plot(zuzycie.autostrada, zuzycie.miasto, main = kor2)

#h
#histogram czestosci dot. wagi 
par(mfrow=c(1,2))
hist(wagaKg, freq=FALSE)
plot(density(wagaKg))

#############################################################
############### ZADANIE 2.1 #################################
### a)
data(hills)
attach(hills)

### b)
par(mfrow=c(1,2))
kor1 <- cor(dist, time)
plot(dist, time, main=kor1)
kor2 <- cor(time, climb)
plot(climb, time, main=kor2)

# c) 
# model liniowy time od dist
model.time.dist <- lm(time~dist)
model.time.dist
# czas=8,330*dlugosc-4,841+blad
summary(model.time.dist)
# model liniowy time od climb
model.time.climb <- lm(time~climb)
model.time.climb
# czas=0,02489*roznica wzniesien+12,69917+blad
summary(model.time.climb)

par(mfrow=c(1,2))
plot(dist, time, main=kor1)
abline(model.time.dist)
plot(climb, time, main=kor2)
abline(model.time.climb)

# d) Diagnostyka dopasowania
# Wspolczynnik determinacji
summary(model.time.dist)$r.squared #=0.8456444
# 84,56% zmiennosci czasu pokonywania trasy jest wyjasniane 
# przez model (czyli przez dlugosc trasy).
# Residua
par(mfrow=c(2,2))
plot(model.time.dist)
# Wykres lewy górny przedstawia wykres rozrzutu residuow 
# w funkcji wartosci prognozowanych (powinien przypominac
# cmure punktow bez zadnej wyraznej struktury czy tendencji)
# Tutaj jest OK.
# Wykres prawy gorny to wykres kwantylowy dla residuow 
# standardyzowanych (punkty powinny ukladac sie wzdluz 
# prostej)
# Mniej-wiecej jest OK, tylko 4 obserwacje psuja idealny 
# wykres.
# Wykres lewy dolny to wykres pierwiatkow z modulow residuow 
# standardyzowanych w funkcji wartosci prognozowanych
# (sluzy do wykrywania nierownych wariancji)
# wykres prawy dolny widac odleglosci Cooke'a
# Obserwacje o du¿ych odlegosciach Cooke'a to obserwacje
# potencjalnie wpywowe lub odstajace
par(mfrow=c(1,1))
plot(model.time.dist,which=4)
# Potencjalne obserwacje wplywowe lub odstajace to 11,7 i 18
# Obserwacje 11 i 7 to obserwacje o bardzo dlugich trasach
hills

# zadania do domu: Zad 2.2 a, b, Zad 2.3 a, b

