## zadanie 1.1
# przecinki traktujemy jako kropki
# wtorzymy wektory
waga <- c(60, 72, 57, 90, 95, 72)
  # temu co sie nazywa waga przyporzadkuwujemy, mozemy z = ale lepiej tak
wzrost <- c(1.75, 1.80, 1.65, 1.9, 1.74, 1.91)
# jak odpalic ten kod? 
# stworzylismy wektor danych

#a2. - ramka danych - bo nam jest wygodniej

dane <- data.frame(waga, wzrost)

dane
# odwolanie sie
dane$waga

#korzystanie z ramki danych
attach(dane)
# mowimy ze nie chcemy juz korzystac z tych danych
detach(dane)
# wektor z ramki danych jak streaming

# b. W R pracujemy na wektorach
bmi <- waga / (wzrost^2)
bmi

# rep od repetition 
# elementy typu charakter piszemy w cudzyslowie
komentarz <- rep("", 6)
komentarz

for (i in 1:6) {
  if (bmi[i] < 20.7) komentarz[i] <- "niedowaga"
  else if (bmi[i] >= 26.5) komentarz[i] <- "nadwaga"
  else komentarz[i] <- "jest ok"
}

komentarz

# c.
# srednia
mean(waga)

# mediana - obserwacja srodkowa po ustawieniu obserwacji w kolejnosci rosnacej
median(waga)

#wariancia
var(waga)

# odchylenie standarowe
sd(waga)


# wspolczynnik korelacji pomiedzy waga a wzrostem
cor(waga, wzrost)
# miara sily zaleznosci liniowej

# jak cor = 1 to idealna zaleznosc liniowa - obie rosna 
# -1 to malejaca czyli jak jedna rosnie to druga maleje
# wykres rozrzutu



# liczba wierszy, liczba kolumn
par(mfrow=c(1,2))


# f. wykres rozrzutu wagi wzgledem wzrostu
plot(wzrost, waga, xlab="wzrost", ylab="waga")
# plot(x, y)

# wzgledem kwadratu
plot(wzrost^2, waga, xlab="kwadrat wzrostu", ylab="waga")

# e. histogram i wykres skrzynkowy
# histogram licznosci
par(mfrow=c(1,1))

hist(waga)
hist(waga, breaks=2)

# wykres skrzynkowy
# taki pocisk, umieszczamy na nim mediane, dolny kwartyl, albo 1 kwartyl i gorny kwartyl - 3kwartyl
# dlugosc skrzynki - ?
# ognonek dlugi - max obserwacji bo liczymy go ze wzroru Q3+1.5(Q3-Q1)
# ogonek mowi 
# obserwacje odstajace - tez widoczne

boxplot(waga, horizontal = T)
# widac zaznaczona mediane, ale wykres ubogi

boxplot(waga, horizontal = T)$stats
# pisze stats po to zeby mi sie te numerki wyswietlily

# kwartyle
quantile(waga)

quantile(waga, c(0.25, 0.75))

# zad. 1.2.
# wybieramy ten plik sobie
zbiory <- read.table(file.choose(), header = T)
# mamy ramke danych sciagnieta
zbiory$yield
attach(zbiory)
yield

# ekstra tabelka
table(year, site)


# e- zadania dokonczyc




# 1.4
?rnorm

set.seed(1764) # wtedy zawsze bedziemy miec te same warunki

probka1 <- rnorm(n=500, mean=20, sd=5)
#lub
rnorm(500, 20,5)

probka2 <- runif(500, -1, 1)
probka2

# sprawdzic co to jest rate 
probka3 <- rexp(500, 1/5)
# rozklad wykladniczy - do modelowania czasu dzialania - wszystko jest dodatnie - rozklad nie mozeb cy ujemny

# rozklad plasona
probka4 <- rpois(500, lambda=3) # 3 bo srednia

# czy dane pochodza z rozkaldu normlanego czy nie z normlaneog
# przy weryfikacji hipotez to sie ogarnia

# b. sprawdzimy czy dane pochdza z rozkladu normalnego
# wykres normlanosci - wykres kwantylowy



par(mfrow=c(2,2))
qqnorm(probka1)
qqline(probka1)


qqnorm(probka2)
qqline(probka2)


qqnorm(probka3)
qqline(probka3)


qqnorm(probka4)
qqline(probka4)

# jesli dane sa z rozkladu normalnego to ukladaja sie wzdluz prostej




# TEST SHAPIRO-WILKA
shapiro.test(probka1)

# trzeba postawi hipoteze 0 i hipoteze alternatywna(przeciwna do H0)
# H_O: rozklad z ktorego pochodzi proba jest normalny
# H_A: rozklad z ktorego pochodzi proba NIE jest rozkladem normalnym
# 
# Shapiro-Wilk normality test
#data:  probka1
#W = 0.9981, p-value = 0.8583
# p-value - poziom istotnosci - musi byc male, najczesciej przyjmowane 0.05 => nie mamy podstaw do odrzucenia
# hipotezy h0 => mozemy przyjac ze proba pochodzi z rozkladu normalnego

shapiro.test(probka3)
# p-value < 2.2e-16 - mega male  bo < alfa => odrzucamy H_0 => czyli stwierdzamy ze dane nie pochodza z rozkladu
# normalnego


#Do domu zad 1.2 bez e i zad 1.3
#- do kolejnego laboratorium
# estymacja pu


