# 1.2

# wczytac ten zbior
zbior <- read.table(file.choose(), header = T)

# uzyskac bezposredni dostep do zmiennych w tym zbiorze
attach(zbior)
zbior$yield

# wczytac podstawowe statystyki probkowe dla danych opisujacych wielkosc
# zbiorow w 1931 roku.

filtered <- zbior[zbior$year == '1931', ]

# mean - srednia
mean(yield)

#mediana 
median(yield)

#wartosci ekstremalne,
max(yield)
min(yield)
#dolny i gorny kwantyl, 
quantile(yield, 0.25)
quantile(yield, 0.75)

#variancja
var(yield)

# standard deviation
sd(yield)

# obliczyc kwartyl rzedu 0.9 dla tych danych 
quantile(yield, c(0.1, 0.99, 0.9))
# pierwszy decyl, 99-percentyl i kwartyl rzedu 0.9
treshold <- quantile(yield, c(0.9))

# (c) Wypisac te wielkosci zbiorow w roku 1931, ktore by ly wyzsze od kwantyla wyznaczonego w punkcie (b).
treshold
filtered

another_filtered <- filtered[filtered$yield >= treshold, ]
another_filtered

# (d) Narysowac histogram dla danych opisujacych wielkosc zbiorow w 1931 roku w miejscowosci Grand
dane <- filtered[filtered$site == 'Grand.Rapids', ]
dane

hist(dane$yield, main="Histogram licznosci") # domyslny histogram to histogram LICZNOSCI
hist(dane$yield, breaks=2)

# tablica kontyngencji opisujaca ile pomiarow zebranow kazdym z miejsc eksperymentow w poszczegolnych latach
table(zbior$year, zbior$site)



# zadanie 1.3
# Dane zawarte w pliku gala_data.txt zawieraja informacje o kilkudziesieciu wyspach.
# (a) Wczytac te dane.
gala <- read.table(file.choose(), header = T)
gala

# (b) Wyznaczyc podstawowe statystyki probkowe (
#srednia, 
#mediane, 
#dolny i gorny kwantyl, 
#wartosci ekstremalne, 
#wariancje
#odchylenie standardowe) 
# dla danych opisujacych liczbe gatunkow _zo lwi wyste-
#  pujacych na badanych wyspach (zmienna Species).
attach(gala)
mean(Species)
median(Species)
quantile(Species, 0.25)
quantile(Species, 0.75)
min(Species)
max(Species)
var(Species)
sd(Species)


# Narysowac histogram o pieciu klasach dla danych opisujacych powierzchnie 
# badanych wysp (zmienna Area). Podpisac osie i umiescic naglowek.
# breaks = klasa
hist(Area, freq = TRUE, breaks = 5, main = "Histogram LICZNOSCI zad 1.3", xlab="Powierzchnia (AREA)", ylab="Licznosc")

# (d) W jednym oknie sporzadzic wykres rozrzutu liczby gatunkow _zo lwi wzgledem powierzchni wyspy
# oraz wykres rozrzutu liczby gatunkow _zo lwi wzgledem logarytmu naturalnego (log) z powierzchni wyspy.
# Na ktorym wykresie widac silniejsza zale_znosc liniowa?
par(mfrow=c(1,2))

# plot(x-variable, y-variable)
plot(Area, Species, main="Liczba gatunkow od pow wyspy")
plot(log(Area), Species)

# e) Narysowac i opisac wykres skrzynkowy dla danych przedstawiajacych liczbe gatunkow _zo lwi na
# wyspach, ktorych powierzchnia jest mniejsza od 25 (km2).  
selected <- gala[Area <25.00, ]
selected

par(mfrow=c(1,1))
boxplot(selected$Species, horizontal = T, main = "Wykres skrzynkowy", xlab="Liczba gatownkow (Species)", add=TRUE)$stats
  # srodkowa kreska - mediana
# lewy koniec skrzynki - pierwszy kwartyl
# prawy koniec skrzynki - trzeci kwartyl
# lewy ogonek = 2 
# prawy ogonek = 70
# prawa kropeczka - obserwacja ostajaca
