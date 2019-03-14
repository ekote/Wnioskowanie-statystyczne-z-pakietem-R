# Zbiór 4 - Testy parametryczne dla jedne populacji
#

# ZAD 4.2 a

kozy <- read.table('https://www.mini.pw.edu.pl/~dembinsk/www/?download=goats.txt', header=T)
head(kozy)

# X - waga kozy
# X ~ N(mi, sigma) gdzie mi i sigma są nieznane
# H_0 mi = 23 kg
# H_1 mi > 23 kg (nasza hipoteza - pytanie, dlaczego nie H_0?)

# Przypadek pasuje do Modelu II weryfikacji hipotez dotyczacych średniej

t.test(kozy$WeightInitial, alternative="greater", mu=23) # "less", "two.sided"
# p-value = 0.3936 > alfa = 0.05 -> nie ma podstaw do odrzucenia H_0
# nie ma podstaw by stwierdzić, że średnia waga młodych kóz przekracza 23 kg

# co ciekawe, średnia próbowa przekracza 23 kg
mean(kozy$WeightInitial)
# = 23.15 kg

# 4.2 b
# przy zalożeniu że średnia waga kóz wynosi 24 kg, wyznaczyć prawd, że przeprowadzając
# test na poziomie istotności 0,05 dla 40 obserwacji, błędnie uznamy że średnia waga
# kóz nie przepracza 23 kg

# Teoria| Moc testu(teta) = prawd odrzucenia H_0 gdy nieznany parametr ma wartosc teta
# P(przyjmujemy H_0 mi=24)=1-P(odrzucimy H_0, i mi=24)=1-moc-testu(24)
# mi - 1 parametr rozkladu normalnego

# funkcją liczącą moc testu jest w R power

1-power.t.test(n=40, delta=1, sd=sd(kozy$WeightInitial),
               type="one.sample", alternative="one.side")$power
             
# zadania do domu
# zad 4.1, 
# zad 4.3 (sprawdzić czy rozkład jest normalny) - a i b
# zad 4.7 a, b i c