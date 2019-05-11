###################### zajęcia 7
# Dyskusja
# X - waga ptaków
# X~N(u,sigma^2) - obu nie znamy

# H0: u = 5.20
# H1: u < 5.20

# Uwaga - chcemy sprawdzić jak nasz test się sprawuje, gdy u (mi) nie znamy
# Zakładamy jakąś wartośc mi, np. u=u1 i liczymy gdy u1=u0

# P (odrzucimy H0 | u = u0 [~H0 prawdziwe] ) = male (nasza alfa) -> w takiej sytuacji H0 faktycznie spelnione, chcemy miec male
#                                                  prawdopodobienstwo aby nasz test odrzucil hipoteze H0
# P (odrzucimy H0 | u = u1 [~H0 falszywe] ) = duze (moc testu) -> w takiej sytuacji chcemy wlasnie odrzucic H0 i zeby to mialo
#                                                 duze prawdopodobieństwo

# Moc testu (moc statystyczna) to prawdopodobieństwo niepopełnienia błędu drugiego rodzaju
# – nieodrzucenia hipotezy zerowej, gdy w rzeczywistości jest ona fałszywa.
# moc.testu(teta) - > od konkretnego parametru

# 4.4 d)
# Ile musiałby wynosić średni tygodniowy dochód mężczyzn pracuja̧cych w USA w 1988 roku, aby
# test z pkt. (b) z prawdopodobieństwem 0,8 przyjmowa l hipotezę (tj H_1 - alternatywną), 
# że badany średni dochód jest większy niż 600 $?

# H_0 - średni dochód m = 600 $
# H_1 - średni dochód m > 600 $
# Chcemy wyznaczyć wartość m = m1, przy której z P = 0.8
# przyjmiemy H1, że m > 600
# P( przyjecie H1 | m = m1) = 0.8
# zatem 
# P( odrzucenie H0 | m = m1) = 0.8
# czyli moc.testu(m1) = 0.8
# 

library(faraway)
data("uswages")
attach(uswages)
power.t.test(n=2000, sd=sd(wage), type="one.sample", alternative="one.sided", power=0.8)

# skad otrzymujemy delta = 25.57 (roznica pomiedzty u1 i u0, tj |u1 - u0|)
# Odp. sredni tygodniowy dochów musiałby być równy 600 + delta = 625.57$
# Przy taki srednim dochodzie odrzucane byloby H0 i przyjmowane H1

# zad 4.6
mineral <- c(5.63, 9.01, 4.56, 3.11, 6.70, 11.78, 7.67, 4.82, 8.30, 9.46, 7.74, 5.49)
# Z zadania 3.5 wiemy, że powyższe dane pochodza̧ z rozkłladu normalnego. Sprawdzić, czy wariancja
# zawartości owego minerału w badanej formacji jest mniejsza niż 6. Przyja̧ć poziom istotności
# X: zawartośc procentowa minerału w próbce
# X: ma rozkład normalny N(m, sigma) -> m, sigma nieznane
# H0: sigma^2 = sigma0^2 = 6
# H1: sigma^2 < 6
n <- length(mineral)
s <- sd(mineral)
statqkw <- ((n-1)*s^2)/6
statqkw

# Model z weryfikacja hipotezy dotycza̧cej jednej wariancji na poziomie istotności α
# z kartki teoria dot. testów statystycznych
# statqkw = 10.95094
qchisq(1-0.1, 12-1)

# wyznaczamy zbiór krytyczny W = (0, 5.57)
# Statystykta testowa nie należy do W
# -> nie ma podstaw do odrzucenia H0
# Nie możemy stwierdzić, że wariancja zawartości minerału w badanej próbce jest < 6

############################# zad 4.8 ###########################

# Pełnomocnik rza̧du Alfalandii d/s równego statusu kobiet i mężczyzn podejrzewa
# że udział mężczyzn wśród pracowników przedszkoli jest niższy niż minimum przewidziane w ustawie,
# a wynosza̧ce 35%.

# (a) Czy na poziomie istotności 0,05 można uznać to stwierdzenie za uzasadnione, jeśli wśród losowo
# badanych 400 pracowników przedszkoli było 128 mężczyzn?

# X - rozklad dwupunktowy (X=1 jeśli pracownik jest mężczyzną, X=0 jeśli jest kobietą)
# X - dwupunktowy
# H0: p = 0.35
# H1: p < 0.35 (to co podejrzewa pełnomocnik)
# liczba.sukcesow = 128 > 5
# liczba.sukcesów = 400-128 = 272 > 5

# Zatem pasuje nam prob.test z 
# "Weryfikacje hipotez dotycza̧cych wartości średniej na poziomie istotności α", Model I
prop.test(128, 400, 0.35, alternative ="less", conf.level=)
# p-value = 0.114 > 0.05 - brak podstaw do odrzucenia H0
# (tak zawsze robimy, jeśli p-value > alfa, nie odrzucamy hipotezy)

# conf.level = poziom ufności != poziom istotności
# odp a) nie uznajemy tego stwierdzenia za uzasadnione na poziomie istotności 0.05

# b)
# X - rozklad dwupunktowy (X=1 jeśli pracownik jest mężczyzną, X=0 jeśli jest kobietą)
# X - dwupunktowy
# H0: p = 0.35
# H1: p < 0.35 (to co podejrzewa pełnomocnik)
# liczba.sukcesow = 3 < 5
# liczba.sukcesów = 7 < 5

# bardzo mało próbek, nie możemy zastosowac prob.test, musimy zastosowac binom.test
binom.test(3, 10, 0.35, alternative ="less")
p-value = 0.5138 > alfa = 0.06
# nie ma podstaw do odrzucenia H0, więc odpowiedź jest identyczna jak w punkcie a)



############################ Testy dla dwóch populacji - 5.3 ##########################
domy <- read.table('http://www.mini.pw.edu.pl/~dembinsk/www/?download=domy.txt', header=T)
attach(domy)
domy

# "(a) Czy dane te potwierdzaja̧, że średnie stȩżenie dwutlenku wȩgla w domu energooszczȩdnym jest
# wyższe niż w domu zbudowanym tradycyjna̧ technika̧? Przyja̧ć poziom istotności 0,0"
#
# Mamy tutaj dwie populacje :-)
# X - stężenie CO2 w domu energooszczędnym
# Y - stężenie CO2 w domu tradycyjnym
# nasze próby nie są zależne - są połączone dniem 
# (stwoierdzamy to arbitralnie - pobieramy proby tego samego dnia, domy są obok siebie)
# -> obserwacje w parach są zależne, kolejne pary są od siebie niezależne
# Chcielibyśmy użyć paired t-test, ale zanim to zrobimy musimy sprawdzić czy X-Y ma rozkład normalny
# o nieznanych parametrach

# Zanim skorzystamy z 'paired t-testu' musimy sprawdzić, czy rozkład jest normalny
# H0: rozkład jest normalny
# H1: rozkład nie jest normalny
shapiro.test(domE-domS)
# pvalue = 0.2272 > alfa -> nie ma podstaw do odrzucenia H0
# Możemy uznać, że badany rozkład jest normalny

qqnorm(domE-domS)
# H0: u_E = u_S
# H1: u_E > u_S

t.test(x=domE, y=domS, alternative = "greater", paired=T)
# p-value = 0.003196 < 0.05    -> odrzucamy H_0
# Odp: tak, stężenie jest wyższe w domu energooszczędnym

# b). Zakładamy, że m1_E - mi_S = 50
power.t.test(n=12, delta=50, sd=sd(domE-domS), type="paired", alternative="one.sided")
# power = 0.8423817
# Prawdopodobieństwo odrzucenia, że test wykaże, że średnie stężenie CO2
# w domu energooszczędnymjest wyższe niż w domu tradycyjnym w sytuacji gdy
# m1_E - mi_S = 50 wynosi 0.84

# Do domu: 5.4, 5.8 (podobne, też z parami)
przyrzadA = c(144,165,125,149,128,159)
przyrzadB = c(147,167,124,152,127,160)

mean(przyrzadA)
# 145
mean(przyrzadB)
# 146,16

# H0 : uA > uB
# H1 : uA = UB


t.test(x=przyrzadA, y=przyrzadB, alternative = "less", paired=T)
# p-value = 0.09 > alfa = 0.01
# nid odrzucamy H0
  