## artykuł
# przyczynek do pracy
# służy opisaniu przedmiotu badań

########## Zad 4.1 ###########
# Narysować wykres gęstości standardowego rozk ladu normalnego w zakresie od kwantyla
# rzędu 0, 001 do 0, 999. Następnie na wykresie tym nanieść różnymi kolorami wykresy gęstości rozk ladu
# t-Studenta o liczbie stopni swobody równej 5, 10, 20, 50, 100. Zwrócić uwagę na charakter zmiany
# kształtu tych wykresów, gdy wzrasta liczba stopni swobody.

qn1 = qnorm(0.001)
qn2 = qnorm(0.999)

plot(function(x) dnorm(x), qn1, qn2, col = 1)

stopnie_swobody = c(3, 10, 20, 50, 100)

legend_entry = matrix(0,0,2)
legend_entry[1,1] = "dnorm distribution"
legend_entry[1,2] = -1

for(i in 1:5) {
  plot(function(x) dt(x, stopnie_swobody(i)), add=TRUE, col=i-1, lw=2)
  legend_entry(i-1,1) <- paset("t distrib with df = ", i)
  legend_entry(i-2,1) <- i-1 
}

legend(-1, 0.4, legend=legend_entry(,1), col=legend_entry(,2), lty=1:2, cex=0.5 )

# omowienie - im wiecej stopni swobody w t-studencie tym bardziej przypomina normalny
# przy n=100 t-student jest już bardzo podobny do rozkladu 

# Zatem, gdy pasuje model II też możemy używać t.testu z modelu II

######## Zad 4.4 ########## - do domu
# a - łatwe
# b - weryfikujemy hipoteze dot sredniej
# X - tygnodniowe dochody, nie znamy rozkladu, ale proba jest duza (pasuje Model III lub t.test z modelu II)


####### Zad 4.2 ########## 
## kończymy z poprzednich zajęć

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

# 4.3 c
# zakładamy, że mean1 = 24 kg
# Szukamy, n takiego, że P(odrzucimy H_0 | mean1=24) >= 0.8
# = Dla jakiego prawdopodobienstow, ze odrzucimy hipoteze H0 ( przy zalozeniu ze srednia 24) jest >= 0.8

# power.t.test - rozne argumenty, przy czym jakiegos nie podajemy
power.t.test(delta=1, sd=sd(kozy$WeightInitial), type="one.sample", alternative="one.sided", power = 0.8)$n

# 76.68731
# Odp: trzeba zabrać conajmniej 77 pomiarów

# Uwaga, sig.level, tj. poziom istotności, w power.t.test jest domyślnie ustawiony na 0.05

# 4.3 d.1 (d.2 do domu)
# H_0: sigma^2 = 20 kg^2
# H_1: sigma^2 != 20 kg^2 

# 1 - liczymy statystykę testową chi^2 (Tabela - weryfikacja hipotez dot. wariancji)
ch.kwadrat <- (length(kozy$WeightInitial) -1)*var(kozy$WeightInitial)/20
ch.kwadrat
# = 23.755

# oprócz statystyki testowej liczymy zbiór krytyczny W (wzór znów z tablicy) - u nas suma przedziałów 
# musimy policzyc kwantyle potrzebne do przedzialow - kwantyl rozkladu t-studenta to qt

qchisq(0.05, length(kozy$WeightInitial) - 1) # 25.69539
qchisq(1-0.05, length(kozy$WeightInitial)-1) # 54.57223
# zatem W = (0, 25.69) u (54.57, nieskonczonosc)
# ch.kwadrat = 23.75 -> nalezy do w =? odrzucamy H_0
# nei mozemy przyjąc, że wariancja wynosi 20 kg^2

# Do domu d.1 - analogicznie
# odchylenie standardowe = sqrt(wariancja), reszt az tej samej tabelki


############ ZADANIA DOMOWE Z POPRZEDNICH ZAJĘĆ #################
############ Zad 4.3 ############
opony = c(51600, 45400, 49800, 52100, 46500, 47800, 49700, 50900, 44000)

# chcemy stwierdzic, czy prób pochodzi z rozkładu normalnego
# h_0 - dane pochodzą z rozkładu normalnego
# h_1 - dane nie pochodzą z rozkładu normalnego
shapiro.test(opony)

# p-value 0.5493 > alfa 0.05
# brak podstaw do odrzucenia H_0 - dane pochodzą z rozkładu normalnego

# metoda graficzna
qqnorm(opony)
qqline(opony)
# na oko wygląda, że leżą mniej więcej na prostej (ale nie idealnie) - punkty rozkładają się wzdłuż prostej
# Uwaga: poziom istotności 0.05 - w 5 przypadkach na 100 dla danej probki, 
# wyciagniemy bledne wniosku (np. ze rozklad nie jest normalny)

# 4.3 a)
# Czy na poziomie istotności 0,05 można stwierdzić, że faktycznie średni przebieg opon podany
# w reklamie, jest zawyżony?
# rozklad normalny o nieznanych parametrow = uzywamy t.testu
t.test(opony, alternative = "less", mu = 50000)

# p-value = 0.0967
# odp. nie ma podstaw do odrzucenia H_0
# nie możemy stwierdzić, że przebieg podany w reklamie jest zawyżony

# 4.3 b) 
# zakładamy, że rzeczywisty średni przebieg m = 48000
# obliczyć P(blednie uznamy, że średni przebieg opon NIE JEST zawyżony | m = 48000) = 
# = P ( przyjmiemy H_0 | m = 48000 ) = 1 - P (odrzucimy H_0 | m = 48000 )
1-power.t.test(n=9, delta=2000, sd=sd(opony), type="one.sample", alternative = "one.sided", sig.level = 0.05)$power

# P to wynosi = 0.39397 (w 4 przypadkach na 10 mylimy sie)
# można przyjąć, że test jest niezbyt wiarygodny

# 4.2 (d2) i 4.3 c) DO DOMY


######## Zad 4.7 #########
# zadanie z ornitologii :-D

ptaki = c(5.21, 5.15, 5.20, 5.48, 5.19, 5.25, 5.09, 5.17, 4.94, 5.11)
# a) sprawdźmy normalnośc
shapiro.test(ptaki)

# p-value = 0.2793 > 0.05
# nie ma podstaw do odrzucenia hipotezy, ze probka nie pochodzi z rozkladu normalnego

# alfa = 0.05

# b)
# H_0: m = 5.2
# H_1: m < 5.2
# X - waga.ptakow
# x = N(m,sigma): m i sigma sa nieznane
t.test(ptaki, alternative="less", mu = 5.2)
# p-value = 0.3192 > 0.05
#
# nie ma podstaw do odrzucenia hipotezy H_0
# nie mozemy stwierdzić, że średnia waga
# ptaków jest mniejsza niż 5.2 kg
 
# c)
# zakladamy, że m=5.15 kg
# P(odrzucimy H_0 | m = 5.15) = moc_testu(5.15)
power.t.test(n=10, delta=0.05, sd=sd(ptaki), type="one.sample", alternative="one.sided")$power

# P = 0.2830938

# Do domy 4.2 (d2), 4.3 (c), 4.4, 4.6, skończyć 4.7