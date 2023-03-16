library(moments)

#WCZYTYWANIE DANYCH Z PLIKU .CSV
test <- read.csv2("dane.csv", )

#ZAD1
data_1<-c(test[1])
data_2<-c(test[2])

kol_1994 =unlist(data_1, use.names = FALSE)
kol_1995 =unlist(data_2, use.names = FALSE)

skol_1994 = sort(kol_1994)
skol_1995 = sort(kol_1995)

ilosc_el_1994<-length(kol_1994)
ilosc_el_1995<-length(kol_1995)

suma_1994 <- sum(kol_1994)
#print(suma1994)
suma_1995 <- sum(kol_1995)
#print(suma1994)

############################MIARY POLOZENIA#################
#SREDNIA ARYTMETYCZNA
sr_aryt_1994 <- suma_1994/ilosc_el_1994 
sr_aryt_1995 <- suma_1995/ilosc_el_1995

suma_odwr_1994 <- sum((kol_1994)^(-1))
suma_odwr_1995 <- sum((kol_1995)^(-1))

#SREDNIA HARMONICZNA
sr_harm_1994 <- ilosc_el_1994/suma_odwr_1994
sr_harm_1995 <- ilosc_el_1995/suma_odwr_1995

iloczyn_1994 <- prod(kol_1994)
iloczyn_1995 <- prod(kol_1995)

#SREDNIA GEOMETRYCZNA
sr_geo_1994 <- iloczyn_1994^(1/25)
sr_geo_1995 <- iloczyn_1995^(1/25)

#MEDIANA
mediana <- function(kolumna, poczatek, koniec) {
  kolumna = sort(kolumna)
  #poczatek = ceiling(poczatek)
  #koniec = ceiling(koniec)
  dlugosc <- koniec - poczatek + 1
  if(dlugosc %% 2 == 0)
    return((kolumna[poczatek + dlugosc / 2 - 1] + kolumna[poczatek + dlugosc / 2]) / 2)
  else
    return(kolumna[poczatek + (dlugosc + 1) / 2 - 1])
}
mediana_1994 <- mediana(kol_1994, 1,ilosc_el_1994)
mediana_1995 <- mediana(kol_1995, 1,ilosc_el_1995)

mediana_1994 <- median(kol_1994)
mediana_1995 <- median(kol_1995)

#KWARTYLE
kwartyl_1_1994 <- quantile(kol_1994, 0.25)
kwartyl_1_1995 <- quantile(kol_1995, 0.25)
kwartyl_3_1994 <- quantile(kol_1994, 0.75)
kwartyl_3_1995 <- quantile(kol_1995, 0.75)

kwartyl_1_1994 <- mediana(kol_1994,1, ilosc_el_1994/2)
kwartyl_1_1995 <- mediana(kol_1995,1, ilosc_el_1995/2)

kwartyl_3_1994 <- mediana(kol_1994,ilosc_el_1994/2, ilosc_el_1994)
kwartyl_3_1995 <- mediana(kol_1995,ilosc_el_1995/2, ilosc_el_1995)


############################MIARY ZROZNICOWANIA#################
#ROZSTEP WYNIKOW W PROBIE
rozstep_1994 <- max(kol_1994)-min(kol_1994)
rozstep_1995 <- max(kol_1995)-min(kol_1995)

#ROZSTEP MIEDZYKWARTYLOWY
rozstep_kwart_1994 <- kwartyl_3_1994 - kwartyl_1_1994
rozstep_kwart_1995 <- kwartyl_3_1995 - kwartyl_1_1995

#WARIANCJA
wariancja_1994 <- var(kol_1994)
wariancja_1995 <- var(kol_1995)

#ODCHYLENIE STANDARDOWE
odchyl_standard_1994 <- sqrt(wariancja_1994)
odchyl_standard_1995 <- sqrt(wariancja_1995)

#WSPOLCZYNNIK ZMIENNOSCI
wsp_zmiennosci_1994 <- 100*odchyl_standard_1994/sr_aryt_1994
wsp_zmiennosci_1995 <- 100*odchyl_standard_1995/sr_aryt_1995

############################MIARY ASYMETRII#################
#SKOSNOSC
skosnosc_1994 <- (sum((kol_1994-sr_aryt_1994)^3)/ilosc_el_1994)/(sum((kol_1994-sr_aryt_1994)^2)/ilosc_el_1994)^(3/2)
skosnosc_1995 <- (sum((kol_1995-sr_aryt_1995)^3)/ilosc_el_1995)/(sum((kol_1995-sr_aryt_1995)^2)/ilosc_el_1995)^(3/2)
#((sum((kol_1994 - sr_aryt_1994)^3))/length(kol_1994))/(odchyl_standard_1994^3)

#WspOLCZYNNIK SKUPIENIA - KURTOZA
kurtoza_1994 <- ((sum((kol_1994 - sr_aryt_1994)^4))/length(kol_1994))/(odchyl_standard_1994^4)
kurtoza_1995 <- ((sum((kol_1995 - sr_aryt_1995)^4))/length(kol_1995))/(odchyl_standard_1995^4)

print(skewness(kol_1994))
print(kurtosis(kol_1994))
print(kurtosis(kol_1995))


kurtoza_1994 <- ((1/ilosc_el_1994)*sum((kol_1994-sr_aryt_1994)^4))/(odchyl_standard_1994^4)

############################SZEREGI ROZDZIELCZE#################
szer_r_1994 <- table(kol_1994)
print(szer_r_1994)

#szerokosc przedzialu klasowego
szer_przedz_1994 = (max(kol_1994)-min(kol_1994))/sqrt(ilosc_el_1994)
szer_przedz_1995 = (max(kol_1995)-min(kol_1995))/sqrt(ilosc_el_1995)

#punkt przeciecia
pprzec_1994 = seq(min(kol_1994),max(kol_1994),by=szer_przedz_1994)
pprzec_1995 = seq(min(kol_1995),max(kol_1995),by=szer_przedz_1995)

przedzial_1994 <- cut(kol_1994,pprzec_1994)
przedzial_1995 <- cut(kol_1995,pprzec_1995)

sz_1994 <- table(przedzial_1994)
sz_1995 <- table(przedzial_1995)


hist_1994 = hist(kol_1994, breaks = c(16,18,20,22,24,26), main ="Poziom rentowności w 1994r.", xlab = "Rentowność sprzedaży", ylab = "Częstość", xlim=c(16,26),ylim=c(0,12))
hist_1995 = hist(kol_1995, breaks = c(26,28,30,32,34,36), main ="Poziom rentowności w 1995r.", xlab = "Rentowność sprzedaży", ylab = "Częstość", ylim=c(0,13))


############################TEST ZGODNOSCI KOLMOGOROWA#################
#SREDNIA ARYTMETYCZNA
sr_aryt_1994 <- suma_1994/ilosc_el_1994 
sr_aryt_1995 <- suma_1995/ilosc_el_1995
#SORTOWANIE
kol_1994 = sort(kol_1994)
kol_1995 = sort(kol_1995)
#PNORM - ROZKLAD NORMALNY
#SEQ - SEQUENCE GENERATION
p_1994 = pnorm((kol_1994-sr_aryt_1994)/odchyl_standard_1994)
p_1995 = pnorm((kol_1995-sr_aryt_1995)/odchyl_standard_1995)

Dplus_1994 = max(seq(1:ilosc_el_1994)/ilosc_el_1994-p_1994)
Dplus_1995 = max(seq(1:ilosc_el_1995)/ilosc_el_1995-p_1995)

Dminus_1994 = max(p_1994 - (seq(1:ilosc_el_1994)-1)/ilosc_el_1994)
Dminus_1995 = max(p_1995 - (seq(1:ilosc_el_1995)-1)/ilosc_el_1995)

d_1994=max(Dplus_1994,Dminus_1994)
d_1995=max(Dplus_1995,Dminus_1995)

stala_z_tablicy = 0.26404

if(d_1994<stala_z_tablicy){
  print("rozklad normalny")
}
if(d_1994>stala_z_tablicy){
  print("hipoteza o rozkladzie normalnym odrzucona")
}

############################SZACOWANIE PRZEDZIALOWE 1994#################
#T-STUDENT
#wsp. ufnosci 1- alfa = 0.98
#alfa = 1-0.98 = 0.02
#wart. dla t(alfa,24) = 2.492
alfa <- 0.02
wsp_t_stud <- 2.492

dolna_granica_sred <- function(srednia, wsp, odchyl, ilosc) {
  return(srednia - wsp * (odchyl / sqrt(ilosc - 1)))
}

gorna_granica_sred <- function(srednia, wsp, odchyl, ilosc) {
  return(srednia + wsp * (odchyl / sqrt(ilosc - 1)))
}


precyzja <- function(gorna_granica, dolna_granica, srednia){
  return (0.5 * (gorna_granica - dolna_granica) / srednia*100)
}


granica_dolna_1994 <-dolna_granica_sred(sr_aryt_1994,wsp_t_stud, odchyl_standard_1994, ilosc_el_1994)
granica_gorna_1994 <-gorna_granica_sred(sr_aryt_1994,wsp_t_stud, odchyl_standard_1994,ilosc_el_1994)

precyzja_1994 <- precyzja(granica_gorna_1994, granica_dolna_1994, sr_aryt_1994)

#PRECYZJA WYNOSI OKOLO 4.8%, WIEC NASZE OSZACOWANIE CHARAKTERYZUJE SIE DUZA PRECYZJA - DZIEKI TEMU MOZEMY UOGOLNIC PRZEDZIAL NA CALA POPULACJE 
print(precyzja_1994)

############################SZACOWANIE PRZEDZIALOWE 1995#################

#wsp. ufnosci 1- alfa = 0.98
#alfa = 1-0.98 = 0.02
alfa <- 0.02

chi1 <- 42.980 # chi(0.001;24)
chi2 <- 10.856 # chi(0.99;24)

granica <- function(chi, war, ilosc){
  tmp1 = ilosc*war
  granica = tmp1/chi
  return(granica)
} 

dolna <- granica(chi1, wariancja_1995,ilosc_el_1995)
gorna <- granica(chi2, wariancja_1995,ilosc_el_1995)

blad_maksymalny = (gorna-dolna)/2

#PRECYZJA WYNOSI PONAD 10%, WIEC NIE MOZEMY DOKONYWAC ZADNYCH UOGOLNIEN NA POPULACJE GENERALNA
precyzja = blad_maksymalny/wariancja_1995*100

############################WERYFIKACJA HIPOTEZY#################


#Hipoteza zerowa: srednia rentownosc firmy w 1995 jest rowna tej z 1994
#Hipoteza alternatywna: srednia rentownosc firmy poprawila sie

#sprawdzenie zalozenia o rownosci wariancji w obu populacjach:
#hipoteza zerowa: war1=war2
#hipoteza alternatywna: war1>war2

statystyka_f <- function(ilosc_1994, ilosc_1995, odchyl_1994, odchyl_1995){
  s1 = ilosc_1994*odchyl_1994/(ilosc_1994-1)
  s2 = ilosc_1995*odchyl_1995/(ilosc_1995-1)
  
  f = s1/s2
  return(f)
}

wartosc.statystyki_f <- statystyka_f(ilosc_el_1994,ilosc_el_1995, odchyl_standard_1994,odchyl_standard_1995)

# z tablic kwantyli rozkladu F snedecora:
# F(1-0.05;24;24)=1.70185
f <- 1.70185

if(wartosc.statystyki_f>f){
  print("Wartosc statystyki nalezy do przedzialu krytycznego")
}
if(wartosc.statystyki_f<f){
  print("Wartosc statystyki nie nalezy do przedzialu krytycznego, wiec nie ma podstawy do odrzucenia hiopotezy o rownosci wariancji")
}


statystyka_t <- function(sred_1994, sred_1995, war_1994, war_1995, ilosc_el){
#zmienne tmp dla ulatwienia zapisu ostatecznego wyniku wartosci statystyki testowej
  tmp1 = sred_1994-sred_1995
  tmp2 = 1/ilosc_el + 1/ilosc_el  
  tmp3 = ilosc_el*war_1994 + ilosc_el*war_1995
  tmp4 = 2*ilosc_el - 2
  tmp5 = tmp2*tmp3
  tmp6 = tmp5/tmp4
  tmp7 = sqrt(tmp6)

  wart = tmp1/tmp7
  return(wart)
}

wartosc.statystyki_t <- statystyka_t2(sr_aryt_1995, sr_aryt_1994,wariancja_1995,wariancja_1994,ilosc_el_1994)

#zmienne tmp dla ulatwienia zapisu ostatecznego wyniku stopni swobodyy
tmp1 <- ((wariancja_1994+wariancja_1995)/25)^2
tmp2 <- (wariancja_1994/25)^2
tmp3 <- (wariancja_1995/25)^2
tmp5 <- tmp2+tmp3
tmp4 <- tmp1/tmp5
tmp6 <- tmp5/24

stopnie_swobody <- tmp1/tmp6

# z tablic kwantyli dla rozkładu t-studenta 
# t(1-0.05;stopnie_swobody) = t(0.95;48)=1.67722
t <- 1.67722

if(wartosc.statystyki_t>t){
  print("Obliczona wartosc nalezy do przedzialu krytycznego - odrzucamy hipoteze zerowa, wiec srednia rentownosc jest istotnie wyzsza w r 1995 od tej w 1994")
}
if(wartosc.statystyki_t<t){
  print("Obliczona wartosc nalezy do przedzialu")
}


