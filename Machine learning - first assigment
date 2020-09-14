#skorzystałem ze środowiska R, gdyż jest mi jednak o wiele bardziej przyjazne, poza tym we właściwej treści zadań na e-learningu nie ma napisane, że Python jest konieczny 
# tak, jak poruszałem ten temat na konsultacją z Panią -  nie rozumiem dlaczego nauczyciele akademiccy narzucają rozwiązanie danego problemu za pomocą danego języka programowania, w mojej opinii R jest do regresjii o niebo lepszy - mniej skomplikowany i bardziej logiczny
# każdy powinien mieć prawo rozwiązywać dany problem wybraną przez siebie metodą, jeżeli metoda ta prowadzi do prawidłowego rozwiązania

#wczytanie danych
>zad1 <- read.delim(file="C:/Users/xyq/Documents/Studia - APD - ZU/Uczenie maszynowe/Materiały/Zestaw 1/first-assignment/train.tsv", header = FALSE)
>zad1<-data.frame(zad1) 
> colnames(zad1)<-c("Cena_mieszkania","Liczba_pokoi","Metraz","Pietro","Lokalizacja","Opis")
#1 ) Napisać skrypt, który wczyta dane z pliku train.tsv (jako DataFrame) <- DONE :)
#Patrz SS nr 1

> mean(zad1$Cena_mieszkania)
[1] 341.7628
> srednia<-mean(zad1$Cena_mieszkania)
> round(srednia,3)
[1] 341.763
srednia_R<-round(srednia,3)

#wyliczy średnią cenę mieszkania (z dokładnością do złotówki) <- DONE 

>write.csv(srednia_R,"out0.csv") #zapisze tę cenę do pliku out0.csv a następnie.... <- DONE

#2) 

>Cena4m2<-(zad1$Cena_mieszkania*1000/zad1$Metraz)
> zad1<-cbind(zad1, Cena4m2)

#do tabeli danych doda kolumnę z obliczoną ceną za metr kwadratowy dla danego mieszkania <- DONE -Patrz SS2

> # korzystam z pakietu dplyr
>temp1 <- data.frame(filter(zad1, zad1$Liczba_pokoi>=3))
>srednia_m2<-mean(zad1$Cena4m2)  #zgodnie z zapisem w zadaniu, zrozumiałem, że liczymy średnią cenę za m2 dla wszystkich 4500 mieszkań, a nie tylko tych, które mają co najmniej 3 pokoje

>temp2 <- data.frame(filter(temp1, temp1$Cena4m2<srednia_m2))
>out1<-data.frame(temp2$Liczba_pokoi, temp2$Cena4m2)

# tym sposobem mamy wymagane kolumny i wiersze 

>write.csv(out1,"out1.csv") 

# no i zapis , punkt 2 uważam za zrealizowany


#3
#skrypt odnośnie wczyrania danych train.tsv został już napisany w pkt 1) zadania, zatem z czystego lenistwa, pozwolę sobie na jego skopiowianie

>zad1_3train <- read.delim(file="C:/Users/xyq/Documents/Studia - APD - ZU/Uczenie maszynowe/Materiały/Zestaw 1/first-assignment/train.tsv", header = FALSE)
>zad1_3desc <- read.csv(file="C:/Users/xyq/Documents/Studia - APD - ZU/Uczenie maszynowe/Materiały/Zestaw 1/first-assignment/description.csv", header = TRUE)

#no to pół sukcesu za nami, dane dobrze wczytane

> opis<-ifelse(zad1_3train$V4 == 0, "parter", ifelse(zad1_3train$V4 == 2, "drugie piÄ™tro",ifelse(zad1_3train$V4 == 3, "trzecie", "NULL" )))
> out2<-cbind(zad1_3train, opis)

#Metoda na zagnieżdżanie "TRUE OR FALSE"  # można też zamiast liczb po znaku równości i tekstu w "" dać wskazanie na kolumny i wiersze w drugiej tabeli, ale to jest krótsze.

>write.csv(out2,"out2.csv") 

#Chyba zrobione!

#4
 
#Pobieranie ze stackoverflow zrobione , oby nie było wirusów :)
results <- read.csv(file="C:/Users/xyq/Documents/Studia - APD - ZU/Uczenie maszynowe/Materiały/Zestaw 1/ankieta/survey_results_public.csv",header =TRUE)

# Proszę otworzyć plik survey_results_schema.csv. Czytając opisy kolumn/pytań w ankiecie proszęńwybrać przynajmniej dwie kolumny, które można potraktować jako dane ilościowe.  
# Dane wczytane, widać że to spoooora tabela - patrz SS3

#Wybieram kolumnę Age oraz WorkWeekHrs
> results_df<-data.frame(results$WorkWeekHrs, results$Age) #mamy tabelkę danych z powyższymi kolumnami

#Celem uproszczenia wyfiltruję teraz tylko te wiersze, które posiadają kompletne odpowiedzi - tzn, nie ma "NA"

> results_df<-filter(results_df, results_df$results.WorkWeekHrs > 0) # raczej nikt nie obija się przez caluśki tydzień
> results_df<-filter(results_df, results_df$results.Age > 0) #raczej niemowlaków nie pytali :)

# teraz mamy "tylko" 60 536 pozycji

# końcowa ramka danych results_df spełnia zapis zadania 4, zatem uważam, za zrobiony
# trochę zabrakło mi tu regresji liniowej jednokrotnej, może miała być sądząc po zdani w zad. 5 "robimy wykresy, tak jak poprzednio, oddzielne dla każdej z grup"


>write.csv(results_df, "out3.csv") # w razie co zapisuje <- Done

#5

# zgodnie z poleceniem, dodamy sobie do naszych danych również kolumnę z daną jakościową, tak jak w poleceniu - np. płeć

Secresults_df<-data.frame(results$WorkWeekHrs, results$Age, results$Gender)

>Secresults_df<-filter(Secresults_df, Secresults_df$results.WorkWeekHrs>0)
>Secresults_df<-filter(Secresults_df, Secresults_df$results.Age>0)

#no to mamy dane
#teraz zrzutujemy sobie dla uproszczenia płeć na liczbę i podzielimy na najliczniejsze grupy - man i woman
>library(plyr)

>Secresults_df$results.Gender <- mapvalues(Secresults_df$results.Gender, from = c("Man","Woman"), to = c("0", "1"))

>GroupMan<-filter(Secresults_df, Secresults_df$results.Gender == 0)
>GroupWoman<-filter(Secresults_df, Secresults_df$results.Gender == 1) 
#widać, że badanie dotyczyło przede wszystkim mężczyzn, widać zmaskulinizowane hobby/zawód to całe IT

#Chłopcy

GroupMan.lm <- lm(GroupMan$results.Age ~ GroupMan$results.WorkWeekHrs)
					
> summary(GroupMan.lm)

Call:
lm(formula = GroupMan$results.Age ~ GroupMan$results.WorkWeekHrs)

Residuals:
    Min      1Q  Median      3Q     Max 
-31.384  -5.668  -1.641   4.353  67.386 

Coefficients:
                              Estimate Std. Error
(Intercept)                  31.346008   0.057112
GroupMan$results.WorkWeekHrs  0.006698   0.001062
                             t value Pr(>|t|)    
(Intercept)                   548.85  < 2e-16 ***
GroupMan$results.WorkWeekHrs    6.31 2.81e-10 ***
---
Signif. codes:  
0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 8.338 on 55087 degrees of freedom
Multiple R-squared:  0.0007222,	Adjusted R-squared:  0.0007041 
F-statistic: 39.82 on 1 and 55087 DF,  p-value: 2.813e-10

#nasza wartość P jest baaaaardzo niska, zatem stawiam, że hipoteza zależności wieku do czasu pracy jest słaba w przypadku mężczyzn)
> groupMan2<-data.frame(GroupMan$results.WorkWeekHrs, GroupMan$results.Age)
> plot(groupMan2)
> abline(GroupMan.lm)

#wykres patrz SS4 
# btw chyba ktoś się nieźle pomylił w ilości godzin pracy w tygodniu, skoro są przypadki bliskie 1000 oraz 4000 godzin

#Dziewczęta

> GroupWoman.lm <- lm(GroupWoman$results.Age ~ GroupWoman$results.WorkWeekHrs)

> summary(GroupWoman.lm)

Call:
lm(formula = GroupWoman$results.Age ~ GroupWoman$results.WorkWeekHrs)

Residuals:
    Min      1Q  Median      3Q     Max 
-29.176  -5.176  -2.172   2.824  68.996 

Coefficients:
                                Estimate Std. Error
(Intercept)                    30.229251   0.132606
GroupWoman$results.WorkWeekHrs -0.001343   0.001501
                               t value Pr(>|t|)    
(Intercept)                    227.962   <2e-16 ***
GroupWoman$results.WorkWeekHrs  -0.894    0.371    
---
Signif. codes:  
0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 7.501 on 4128 degrees of freedom
Multiple R-squared:  0.0001936,	Adjusted R-squared:  -4.856e-05 
F-statistic: 0.7995 on 1 and 4128 DF,  p-value: 0.3713

#o i tu niespodzianka, duża p-wartość, hipoteza może być sensowna w przypadku kobiet lub to wynik o wiele mniejszej próby

> groupWoman2<-data.frame(GroupWoman$results.WorkWeekHrs, GroupWoman$results.Age)
> plot(groupWoman2)
> abline(GroupWoman.lm)

#regresja (wykres) liniowa pokazała także tendencję spadkową, odwrotnie niż w przypadku mężczyzn - patrz SS5


# kończąc na tym, uważam, że zadania zostały wykonane w miarę poprawnie 
# Przyjemnej lektury!




