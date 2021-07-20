

                        #################################
                        ### PRZYGOTOWANIE BILIBLIOTEK ###
                        #################################


# instalacja niezainstalowanych bibliotek

install.packages("viridis")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("moments")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggthemes")
install.packages("leaflet")
install.packages("scales")
install.packages("gridExtra")
install.packages("ggridges")
install.packages("ggpubr")
install.packages("stats")
install.packages("GGally")
# Dolaczenie bibliotek do projektu

library(ggplot2)
library(dplyr)
library(viridis)
library(moments) 
library(tidyverse)
library(lubridate)
library(ggthemes)
library(leaflet)
library(scales)
library(gridExtra)
library(ggridges)
library(ggpubr)
library(stats)
library(GGally)
                          #############################
                          ### PRZYGOTOWANIE DANYCH ####
                          #############################


# Wczytanie pliku csv

cars <- read.csv("cars.csv", header=TRUE)

# pozostawienie w danych tylko tych kolumn, ktore beda analizowane

cars = cars[,c(1, 4:6, 10, 11, 15, 18, 30)];

# Kolumna manufacturer_name byla czesciowo uszkodzona - 
# niektore nazwy zaczynaly sie od symoblu "Đ" i w sosob oczywisty
# nie byly nazwa marki auta

cars = cars[!grepl("Đ",cars$manufacturer_name),]

# W ten sposob mozna sprawdzic nazwy wszystkich marek
# (i czy bledne pola zostaly usuniete)

cars %>% distinct (manufacturer_name)

write.csv(cars, 'cars-processed.csv')


                #######################
                # FUNKCJE UNIWERSALNE #
                #######################

# Funkcja obliczajaca dominante
mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Funkcja wypisuje podstawowe informacje o danej zmiennej
get_basic_stats <- function(value){
  cat("Wartość średnia:",formatC(mean(value), digits = 2, format = "f"),"\n")
  cat("Mediana:",formatC(median(value), digits = 2, format = "f"),"\n")
  cat("Odchylenie standardowe:",formatC(sd(value), digits = 2, format = "f"),"\n")
  cat("Wariancja:",formatC(var(value), digits = 2, format = "f"),"\n")
  cat("Minimum:",formatC(min(value), digits = 2, format = "f"),"\n")
  cat("Maksimum:",formatC(max(value), digits = 2, format = "f"),"\n")
  cat("Dominanta:",formatC(mode(value), digits = 2, format = "f"),"\n")
  cat("Skośność:",formatC(skewness(value), digits = 2, format = "f"),"\n")
  cat("Kurtoza:",formatC(kurtosis(value), digits = 2, format = "f"),"\n")
  cat("Rozstep miedzykwartylowy:",formatC(IQR(value), digits = 2, format = "f"),
      "\n")
}

                            ###################
                            ### ANALIZA CEN ###
                            ###################


                          ## PODSTAWOWA ANALIZA ##

get_basic_stats(cars$price_usd)

# Postanowilem sprawdzic jaki procent aut jest tanszy niz $10000
# i jak drozszy od $30000

cheaper_cars <- cars[cars$price_usd <10000,]
print(nrow(cheaper_cars)/nrow(cars)*100)
expensive_cars <- cars[cars$price_usd >30000,]
print(nrow(expensive_cars)/nrow(cars)*100)
                              ## WYKRESY ##

# Wykres pudelkowy

# Na wykresie zrezygnowano z rysowania punktow odstajacych (outliers)
# - Ze wzgledu na duza ilosc danych rysowanie tych punktow powodowalo 
# zmniejszenie czytelnosci wykresu

boxplot(cars$price_usd, col="#69b3a2" , ylab="cena w USD" , outline = FALSE,
        main = "Ceny - w. pudełkowy")


# Wykres gestosci cen 

cars %>%
  ggplot( aes(x=price_usd)) +
  geom_density(fill="#69b3a2", color="black", alpha=0.8) +
  ggtitle("Ceny wystawionych aut - rozkład gęstości p.") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  labs(x = "cena w USD", y = "prawdopodobieństwo") 


# Wykresy cen - histogramy
cars %>%
  ggplot( aes(x=price_usd)) + 
  geom_histogram(binwidth=1000, color="white",  fill = "darkblue") +
  scale_x_continuous(expand = c(0,0)) +
  labs(x = "cena w USD", y = "liczba ogłoszeń") +
  ggtitle("Ceny aut - histogram 1.") +
  theme(plot.title = element_text(hjust = 0.5))

cars %>%
  ggplot(aes(x=price_usd)) + 
  geom_histogram(binwidth=5000, color="white", fill = "darkblue") +
  scale_x_continuous(expand = c(0,0)) +
  labs(x = "cena w USD", y = "liczba ogłoszeń") +
  ggtitle("Ceny aut - histogram 2.") +
  theme(plot.title = element_text(hjust = 0.5))


# Wykres gstosci cen wybranych marek

# Ze wzgledu, na duza ilosc marek prezentowane sa tylko wybrane

selected_man <- c("Fiat", "Audi", "Porsche", "Chevrolet")

cars %>%
  filter(manufacturer_name %in% selected_man) %>%
  ggplot( aes(x=price_usd, color=manufacturer_name, fill=manufacturer_name)) +
  geom_density(alpha=0.8, color = "black") +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  ggtitle("Ceny aut wybranych marek") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "cena w USD", y = "prawdopodobieństwo") +
  guides(fill=guide_legend(title="marka"))


# wykres ceny w zaleznosci od koloru
cars %>%
  ggplot(aes(x = price_usd, y = color, 
             group = color, fill=color)) +
  geom_density_ridges(rel_min_height = 0.01) + 
  theme_hc() + 
  ggtitle("Ceny aut o danych kolorach") +
  labs(x = "cena", y = "prawdopodobieństwo")+
  guides(fill=guide_legend(title="kolor"))


                ###########################################
                ### ANALIZA LICZBY ZDJEC W OGLOSZENIACH ###
                ###########################################


                        ## PODSTAWOWA ANALIZA ##

get_basic_stats(cars$number_of_photos)



                              ## WYKRESY ##

# Wykres pudelkowy
# Na wykresie zrezygnowano z rysowania punktow odstajacych (outliers)
# - Ze wzgledu na duza ilosc danych rysowanie tych punktow powodowalo 
# zmniejszenie czytelnosci wykresu

boxplot(cars$number_of_photos, col="#69b3a2" , ylab="liczba zdjęć", outline = FALSE,
        main = "Liczba zdjęć - w. pudełkowy")

# Wykres gestosci liczby zdjec na ogloszeniach
cars %>%
  ggplot( aes(x=number_of_photos)) +
  geom_density(fill="#69b3a2", color="black", alpha=0.8, ylab = "prawdopodobieństwo",
  xlab = "liczba zdjęć") +
  ggtitle("Liczba zdjęć w ogłoszeniach - rozkład gęstości p.") +
  theme(plot.title = element_text(hjust = 0.5))

  
# liczba zdjec - histogramy
cars %>%
  ggplot(aes(x=number_of_photos)) + 
  geom_histogram(binwidth=5, color="white",  fill = "darkblue") +
  scale_x_continuous(expand = c(0,0)) +
  labs(x = "liczba zdjęć", y = "liczba ogłoszeń") +
  ggtitle("Liczba zdjęć - histogram 1.") +
  theme(plot.title = element_text(hjust = 0.5))

cars %>%
  ggplot(aes(x=number_of_photos)) + 
  geom_histogram(binwidth=10, color="white",  fill = "darkblue") +
  scale_x_continuous(expand = c(0,0)) +
  labs(x = "liczba zdjęć", y = "liczba ogłoszeń") +
  ggtitle("Liczba zdjęć - histogram 2.") +
  theme(plot.title = element_text(hjust = 0.5))



                ###############################################
                ### ANALIZA LICZBY PRZEJECHANYCH KILOMETROW ###
                ###############################################


                            ## PODSTAWOWA ANALIZA ##


get_basic_stats(cars$odometer_value)

                                  ## WYKRESY ##

# Wykres pudelkowy
# Na wykresie zrezygnowano z rysowania punktow odstajacych (outliers)
# - Ze wzgledu na duza ilosc danych rysowanie tych punktow powodowalo 
# zmniejszenie czytelnosci wykresu

boxplot(cars$odometer_value, col="#69b3a2" , ylab="liczba przejechanych kilometrów", 
        outline = FALSE, main = "Liczba przejechanych km - w. pudełkowy")

# wykres gestsoci przejechanych kilometrow
cars %>%
  ggplot( aes(x=odometer_value)) +
  geom_density(fill="#69b3a2", color="black", alpha=0.8) +
  ggtitle("Liczba p. kilometrów - rozkład gęstości p.") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "liczba przejechanych kilometrów", y = "prawdopodobieństwo")

# sprawdzenie jak rozklad ma sie do rozkladu normalnego
qqnorm(cars$odometer_value)


# liczba przejechanych kilometrow - histogramy

cars %>%
  ggplot(aes(x=odometer_value)) + 
  geom_histogram(binwidth=10000, color="white",  fill = "darkblue") +
  scale_x_continuous(expand = c(0,0)) +
  labs(x = "liczba przejechanych kilometrów", y = "liczba ogłoszeń") +
  ggtitle("liczba przejechanych km - histogram 1.") +
  theme(plot.title = element_text(hjust = 0.5))

cars %>%
  ggplot(aes(x=odometer_value)) + 
  geom_histogram(binwidth=100000, color="white",  fill = "darkblue") +
  scale_x_continuous(expand = c(0,0)) +
  labs(x = "liczba przejechanych kilometrów", y = "liczba ogłoszeń") +
  ggtitle("liczba przejechanych km - histogram 2.") +
  theme(plot.title = element_text(hjust = 0.5))



            ################################################
            ### ANALIZA ROKU PRODUKCJI SPRZEDAWANYCH AUT ###
            ################################################


                        ## PODSTAWOWA ANALIZA ##

get_basic_stats(cars$year_produced)


                             ## WYKRESY ##

# Na wykresie zrezygnowano z rysowania punktow odstajacych (outliers)


boxplot(cars$year_produced, col="#69b3a2" , ylab="Rok produkcji", 
         main = "Rok produkcji - w. pudełkowy")
# wykres gestisci roku produkcji
cars %>%
  ggplot( aes(x=year_produced)) +
  geom_density(fill="#69b3a2", color="black", alpha=0.8) +
  ggtitle("Rok produkcji - rozkład gęstości p.") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "rok produkcji", y = "prawdopodobieństwo")

# liczba przejechanych kilometrow - histogramy

ggplot(cars, aes(x=year_produced)) + 
  geom_histogram(binwidth=5, color="white",  fill = "darkblue") +
  scale_x_continuous(expand = c(0,0)) +
  labs(x = "rok produkcji", y = "liczba ogłoszeń") +
  ggtitle("Rok produkcji - histogram 1.") +
  theme(plot.title = element_text(hjust = 0.5))


ggplot(cars, aes(x=year_produced)) + 
  geom_histogram(binwidth=15, color="white",  fill = "darkblue") +
  scale_x_continuous(expand = c(0,0)) +
  labs(x = "rok produkcji", y = "liczba ogłoszeń") +
  ggtitle("Rok produkcji - histogram 1.") +
  theme(plot.title = element_text(hjust = 0.5))


                ###############################
                # ANALIZA WYBRANYCH ROZKLADOW #
                ###############################
# Przyjazna funkcja do szukania rozkladow normalnych
for (i in unique(cars$manufacturer_name)){
  print(i)
  tested <- cars[cars$manufacturer_name == i,]
  print(shapiro.test(tested$price_usd))
  
}

# ANALIZA ROZKLADU PRZEJECHANYCH KM W PORSCHE

# wybranie porsche
porsche <- cars[cars$manufacturer_name == "Porsche",]
# test Shapiro - Wilk
print(shapiro.test(porsche$odometer_value))
# wykres  qqnorm
ggqqplot(porsche$odometer_value) +
  ylab ("Kwantyle probki") +
  xlab ("Kwantyle teoretyczne")
# test t- studenta

t.test(porsche$odometer_value)


# ANALIZA ROZKLADU PRZEJECHANYCH KM W CHERY


chery <- cars[cars$manufacturer_name == "Chery",]
# test Shapiro - Wilk
print(shapiro.test(chery$odometer_value))
# wykres  qqnorm
ggqqplot(chery$odometer_value) +
  ylab ("Kwantyle probki") +
  xlab ("Kwantyle teoretyczne")
# test t- studenta

t.test(chery$odometer_value)


# ANALIZA ROZKLADU CEN W SSANGYONG


sy <- cars[cars$manufacturer_name == "SsangYong",]
# test Shapiro - Wilk
print(shapiro.test(sy$price_usd))
# wykres  qqnorm
ggqqplot(sy$price_usd) +
  ylab ("Kwantyle probki") +
  xlab ("Kwantyle teoretyczne")
# test t- studenta

t.test(sy$price_usd)



                    ##########################################
                    # TESTOWANIE ZALEZNOSCI MIEDZY ZMIENNYMI #
                    ##########################################

# Narzedzia do badania 1. modelu
model1 <- lm(cars$price_usd ~ cars$odometer_value) # budowa modelu
summary(model1) # streszczenie modelu
cor(cars$odometer_value, cars$price_usd) # korelacja

ggqqplot(residuals(model))+     #Wykres Q-Q
  xlab("Kwantyle teoretyczne") +
  ylab("Kwantyle probki")

# Wykres badanych zmiennych
plot(cars$odometer_value, cars$price_usd, xlab = "Licba przejechanych km",
     ylab = "Cena [USD]")
abline(model1, col="red")


# Narzedzia do badania 2. modelu
model2 <- lm(cars$price_usd ~ cars$year_produced)
summary(model2)
cor(cars$year_produced, cars$price_usd)

ggplot(cars, aes(x = year_produced, y =price_usd))+ 
  geom_point() +
  xlab("Rok produkcji") +
  ylab("Cena w USD")

plot(cars$year_produced, cars$price_usd, xlab = "Rok produkcji",
     ylab = "Cena [USD]")

abline(model2, col="red")

ggqqplot(residuals(model2))+
  xlab("Kwantyle teoretyczne") +
  ylab("Kwantyle probki")

# Narzedzia do analizy modelu 3.

model3 <- lm(cars$price_usd ~ cars$odometer_value + cars$year_produced +
               cars$engine_capacity + cars$number_of_photos + cars$duration_listed)

ggqqplot(residuals(model3))+
  xlab("Kwantyle teoretyczne") +
  ylab("Kwantyle probki")


# Narzedzia do analizy modelu 4.
model4 <- lm(cars$price_usd ~ cars$odometer_value + cars$year_produced +
              cars$engine_capacity + cars$number_of_photos + cars$manufacturer_name
            + cars$color + cars$body_type)

summary(model4)

ggqqplot(residuals(model4))+
  xlab("Kwantyle teoretyczne") +
  ylab("Kwantyle probki")


###########################################
# BRUDNOPOIS
# Tutaj zamiescilem fragmenty kodow, ktore moze nie znalazly bezposredniego 
# zastosowania, jednak byly pomocne do eksplorowania danych

qqnorm(cars$number_of_photos)

colMax <- function(data) sapply(data, max, na.rm = TRUE)
colMin <- function(data) sapply(data, min, na.rm = TRUE)
colMax(cars)

man <- cars %>% distinct (manufacturer_name)
print(man)




# wykres  qqnorm
ggqqplot(porsche$odometer_value) +
  ylab ("Kwantyle probki") +
  xlab ("Kwantyle teoretyczne")
# test t- studenta

t.test(porsche$odometer_value)

selected_man <- c("Ssang Young")
cars %>%
  filter(manufacturer_name %in% selected_man) %>%
  ggplot( aes(x=price_usd, color=manufacturer_name, fill=manufacturer_name)) +
  geom_density(alpha=0.8, color = "black") +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  ggtitle("Ceny aut wybranych marek") +
  theme(plot.title = element_text(hjust = 0.5))

chery <- cars[cars$manufacturer_name == "Chery",]
qqnorm(r$odometer_value)
shapiro.test(r$odometer_value)
cars %>% distinct (manufacturer_name)
plot(density(r$price_usd))
plot(density(cars$price_usd))
t.test(chery$odometer_value)
man <- cars %>% distinct (manufacturer_name)
print(man)


cars %>%
  ggplot(aes(x = odometer_value, y = manufacturer_name, 
             group = manufacturer_name, fill=manufacturer_name)) +
  geom_density_ridges(rel_min_height = 0.01) + 
  theme_hc() + 
  ggtitle("Ceny aut o danych kolorach") +
  labs(x = "cena", y = "prawdopodobieństwo")+
  guides(fill=guide_legend(title="kolor"))


cars %>%
  ggplot(aes(x = price_usd, y = manufacturer_name, 
             group = manufacturer_name, fill=manufacturer_name)) +
  geom_density_ridges(rel_min_height = 0.01) + 
  theme_hc() + 
  ggtitle("Ceny aut o danych kolorach") +
  labs(x = "cena", y = "prawdopodobieństwo")+
  guides(fill=guide_legend(title="kolor"))

# Ciekawe rozklady:
# odometer porsche
# odometer   Chrysler
# dometer land rover

