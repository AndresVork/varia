## Andres Võrk, 20.juuli 2017 
## https://github.com/AndresVork/varia
## kasutab ideid Ilya Kashnitsky lehelt https://gist.github.com/ikashnitsky

# Kustuta kõik mälust
rm(list = ls(all = TRUE))

#lae paketid
library(ggplot2) 
library(forcats) #funktsiooni fct_rev() jaoks
library(ggjoy)
library(viridis)
library(extrafont)

#andmed on võetud ESA andmebaasist
setwd("C:\\Users\\avork\\Documents\\GitHub\\varia\\")

#Maakondade lõikes internetiühendusega leibkondade osakaalud
df <- read.csv(".\\data\\IT201sm.csv", sep=";", header=FALSE)
df$value <-  as.numeric(as.character(df$V3))
df %>% 
  select(-V3) %>% 
  na.omit(df) %>% 
  #aastad kasvavas järjekorras
  ggplot(aes(x = value, y = as.factor(V1) %>% fct_rev()))+
  #teeme joyplot-i
  geom_joy(aes(fill = as.factor(V1)))+
  #valib värvid jm
  scale_fill_viridis(discrete = T, option = "D", direction = -1, 
                     begin = .1, end = .9)+
  #pealkirjad
  labs(x = "Internetiühendusega leibkondade osatähtsus, %",
       y = "Aasta",
       title = "Internetiühendusega leibkondade keskmise osatähtsuse jaotus üle maakondade",
       subtitle = "Andmed: Statistikaamet, tabel IT201",
       caption = "\nTihedusfunktsioonid on hinnatud maakondade ja Tallinna väärtuste põhjal \njoonise idee: ikashnitsky.github.io")+
  #teksti suurus ja tüüp
  theme_minimal(base_family =  "Roboto Condensed", base_size = 15)+
  #legend välja
  theme(legend.position = "none")

#Interneti kasutavate inimeste osakaalud
df <- read.csv(".\\data\\IT32sm.csv", sep=";", header=FALSE)
df %>% 
  filter(V1=="Interneti kasutamine") %>% 
  select(-V3, -V1) %>%
  na.omit(df) %>% 
  #aastad kasvavas järjekorras
  ggplot(aes(x = V4, y = as.factor(V2) %>% fct_rev()))+
  #teeme joyplot-i
  geom_joy(aes(fill = as.factor(V2)))+
  #valib värvid jm
  scale_fill_viridis(discrete = T, option = "D", direction = -1, 
                     begin = .1, end = .9)+
  #pealkirjad
  labs(x = "Interneti kasutajate osakaal, %",
       y = "Aasta",
       title = "Interneti kasutajate osakaalu jaotus üle sotsiaaldemograafiliste rühmade",
       subtitle = "Andmed: Statistikaamet, tabel IT32",
       caption = "\nTihedusfunktsioonid on joonistatud erinevate (osaliselt kattuvate) sotsiaaldemograafiliste rühmade keskmiste põhjal \njoonise andmed ja kood: https://github.com/AndresVork/varia")+
  #teksti suurus ja tüüp
  theme_minimal(base_family =  "Roboto Condensed", base_size = 15)+
  #legend välja
  theme(legend.position = "none")
