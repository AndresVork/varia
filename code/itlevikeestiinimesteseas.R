## Andres Võrk, 23.juuli 2017 
## https://github.com/AndresVork/varia

# puhastab keskkonna
rm(list = ls(all = TRUE))

#lae paketid
library(ggplot2) 

#andmed on võetud ESA andmebaasist
setwd("C:\\Users\\avork\\Documents\\GitHub\\varia\\")
#Maakondade lõikes internetiühendusega leibkondade osakaalud
df1 <- read.csv(".\\data\\IT201sm.csv", sep=";", header=FALSE)
df1$value <-  as.numeric(as.character(df1$V3))

#violin + punktid
p <-  df1 %>% 
  select(-V3) %>% 
  na.omit(df) %>% 
  #aastad kasvavas järjekorras
  ggplot(aes(as.factor(V1), value))+
  geom_point(aes(colour=V2)) +
  geom_violin(alpha = 0.5) +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100), limits=c(0,100)) +
  #pealkirjad
  labs(y = "Internetiühendusega leibkondade osatähtsus, %",
       x = "",
       title = "Internetiühendusega leibkondade jaotus üle maakondade",
       subtitle = "Andmed: Statistikaamet, tabel IT201",
       caption = "\nJaotus on joonistatud maakondade väärtuste keskmiste põhjal \njoonise andmed ja kood: https://github.com/AndresVork/varia")+
  #teksti suurus ja tüüp
  theme_minimal(base_size = 8) +
  theme(legend.position = "none")
p
ggsave("joonis1.png", plot=p)

#Interneti kasutavate inimeste osakaalud
df2 <- read.csv(".\\data\\IT32sm.csv", sep=";", header=FALSE)
q <-  df2 %>% 
  filter(V1=="Interneti kasutamine") %>% 
  select(-V1) %>%
  na.omit(df) %>% 
  #aastad kasvavas järjekorras
  ggplot(aes(as.factor(V2), V4))+
  geom_point(aes(colour=V3)) +
  geom_violin(alpha = 0.5) +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100), limits=c(0,100)) +
  #pealkirjad
  labs(y = "Interneti kasutajate osakaal, %",
       x = "",
       title = "Interneti kasutajate osakaalu jaotus üle sotsiaaldemograafiliste rühmade",
       subtitle = "Andmed: Statistikaamet, tabel IT32",
       caption = "\nJaotus on joonistatud erinevate (osaliselt kattuvate) sotsiaaldemograafiliste rühmade keskmiste põhjal \njoonise andmed ja kood: https://github.com/AndresVork/varia")+
  #teksti suurus ja tüüp
  theme_minimal(base_family =  "Roboto Condensed", base_size = 8) +
#legend välja
 theme(legend.position = "none")
q
ggsave("joonis2.png", plot=q)


# library(plotly)
# ggplotly(q)

########################
#teised joonise tüübid
#boxplot
p <-  df1 %>% 
  select(-V3) %>% 
  na.omit(df) %>% 
  #aastad kasvavas järjekorras
  ggplot(aes(as.factor(V1), value))+
  geom_boxplot() +
  #pealkirjad
  labs(y = "Internetiühendusega leibkondade osatähtsus, %",
       x = "",
       title = "Internetiühendusega leibkondade jaotus üle maakondade",
       subtitle = "Andmed: Statistikaamet, tabel IT201",
       caption = "\nJaotus on joonistatud maakondade ja Tallinna väärtuste keskmiste põhjal \njoonise andmed ja kood: https://github.com/AndresVork/varia")+
  #teksti suurus ja tüüp
  theme_minimal(base_size = 8)
p
# ggsave("joonis1.png", plot=p)

q <-  df2 %>% 
  filter(V1=="Interneti kasutamine") %>% 
  select(-V3, -V1) %>%
  na.omit(df) %>% 
  #aastad kasvavas järjekorras
  ggplot(aes(as.factor(V2), V4))+
  geom_boxplot() +
  #pealkirjad
  labs(y = "Interneti kasutajate osakaal, %",
       x = "Aasta",
       title = "Interneti kasutajate osakaalu jaotus üle sotsiaaldemograafiliste rühmade",
       subtitle = "Andmed: Statistikaamet, tabel IT32",
       caption = "\nJaotus on joonistatud erinevate (osaliselt kattuvate) sotsiaaldemograafiliste rühmade keskmiste põhjal \njoonise andmed ja kood: https://github.com/AndresVork/varia")+
  #teksti suurus ja tüüp
  theme_minimal(base_family =  "Roboto Condensed", base_size = 8)
  #legend välja
#  theme(legend.position = "none")
q
# ggsave("joonis2.png", plot=q)


library(forcats) #funktsiooni fct_rev() jaoks
library(ggjoy)
library(viridis)
library(extrafont)

#geom_joy
p <-  df1 %>% 
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
       title = "Internetiühendusega leibkondade jaotus üle maakondade",
       subtitle = "Andmed: Statistikaamet, tabel IT201",
       caption = "\nTihedusfunktsioonid on hinnatud maakondade ja Tallinna väärtuste keskmiste põhjal \njoonise andmed ja kood: https://github.com/AndresVork/varia")+
  #teksti suurus ja tüüp
  theme_minimal(base_size = 8)+
  #legend välja
  theme(legend.position = "none")
p
ggsave("joonis1_density.png", plot=p)

q <-  df2 %>% 
  filter(V1=="Interneti kasutamine") %>% 
  select(-V3, -V1) %>%
  na.omit(df) %>% 
  #aastad kasvavas järjekorras
  ggplot(aes(x = V4, y = as.factor(V2) %>% fct_rev()))+
  #teeme joyplot-i
  geom_joy(aes(fill = as.factor(V2)))+
  #valib värvid jm
  scale_fill_viridis(discrete = T, option = "D", direction = -1, 
                     begin = 0, end = 1)+
  #pealkirjad
  labs(x = "Interneti kasutajate osakaal, %",
       y = "Aasta",
       title = "Interneti kasutajate osakaalu jaotus üle sotsiaaldemograafiliste rühmade",
       subtitle = "Andmed: Statistikaamet, tabel IT32",
       caption = "\nTihedusfunktsioonid on joonistatud erinevate (osaliselt kattuvate) sotsiaaldemograafiliste rühmade keskmiste põhjal \njoonise andmed ja kood: https://github.com/AndresVork/varia")+
  #teksti suurus ja tüüp
  theme_minimal(base_family =  "Roboto Condensed", base_size = 8)+
  #legend välja
  theme(legend.position = "none")
q

ggsave("joonis2density.png", plot=q)

#histogram
df2 %>% 
  filter(V1=="Interneti kasutamine") %>% 
  select(-V3, -V1) %>%
  na.omit(df2) %>% 
  #aastad kasvavas järjekorras
  ggplot(aes(x = V4))+
  geom_histogram(binwidth = 10)  +
  facet_wrap(~V2, ncol=1)


##########
#violin ilma värvideta
p <-  df1 %>% 
  select(-V3) %>% 
  na.omit(df) %>% 
  #aastad kasvavas järjekorras
  ggplot(aes(as.factor(V1), value))+
  geom_point() +
  geom_violin(alpha = 0.5) +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100), limits=c(0,100)) +
  #pealkirjad
  labs(y = "Internetiühendusega leibkondade osatähtsus, %",
       x = "",
       title = "Internetiühendusega leibkondade jaotus üle maakondade",
       subtitle = "Andmed: Statistikaamet, tabel IT201",
       caption = "\nJaotus on joonistatud maakondade väärtuste keskmiste põhjal \njoonise andmed ja kood: https://github.com/AndresVork/varia")+
  #teksti suurus ja tüüp
  theme_minimal(base_size = 8)
p
ggsave("joonis1.png", plot=p)

#Interneti kasutavate inimeste osakaalud
q <-  df2 %>% 
  filter(V1=="Interneti kasutamine") %>% 
  select(-V3, -V1) %>%
  na.omit(df) %>% 
  #aastad kasvavas järjekorras
  ggplot(aes(as.factor(V2), V4))+
  geom_point() +
  geom_violin(alpha = 0.5) +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100), limits=c(0,100)) +
  #pealkirjad
  labs(y = "Interneti kasutajate osakaal, %",
       x = "",
       title = "Interneti kasutajate osakaalu jaotus üle sotsiaaldemograafiliste rühmade",
       subtitle = "Andmed: Statistikaamet, tabel IT32",
       caption = "\nJaotus on joonistatud erinevate (osaliselt kattuvate) sotsiaaldemograafiliste rühmade keskmiste põhjal \njoonise andmed ja kood: https://github.com/AndresVork/varia")+
  #teksti suurus ja tüüp
  theme_minimal(base_family =  "Roboto Condensed", base_size = 8)
q
ggsave("joonis2.png", plot=q)
