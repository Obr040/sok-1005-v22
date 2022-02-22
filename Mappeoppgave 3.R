library(tidyverse)
library(rvest)
library(ggplot2)

## Oppgave 1

url <- read_html("https://www.motor.no/aktuelt/motors-store-vintertest-av-rekkevidde-pa-elbiler/217132") 

liste <- html_table(html_nodes(url, "table")[[1]], header = TRUE)

liste1 <-
  subset(liste, !STOPP == "x")

liste1 <- liste1 %>% 
  rename(wltp = `WLTP-tall`, stopp = STOPP) %>% 
  mutate(Avvik = str_replace(Avvik, ",","."),
         Avvik = gsub("%", "",as.character(Avvik)), 
         stopp = gsub("km", "",as.character(stopp)), 
         wltp = substr(wltp,1,3))

liste1 <- liste1 %>% 
  mutate(wltp = as.numeric(as.character(wltp)), 
         stopp = as.numeric(as.character(stopp)), 
         Avvik = as.numeric(as.character(Avvik))) 


ggplot(liste1, aes(wltp, stopp)) +
  geom_point(size = 1) +
  geom_abline(intercept = 0, slope = 1, size = 0.5, color = "red") +
  scale_x_continuous("wltp", limits = c(200,600)) +
  scale_y_continuous("stopp", limits = c(200,600)) +
  labs(title="EL-bilenes rekkevidde i forhold til oppgitte WLTP-tall") +
  theme_classic()
  

## Oppgave 2

lm(stopp ~ wltp, data = liste1)


ggplot(liste1, aes(wltp, stopp)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, size = 0.5, color = "red") +
  geom_smooth(method = lm, se = FALSE, size = 0.75, color = "blue") +
  labs(title="EL-bilenes rekkevidde i forhold til oppgitte WLTP-tall") +
  scale_x_continuous("wltp", limits = c(200,600)) +
  scale_y_continuous("stopp", limits = c(200,600)) +
  theme_classic() 

