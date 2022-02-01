
library(tidyverse)
library(readr)
library(data.table)
library(cowplot)
library(dplyr)
library(zoo)
library(ggplot2)

## Oppgave 1

Temp <- fread("http://vortex.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt")
Temp <- Temp[-c(518)]
Temp <- as.data.frame(apply(Temp, 2, as.numeric))

Values <- c("Year", "Mo", "Globe")
Temp <- Temp[Values]

Temp1 <- Temp %>% 
  arrange(Year) %>% 
  group_by(Year) %>% 
  mutate(Mean_Globe = rollmean(Globe,k = 12, fill = NA)) %>% 
  ungroup()

Temp2 <- Temp1 %>% drop_na()
Temp2 <- Temp2 %>% 
  rename(Filler = Globe,
        Globe = Mean_Globe)

ggplot(NULL, aes(Year,Globe)) +
  geom_point(data = Temp1, color="blue",size= 0.4) +
  geom_line(data = Temp1, color ="blue", size = 0.4) +
  geom_line(data = Temp2, color ="red1", size = 1.2) +
  labs(title = "Temperaturendring i Nedre troposfære i forhold til normalperioden 1991-2020",
       x = "År",
       y = "Temperaturendring") +
  geom_hline(yintercept = 0, size = 0.5) +
  theme_bw()

## Oppgave 2

LowTrop <- fread("http://vortex.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt")
LowTrop <- LowTrop[-c(518)]
LowTrop <- as.data.frame(apply(LowTrop, 2, as.numeric))

MidTrop <- fread("http://vortex.nsstc.uah.edu/data/msu/v6.0/tmt/uahncdc_mt_6.0.txt") 
MidTrop <- MidTrop[-c(518)]
MidTrop <- as.data.frame(apply(MidTrop, 2, as.numeric))

Trop <- fread("http://vortex.nsstc.uah.edu/data/msu/v6.0/ttp/uahncdc_tp_6.0.txt")
Trop <- Trop[-c(518)]
Trop <- as.data.frame(apply(Trop, 2, as.numeric))

LowStrat <- fread("http://vortex.nsstc.uah.edu/data/msu/v6.0/tls/uahncdc_ls_6.0.txt")
LowStrat <- LowStrat[-c(518)]
LowStrat <- as.data.frame(apply(LowStrat, 2, as.numeric))

Values1 <- c("Year","Mo","NoPol")
LowTrop <- LowTrop[Values1]
MidTrop <- MidTrop[Values1]
Trop <- Trop[Values1]
LowStrat <- LowStrat[Values1]

Mean <- rbindlist(list(LowTrop,MidTrop,Trop,LowStrat))[,lapply(.SD,mean),list(Year,Mo)]

p1 <- ggplot(LowTrop, aes(Year,NoPol)) +
  geom_point(color = "cyan1", size = 0.3) +
  geom_line(color = "cyan1", size = 0.3) +
  coord_fixed(ratio = 2.2) +
  labs(title = "Nedre Troposfære",
       x = "År",
       y = "Temperaturendring") +
  theme_bw()

p2 <- ggplot(MidTrop, aes(Year,NoPol)) +
  geom_point(color = "darkorange1", size = 0.3) +
  geom_line(color = "darkorange1", size = 0.3) +
  coord_fixed(ratio = 2) +
  labs(title = "Midtre troposfære",
       x = "År",
       y = "Temperaturendring") +
  theme_bw()

p3 <- ggplot(Trop, aes(Year,NoPol)) +
  geom_point(color = "blue", size = 0.3) +
  geom_line(color = "blue", size = 0.3) +
  coord_fixed(ratio = 1) +
  labs(title = "Troposfæren",
       x = "År",
       y = "Temperaturendring") +
  theme_bw()

p4 <- ggplot(LowStrat, aes(Year,NoPol)) +
  geom_point(color = "gold", size = 0.3) +
  geom_line(color = "gold", size = 0.3) +
  coord_fixed(ratio = 0.5) +
  labs(title = "Lavere stratosfære",
       x = "År",
       y = "Temperaturendring") +
  theme_bw()

p5 <- ggplot(Mean, aes(Year,NoPol)) +
  geom_point(color = "red1", size = 0.3) +
  geom_line(color = "red1", size = 0.3) +
  coord_fixed(ratio = 1.25) +
  labs(title = "Gjennomsnittlig temperaturendring i forhold til normalperioden 1991-2020",
       x = "År",
       y = "Temperaturendring") +
  theme_bw()

plot_grid(p1,p2,p3,p4,p5,ncol = 2)  

ggplot(NULL, aes(Year,NoPol)) +
  geom_smooth(data = LowTrop, color = "cyan1", size = 0.8, alpha = 0) +
  geom_smooth(data = MidTrop, color = "darkorange1", size = 0.8, alpha = 0) +
  geom_smooth(data = Trop, color = "blue", size = 0.8, alpha = 0) +
  geom_smooth(data = LowStrat, color = "gold", size = 0.8, alpha = 0) +
  geom_smooth(data = Mean, color = "red1", size = 1.5, alpha = 0) +
  coord_fixed(ratio = 8.5) +
  labs(title = "Gjennomsnittlig temperaturendring i forhold til normalperioden 1991-2020",
       x = "År",
       y = "Temperaturendring") +
  theme_bw()
  
  
