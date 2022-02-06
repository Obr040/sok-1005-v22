library(tidyverse)
library(dplyr)
library(ggplot2)
library(jsonlite)
library(ggrepel)

## Oppgave 1
url <- fromJSON("https://static01.nyt.com/newsgraphics/2021/12/20/us-coronavirus-deaths-2021/ff0adde21623e111d8ce103fedecf7ffc7906264/scatter.json")

url$name <- state.abb[match(url$name, state.name)]
url[is.na(url)] <- "DC"

url %>%
  ggplot(aes(fully_vaccinated_pct_of_pop, deaths_per_100k)) +
  geom_point(size = 3.5, pch = 20, col="darkslategray4", alpha = 0.5) +
  geom_text_repel(aes(label = name)) +
  scale_x_continuous(labels = scales::percent, limits=c(0.45, 0.80), breaks=seq(0.45, 0.80, by = 0.05)) +
  labs(title="Covid-19 deaths since universal adult vaccine eligibility compared with vaccination rates",
       x ="Share of total population fully vaccinated",
       y = "20 avg. Monthly deaths per 100,000") +
  theme_bw()

## Oppgave 2

lm(deaths_per_100k ~ fully_vaccinated_pct_of_pop, url)

url %>%
  ggplot(aes(fully_vaccinated_pct_of_pop, deaths_per_100k)) +
  geom_point(size = 3.5, pch = 20, col="darkslategray4", alpha = 0.5) +
  geom_smooth(method = lm, alpha = 0, col = "red1") +
  geom_text_repel(aes(label = name)) +
  scale_x_continuous(labels = scales::percent, limits=c(0.45, 0.80), breaks=seq(0.45, 0.80, by = 0.05)) +
  labs(title="Covid-19 deaths since universal adult vaccine eligibility compared with vaccination rates",
       x ="Share of total population fully vaccinated",
       y = "20 avg. Monthly deaths per 100,000") +
  theme_bw()