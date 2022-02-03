library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(data.table)
library(rjstat)

## Oppgave 1
url <- "https://www.nytimes.com/interactive/2021/10/28/us/covid-breakthrough-cases.html"

JP <- fromJSONstat(url)
