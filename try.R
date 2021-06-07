library(tidyverse)
library(data.table)
library(dplyr)
library(ggplot2)
library(wesanderson)
library(ggthemr)
library(plotly)
library(gapminder)
library(highcharter)
library(ggiraph)
library(scales)
ggthemr('pale')

label_percent(0.30999993 , accuracy = 3)
percent_format(0.30999993 , accuracy = 3)
percent(0.030999993\)

highchart() %>% 
  hc_yAxis_multiples(
    list(lineWidth = 3),
    list(showLastLabel = FALSE, opposite = TRUE)
  ) %>% 
  hc_add_series(data = rnorm(10)) %>% 
  hc_add_series(data = rexp(10), type = "spline", yAxis = 1)
