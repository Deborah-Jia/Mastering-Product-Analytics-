# load data and packages --------------------------------------------------
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
ggthemr('pale')

# registration data
rg <- fread("~/Desktop/Mastering Product Analytics/registrations.csv") 
str(rg) # check the structure

# activity data
acvt <- fread("~/Desktop/Mastering Product Analytics/activity.csv")
str(acvt) # check the structure

# Task 1: Acquisition -----------------------------------------------------
# Basic task (T1)
# – Plot the number of registrations in each month
table(rg$registration_month) # there are a lot of numbers larger than 12.
# change numbers to month names.
rg[,month_rg2 := fifelse(registration_month <= 12, 
                          rep(month.abb, 2)[registration_month], 
                          paste0("2nd Year ", rep(month.abb, 2)[registration_month]))]

# sorting bars by factor ordering
rg$month_rg2 <- factor(rg$month_rg2,levels = unique((rg$month_rg2)))

# plot the bar chart and add text above each bar
p <- rg[, .(.N), by=month_rg2][, yoy := (N - lag(N, 12)) / lag(N, 12)] %>% 
  ggplot(aes(label = month_rg2, label2 = N))  +
  geom_bar(aes(x=month_rg2, y=N, group = 1),stat="identity")+
  geom_line(aes(x=month_rg2, y=N*1.2, group = 1)) +
  # geom_text(aes(label=N, x=month_rg2, y=N), vjust= -2) +
  labs(title = "Registration in each month") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank()) +
  geom_bar_interactive() 

ggplotly(p) 

rg[, .(.N), by=month_rg2][, yoy := (N - lag(N, 12)) / lag(N, 12)] %>% 
  highcharter::hw_grid(
    hchart("column", hcaes(x = month_rg2, y = N)),
    hchart("line", hcaes(x = month_rg2, y = N))
  ) %>% 
  hc_title(text = "Registration in Each Month") %>% 
  hc_add_theme(hc_theme_chalk())


# – Make some comments on the trends and any lower periods (remember that Month 1 is January)
#  每6个月有一次高峰和谷底

# Extra tasks
# – T1A: Calculate year-over-year growth of registrations (i.e. percentage increase or decline of Month 13 over Month 1). What would you project for Month 22?

# – T1B: Do you see any regional difference in registration trends? Which geography is likely to drive future growth in registrations?
