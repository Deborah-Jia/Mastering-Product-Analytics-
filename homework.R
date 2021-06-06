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
library(scales)
library(forecast)
library(Metrics)
library('prophet')
ggthemr('pale')

# registration data
rg <- fread("~/Desktop/Mastering-Product-Analytics/MPA/registrations.csv") 
str(rg) # check the structure

# activity data
acvt <- fread("~/Desktop/Mastering-Product-Analytics/MPA/activity.csv")
str(acvt) # check the structure

# Task 1: Acquisition -----------------------------------------------------
# Basic task (T1)
# – Plot the number of registrations in each month
table(rg$registration_month) # there are a lot of numbers larger than 12.
# change numbers to month names.
rg[, Month := fifelse(registration_month <= 12, 
                          rep(month.abb, 2)[registration_month], 
                          paste0("2nd Year ", rep(month.abb, 2)[registration_month]))]

# sorting bars by factor ordering
rg$Month <- factor(rg$Month,levels = unique((rg$Month)))

# group the registration and create a new data.table to save the result
agg_rg <- rg[, .(.N), by=Month][, yoy := round((N - lag(N, 12))/ lag(N, 12), digits = 3) ]

# plot registration numbers and year over year ratio
highchart() %>% 
  hc_yAxis_multiples(
    list(lineWidth = 3,title = list(text = "Registration")),
    list(showLastLabel = F, opposite = T, title = list(text = "yoy rate")))%>% 
  hc_add_series(data = agg_rg, hcaes(x = Month, y = N),type = "column", yAxis = 0) %>% 
  hc_add_series(data = agg_rg, hcaes(x = Month, y = yoy),color = "red",type = "spline", yAxis = 1) %>%
  hc_tooltip(
    useHTML = TRUE,                            
    formatter = JS(
      "
      function(){
        outHTML = '<b>' + this.point.Month + '</b> <br> Registration ' + this.point.N + '<br> Year over year ratio ' + this.point.yoy*100 + '%'
        return(outHTML)
      }

      "
    ),
    shape = "square", 
    borderWidth = 0   
  ) %>% 
  hc_xAxis(categories = agg_rg$Month ) %>% 
  hc_legend(enabled = F) %>% 
  hc_title(text = "Registration & Year over Year Ratio in Each Month") 

  
# – Make some comments on the trends and any lower periods (remember that Month 1 is January)
#  每6个月有一次高峰和谷底

# Extra tasks
# – T1A: Calculate year-over-year growth of registrations (i.e. percentage increase or decline of Month 13 over Month 1). What would you project for Month 22?
r22g <- rg[, .(.N), by=registration_month]
# splitting data into train and valid sets
## 75% of the sample size
# splitting data into train and valid sets
train = r22g[1:15,]
valid = r22g[16:21,]
# removing "Month" column
train$registration_month = NULL
# training model
model = auto.arima(train)

# model summary
summary(model)

# forecasting
forecast = predict(model,6)

# evaluation
rmse(valid$N, forecast$pred)


# – T1B: Do you see any regional difference in registration trends? Which geography is likely to drive future growth in registrations?





