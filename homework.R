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
library(hrbrthemes)
library(kableExtra)
library(gganimate)
library(babynames)
library(viridis)

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

# plot the new bar/prediction
pred_row <- data.table("2nd Year Oct", forecast$pred[1], NA)

#Naming the Data Frame - Step 2  
names(pred_row) <- names(agg_rg)
  
agg_rg <- rbind(agg_rg, pred_row) 

# plot the prediction
agg_rg %>% 
hchart("column", hcaes(x = Month , y = N)) %>% 
  hc_xAxis(categories = agg_rg$Month ) %>% 
  hc_legend(enabled = F) %>% 
  hc_title(text = "Predicted Registration in Each Month") %>% 
  hc_add_theme(hc_theme_chalk())

# – T1B: Do you see any regional difference in registration trends? Which geography is likely to drive future growth in registrations?
p <- rg[, .(.N, region), by=Month] %>% 
ggplot(aes(text = paste("Registraton: ", N )))  +
  geom_bar(aes(x=Month, y=N, group = 1),stat="identity")+
  geom_line(aes(x=Month, y=N*1.2, group = 1)) +
  facet_wrap(~ region) +
  geom_text(aes(label=N, x=Month, y=N), vjust= -2) +
  labs(title = "Registration in each month") +
  theme(axis.text.y=element_blank(),
        axis.text.x=element_blank()) 

ggplotly(p) 


# Task 2: Activity --------------------------------------------------------
# • Basic task (T2)
# – Plot the number of active users in each month
# group the registration and create a new data.table to save the result
acvt[, acvt_Month := fifelse(activity_month <= 12, 
                      rep(month.abb, 2)[activity_month], 
                      paste0("2nd Year ", rep(month.abb, 2)[activity_month]))]

acvt[, .(.N, acvt_Month),by=activity_month] %>% 
  hchart("column", hcaes(x = acvt_Month , y = N)) %>% 
  hc_yAxis(title = list(text = "")) %>% 
  hc_xAxis(title = list(text = "")) %>% 
  hc_legend(enabled = F) %>% 
  hc_title(text = "Active Users in Each Month") %>% 
  hc_add_theme(hc_theme_tufte()) %>% 
  hc_tooltip(
    useHTML = TRUE,                            
    formatter = JS(
      "
      function(){
        outHTML = '<b>' +this.point.acvt_Month + '</b> <br> Active Users: ' + this.point.N 
        return(outHTML)
      }
      "
    )
  ) 

# – Make some comments on the trends and any lower periods (remember that Month 1 is January)
# • Extra tasks
# – T2A: Plot the percentage of America among active users in each month
round(acvt[region == "America", .(.N),]/nrow(acvt), 2)

merge(acvt[region == "America", .(.N),by= activity_month],
      acvt[,.(.N),by= activity_month],
      by = "activity_month")[, .(American_ratio = round(N.x/N.y, 3),activity_month )] %>% 
  ggplot(aes(x=activity_month, y=American_ratio)) +
  geom_line(color="grey") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=4)+
  theme_ipsum() +
  geom_hline( yintercept=0.2, color="orange", size=0.5) +
  scale_x_continuous(breaks=seq(1, 21, by = 1),
                     labels = unique(acvt$acvt_Month))

# – T2B: Classify each active user as New (registered in that month), Retained (was active the previous month as well), and Resurrected (was inactive the previous month and not New). Plot the number of Retained active users in each month.
acvt[, type:= fifelse(activity_month== registration_month, "New",
                      fifelse(activity_month > registration_month,"Retained","Resurrected")), ]


x <- acvt[,.(activity_month, registration_month), by=id]

# Task 3: Retention -------------------------------------------------------
# Basic task (T3)
# – Calculate what percentage of Month 1 registered users have been active in Month 2 (second month retention rate)
table(acvt$id %in% rg$id)
table(rg$id %in% acvt$id )

rertain_2nd_M <- acvt[activity_month == registration_month +1,.(activity_month, registration_month)] 

avg_ratio <- nrow(rertain_2nd_M) / length(unique(acvt$id))

rertain_ratio <- merge(rertain_2nd_M[,.N,by = registration_month],
                       rg[,.(.N), by = registration_month], by = "registration_month")[,.(Month =registration_month,No.Register = N.y, No.Retention=N.x,ratio=round(N.x/N.y, 3))]

kbl(rertain_ratio) %>%
  kable_paper(full_width = F) %>%
  row_spec(c(1,13), bold = T, color = "white", background = "#D7261E") %>% 
  scroll_box(height = "250px")

rertain_ratio%>% 
  ggplot(aes(x=Month, y=ratio)) +
  geom_line(color="grey") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=4)+
  theme_ipsum() +
  geom_hline( yintercept=avg_ratio, color="orange", size=0.5) +
  scale_x_continuous(breaks=seq(1, 21, by = 1),
                     labels = unique(acvt$acvt_Month))

# – Calculate the same rate for users who registered in Month 13 (What percentage of them have been active in Month 14?)
# – What can explain the difference?
#   • Extra tasks
# – T3A: Plot the second month retention rate over time (from Month 1 to Month 20). Do you see a big change somewhere?


#   – T3B: Compare the Month 1 to Month 2 retention rate among users with different operating systems. Do you see any difference?
# os_retain <- 
os_retain <- acvt[activity_month == registration_month +1, .(No_retention = .N), by = c('registration_month','operating_system')][, .(operating_system,No_retention, Ratio = No_retention/sum(No_retention)),by =registration_month]
#plot
os_retain %>% 
  ggplot(aes(registration_month)) + # bar automatically calculate numbers, so it needs x axis only
  geom_bar(aes(fill = operating_system)) +
  labs(x= 'Cities', y= 'Number of hotels', fill= 'Distance from city center') +
  theme(legend.position="top")

# Plot
  os_retain %>%
  ggplot( aes(x=registration_month, y=No_retention, group=operating_system, color=operating_system)) +
  geom_line() +
  geom_point() +
  scale_colour_viridis_d(option = "plasma") +
  ggtitle("Popularity of American names in the previous 30 years") +
  theme_ipsum() +
  ylab("Number of babies born") +
  transition_reveal(registration_month)
  
  
  
  
  
  
