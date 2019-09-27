
rm(list=ls(all=TRUE))



library(readxl)
library(dplyr)
library(data.table)
library(XLConnect)
library(tidyr)
library(mosaic)
library(gbm)



process_weather_data <- function(weather_df,year_val){
  
  if(year_val != "2014"){
    weather_df <- tail(weather_df,-1)  
  }else{
    weather_df <- tail(weather_df,-2) 
    weather_df <- weather_df[-nrow(weather_df),]
  }
  
  cols <- c(1:2,5,8,11,14,17,20,23)
  weather_df<-weather_df[,cols]
  colnames(weather_df)<-c('year','month','temp_avg','dew_avg','humidity_avg','sea_level_avg','visibility_avg','wind_avg','weather_event')
  weather_df$year<-year_val
  weather_df[,c(1,3,4,5,6,7,8)] <- sapply( weather_df[,c(1,3,4,5,6,7,8)], as.numeric )
  weather_df[,c(2)] <- sapply(weather_df[,c(2)],as.factor)
  head(weather_df)
  weather_df <- weather_df %>% mutate(weather_event = ifelse(is.na(weather_event),'normal','abnormal'))
  weather_df[,c(9)] <- sapply(weather_df[,c(9)],as.factor)
  
  weather_df$temp_avg[is.na(weather_df$temp_avg)] = mean(weather_df$temp_avg, na.rm = TRUE)
  weather_df$dew_avg[is.na(weather_df$dew_avg)] = mean(weather_df$dew_avg, na.rm = TRUE)
  weather_df$humidity_avg[is.na(weather_df$humidity_avg)] = mean(weather_df$humidity_avg, na.rm = TRUE)
  weather_df$sea_level_avg[is.na(weather_df$sea_level_avg)] = mean(weather_df$sea_level_avg, na.rm = TRUE)
  weather_df$visibility_avg[is.na(weather_df$visibility_avg)] = mean(weather_df$visibility_avg, na.rm = TRUE)
  weather_df$wind_avg[is.na(weather_df$wind_avg)] = mean(weather_df$wind_avg, na.rm = TRUE)
  
  output <- setDT(weather_df)[ , .(count=.N) , by = .(weather_event,month)]
  weather_event_counts<-spread(data=output, key =weather_event, count)
  
  weather_df$weather_event <- NULL
  weather_df = weather_df %>% group_by(year,month ) %>% summarise_each(funs(mean))
  
  new_weather_df<-merge(weather_df,weather_event_counts, by='month')
  new_weather_df$year_month <- paste(new_weather_df$year, new_weather_df$month, sep=" - ")
  new_weather_df$month<-NULL
  new_weather_df$year<-NULL

  return(new_weather_df)
}



wb = loadWorkbook("/home/1320B29/phd/WeatherData.xlsx")
df_2009 = readWorksheet(wb, sheet = "2009", header = FALSE)
df_2010 = readWorksheet(wb, sheet = "2010", header = FALSE)
df_2011 = readWorksheet(wb, sheet = "2011", header = FALSE)
df_2012 = readWorksheet(wb, sheet = "2012", header = FALSE)
df_2013 = readWorksheet(wb, sheet = "2013", header = FALSE)
df_2014 = readWorksheet(wb, sheet = "2014", header = FALSE)
df_2015 = readWorksheet(wb, sheet = "2015", header = FALSE)
df_2016 = readWorksheet(wb, sheet = "2016", header = FALSE)

new_df_2009 <- process_weather_data(df_2009,"2009")
new_df_2010 <- process_weather_data(df_2010,"2010")
new_df_2011 <- process_weather_data(df_2011,"2011")
new_df_2012 <- process_weather_data(df_2012,"2012")
new_df_2013 <- process_weather_data(df_2013,"2013")
new_df_2014 <- process_weather_data(df_2014,"2014")
new_df_2015 <- process_weather_data(df_2015,"2015")
new_df_2016 <- process_weather_data(df_2016,"2016")

weather_total <- rbind(new_df_2009,new_df_2010,new_df_2011,new_df_2012,new_df_2013,new_df_2014,new_df_2015,new_df_2016)

sum(is.na(weather_total))
nrow(weather_total)



sales_data<-read.csv('/home/1320B29/phd/Train.csv')
colnames(sales_data)[4] <- "sales"
women_sales<-filter(sales_data, ProductCategory %in% c("WomenClothing"))
women_sales$sales <- na.kalman(women_sales$sales,model='auto.arima')

women_sales <- mutate(women_sales, newvar = derivedFactor(
  "Jan" = Month %in% c(1),
  "Feb" = Month %in% c(2),
  "Mar" = Month %in% c(3),
  "Apr" = Month %in% c(4),
  "May" = Month %in% c(5),
  "Jun" = Month %in% c(6),
  "Jul" = Month %in% c(7),
  "Aug" = Month %in% c(8),
  "Sep" = Month %in% c(9),
  "Oct" = Month %in% c(10),
  "Nov" = Month %in% c(11),
  "Dec" = Month %in% c(12),
  .default = NA
))

women_sales$Month<-NULL
women_sales$year_month <- paste(women_sales$Year, women_sales$newvar, sep=" - ")
women_sales$Year<-NULL
women_sales$newvar<-NULL
women_sales$ProductCategory<-NULL
head(women_sales)
nrow(women_sales)




women_weather <- merge(women_sales,weather_total,by='year_month',all.y=T)
head(women_weather)
nrow(women_weather)
sum(is.na(women_weather))
tail(women_weather)




temp1 <- read_excel("/home/1320B29/phd/Events_HolidaysData.xlsx", sheet = 'Sheet1')
setDT(temp1)[, MonthDate := as.numeric(format(as.Date(MonthDate), "%m")) ]

temp1 <- mutate(temp1, newvar = derivedFactor(
  "Jan" = MonthDate %in% c(01),
  "Feb" = MonthDate %in% c(02),
  "Mar" = MonthDate %in% c(03),
  "Apr" = MonthDate %in% c(04),
  "May" = MonthDate %in% c(05),
  "Jun" = MonthDate %in% c(06),
  "Jul" = MonthDate %in% c(07),
  "Aug" = MonthDate %in% c(08),
  "Sep" = MonthDate %in% c(09),
  "Oct" = MonthDate %in% c(10),
  "Nov" = MonthDate %in% c(11),
  "Dec" = MonthDate %in% c(12),
  .default = NA
))
temp1<-temp1[,c(1,4,5)] 
colnames(temp1)<-c('year','type','month')  
head(temp1)


temp1$year_month <- paste(temp1$year, temp1$month, sep=" - ")
temp1$year<-NULL
temp1$month<-NULL
output <- setDT(temp1)[ , .(count=.N) , by = .(type,year_month)]  
event_counts<-spread(data=output, key =type, count)
colnames(event_counts)<-c('year_month','event_holiday','federal_holiday')
event_counts <- event_counts %>%
      mutate(event_holiday = ifelse(is.na(event_holiday),0,event_holiday))
holidays <- event_counts %>%
      mutate(federal_holiday = ifelse(is.na(federal_holiday),0,federal_holiday))

new.holidays <- data.frame(year_month = '2016 - Aug', event_holiday = c(0), federal_holiday = c(0))
holidays <- rbind(holidays,new.holidays)

#existingDF <- rbind(existingDF[1:r,],newrow,existingDF[-(1:r),])
new_rows <- data.frame(year_month = c('2009 - Mar','2009 - Aug','2010 - Mar','2010 - Aug','2011 - Mar','2011 - Aug','2012 - Mar','2012 - Aug','2013 - Apr','2013 - Aug','2014 - Mar','2014 - Aug','2015 - Mar','2015 - Aug'), event_holiday = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0), federal_holiday = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0))

holidays <- rbind(holidays[1:70,],new_rows,holidays[-(1:70),])
nrow(holidays)
tail(holidays[70:76,])
sum(is.na(holidays))



women_weather_holiday <- merge(women_weather,holidays,by='year_month',all.x = T)
nrow(women_weather_holiday)
sum(is.na(women_weather_holiday))
colSums(is.na(women_weather_holiday))
str(women_weather_holiday)


#MODEL WITHOUT MACRO_ECONOMICS

final1<-women_weather_holiday[,-1]
str(final1)

train <- final1[1:84,]
test <- final1[85:96,]




write_results <- function(target){
submission_file <- read.csv('template_reg.csv')
submission_file$target <- target
write.csv(submission_file,'predictions.csv',row.names=F)
}



gbm1 <- gbm(sales~.,
         data=train,
         distribution = "gaussian",
         interaction.depth=4,
         n.minobsinnode = 2,
         bag.fraction=1.0,
         n.trees = 10000,
         cv.folds=10)


gbm.perf(gbm1)

preds <- predict(gbm1,test)
preds

write_results(preds)



gbm2 <- gbm(sales~.,
         data=train,
         distribution = "gaussian",
         interaction.depth=5,
         n.minobsinnode = 2,
         bag.fraction=1.0,
         n.trees = 50000,
         cv.folds=10)


gbm.perf(gbm2)

preds <- predict(gbm2,test)
preds

write_results(preds)


