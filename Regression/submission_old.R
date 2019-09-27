


rm(list=ls(all=TRUE))


#Loading Libraries
library(readxl)
library(caret)
library(dplyr)
library(MASS)
library(ggplot2)
library(data.table)
library(XLConnect)
library(tidyr)
library(mosaic)
library(randomForest)
library(gbm)
library(forecast)
library(lubridate)
library(DataCombine)
library(imputeTS)
library(plyr)
library(dplyr)
library(TTR)
library(graphics)
library(data.table)
library(Quandl)
library(DMwR)
library(timeDate)
library(corrplot)
library(rpart)


#Function to handle all the sheets from weather_data
process_weather_data <- function(weather_df,year_val){
  
  if(year_val != "2014"){
    weather_df <- tail(weather_df,-1)  
  }else{
    weather_df <- tail(weather_df,-2) #First row is junk
    weather_df <- weather_df[-nrow(weather_df),] ##last row is junk
  }
  
  #Taking only the cols that have average values as these values will be talking about average values of that particular month.
  cols <- c(1:2,5,8,11,14,17,20,23)
  weather_df<-weather_df[,cols]
  colnames(weather_df)<-c('year','month','temp_avg','dew_avg','humidity_avg','sea_level_avg','visibility_avg','wind_avg','weather_event')
  weather_df$year<-year_val
  weather_df[,c(1,3,4,5,6,7,8)] <- sapply( weather_df[,c(1,3,4,5,6,7,8)], as.numeric )
  weather_df[,c(2)] <- sapply(weather_df[,c(2)],as.factor)
  head(weather_df)
  
  #GETTING COUNT OF NORMAL AND ABNORMAL WEATHER IN THAT MONTH AS THIS PLAYS A ROLE IN SHOPPING
  weather_df <- weather_df %>% mutate(weather_event = ifelse(is.na(weather_event),'normal','abnormal'))
  weather_df[,c(9)] <- sapply(weather_df[,c(9)],as.factor)
  
  #IMPUTING NULL VALUES WITH MEAN OF THE COLUMNS
  weather_df$temp_avg[is.na(weather_df$temp_avg)] = mean(weather_df$temp_avg, na.rm = TRUE)
  weather_df$dew_avg[is.na(weather_df$dew_avg)] = mean(weather_df$dew_avg, na.rm = TRUE)
  weather_df$humidity_avg[is.na(weather_df$humidity_avg)] = mean(weather_df$humidity_avg, na.rm = TRUE)
  weather_df$sea_level_avg[is.na(weather_df$sea_level_avg)] = mean(weather_df$sea_level_avg, na.rm = TRUE)
  weather_df$visibility_avg[is.na(weather_df$visibility_avg)] = mean(weather_df$visibility_avg, na.rm = TRUE)
  weather_df$wind_avg[is.na(weather_df$wind_avg)] = mean(weather_df$wind_avg, na.rm = TRUE)
  
  #GETTING COUNT OF NORMAL AND ABNORMAL WEATHER IN THAT MONTH AS THIS PLAYS A ROLE IN SHOPPING
  output <- setDT(weather_df)[ , .(count=.N) , by = .(weather_event,month)]
  weather_event_counts<-spread(data=output, key =weather_event, count)
  
  weather_df$weather_event <- NULL
  weather_df = weather_df %>% group_by(year,month ) %>% summarise_each(funs(mean))
  
  new_weather_df<-merge(weather_df,weather_event_counts, by='month')
  #CREATING 'YYYY - MMM' KEY IN ALL THE DATAFRAMES TO USE AS KEY FOR JOINING.
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

#CALLING THE ABOVE FUNCTION TO PROCESS YEAR WISE WEATHER_DATA
new_df_2009 <- process_weather_data(df_2009,"2009")
new_df_2010 <- process_weather_data(df_2010,"2010")
new_df_2011 <- process_weather_data(df_2011,"2011")
new_df_2012 <- process_weather_data(df_2012,"2012")
new_df_2013 <- process_weather_data(df_2013,"2013")
new_df_2014 <- process_weather_data(df_2014,"2014")
new_df_2015 <- process_weather_data(df_2015,"2015")
new_df_2016 <- process_weather_data(df_2016,"2016")

#COMBINE ALL THE YEAR'S DATA
weather_total <- rbind(new_df_2009,new_df_2010,new_df_2011,new_df_2012,new_df_2013,new_df_2014,new_df_2015,new_df_2016)

sum(is.na(weather_total))
nrow(weather_total)


#GET WOMEN SALES DATA FROM TRAIN.CSV
sales_data<-read.csv('/home/1320B29/phd/Train.csv')
colnames(sales_data)[4] <- "sales"
women_sales<-filter(sales_data, ProductCategory %in% c("WomenClothing"))
women_sales$sales <- na.kalman(women_sales$sales,model='auto.arima')

#REPLACE NUMERIC VALUES WITH MONTH NAMES.
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
#CREATING 'YYYY - MMM' KEY IN ALL THE DATAFRAMES TO USE AS KEY FOR JOINING.
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

#PROCESS HOLIDAYS DATA. 
#GET COUNT OF EACH HOLIDAY TYPES FOR EACH MONTH. HOLIDAY TYPE CAN PLAY A ROLE. 
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

#DATA IS MISSING FOR SOME MONTHS. IMPUTING THOSE MONTH'S VALUES AS 0. (MISSING IS BECAUSE OF THERE ARE NO HOLIDAYS FOR THAT MONTH)
new_rows <- data.frame(year_month = c('2009 - Mar','2009 - Aug','2010 - Mar','2010 - Aug','2011 - Mar','2011 - Aug','2012 - Mar','2012 - Aug','2013 - Apr','2013 - Aug','2014 - Mar','2014 - Aug','2015 - Mar','2015 - Aug'), event_holiday = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0), federal_holiday = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0))

holidays <- rbind(holidays[1:70,],new_rows,holidays[-(1:70),])
nrow(holidays)
tail(holidays[70:76,])
sum(is.na(holidays))


#MERGE WOMENSALES, HOLIDAYS, WEATHER DATA. 
women_weather_holiday <- merge(women_weather,holidays,by='year_month',all.x = T)
nrow(women_weather_holiday)
sum(is.na(women_weather_holiday))
colSums(is.na(women_weather_holiday))
str(women_weather_holiday)


#MODEL WITHOUT MACRO_ECONOMICS

final1<-women_weather_holiday[,-1]
str(final1)

#TILL 2015 IS TRAINING DATA AND 2016 IS TEST DATA
train <- final1[1:84,]
test <- final1[85:96,]

#FUNCTION TO WRITE DATA TO PREDICTIONS.CSV EXCEL
write_results <- function(target){
submission_file <- read.csv('template_reg.csv')
submission_file$target <- target
write.csv(submission_file,'predictions.csv',row.names=F)
}


#MODEL1 - GBM
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


#MODEL 2 -> TUNING PARAMETERS FOR GBM
gbm2 <- gbm(sales~.,
         data=train,
         distribution = "gaussian",
         interaction.depth=5,
         n.minobsinnode = 2,
         bag.fraction=1.0,
         n.trees = 50000,
         cv.folds=5)


gbm.perf(gbm2)

preds <- predict(gbm2,test)
preds

write_results(preds)


#ADDING MOCRO_ECONOMICS DATA FOR CURRENT. 
macro_data <- read_excel("/home/1320B29/phd/MacroEconomicData.xlsx", sheet = 1, na='?')


colnames(macro_data)<-c('year_month','nominal_gdp','real_gdp','cpi','party_in_power','unemployment_rate','credit_interest','pers_interest','wages_per_hour','ad_expense','cotton_price','price_change_pers','upland_planted','upland_harvested','yield','production','mill_use','exports')

colSums(is.na(macro_data))
macro_data$ad_expense<-NULL
macro_data$party_in_power<-NULL

str(macro_data)
macro_data$year_month <- as.factor(macro_data$year_month)

#Find correlation of the macro economic data
corrplot(cor(macro_data[,-c(1)]),method="number")

highlyCorDescr <- findCorrelation(cor(macro_data[,-c(1)]), cutoff = 0.90)


#Feature engineering
#There are 6 highly correlated columns - nominal_gdp, real_gdp, wages_per_hour, unemployment_rate, production
#Production is highly correlated with upland_harvested- we will consider production
#We will drop nominal_gdp, real_gdp, unemployment_rate and retain wages per hour
macro_df_final = macro_data[,c("year_month","cpi","credit_interest","pers_interest","wages_per_hour","cotton_price","price_change_pers","upland_planted","yield","production","mill_use","exports")]


temp_macro = macro_df_final %>% group_by(year_month) %>% summarise_each(funs(mean))

macro_df_final = temp_macro
head(macro_df_final)
nrow(macro_df_final)


#FINAL MERGE OF COMPLETE DATA THAT WE HAVE. 
women_weather_holiday_macro <- merge(women_weather_holiday,macro_df_final,by='year_month',all.x = T)
nrow(women_weather_holiday_macro)
sum(is.na(women_weather_holiday_macro))
colSums(is.na(women_weather_holiday_macro))

#STANDARDIZE THE DATA.
std_model <- preProcess(women_weather_holiday_macro[,!names(women_weather_holiday_macro) %in% c("year_month","sales")],method = c("center","scale"))

women_weather_holiday_macro[,!names(women_weather_holiday_macro) %in% c("year_month","sales")] <- predict(object=std_model,newdata = women_weather_holiday_macro[,!names(women_weather_holiday_macro) %in% c("year_month","sales")])

head(women_weather_holiday_macro)


#GET TRAIN AND TEST DATA
final_df<-women_weather_holiday_macro[,-1]
str(final_df)

train <- final_df[1:84,]
test <- final_df[85:96,]


#GBM
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


#LINEAR REGRESSION
lr <- lm(formula = sales~. , data = train)
preds <- predict(lr,test)
preds
write_results(preds)


#APPLIED STEP_AIC ON ABOVE LINEAR MODEL TO GET THE LESS CORRELATED FEATURES.
lr_aic <- stepAIC(lr, direction = "both")
preds <-predict(lr_aic,test)
preds
write_results(preds)


#GRID SEARCH FOR GBM
gbmGrid <-  expand.grid(interaction.depth = c(2,5,8),
                        n.trees = c(2000,5000,10000,15000),
                        shrinkage = c(0.1,0.01),
                        n.minobsinnode = c(10))
                        
nrow(gbmGrid)

set.seed(545)
gbm.fit.control = trainControl(method = "cv", 
                               number = 5,
                               repeats = 1,
                               p = 0.75, 
                               verboseIter = T,
                               returnData = TRUE,
                               summaryFunction = defaultSummary,
                               selectionFunction = "best",
                               allowParallel = FALSE)

gbmFit2 <- train(sales~., data = train,
                 method = "gbm",
                 trControl = gbm.fit.control,
                 verbose = FALSE,
                 tuneGrid = gbmGrid,
                 ## Specify which metric to optimize
                 metric = "MAPE")
gbmFit2


preds <- predict(gbmFit2,test)
write_results(preds)



#Creting validation data
train_data <- final_df[1:72,]
validation_data <- final_df[73:84,]
test_data <- final_df[85:96,]


#RANDOM FOREST
# Set a random seed
set.seed(754)

rf_model <- randomForest(sales ~ .,data = train,mtry=3,importance=T,ntree=500)

# Get importance
importance    <- importance(rf_model)

preds <- predict(rf_model,test)
write_results(preds)


#CV METHOD FOR RANDOM FOREST MODEL
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
set.seed(545)
mtry <- sqrt(ncol(train))
rf_random <- train(sales~., data=train, method="rf", tuneLength=15, trControl=control)

preds <- predict(rf_random,test)
write_results(preds)

#RPART TREE FOR REGRESSION
reg_tree <- rpart(sales~., train)
reg_tree$variable.importance
rpart.plot(reg_tree)

preds <- predict(reg_tree, test)
write_results(preds)