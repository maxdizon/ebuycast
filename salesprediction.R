# Load Libraries

rm(list=ls())

suppressMessages(library(data.table))
suppressMessages(library(DT))
suppressMessages(library(timeSeries))
suppressMessages(library(tidyverse))
suppressMessages(library(reshape))
suppressMessages(library(stringr))
suppressMessages(library(doBy))
suppressMessages(library(formattable))
suppressMessages(library(gridExtra))

suppressMessages(library(ggplot2))
suppressMessages(library(plotly))
suppressMessages(library(corrplot))
suppressMessages(library(wesanderson))
suppressMessages(library(RColorBrewer))
suppressMessages(library(gridExtra))
suppressMessages(library(zoo))

suppressMessages(library(forecast))
suppressMessages(library(prophet)) ### For Prophet Forecasting
suppressMessages(library(nnfor))    ### For Neural Network Forecasting

# Load Data, Structure & Summary of the Data

train=fread("../dataset/train1.csv")
sprintf("The train data set has %d rows and %d columns", nrow(train), ncol(train) )
str(train)

test  <- fread("../dataset/test1.csv")
sprintf("The test data set has %d rows and %d columns", nrow(test), ncol(test) )
str(test)

print("the summary of train sales is:")
summary(train$sales)


# Extraction of Year and Month of Year :
train$Year=year(train$date)        #returns the year from date i.e. 2013, 2014 etc.
train$Month=as.yearmon(train$date) #this yearmon() function is coming from zoo package returns the month of an year i.e Jan 2013, Feb 2015 etc

View(train)

MSP <- aggregate(sales ~date, train, mean)
# MSP <-na.omit(ddply(data, 'date', summarise, mean(Sale_Prices, na.rm=T)))

sl1 <-ggplot(MSP, aes(x=as.factor(date), y=sales))+
        geom_line(color='green', aes(group=1), size=1.5)+
        geom_point(colour='green', size = 3.5, alpha=0.5)+
        labs(title="Fluctuation of Sales Quantity by date", x=NULL, y="Sale Price")+
        theme( plot.title=element_text(vjust=3, size=15) ) + theme_minimal()

MSP$rate = c(0, 100*diff(MSP$sales)/MSP[-nrow(MSP),]$sales)

sl2 <-ggplot(MSP, aes(x=as.factor(date), y=rate))+
        geom_line(color= "gray50", aes(group=1), size=1)+
        #geom_point(colour='green', size = 3.5, alpha=0.5)+
        labs(title="Change rate of Sales Quantity", x="date", y="rate of change")+
        geom_hline(yintercept = 0, color = 'purple' )+
        theme(plot.title=element_text(size=15))+ theme_minimal()

grid.arrange(sl1,sl2)

MSP <- aggregate(sales ~Month, train, mean)
# MSP <-na.omit(ddply(data, 'date', summarise, mean(Sale_Prices, na.rm=T)))

sl1 <-ggplot(MSP, aes(x=as.factor(Month), y=sales))+
        geom_line(color='green', aes(group=1), size=1.5)+
        geom_point(colour='green', size = 3.5, alpha=0.5)+
        labs(title="The Fluctuation of Sales Quantity by Month of Year", x=NULL, y="Sale Price")+
        theme( plot.title=element_text(vjust=3, size=15) ) + theme_minimal()

MSP$rate = c(0, 100*diff(MSP$sales)/MSP[-nrow(MSP),]$sales)

sl2 <-ggplot(MSP, aes(x=as.factor(Month), y=rate))+
        geom_line(color= "gray50", aes(group=1), size=1)+
        #geom_point(colour=gbp1, size = 3.5, alpha=0.5)+
        labs(title="Change rate of Sales Quantity", x="Month", y="rate of change")+
        geom_hline(yintercept = 0, color = 'purple' )+
        theme(plot.title=element_text(size=15))+ theme_minimal()

grid.arrange(sl1,sl2)

# Model building using PROPHET: store= 1, Product_ID=1

train_final_store1_item1=subset(train,train$store==1 & train$item==1)

stats=data.frame(y=log1p(train_final_store1_item1$sales)
                 ,ds=train_final_store1_item1$date)
stats=aggregate(stats$y,by=list(stats$ds),FUN=sum)
head(stats)
colnames(stats)<- c("ds","y")

model_prophet = prophet(stats)
summary(model_prophet)
future = make_future_dataframe(model_prophet, periods = 90)
forecast = predict(model_prophet, future)

#

add_changepoints_to_plot <- function(m, threshold = 0.01, cp_color = "red",
                                     cp_linetype = "dashed", trend = TRUE, ...) {
        layers <- list()
        if (trend) {
                trend_layer <- ggplot2::geom_line(
                        ggplot2::aes_string("ds", "trend"), color = cp_color, ...)
                layers <- append(layers, trend_layer)
        }
        signif_changepoints <- m$changepoints[abs(m$params$delta) >= threshold]
        cp_layer <- ggplot2::geom_vline(
                xintercept = as.integer(signif_changepoints), color = cp_color,
                linetype = cp_linetype, ...)
        layers <- append(layers, cp_layer)
        return(layers)
}
plot(model_prophet, forecast)+ add_changepoints_to_plot(model_prophet)

prophet_plot_components(model_prophet, forecast)


# Customizing holidays and events

#New Year's Day & Labour Day holiday
playoffs <- data_frame(
        holiday = 'playoff',
        ds = as.Date(c('2018-01-01', '2018-05-01',
                       '2019-01-01', '2019-05-01',
                       '2020-01-01', '2020-05-01')),
        lower_window = 0,
        upper_window = 1
)

#Christmas & Last Day of Year & National Day holiday
superbowls <- data_frame(
        holiday = 'superbowl',
        ds = as.Date(c('2018-08-09', '2018-12-25', '2018-12-31', 
                       '2019-08-09', '2019-12-25', '2019-12-31',
                       '2020-08-09', '2020-12-25', '2020-12-31')),
        lower_window = 0,
        upper_window = 1
)
holidays <- bind_rows(playoffs, superbowls)

# IncludeFourier Order of Seasonality

model_prophet <- prophet()
model_prophet <- add_seasonality(model_prophet, name='daily', period=60, fourier.order=5)
model_prophet <- prophet(stats, holidays = holidays,holidays.prior.scale = 0.5, yearly.seasonality = 4,
                         interval.width = 0.95,changepoint.prior.scale = 0.006,daily.seasonality = T)
future = make_future_dataframe(model_prophet, periods = 90, freq = 'days')
forecast = predict(model_prophet, future)

plot(model_prophet, forecast) + add_changepoints_to_plot(model_prophet)
prophet_plot_components(model_prophet, forecast)

predict_store1_item1=data.frame(date=forecast$ds,forecast=expm1(forecast$yhat))
predict_store1_item1$yearmonth=as.yearmon(predict_store1_item1$date)

colnames(predict_store1_item1)<-c("ds","forecast","yearmonth")

# SMAPE CALCULATION

smape_cal <- function(outsample, forecasts){
        outsample <- as.numeric(outsample)
        forecasts<-as.numeric(forecasts)
        smape <- (abs(outsample-forecasts))/((abs(outsample)+abs(forecasts))/2)
        return(smape)
}

str(stats)
stats$ds=as.Date(stats$ds)
predict_store1_item1$ds=as.Date(predict_store1_item1$ds)

train_predict=merge(stats,predict_store1_item1,by="ds",all.x=T)
SMAPE_ERR <- smape_cal(outsample=train_predict$y, forecasts=train_predict$forecast)
SMAPE<-mean(SMAPE_ERR,na.rm = T)
sprintf("The value of SMAPE for Store-1 & Item-1 is %f ", SMAPE )


# Automation for Prophet: Splitting data by Store and Item

train$Year=NULL
train$Month=NULL
head(train)
train$sales=log1p(train$sales)

colnames(train)<- c("ds","store","item","y")
train_splitting= split(train, by=c('store', 'item'), keep.by=FALSE)
class(train_splitting)

prediction<-function(df)
{
        playoffs <- data_frame(
                holiday = 'playoff',
                ds = as.Date(c('2018-01-01', '2018-05-01',
                               '2019-01-01', '2019-05-01',
                               '2020-01-01', '2020-05-01')),
                lower_window = 0,
                upper_window = 1
        )
        
        superbowls <- data_frame(
                holiday = 'superbowl',
                ds = as.Date(c('2018-08-09', '2018-12-25', '2018-12-31', 
                               '2019-08-09', '2019-12-25', '2019-12-31',
                               '2020-08-09', '2020-12-25', '2020-12-31')),
                lower_window = 0,
                upper_window = 1
        )
        holidays <- bind_rows(playoffs, superbowls)
        
        
        
        
        model_prophet <- prophet()
        model_prophet <- add_seasonality(model_prophet, name='daily', period=60, fourier.order=5)
        model_prophet <- prophet(stats, holidays = holidays,holidays.prior.scale = 0.5, yearly.seasonality = 4,
                                 interval.width = 0.95,changepoint.prior.scale = 0.006,daily.seasonality = T)
        future = make_future_dataframe(model_prophet, periods = 90)
        forecast = predict(model_prophet, future)
        forecast_final<-  xts::last(forecast[, c("ds","yhat")],90)
        return(forecast_final)
        
}

prediction_final=as.data.frame(sapply(train_splitting[c(1,2)],prediction))

library(reshape)
dim(prediction_final)
md <- melt(prediction_final)
dim(md)
colnames(md)<-c("store","date","sales")
md$sales=expm1(md$sales)
head(md)
View(md)
write.csv(md, "../dataset/predicted1-1.csv", row.names = FALSE)
