setwd("C://Data Mining/Case Study")
WHR.df <- readxl::read_xlsx("Final Data File.xlsx")
#Data visualization
var_year17 <- WHR.df[which(WHR.df$year == 2017),3]
var_year16 <- WHR.df[which(WHR.df$year == 2016),3]
var_year15 <- WHR.df[which(WHR.df$year == 2015),3]
var_year14 <- WHR.df[which(WHR.df$year == 2014),3]
par(mfrow = c(2,2))
hist(var_year$`Life Ladder`, main = "Histogram of World Happiness Score 2017", col = "blue")
hist(var_year16$`Life Ladder`, main = "Histogram of World Happiness Score 2016", col = " light blue")
hist(var_year15$`Life Ladder`, main = "Histogram of World Happiness Score 2015", col = " sky blue")
hist(var_year14$`Life Ladder`, main = "Histogram of World Happiness Score 2014", col = " navy blue")
var_India <- WHR.df[which(WHR.df$country == 'India'),3]
var_India <- WHR.df[which(WHR.df$country == 'India'),c(2:3)]
plot(var_India$year, var_India$`Life Ladder`, type = "o", main = "Happiness Score for India", xlab = "Years", ylab = "Happiness Score")

library(gplots)
WHR.df_heatmap <- WHR.df[,-c(1:2)]
par(mfrow = c(1,1))
heatmap.2(cor(WHR.df_heatmap), Rowv = FALSE, Colv = FALSE, cellnote = round(cor(WHR.df_heatmap),2), notecol = "black", key = FALSE, trace = "none", margins = c(6,6))                   

#Standardize the dataset
funct_std <- function(x){(x - mean(x))/(sd(x))}
WHR.std.df <- as.data.frame(lapply(WHR.df[,-c(1:3)], funct_std))
var1 <- WHR.df[,1:3]
newdata.df <- c(var1,WHR.std.df)
newdata.df <- as.data.frame(newdata.df)
#Partition the dataset
train.df <- newdata.df[which(newdata.df$year < 2017),]
valid.df <- newdata.df[which(newdata.df$year == 2017),]
train.df <- train.df[,-c(1:2)]
valid.df <- valid.df[,-c(1:2)]

#Random Forest
library(party)
library(caret)
rf <- cforest(Life.Ladder ~ ., data = train.df)
cforestStats(rf)
rev(sort(varImp(rf)))
rf.predict <- predict(rf, newdata = valid.df)
RMSE(rf.predict, valid.df$Life.Ladder)
India.df <- newdata.df[which(newdata.df$country == "India"),]
India.pred <- predict(rf, newdata = India.df)
RMSE(India.pred, India.df$Life.Ladder)
cor(India.pred, India.df$Life.Ladder)
cor(rf.predict, valid.df$Life.Ladder)

#Multiple Linear Regression
library(car)
fit1 <- lm(Life.Ladder ~ Log.GDP.per.capita + Social.support + Healthy.life.expectancy.at.birth + Freedom.to.make.life.choices + Generosity + Perceptions.of.corruption + Positive.affect + Negative.affect + Confidence.in.national.government + Democratic.Quality + Delivery.Quality, data = newdata.df)
summary(fit1)
fit2 <- lm(Life.Ladder ~ Log.GDP.per.capita + Healthy.life.expectancy.at.birth + Social.support + Delivery.Quality + Positive.affect + Democratic.Quality, data = newdata.df)
summary(fit2)

#Neural Net
library(neuralnet)
nn.1 <- neuralnet(Life.Ladder ~ Log.GDP.per.capita + Healthy.life.expectancy.at.birth + Social.support + Delivery.Quality + Positive.affect + Democratic.Quality, data = train.df, hidden = 4, threshold = 0.1, stepmax = 1e+06)
predict1 <- compute(nn.1, data.frame(valid.df$Log.GDP.per.capita,  valid.df$Healthy.life.expectancy.at.birth, valid.df$Social.support,  valid.df$Delivery.Quality, valid.df$Positive.affect, valid.df$Democratic.Quality))
nn.2 <- neuralnet(Life.Ladder ~ Log.GDP.per.capita + Healthy.life.expectancy.at.birth + Social.support + Delivery.Quality + Positive.affect + Democratic.Quality, data = train.df, hidden = 5, threshold = 0.1, stepmax = 1e+06)
predict2 <- compute(nn.2, data.frame(valid.df$Log.GDP.per.capita,  valid.df$Healthy.life.expectancy.at.birth, valid.df$Social.support,  valid.df$Delivery.Quality, valid.df$Positive.affect, valid.df$Democratic.Quality))
nn.3 <- neuralnet(Life.Ladder ~ Log.GDP.per.capita + Healthy.life.expectancy.at.birth + Social.support + Delivery.Quality + Positive.affect + Democratic.Quality, data = train.df, hidden = c(4,3), threshold = 0.1, stepmax = 1e+06)
predict3 <- compute(nn.3, data.frame(valid.df$Log.GDP.per.capita,  valid.df$Healthy.life.expectancy.at.birth, valid.df$Social.support,  valid.df$Delivery.Quality, valid.df$Positive.affect, valid.df$Democratic.Quality))
nn.4 <- neuralnet(Life.Ladder ~ Log.GDP.per.capita + Healthy.life.expectancy.at.birth + Social.support + Delivery.Quality + Positive.affect + Democratic.Quality, data = train.df, hidden = c(4,3,2), threshold = 0.1, stepmax = 1e+06)
predict4 <- compute(nn.4, data.frame(valid.df$Log.GDP.per.capita,  valid.df$Healthy.life.expectancy.at.birth, valid.df$Social.support,  valid.df$Delivery.Quality, valid.df$Positive.affect, valid.df$Democratic.Quality))
RMSE(predict1$net.result, valid.df$Life.Ladder)
RMSE(predict2$net.result, valid.df$Life.Ladder)
RMSE(predict3$net.result, valid.df$Life.Ladder)
RMSE(predict4$net.result, valid.df$Life.Ladder)


#Time Series Forecasting

library(forecast)

auto.arima(India.df$Life.Ladder)
Box.test(India.df$Life.Ladder, type = "Ljung-Box")
ind.forecast <- forecast(India.df$Life.Ladder, h= 5)
plot(ind.forecast, xlab = "Years", ylab = "Life Ladder (Happiness Score", main = "Forecast of Happiness Score for India for next 5 years")
