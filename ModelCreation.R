#LIBRARIES______________________________________________________________________________________________________________________________________________________________________________________________________________
library(fpp)
library(scatterplot3d)
library(nnet)
library(lubridate)
library(forecast)
library(plyr)
library(tidyr)
library(corrplot)
library(schrute)
library(sentimentr)
library(readr)
library(stringr)

testdata <- (schrute::theoffice)
write.csv(testdata,"ogdata.csv")
#PULLING IN THE DATA___________________________________________________________________________________________________________________________________________________________________________________________________
rawdata <- read.csv("OfficeDataConditioned.csv")
data<- data.frame(rawdata)
rawtestdata <- read.csv("OfficeTestData.csv")
testdata<- data.frame(rawtestdata)

#sentimenttext <- read_lines("Office.csv")
#Sentiment_Score <- data.frame(sentiment_by(sentimenttext, by=NULL))
#Sentiment_Score <- Sentiment_Score[-c(1:3)]
#Emotion_Score <- data.frame(emotion_by(sentimenttext, by=NULL))
#Emotion_Score <- Emotion_Score[-c(1,3,5)]
#Profanity_Score <- data.frame(profanity_by(sentimenttext, by=NULL))
#Profanity_Score <- Profanity_Score[-c(1,2,4)]

#data <- cbind(data, Sentiment_Score)
#data <- cbind(data, Emotion_Score)
#data <- cbind(data, Profanity_Score)

#write.csv(Emotion_Score, "Emotion_Score.csv")
#write.csv(Sentiment_Score, "Sentiment_Score.csv")
#write.csv(Profanity_Score, "Profanity_Score.csv")

#INDICATORS & VARIABLES___________________________________________________________________________________________________________________________________________________________________________________________________
#DayHourOfWeekIndicator<-class.ind(TrainingNetDemandData$DaysHours)
#char_ErinIndicator<-class.ind(data$char_Erin)
#char_HollyIndicator<-class.ind(data$char_Holly)
#char_KellyIndicator<-class.ind(data$char_Kelly)
#char_TobyIndicator<-class.ind(data$char_Toby)
#char_KevinIndicator<-class.ind(data$char_Kevin)
#char_OscarIndicator<-class.ind(data$char_Oscar)
#char_MeredithIndicator<-class.ind(data$char_Meredith)
#char_CreedIndicator<-class.ind(data$char_Creed)
#char_JanIndicator<-class.ind(data$char_Jan)
#char_AndyIndicator<-class.ind(data$char_Andy)
#char_PhyllisIndicator<-class.ind(data$char_Phyllis)
#char_StanleyIndicator<-class.ind(data$char_Stanley)
#char_DwightIndicator<-class.ind(data$char_Dwight)
#char_AngelaIndicator<-class.ind(data$	char_Angela)
#char_PamIndicator<-class.ind(data$char_Pam)
#char_JimIndicator<-class.ind(data$char_Jim)
#char_MichaelIndicator<-class.ind(data$char_Michael)

#director_Alex.HardcastleIndicator<-class.ind(data$director_Alex.Hardcastle)
#director_Amy.HeckerlingIndicator<-class.ind(data$director_Amy.Heckerling)
#director_Asaad.KeladaIndicator<-class.ind(data$director_Asaad.Kelada)
#director_B.J.NovakIndicator<-class.ind(data$director_B.J.Novak)
#director_Brent.ForresterIndicator<-class.ind(data$director_Brent.Forrester)
#director_Brian.BaumgartnerIndicator<-class.ind(data$director_Brian.Baumgartner)
#director_Bryan.CranstonIndicator<-class.ind(data$director_Bryan.Cranston)
#director_Bryan.GordonIndicator<-class.ind(data$director_Bryan.Gordon)
#director_Charles.McDougalIndicator<-class.ind(data$director_Charles.McDougal)
#director_Charlie.GrandyIndicator<-class.ind(data$director_Charlie.Grandy)
#director_Claire.ScanlonIndicator<-class.ind(data$director_Claire.Scanlon)
#director_Craig.ZiskIndicator<-class.ind(data$director_Craig.Zisk)
#director_Daniel.ChunIndicator<-class.ind(data$director_Daniel.Chun)
#director_Danny.LeinerIndicator<-class.ind(data$director_Danny.Leiner)
#director_David.RogersIndicator<-class.ind(data$director_David.Rogers)
#director_Ed.HelmsIndicator<-class.ind(data$director_Ed.Helms)
#director_Greg.DanielsIndicator<-class.ind(data$director_Greg.Daniels)
#director_J.J.AbramsIndicator<-class.ind(data$director_J.J.Abrams)
#director_Jon.FavreauIndicator<-class.ind(data$director_Jon.Favreau)
#director_Ken.WhittinghamIndicator<-class.ind(data$director_Ken.Whittingham)
#director_Ken.KwapisIndicator<-class.ind(data$director_Ken.Kwapis)
#director_Lee.EisenbergIndicator<-class.ind(data$director_Lee.Eisenberg)
#director_Paul.FeigIndicator<-class.ind(data$director_Paul.Feig)
#director_Stephen.MerchantIndicator<-class.ind(data$director_Stephen.Merchant)
#director_Joss.WhedonIndicator<-class.ind(data$director_Joss.Whedon)											

#writer_Paul.LiebersteinIndicator<-class.ind(data$writer_Paul.Lieberstein)
#writer_Greg.DanielsIndicator<-class.ind(data$writer_Greg.Daniels)
#writer_Steve.CarellIndicator<-class.ind(data$writer_Steve.Carell)
#writer_B.J.NovakIndicator<-class.ind(data$writer_B.J.Novak)
#writer_Larry.WillmoreIndicator<-class.ind(data$writer_Larry.Willmore)
#writer_Lee.EisenbergIndicator<-class.ind(data$writer_Lee.Eisenberg)
#writer_Ricky.GervaisIndicator<-class.ind(writer_Ricky.Gervais)
#writer_Stephen.MerchantIndicator<-class.ind(writer_Stephen.Merchant)
#writer_Michael.SchurIndicator<-class.ind(data$writer_Michael.Schur)
#writer_Mindy.KalingIndicator<-class.ind(data$writer_Mindy.Kaling)
#writer_Warren.LiebersteinIndicator<-class.ind(data$writer_Warren.Lieberstein)

CharacterIndicator<-class.ind(data$character_index)
DirectorIndicator<-class.ind(data$director_index)
WriterIndicator<-class.ind(data$writer_index)
allIndicators<-data.frame(DirectorIndicator[,1:53],WriterIndicator[,1:47],CharacterIndicator[,1:31])

#NORMALIZATION FUNCTION
normalizefunction = function(x) {
  num = x - min(x)
  denom = max(x) - min(x)
  return(num/denom)
}
#UNNORMALIZATION FUNCTION
unnormalizefunction = function(x,min,max) {
  return(x*(max-min)+min)
}
#CONTINUOUS DATA
minRating<- 6.7
maxRating<- 9.7
allContinuous = data.frame(data$total_votes, data$ave_sentiment, data$ave_emotion, data$ave_profanity,data$imdb_rating)
colnames(allContinuous) = c("total_votes","ave_sentiment","ave_emotion","ave_profanity","imdb_rating")
normContinuous = data.frame(sapply(allContinuous, normalizefunction))
#SELECTING TRAINING DATA
allDataReady<-data.frame(data$season.episode,allIndicators,normContinuous)
training_sample_size <- floor(0.80 * nrow(allDataReady))
set.seed(1234567)
#TRAINING DATA LIST
train_ind <- sample(seq_len(nrow(allDataReady)), size = training_sample_size)
train <- allDataReady[train_ind, ]
validation <- allDataReady[-train_ind, ]
predictorsTrain <-train[,2:136]
targetTrain<-train[,137]
predictorsValidation<-validation[,2:136]
targetValidation<-validation[,137]
trainingResults<-data.frame(HiddenNodes=numeric(),Decay=numeric(),Iterations=numeric(),MSE_Fit=numeric(), MSE_Validation=numeric(),RMSE_Validation_Unnorm=numeric(),MAPE=numeric())
names(trainingResults)=c("HiddenNodes","Decay","Iterations","MSE","MSE_Validation","RMSE_Validation_Unnorm","MAPE")


#TRAINING NEURAL NET - This section should not be run, it takes over an hour. Run line 182 to see results.___________________________________________________________________________________________________________________________________________________________________________________________________
for(h in c(1:6)){
  for(d in c(0.01,0.05,0.1)){
    for(maxIter in c(3:6)){
      maxiter=maxIter*100
      nnetFit<-nnet(predictorsTrain, # the regressor variables
                    targetTrain, #what you are trying to predict
                    size=h, #number of hidden nodes
                    decay = d, #gives a penalty for large weights
                    linout = TRUE, #says you want a linear output (as oppposed to a classification output)
                    trace=FALSE, #reduces amount of output printed to screen
                    maxit = maxiter, # increases max iterations to 500 from default of 100
                    MaxNWts = h*(ncol(predictorsTrain)+1)+h+1) #says you can have one weight for each input + an additional intercept term
      #CALCULATE ERROR AND RESULTS
      MSE_Fit<-mean((nnetFit$residuals)^2)
      #FORECASTING VALIDATION SET
      predictions<-predict(nnetFit,predictorsValidation)
      MSE_Validation <-(mean(predictions - targetValidation)^2)
      #RMSE ON ORIGINAL SCALE
      unnormalizedPredictions = unnormalizefunction(x=predictions,min=minRating,max=maxRating)
      unnormalizedTargetValidation = unnormalizefunction(x=targetValidation,min=minRating,max=maxRating)
      RMSE_Validation_Unnorm <-sqrt(mean((unnormalizedPredictions-unnormalizedTargetValidation)^2))
      MAPE <-mean(abs(unnormalizedTargetValidation - unnormalizedPredictions)/unnormalizedTargetValidation)*100
      results<-data.frame(h,d,maxIter,MSE_Fit,MSE_Validation,RMSE_Validation_Unnorm,MAPE)
      print(results)
      names(results)=c("HiddenNodes","Decay","Iterations","MSE_Fit","MSE_Validation","RMSE_Validation_Unnorm","MAPE")
      trainingResults<-rbind(trainingResults,results)
    }}}  
write.csv(trainingResults,file="trainingResultsfinal.csv") 
#CALCULATING RESIDUALS
Residuals<-data.frame(validation$data.index,unnormalizedTargetValidation-unnormalizedPredictions)
colnames(Residuals)=c("Time","Residuals")
Res<-as.vector(unnormalizedTargetValidation-unnormalizedPredictions)
#ACF/PACF OF RESIDUALS
par(mfrow=c(2,1))
acf(Res,lag.max=50,type="correlation",main="ACF of the Residuals",na.action = na.pass)
acf(Res,lag.max=50, type = "partial",main="PACF of the Residuals",na.action = na.pass)
#RESIDUAL PLOTS
par(mfrow=c(2,2), oma=c(0,0,0,0))
qqnorm(Res,datax=TRUE,pch=16,xlab="Residual",main="Normal Probability Plot")
qqline(Res,datax=TRUE)
plot(unnormalizedPredictions,Res,pch=16,xlab="Fitted Value",ylab="Residual", main="Fitted Value vs Residuals")
abline(h=0)
hist(Res,col="gray",xlab="Residual", main="Histogram of Residuals")
plot(Res,type="l",xlab="Observation Order",ylab="Residual", main="Residuals vs Order")
points(Res,pch=16,cex=0.5)
abline(h=0)
#PARAMETER PLOTS
trainingResults <-read.table(file="trainingResultsfinal.csv",header=T,sep=",")
trainingResults$X <- NULL
plot(trainingResults[,c(1,6)], type="l",col="red",lwd=2, ylab="RMSE",xlab="Hidden Nodes",main = "Hidden Nodes vs RMSE")
plot(trainingResults[,c(3,5)],type="l",col="blue",lwd=2,ylab="MSE_Validation",xlab="Iterations",main = "MSE_Validation versus # Iterations")
plot(trainingResults[trainingResults$Decay==0.01,c(1,4)], type="l",col="red",lwd=2, main = "MSE versus Hidden Nodes and Decay Rate")
lines(trainingResults[trainingResults$Decay==0.05,c(1,4)], col="blue", lwd=2)
lines(trainingResults[trainingResults$Decay==0.1,c(1,4)], col="green",lwd=2)
legend("topright",c("decay = 0.01", "decay=0.05","decay=0.1"),lty=c(1,1,1),col=c("red","blue","green"), lwd=c(2,2,2))



#BEST PARAMETERS___________________________________________________________________________________________________________________________________________________________________________________________________
#IDENTIFYING PARAMETERS
h=1
d=.01   #.05
maxIter=500   #800
#RETRAINING THE MODEL
nnetFinalFit<-nnet(allDataReady[,2:136],
                   allDataReady[,137],
                   size=h,
                   decay = d,
                   linout = TRUE,
                   trace=FALSE,
                   maxit = maxIter,
                   MaxNWts = h*(ncol(predictorsTrain)+1)+h+1)
weights <- data.frame(nnetFinalFit$wts)
#CALCULATING ERROR AND RESULTS
MSE_Fit<-mean((nnetFinalFit$residuals)^2)
#FORECASTING VALIDATION SET
predictions<-predict(nnetFinalFit,predictorsValidation)
MSE_Validation <-(mean(predictions - targetValidation)^2)
#RMSE ON ORIGINAL SCALE
unnormalizedPredictions = unnormalizefunction(x=predictions,min=minRating,max=maxRating)
unnormalizedTargetValidation = unnormalizefunction(x=targetValidation,min=minRating,max=maxRating)
RMSE_Validation_Unnorm <-sqrt(mean((unnormalizedPredictions-unnormalizedTargetValidation)^2))
MAPE <-mean(abs(unnormalizedTargetValidation - unnormalizedPredictions)/unnormalizedTargetValidation)*100
testresults<-data.frame(h,d,maxIter,MSE_Fit,MSE_Validation,RMSE_Validation_Unnorm,MAPE)
print(testresults)


#FORECASTS___________________________________________________________________________________________________________________________________________________________________________________________________
#INDICATOR VARIABLES
FinalDirectorIndicator<-class.ind(testdata$director_index)
FinalWriterIndicator<-class.ind(testdata$writer_index)
FinalCharacterIndicator<-class.ind(testdata$character_index)
allIndicatorsForecast<-data.frame(FinalDirectorIndicator[,1:53],FinalWriterIndicator[,1:47],FinalCharacterIndicator[,31])

#CONTINUOUS VARIABLES  
allContinuousForecast = data.frame(testdata$total_votes, testdata$ave_sentiment, testdata$ave_emotion, testdata$ave_profanity)
colnames(allContinuous) = c("total_votes","ave_sentiment","ave_emotion","ave_profanity")
sum(is.na(allContinuousForecast))
normContinuousForecast = data.frame(sapply(allContinuousForecast, normalizefunction))
allDataReadyForecast<-data.frame(testdata$season.episode,allIndicatorsForecast,normContinuousForecast)

#MAKING FORECASTS
testPredictions <-predict(nnetFinalFit,allDataReadyForecast[,2:33])
forecasts <-data.frame(season.episode=allDataReadyForecast$testdata.season.episode,testPredictions)
testPredictionsUnnorm<-unnormalizefunction(x=testPredictions,min=minRating,max=maxRating)
forecastOriginalScale<-data.frame(Date=allDataReadyForecast$testdata.season.episode,testPredictionsUnnorm)

#PLOTS
Corner_text <- function(text, location="topright"){
  legend(location,legend=text, bty ="n", pch=NA) 
}
par(mfrow=c(1,1))
plot(EasyPlotData,type="p",pch=16,cex=0.5,xlab="Time",ylab="Demand",main="Easy Forecast vs Actuals",ylim=c(0,8000))
lines(EasyforecastOriginalScale, col="red")
legend("topleft",c("y(t)","yhat(t)"),pch=c(16,NA),col=c("black","red"),lwd=c(NA,0.5),cex=0.55)
EasyMAPE <-mean(abs(EasytestWeekPredictionsUnnorm - EasyPlotData$Demand[1:169])/EasyPlotData$Demand[1:169])*100
textforGraph<-paste("MAPE =",EasyMAPE)
Corner_text(text=textforGraph,location= "bottomright")
par(mfrow=c(1,1))

#RESIDUALS__________________________________________________________________________________________________________________________________________________
#CALCULATING RESIDUALS
Residuals<-data.frame(testdata$season.episode,testdata$imdb_rating-forecastOriginalScale$testPredictionsUnnorm)
colnames(Residuals)=c("Season.Episode","Residuals")
Res<-as.vector(testdata$imdb_rating-forecastOriginalScale$testPredictionsUnnorm)
#ACF/PACF OF RESIDUALS
par(mfrow=c(2,1))
acf(Res,lag.max=50,type="correlation",main="ACF of the Residuals",na.action = na.pass)
acf(Res,lag.max=50, type = "partial",main="PACF of the Residuals",na.action = na.pass)
#RESIDUAL PLOTS
par(mfrow=c(2,2), oma=c(0,0,0,0))
qqnorm(Res,datax=TRUE,pch=16,xlab="Residual",main="Normal Probability Plot")
qqline(Res,datax=TRUE)
plot(testPredictions,Res,pch=16,xlab="Fitted Value",ylab="Residual", main="Fitted Value vs Residuals")
abline(h=0)
hist(Res,col="gray",xlab="Residual", main="Histogram of Residuals")
plot(Res,type="l",xlab="Observation Order",ylab="Residual", main="Residuals vs Order")
points(Res,pch=16,cex=0.5)
abline(h=0)

#h    d maxIter    MSE_Fit MSE_Validation RMSE_Validation_Unnorm     MAPE
#1 4 0.01     500 0.01998326   1.705192e-06              0.4367599 1.578055

forecastOriginalScale = cbind(forecastOriginalScale)
write.csv(forecastOriginalScale,file="forecast.csv")
