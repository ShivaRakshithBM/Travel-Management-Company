options(warn=-1)
library(shiny)
library(ggplot2)
setwd("C:\\Carlson MSBA\\Summer\\Analytics for competitive advantage\\For IDsc")

#Country Level
model_data <- read.csv("model_data.csv")
model_data <- model_data[(is.na(model_data$Country)) | (model_data$Country == 'US'),]
model_data$Revenue <- model_data$Revenue/2500
model_data$Transaction <- model_data$Transaction/2500
model_data$AvgPrice <- model_data$AvgPrice/250

model_data$Revenue <- model_data$Revenue + sample(-1:1,length(model_data$Revenue),replace = T)* model_data$Revenue/20
model_data$Transaction <- model_data$Transaction + sample(-1:1,length(model_data$Transaction),replace = T)* model_data$Transaction/20
model_data$AvgPrice <- model_data$AvgPrice + sample(-1:1,length(model_data$AvgPrice),replace = T)* model_data$AvgPrice/20


model_data$Dep_Month <- as.factor(model_data$Dep_Month)
model_data$Date <- as.Date(model_data$Date)
trnset <- model_data[model_data$Dep_Year < 2015,]
testset <- model_data[model_data$Dep_Year==2015,]

#Country Level
model_data1 <- read.csv("model_data1.csv")
model_data1$Revenue <- model_data1$Revenue/2500
model_data1$Transaction <- model_data1$Transaction/2500
model_data1$AvgPrice <- model_data1$AvgPrice/250
model_data1$Revenue <- model_data1$Revenue + sample(-1:1,length(model_data1$Revenue),replace = T)* model_data1$Revenue/20
model_data1$Transaction <- model_data1$Transaction + sample(-1:1,length(model_data1$Transaction),replace = T)* model_data1$Transaction/20
model_data1$AvgPrice <- model_data1$AvgPrice + sample(-1:1,length(model_data1$AvgPrice),replace = T)* model_data1$AvgPrice/20



model_data1$Dep_Month <- as.factor(model_data1$Dep_Month)
model_data1$Date <- as.Date(model_data1$Date)
trnset1 <- model_data1[model_data1$Dep_Year < 2015,]
testset1 <- model_data1[model_data1$Dep_Year==2015,]

#Region Level
model_data2 <- read.csv("model_data2.csv")
model_data2 <- model_data2[(is.na(model_data2$Region_code)) | (model_data2$Region_code == 'NA'),]
model_data2$Revenue <- model_data2$Revenue/2500
model_data2$Transaction <- model_data2$Transaction/2500
model_data2$AvgPrice <- model_data2$AvgPrice/250
model_data2$Revenue <- model_data2$Revenue + sample(-1:1,length(model_data2$Revenue),replace = T)* model_data2$Revenue/20
model_data2$Transaction <- model_data2$Transaction + sample(-1:1,length(model_data2$Transaction),replace = T)* model_data2$Transaction/20
model_data2$AvgPrice <- model_data2$AvgPrice + sample(-1:1,length(model_data2$AvgPrice),replace = T)* model_data2$AvgPrice/20



model_data2$Dep_Month <- as.factor(model_data2$Dep_Month)
model_data2$Region_code <- as.character(model_data2$Region_code)
model_data2[is.na(model_data2$Region_code),"Region_code"] <- "NA"
model_data2$Date <- as.Date(model_data2$Date)
trnset2 <- model_data2[model_data2$Dep_Year < 2015,]
testset2 <- model_data2[model_data2$Dep_Year==2015,]
# 
# 
# fit = lm(Revenue ~ AvgPrice + oilprice, data=  trnset2)
# 
# summary(fit)
# 
# fit$coefficients[2]
# fit$coefficients[3]

shinyServer(
  function(input, output, session) {
    #compute the formula for regressors, for example am + hp
    regressors <- reactive({
      do.call(paste, input$inRegressors, sep=" + ")
    })
    #compute the final linear model formula 
    formulaText <- reactive({
      paste(input$regressand,regressors() , sep=' ~ ')
    })

    #linear model
    fit <- reactive({ 
      if((input$Country == "ALL") & (input$Region_code == "ALL")){
        lm(as.formula(formulaText()) , data = trnset1)
      }else if((input$Country == "ALL") & (input$Region_code != "ALL")){
        lm(as.formula(formulaText()) , data = trnset2[trnset2$Region_code == input$Region_code,])    
      }
      else{
        lm(as.formula(formulaText()) , data = trnset[trnset$Country == input$Country,])  
      }
    })
    
    #Error Rates: Next month
    output$error_list_30 <- renderPrint({ 
      if((input$Country == "ALL") & (input$Region_code == "ALL")){
      list_30 <- numeric()
      for (i in 37:48){
        fit1 <- lm(as.formula(formulaText()),data = model_data1[1:(i-1),])
        pred_test <- predict(fit1, newdata=model_data1[i:48,])
        acc <- abs((model_data1[i:48,input$regressand][1] - pred_test[1]) * 100 / (model_data1[i:48,input$regressand][1]))
        list_30 <- append(list_30,acc)
      }} else if((input$Country == "ALL") & (input$Region_code != "ALL")){
        list_30 <- numeric()
        X <- model_data2[model_data2$Region_code == input$Region_code,][1:54,]
        for (i in 37:48){
          fit1 <- lm(as.formula(formulaText()),data = X[1:(i-1),])
          pred_test <- predict(fit1, newdata=X[i:48,])
          acc <- abs((X[i:48,input$regressand][1] - pred_test[1]) * 100 / (X[i:48,input$regressand][1]))
          list_30 <- append(list_30,acc)
      }} else {
        list_30 <- numeric()
        X <- model_data[model_data$Country == input$Country,][19:72,]
        for (i in 37:48){
          fit1 <- lm(as.formula(formulaText()),data = X[1:(i-1),])
          pred_test <- predict(fit1, newdata=X[i:48,])
          acc <- abs((X[i:48,input$regressand][1] - pred_test[1]) * 100 / (X[i:48,input$regressand][1]))
          list_30 <- append(list_30,acc)
      }
      }
      summary(list_30)
    })
    
    output$error_list_90 <- renderPrint({ 
      if((input$Country == "ALL") & (input$Region_code == "ALL")){
        list_90 <- numeric()
        for (i in 37:46){
          fit1 <- lm(as.formula(formulaText()),data = model_data1[1:(i-1),])
          pred_test <- predict(fit1, newdata=model_data1[i:48,])
          acc <- abs((sum(model_data1[i:48,input$regressand][1:3]) - sum(pred_test[1:3])) * 100 / sum(model_data1[i:48,input$regressand][1:3]))
          list_90 <- append(list_90,acc)
        }} else if((input$Country == "ALL") & (input$Region_code != "ALL")){
          list_90 <- numeric()
          X <- model_data2[model_data2$Region_code == input$Region_code,][1:54,]
          for (i in 37:46){
            fit1 <- lm(as.formula(formulaText()),data = X[1:(i-1),])
            pred_test <- predict(fit1, newdata=X[i:48,])
            acc <- abs((sum(X[i:48,input$regressand][1:3]) - sum(pred_test[1:3])) * 100 / sum(X[i:48,input$regressand][1:3]))
            list_90 <- append(list_90,acc)
          }} else {
            list_90 <- numeric()
            X <- model_data[model_data$Country == input$Country,][19:72,]
            for (i in 37:46){
              fit1 <- lm(as.formula(formulaText()),data = X[1:(i-1),])
              pred_test <- predict(fit1, newdata=X[i:48,])
              acc <- abs((sum(X[i:48,input$regressand][1:3]) - sum(pred_test[1:3])) * 100 / sum(X[i:48,input$regressand][1:3]))
              list_90 <- append(list_90,acc)
            }
          }
      summary(list_90)
    })
    
    output$error_list_yearly <- renderPrint({ 
      if((input$Country == "ALL") & (input$Region_code == "ALL")){
      pred_test <- predict(fit(), newdata=testset1)
      abs((sum(testset1[,input$regressand]) - sum(pred_test)) * 100 / sum(testset1[,input$regressand]))
      } else if((input$Country == "ALL") & (input$Region_code != "ALL")){
        X <- testset2[testset2$Region_code == input$Region_code,]
        pred_test <- predict(fit(), newdata=X)
        abs((sum(X[,input$regressand]) - sum(pred_test)) * 100 / sum(X[,input$regressand]))
      }else{
        X <- testset[testset$Country == input$Country,]
        pred_test <- predict(fit(), newdata=X)
        abs((sum(X[,input$regressand]) - sum(pred_test)) * 100 / sum(X[,input$regressand]))
      }
    })
    #display model fomrula
    output$model <- renderText({
      paste("Model: ",formulaText(), sep = " ")
    })
    output$summary <- renderPrint({
      summary(fit())
    })
    
    # Plots
    output$plot1 <- renderPlot({
      if((input$Country == "ALL") & (input$Region_code == "ALL")){
      X <- data.frame(Date = trnset1$Date,fitted = predict(fit(), newdata=trnset1))
      Y <- data.frame(Date = testset1$Date,fitted = predict(fit(), newdata=testset1))
      pred <- predict(fit(), newdata=model_data1[model_data1$Date > as.Date("2015-12-31"),])
      Z <- data.frame(Date = model_data1[model_data1$Date > as.Date("2015-12-31"),"Date"],fitted = pred)
      ggplot() + geom_line(data=model_data1, aes(x=Date, y=model_data1[,input$regressand]), color='black') + 
        geom_line(data = X,aes(x = Date, y= fitted), color='green') +
        geom_line(data = Y,aes(x = Date, y= fitted), color='blue') +
        geom_line(data = Z,aes(x = Date, y= fitted), color='red') + labs(title="Model Forecast", x="Date", y=input$regressand) + theme(text = element_text(size=14))
      }else if((input$Country == "ALL") & (input$Region_code != "ALL")){
        X <- data.frame(Date = trnset2[trnset2$Region_code == input$Region_code,"Date"],fitted = predict(fit(), newdata=trnset2[trnset2$Region_code == input$Region_code,]))
        Y <- data.frame(Date = testset2[testset2$Region_code == input$Region_code,"Date"],fitted = predict(fit(), newdata=testset2[testset2$Region_code == input$Region_code,]))
        z <- model_data2[(model_data2$Date > as.Date("2015-12-31")) & (model_data2$Region_code == input$Region_code),]
        z1 <- model_data2[379:396,]
        z <- rbind(z,z1)
        rownames(z) <- NULL
        pred <- predict(fit(), newdata= z)
        Z <- data.frame(Date = z[,"Date"],fitted = pred)
        ggplot() + geom_line(data=model_data2[model_data2$Region_code == input$Region_code,], aes(x=Date, y=model_data2[model_data2$Region_code == input$Region_code,input$regressand]), color='black') + 
          geom_line(data = X,aes(x = Date, y= fitted), color='green') +
          geom_line(data = Y,aes(x = Date, y= fitted), color='blue') +
          geom_line(data = Z,aes(x = Date, y= fitted), color='red') + labs(title="Model Forecast", x="Date", y=input$regressand) + theme(text = element_text(size=14))        
      } else{
        X <- data.frame(Date = trnset[trnset$Country == input$Country,"Date"],fitted = predict(fit(), newdata=trnset[trnset$Country == input$Country,]))
        Y <- data.frame(Date = testset[testset$Country == input$Country,"Date"],fitted = predict(fit(), newdata=testset[testset$Country == input$Country,]))
        z <- model_data[(model_data$Date > as.Date("2015-12-31")) & (model_data$Country == input$Country),]
        z1 <- model_data[1:18,]
        z <- rbind(z,z1)
        rownames(z) <- NULL
        z <- z[!is.na(z$Dep_Year),]
        pred <- predict(fit(), newdata= z)
        Z <- data.frame(Date = z[,"Date"],fitted = pred)
        ggplot() + geom_line(data=model_data[model_data$Country == input$Country,], aes(x=Date, y=model_data[model_data$Country == input$Country,input$regressand]), color='black') + 
          geom_line(data = X,aes(x = Date, y= fitted), color='green') +
          geom_line(data = Y,aes(x = Date, y= fitted), color='blue') +
          geom_line(data = Z,aes(x = Date, y= fitted), color='red') + labs(title="Model Forecast", x="Date", y=input$regressand) + theme(text = element_text(size=14))          
      }
    })
    
    output$pred_values <- renderPrint({
      if((input$Country == "ALL") & (input$Region_code == "ALL")){
      pred <- predict(fit(), newdata=model_data1[model_data1$Date > as.Date("2015-12-31"),])
      data.frame(Date = model_data1[model_data1$Date > as.Date("2015-12-31"),"Date"],Predictions = pred)
      } else if((input$Country == "ALL") & (input$Region_code != "ALL")){
      z <- model_data2[(model_data2$Date > as.Date("2015-12-31")) & (model_data2$Region_code == input$Region_code),]
      z1 <- model_data2[379:396,]
      z <- rbind(z,z1)
      rownames(z) <- NULL
      pred <- predict(fit(), newdata= z)
      data.frame(Date = z[,"Date"],Predictions = pred)
      }else{
        z <- model_data[(model_data$Date > as.Date("2015-12-31")) & (model_data$Country == input$Country),]
        z1 <- model_data[1:18,]
        z <- rbind(z,z1)
        rownames(z) <- NULL
        z <- z[!is.na(z$Dep_Year),]
        pred <- predict(fit(), newdata= z)
        data.frame(Date = z[,"Date"],Predictions = pred)  
      }
    })
}
)
