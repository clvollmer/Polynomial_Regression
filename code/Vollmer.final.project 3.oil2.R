# Christopher Vollmer
# Project 3 Part 1 - Linear Regression
# Dr. Azencott- Spring 2016- Machine Learning


num_dates.train = data.frame(1:length(training$Date))
num_dates.test = data.frame(1:length(testing$Date))
#---------------------------------------------------------------------------------------------------------#
#-------------------------------------VERSUS REGRESSION ESTIMATE------------------------------------------#
#------------------------------------Conventional Gasoline Prices-----------------------------------------#
###Actual vs. Predicted in Training
ggplot(data = training, aes(x = Date)) +
  geom_line(aes(y=E, color ='a'), size = 1) + ylab("Conventional Gas Price") + xlab("Day") +
  ggtitle("Conventional Gas Price for training set") + geom_line(aes(y=predYn_En, color = 'b'))+
  scale_color_manual(name="PRICE",values = c('a'="blue", 'b'="red"),
                     labels = c('Actual Price', 'Predicted Price'))
###Actual vs. Predicted in Testing
ggplot(data = testing, aes(x = Date)) +
  geom_line(aes(y=E, color ='a'), size = 1) + ylab("Conventional Gas Price") + xlab("Day") +
  ggtitle("Conventional Gas Price for testing set") + geom_line(aes(y=predYn_En, color = 'b'))+
  scale_color_manual(name="PRICE",values = c('a'="blue", 'b'="red"),
                     labels = c('Actual Price', 'Predicted Price'))

###Actual vs. Predicted in whole set
tempdf <- data.frame(Date = c(training$Date,testing$Date), E = c(training$E, testing$E),
                     Predict = c(training$predYn_En, testing$predYn_En))
ggplot(data = tempdf, aes(x = Date)) +
  geom_line(aes(y=E, color = 'a'), size = 1) + ylab("Conventional Gas Price") + xlab("Day") +
  geom_line(aes(y=Predict, color = 'b')) +
  scale_color_manual(name="PRICE",values = c('a'="blue", 'b'="red"),
                     labels = c('Actual Price', 'Predicted Price')) +
  geom_vline(xintercept = 952, size = 1, linetype = 2) +
  geom_text(x = 952, y = 2, label = "Test data --->", hjust = 1.1, size = 5) +
  ggtitle("Conventional Gas Price for entire set")

##Training
tempdata=data.frame(num_dates.train,En_lm$residuals)
ggplot(tempdata, aes(x = tempdata[,1])) +
  xlab("Time(days)") +  ylab("Residual Value") + ggtitle("Residual Values of Gasoline on Training Set") +
  geom_point(aes(y = tempdata[,2]))
##Testing
testing_gas_res = data.frame(testing$E-testing$predYn_En)
tempdata=data.frame(num_dates.test,testing_gas_res)
ggplot(tempdata, aes(x = tempdata[,1])) +
  xlab("Time(days)") +  ylab("Residual Value") + ggtitle("Residual Values of Gasoline on Testing Set") +
  geom_point(aes(y = tempdata[,2]))

#---------------------------------------------------------------------------------------------------------#
#-------------------------------------VERSUS REGRESSION ESTIMATE------------------------------------------#
#-----------------------------------------Heating Oil Prices----------------------------------------------#
###Actual vs. Predicted in Training
ggplot(data = training, aes(x = Date)) +
  geom_line(aes(y=I, color ='a'), size = 1) + ylab("Heating Oil Price") + xlab("Day") +
  ggtitle("Heating Oil Price for training set") + geom_line(aes(y=predYn_In, color = 'b'))+
  scale_color_manual(name="PRICE",values = c('a'="blue", 'b'="red"),
                     labels = c('Actual Price', 'Predicted Price'))
###Actual vs. Predicted in Testing
ggplot(data = testing, aes(x = Date)) +
  geom_line(aes(y=I, color ='a'), size = 1) + ylab("Heating Oil Price") + xlab("Day") +
  ggtitle("Heating Oil Price for testing set") + geom_line(aes(y=predYn_In, color = 'b'))+
  scale_color_manual(name="PRICE",values = c('a'="blue", 'b'="red"),
                     labels = c('Actual Price', 'Predicted Price'))

###Actual vs. Predicted in whole set
tempdf <- data.frame(Date = c(training$Date,testing$Date), I = c(training$I, testing$I),
                     Predict = c(training$predYn_In, testing$predYn_In))
ggplot(data = tempdf, aes(x = Date)) +
  geom_line(aes(y=I, color = 'a'), size = 1) + ylab("Heating Oil Price") + xlab("Day") +
  geom_line(aes(y=Predict, color = 'b')) +
  scale_color_manual(name="PRICE",values = c('a'="blue", 'b'="red"),
                     labels = c('Actual Price', 'Predicted Price')) +
  geom_vline(xintercept = 952, size = 1, linetype = 2) +
  geom_text(x = 952, y = 2, label = "Test data --->", hjust = 1.1, size = 5) +
  ggtitle("Heating Oil Price for entire set")



##Training
tempdata=data.frame(num_dates.train,In_lm$residuals)
ggplot(tempdata, aes(x = tempdata[,1], y = tempdata[,2])) +
  xlab("Time(days)") +  ylab("Residual Value") + ggtitle("Residual Values of Heated Oil on Training Set") +
  geom_point(aes(y = tempdata[,2]))
##Testing
testing_hoil_res = data.frame(testing$I-testing$predYn_In)
tempdata=data.frame(num_dates.test,testing_hoil_res)
ggplot(tempdata, aes(x = tempdata[,1])) +
  xlab("Time(days)") +  ylab("Residual Value") + ggtitle("Residual Values of Heated Oil on Testing Set") +
  geom_point(aes(y = tempdata[,2]))
  

