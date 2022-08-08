##Christopher Vollmer
##Dr. Azencott - Machine Learning
##Project 3 - Part 2 and part 3 - Kernel Ridge Regression PLOTTING

###Not ALL of the plots generated in the paper have their own code. I changed/deleted
###values in the ggplots to generate some of the plots I need without having to copy/paste
###a bunch more times.


##Plot actual next day price for training set
ggplot(data = training, aes(x = Date)) +
  geom_line(aes(y=JJ, color = 'a'), size = 1) +
  scale_color_manual(name="PRICE",values = c('a'="blue"),
                     labels = c('Actual Price')) +
  xlab("Time(days)") +  ylab("Aluminum Price ($)") +
  ggtitle("Actual Price vs. Time of next-day Aluminium for the Training Set")
##Plot actual next day price for testing set
ggplot(data = testing, aes(x = Date)) +
  geom_line(aes(y=JJ, color = 'a'), size = 1) +
  scale_color_manual(name="PRICE",values = c('a'="blue"),
                     labels = c('Actual Price')) +
  xlab("Time(days)") +  ylab("Aluminum Price ($)") +
  ggtitle("Actual Price vs. Time of next-day Aluminium for the Testing Set")
##Plot actual next day price for whole set
tempdf <- data.frame(Date = df$Date, JJ = c(training$JJ, testing$JJ))

ggplot(data = tempdf, aes(x = as.numeric(Date), y = JJ)) +
  geom_line(aes(color = 'a')) +
  scale_color_manual(name="PRICE",values = c('a'="blue"),
                     labels = c('Actual Price')) +
  geom_vline(xintercept = 600, size = 1, linetype = 2) +
  geom_text(x = 600, y = 1.1, label = "Test data --->", hjust = 1.1, size = 5) +
  xlab("Time(days)") +  ylab("Aluminum Price ($)") +
  ggtitle("Actual Price vs. Time of next-day Aluminium for the Whole Set")






##Plot actual vs. predifted next day price for training set
ggplot(data = training, aes(x = Date)) +
  geom_line(aes(y=JJ, color = 'a'), size = 1) +
  geom_line(aes(y=Predict, color = 'b'))+
  scale_color_manual(name="PRICE",values = c('a'="blue", 'b'="red"),
                     labels = c('Actual Price', 'Predicted Price')) +
  xlab("Time(days)") +  ylab("Aluminum Price ($)") +
  ggtitle("Actual Price vs. Predicted Price of Aluminium for the Training Set")

##Plot actual vs. predifted next day price for training set
ggplot(data = testing, aes(x = Date)) +
  geom_line(aes(y=JJ, color = 'a'), size = 1) +
  geom_line(aes(y=Predict, color = 'b'))+
  scale_color_manual(name="PRICE",values = c('a'="blue", 'b'="red"),
                     labels = c('Actual Price', 'Predicted Price')) +
  xlab("Time(days)") +  ylab("Aluminum Price ($)") +
  ggtitle("Actual Price vs. Predicted Price of Aluminium for the Testing Set")



###Plotting over the whole 858 days. Vertical line represents the cutoff from training to testing
tempdf <- data.frame(Date = df$Date, JJ = c(training$JJ, testing$JJ), Predict = c(training$Predict, testing$Predict),
                     Predict.krr = c(training$Predict.krr, testing$Predict.krr))


ggplot(data = tempdf, aes(x = as.numeric(Date))) +
  geom_line(aes(y=JJ, color = 'a'), size = 1) +
  geom_line(aes(y=Predict, color = 'b')) +
  geom_line(aes(y=Predict.krr, color = 'c')) +
  geom_vline(xintercept = 600, size = 1, linetype = 2) +
  geom_text(x = 600, y = 1.1, label = "Test data --->", hjust = 1.1, size = 5) +
  scale_color_manual(name="PRICE",values = c('a'="blue", 'b'="red", 'c'="green"),
                     labels = c('Actual Price', 'Predicted Price w/o KRR', 'Predicted Price with KRR')) +
  xlab("Time(days)") +  ylab("Aluminum Price ($)") +
  ggtitle("Actual Price vs. Predicted Price of Aluminium")




##Actual Vs. Predicted for training set with KRR
ggplot(data = training, aes(x = Date)) +
  geom_line(aes(y=JJ, color = 'a'), size = 1) +
  geom_line(aes(y=Predict.krr, color = 'c'))+
  scale_color_manual(name="PRICE",values = c('a'="blue", 'c'="green"),
                     labels = c('Actual Price', 'Predicted Price: KRR')) +
  xlab("Time(days)") +  ylab("Aluminium Price ($)") +
  ggtitle("Actual Price vs. Predicted Price for the Training Set")

##Actual Vs. Predicted for testing set with KRR
ggplot(data = testing, aes(x = Date)) +
  geom_line(aes(y=JJ, color = 'a'), size = 1) +
  geom_line(aes(y=Predict.krr, color = 'c')) +
  scale_color_manual(name="PRICE",values = c('a'="blue",  'c'="green"),
                     labels = c('Actual Price','Predicted Price: KRR')) +
  xlab("Time(days)") +  ylab("Aluminium Price ($)") +
  ggtitle("Actual Price vs. Predicted Price for the Testing Set")

##Actual Vs. Predicted for testing set with KRR
ggplot(data = testing, aes(x = Date)) +
  geom_line(aes(y=JJ, color = 'a'), size = 1) +
  geom_line(aes(y=Predict, color = 'b')) +
  geom_line(aes(y=Predict.krr, color = 'c')) +
  scale_color_manual(name="PRICE",values = c('a'="blue", 'b'="red", 'c'="green"),
                     labels = c('Actual Price', 'Predicted Price w/o KRR','Predicted Price with KRR')) +
  xlab("Time(days)") +  ylab("Aluminium Price ($)") +
  ggtitle("Actual Price vs. Predicted Price of Aluminium for the Training Set")




#TRAINING SET---------------------------------------------------------------------------------------------#
#------------------------------------------ ALUMINIUM PRICES: RESIDUAL------------------------------------#
#---------------------------------------------------------------------------------------------------------#

num_dates = data.frame(Date = 1:nrow(training))
tempdata=data.frame(Date = num_dates, Residual1 = training$JJ-training$Predict,
                    Residual2 = training$JJ-training$Predict.krr)

ggplot(tempdata, aes(x = Date)) +
  xlab("Time(days)") +  ylab("Residual Value") +
  ggtitle("Residual Values of Future price of Aluminium (W/O KRR) in Training")+
  geom_point(aes(y = Residual1))

ggplot(tempdata, aes(x = Date)) +
  xlab("Time(days)") +  ylab("Residual Value") +
  ggtitle("Residual Values of Future price of Aluminium (WITH KRR) in Training")+
geom_point(aes(y = Residual2))

#TESTING SET----------------------------------------------------------------------------------------------#
#------------------------------------------ ALUMINIUM PRICES: RESIDUAL------------------------------------#
#---------------------------------------------------------------------------------------------------------#

num_dates = data.frame(Date = 1:nrow(testing))
tempdata=data.frame(Date = num_dates, Residual1 = testing$JJ-testing$Predict,
                    Residual2 = testing$JJ-testing$Predict.krr)

ggplot(tempdata, aes(x = Date)) +
  xlab("Time(days)") +  ylab("Residual Value") +
  ggtitle("Residual Values of Future price of Aluminium (W/O KRR) in Testing")+
  geom_point(aes(y = Residual1))

ggplot(tempdata, aes(x = Date)) +
  xlab("Time(days)") +  ylab("Residual Value") +
  ggtitle("Residual Values of Future price of Aluminium (WITH KRR) in Testing")+
  geom_point(aes(y = Residual2))




  