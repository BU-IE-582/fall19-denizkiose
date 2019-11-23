install.packages("readr")
install.packages("Metrics")
library(readr)
library(Metrics)

#read data from csv
#elConsumption is the training and elConsumptionTest the test data
elConsumption <- read_csv(file="C:/Users/deniz/Downloads/RealTimeConsumption-01012?16-31102019.csv", col_names=TRUE, col_types = NULL ) 
elConsumptionTest <- read_csv(file="C:/Users/deniz/Downloads/RealTimeConsumption-01112019-10112019.csv",col_names=TRUE, col_types = NULL)

# I have changed headers due to turkish characters
#Training pe?iod includes 01012016-31102019 (1st november is not included)
#Test period includes 01112019-10112019 (total of 10 days)
ntest = nrow(elConsumptionTest) #10 days
ntrain = NROW(elConsumption)



#Task (a)-Use lag48 and lag168 as prediction 
#lag48 

lag = 4? #take predictions for the lag periods 48 and 168
i = 1

task1Lag48 <- elConsumption$`Consumption (MWh)`
task1Lag48 = data.frame(Consumption = task1Lag48)

lag48Pred <- data.frame(Consumption = matrix(0,ntest,1))

for(i in 1:lag) {# for first 48 hours in t?st set
  lag48Pred$Consumption[i] <- task1Lag48$Consumption[ntrain-lag+i]
}

task1Lag48 <- rbind(task1Lag48,lag48Pred)
ii = ntrain+lag
il = ntrain+ntest

for(i in ii:il){# for the rest test set
  task1Lag48$Consumption[i] <- task1Lag48$Consumption[i-lag]
}?
mapeLag48 = mape(elConsumptionTest$`Consumption (MWh)`,task1Lag48$Consumption[33577:33816])

#lag168
lag = 168 #take predictions for the lag periods 48 and 168
i = 1

task1Lag168 <- elConsumption$`Consumption (MWh)`
task1Lag168 = data.frame(Consumption = ?ask1Lag168)

lag168Pred <- data.frame(Consumption = matrix(0,ntest,1))

for(i in 1:lag) {# for first 168 hours in test set
  lag168Pred$Consumption[i] <- task1Lag168$Consumption[ntrain-lag+i]
}

task1Lag168 <- rbind(task1Lag168,lag168Pred)
ii = ntrain+lag
?for(i in ii:il){# for the rest test set
  task1Lag168$Consumption[i] <- task1Lag168$Consumption[i-lag]
}

mapeLag168 = mape(elConsumptionTest$`Consumption (MWh)`,task1Lag168$Consumption[33577:33816])
print("The median absolute percentage error for lag48 an? lag 168 predictions are 0.0608 and 0.0393 respectively. Lag168 seems to be a better naive prediction measure if only mape is considered.")

#Task (b)-Use lag48 and lag168 to find regression parameters; then use them to predict the test data and calculate ?APE

task2train48 <- elConsumption$`Consumption (MWh)`
task2train48 = data.frame(Lag48 = task2train48)

task2train168 <- elConsumption$`Consumption (MWh)`
task2train168 = data.frame(Lag168 = task2train168)

i = 1
lag = 48
lagi = lag+1
for(i in lagi:ntrain)?
  task2train48$Lag48[i] <- task2train48$Lag48[i-lag]
}

i = 1
lag = 168
lagi = lag+1
for(i in lagi:ntrain){
  task2train168$Lag168[i] <- task2train168$Lag168[i-lag]
}

task2Data = cbind(elConsumption$`Consumption (MWh)`,task2train48$Lag48,task2train168$La?168) #regression data set. I will use data after 169th instance because no lag 168 is computed for previous ones.
task2Data = data.frame(realConsumption = task2Data[,1],lag48 = task2Data[,2], lag168 = task2Data[,3])
task2Data = task2Data[169:ntrain,]
#Buil? linear regression model


linearMod <- lm(realConsumption~ lag48+lag168, data=task2Data)
print(summary(linearMod))

#prepare dataframe to predict test data based on lag values
task2Test = data.frame(realConsumption = elConsumptionTest$`Consumption (MWh)`,?lag48 = task1Lag48$Consumption[33577:33816], lag168 = task1Lag168$Consumption[33577:33816])
linConsumption = predict(linearMod,task2Test)
mapeLinearModeal = mape(task2Test$realConsumption,linConsumption)

print("MAPE when lag48 and lag168 are used as predi?tors in the linear model is 8.73%")

#Task (c)-Construct different models for each hour 
#split dataframe based on hour 
data0 = data.frame("realConsumption" = matrix(0,nrow(task2Data)/24,1),"lag48"= matrix(0,nrow(task2Data)/24,1),"lag168"= matrix(0,nrow(t?sk2Data)/24,1))
data1 = data.frame("realConsumption" = matrix(0,nrow(task2Data)/24,1),"lag48"= matrix(0,nrow(task2Data)/24,1),"lag168"= matrix(0,nrow(task2Data)/24,1))
data2 = data.frame("realConsumption" = matrix(0,nrow(task2Data)/24,1),"lag48"= matrix(0,?row(task2Data)/24,1),"lag168"= matrix(0,nrow(task2Data)/24,1))
data3 = data.frame("realConsumption" = matrix(0,nrow(task2Data)/24,1),"lag48"= matrix(0,nrow(task2Data)/24,1),"lag168"= matrix(0,nrow(task2Data)/24,1))
data4 = data.frame("realConsumption" = ma?rix(0,nrow(task2Data)/24,1),"lag48"= matrix(0,nrow(task2Data)/24,1),"lag168"= matrix(0,nrow(task2Data)/24,1))
data5 = data.frame("realConsumption" = matrix(0,nrow(task2Data)/24,1),"lag48"= matrix(0,nrow(task2Data)/24,1),"lag168"= matrix(0,nrow(task2Data)/2?,1))
data6 = data.frame("realConsumption" = matrix(0,nrow(task2Data)/24,1),"lag48"= matrix(0,nrow(task2Data)/24,1),"lag168"= matrix(0,nrow(task2Data)/24,1))
data7 = data.frame("realConsumption" = matrix(0,nrow(task2Data)/24,1),"lag48"= matrix(0,nrow(task2D?ta)/24,1),"lag168"= matrix(0,nrow(task2Data)/24,1))
data8 = data.frame("realConsumption" = matrix(0,nrow(task2Data)/24,1),"lag48"= matrix(0,nrow(task2Data)/24,1),"lag168"= matrix(0,nrow(task2Data)/24,1))
data9 = data.frame("realConsumption" = matrix(0,nrow?task2Data)/24,1),"lag48"= matrix(0,nrow(task2Data)/24,1),"lag168"= matrix(0,nrow(task2Data)/24,1))
data10 = data.frame("realConsumption" = matrix(0,nrow(task2Data)/24,1),"lag48"= matrix(0,nrow(task2Data)/24,1),"lag168"= matrix(0,nrow(task2Data)/24,1))
data?1 = data.frame("realConsumption" = matrix(0,nrow(task2Data)/24,1),"lag48"= matrix(0,nrow(task2Data)/24,1),"lag168"= matrix(0,nrow(task2Data)/24,1))
data12 = data.frame("realConsumption" = matrix(0,nrow(task2Data)/24,1),"lag48"= matrix(0,nrow(task2Data)/24,?),"lag168"= matrix(0,nrow(task2Data)/24,1))
data13 = data.frame("realConsumption" = matrix(0,nrow(task2Data)/24,1),"lag48"= matrix(0,nrow(task2Data)/24,1),"lag168"= matrix(0,nrow(task2Data)/24,1))
data14 = data.frame("realConsumption" = matrix(0,nrow(task2?ata)/24,1),"lag48"= matrix(0,nrow(task2Data)/24,1),"lag168"= matrix(0,nrow(task2Data)/24,1))
data15 = data.frame("realConsumption" = matrix(0,nrow(task2Data)/24,1),"lag48"= matrix(0,nrow(task2Data)/24,1),"lag168"= matrix(0,nrow(task2Data)/24,1))
data16 = d?ta.frame("realConsumption" = matrix(0,nrow(task2Data)/24,1),"lag48"= matrix(0,nrow(task2Data)/24,1),"lag168"= matrix(0,nrow(task2Data)/24,1))
data17 = data.frame("realConsumption" = matrix(0,nrow(task2Data)/24,1),"lag48"= matrix(0,nrow(task2Data)/24,1),"la?168"= matrix(0,nrow(task2Data)/24,1))
data18 = data.frame("realConsumption" = matrix(0,nrow(task2Data)/24,1),"lag48"= matrix(0,nrow(task2Data)/24,1),"lag168"= matrix(0,nrow(task2Data)/24,1))
data19 = data.frame("realConsumption" = matrix(0,nrow(task2Data)/?4,1),"lag48"= matrix(0,nrow(task2Data)/24,1),"lag168"= matrix(0,nrow(task2Data)/24,1))
data20 = data.frame("realConsumption" = matrix(0,nrow(task2Data)/24,1),"lag48"= matrix(0,nrow(task2Data)/24,1),"lag168"= matrix(0,nrow(task2Data)/24,1))
data21 = data.fr?me("realConsumption" = matrix(0,nrow(task2Data)/24,1),"lag48"= matrix(0,nrow(task2Data)/24,1),"lag168"= matrix(0,nrow(task2Data)/24,1))
data22 = data.frame("realConsumption" = matrix(0,nrow(task2Data)/24,1),"lag48"= matrix(0,nrow(task2Data)/24,1),"lag168"=?matrix(0,nrow(task2Data)/24,1))
data23 = data.frame("realConsumption" = matrix(0,nrow(task2Data)/24,1),"lag48"= matrix(0,nrow(task2Data)/24,1),"lag168"= matrix(0,nrow(task2Data)/24,1))

a = 1
nDataTask3 = nrow(task2Data)
for(i in seq(1, nDataTask3, by = 24?){
  data0[a,] = task2Data[i,]
  data1[a,]= task2Data[i+1,]
  data2[a,]= task2Data[i+2,]
  data3[a,]= task2Data[i+3,]
  data4[a,]= task2Data[i+4,]
  data5[a,]= task2Data[i+5,]
  data6[a,]= task2Data[i+6,]
  data7[a,]= task2Data[i+7,]
  data8[a,]= task2Data?i+8,]
  data9[a,]= task2Data[i+9,]
  data10[a,]= task2Data[i+10,]
  data11[a,]= task2Data[i+11,]
  data12[a,]= task2Data[i+12,]
  data13[a,]= task2Data[i+13,]
  data14[a,]= task2Data[i+14,]
  data15[a,]= task2Data[i+15,]
  data16[a,]= task2Data[i+16,]
  da?a17[a,]= task2Data[i+17,]
  data18[a,]= task2Data[i+18,]
  data19[a,]= task2Data[i+19,]
  data20[a,]= task2Data[i+20,]
  data21[a,]= task2Data[i+21,]
  data22[a,]= task2Data[i+22,]
  data23[a,]= task2Data[i+23,]

  a=a+1
}

linearMod0 <- lm(realConsumption? lag48+lag168, data0)
linearMod1 <- lm(realConsumption~ lag48+lag168, data1)
linearMod2 <- lm(realConsumption~ lag48+lag168, data2)
linearMod3 <- lm(realConsumption~ lag48+lag168, data3)
linearMod4 <- lm(realConsumption~ lag48+lag168, data4)
linearMod5 <- ?m(realConsumption~ lag48+lag168, data5)
linearMod6 <- lm(realConsumption~ lag48+lag168, data6)
linearMod7 <- lm(realConsumption~ lag48+lag168, data7)
linearMod8 <- lm(realConsumption~ lag48+lag168, data8)
linearMod9 <- lm(realConsumption~ lag48+lag168, dat?9)
linearMod10 <- lm(realConsumption~ lag48+lag168, data10)
linearMod11 <- lm(realConsumption~ lag48+lag168, data11)
linearMod12 <- lm(realConsumption~ lag48+lag168, data12)
linearMod13 <- lm(realConsumption~ lag48+lag168, data13)
linearMod14 <- lm(realCon?umption~ lag48+lag168, data14)
linearMod15 <- lm(realConsumption~ lag48+lag168, data15)
linearMod16 <- lm(realConsumption~ lag48+lag168, data16)
linearMod17 <- lm(realConsumption~ lag48+lag168, data17)
linearMod18 <- lm(realConsumption~ lag48+lag168, data1?)
linearMod19 <- lm(realConsumption~ lag48+lag168, data19)
linearMod20 <- lm(realConsumption~ lag48+lag168, data20)
linearMod21 <- lm(realConsumption~ lag48+lag168, data21)
linearMod22 <- lm(realConsumption~ lag48+lag168, data22)
linearMod23 <- lm(realCons?mption~ lag48+lag168, data23)

#split test data hourly
data0t = data.frame("realConsumption" = matrix(0,nrow(task2Test)/24,1),"lag48"= matrix(0,nrow(task2Test)/24,1),"lag168"= matrix(0,nrow(task2Test)/24,1))
data1t = data.frame("realConsumption" = matrix(0?nrow(task2Test)/24,1),"lag48"= matrix(0,nrow(task2Test)/24,1),"lag168"= matrix(0,nrow(task2Test)/24,1))
data2t = data.frame("realConsumption" = matrix(0,nrow(task2Test)/24,1),"lag48"= matrix(0,nrow(task2Test)/24,1),"lag168"= matrix(0,nrow(task2Test)/24,1))?data3t = data.frame("realConsumption" = matrix(0,nrow(task2Test)/24,1),"lag48"= matrix(0,nrow(task2Test)/24,1),"lag168"= matrix(0,nrow(task2Test)/24,1))
data4t = data.frame("realConsumption" = matrix(0,nrow(task2Test)/24,1),"lag48"= matrix(0,nrow(task2Test?/24,1),"lag168"= matrix(0,nrow(task2Test)/24,1))
data5t = data.frame("realConsumption" = matrix(0,nrow(task2Test)/24,1),"lag48"= matrix(0,nrow(task2Test)/24,1),"lag168"= matrix(0,nrow(task2Test)/24,1))
data6t = data.frame("realConsumption" = matrix(0,nrow(?ask2Test)/24,1),"lag48"= matrix(0,nrow(task2Test)/24,1),"lag168"= matrix(0,nrow(task2Test)/24,1))
data7t = data.frame("realConsumption" = matrix(0,nrow(task2Test)/24,1),"lag48"= matrix(0,nrow(task2Test)/24,1),"lag168"= matrix(0,nrow(task2Test)/24,1))
data8? = data.frame("realConsumption" = matrix(0,nrow(task2Test)/24,1),"lag48"= matrix(0,nrow(task2Test)/24,1),"lag168"= matrix(0,nrow(task2Test)/24,1))
data9t = data.frame("realConsumption" = matrix(0,nrow(task2Test)/24,1),"lag48"= matrix(0,nrow(task2Test)/24,1?,"lag168"= matrix(0,nrow(task2Test)/24,1))
data10t = data.frame("realConsumption" = matrix(0,nrow(task2Test)/24,1),"lag48"= matrix(0,nrow(task2Test)/24,1),"lag168"= matrix(0,nrow(task2Test)/24,1))
data11t = data.frame("realConsumption" = matrix(0,nrow(task?Test)/24,1),"lag48"= matrix(0,nrow(task2Test)/24,1),"lag168"= matrix(0,nrow(task2Test)/24,1))
data12t = data.frame("realConsumption" = matrix(0,nrow(task2Test)/24,1),"lag48"= matrix(0,nrow(task2Test)/24,1),"lag168"= matrix(0,nrow(task2Test)/24,1))
data13t ? data.frame("realConsumption" = matrix(0,nrow(task2Test)/24,1),"lag48"= matrix(0,nrow(task2Test)/24,1),"lag168"= matrix(0,nrow(task2Test)/24,1))
data14t = data.frame("realConsumption" = matrix(0,nrow(task2Test)/24,1),"lag48"= matrix(0,nrow(task2Test)/24,1)?"lag168"= matrix(0,nrow(task2Test)/24,1))
data15t = data.frame("realConsumption" = matrix(0,nrow(task2Test)/24,1),"lag48"= matrix(0,nrow(task2Test)/24,1),"lag168"= matrix(0,nrow(task2Test)/24,1))
data16t = data.frame("realConsumption" = matrix(0,nrow(task2?est)/24,1),"lag48"= matrix(0,nrow(task2Test)/24,1),"lag168"= matrix(0,nrow(task2Test)/24,1))
data17t = data.frame("realConsumption" = matrix(0,nrow(task2Test)/24,1),"lag48"= matrix(0,nrow(task2Test)/24,1),"lag168"= matrix(0,nrow(task2Test)/24,1))
data18t =?data.frame("realConsumption" = matrix(0,nrow(task2Test)/24,1),"lag48"= matrix(0,nrow(task2Test)/24,1),"lag168"= matrix(0,nrow(task2Test)/24,1))
data19t = data.frame("realConsumption" = matrix(0,nrow(task2Test)/24,1),"lag48"= matrix(0,nrow(task2Test)/24,1),?lag168"= matrix(0,nrow(task2Test)/24,1))
data20t = data.frame("realConsumption" = matrix(0,nrow(task2Test)/24,1),"lag48"= matrix(0,nrow(task2Test)/24,1),"lag168"= matrix(0,nrow(task2Test)/24,1))
data21t = data.frame("realConsumption" = matrix(0,nrow(task2T?st)/24,1),"lag48"= matrix(0,nrow(task2Test)/24,1),"lag168"= matrix(0,nrow(task2Test)/24,1))
data22t = data.frame("realConsumption" = matrix(0,nrow(task2Test)/24,1),"lag48"= matrix(0,nrow(task2Test)/24,1),"lag168"= matrix(0,nrow(task2Test)/24,1))
data23t = ?ata.frame("realConsumption" = matrix(0,nrow(task2Test)/24,1),"lag48"= matrix(0,nrow(task2Test)/24,1),"lag168"= matrix(0,nrow(task2Test)/24,1))

a = 1
f = nrow(task2Test)
for(i in seq(1, f, by = 24)){
  data0t[a,] = task2Test[i,]
  data1t[a,]= task2Test[i+1?]
  data2t[a,]= task2Test[i+2,]
  data3t[a,]= task2Test[i+3,]
  data4t[a,]= task2Test[i+4,]
  data5t[a,]= task2Test[i+5,]
  data6t[a,]= task2Test[i+6,]
  data7t[a,]= task2Test[i+7,]
  data8t[a,]= task2Test[i+8,]
  data9t[a,]= task2Test[i+9,]
  data10t[a,]=?task2Test[i+10,]
  data11t[a,]= task2Test[i+11,]
  data12t[a,]= task2Test[i+12,]
  data13t[a,]= task2Test[i+13,]
  data14t[a,]= task2Test[i+14,]
  data15t[a,]= task2Test[i+15,]
  data16t[a,]= task2Test[i+16,]
  data17t[a,]= task2Test[i+17,]
  data18t[a,]= ?ask2Test[i+18,]
  data19t[a,]= task2Test[i+19,]
  data20t[a,]= task2Test[i+20,]
  data21t[a,]= task2Test[i+21,]
  data22t[a,]= task2Test[i+22,]
  data23t[a,]= task2Test[i+23,]
  
  a=a+1
}

linConsumption0 = predict(linearMod0,data0t)
linConsumption1 = pre?ict(linearMod1,data1t)
linConsumption2 = predict(linearMod2,data2t)
linConsumption3 = predict(linearMod3,data3t)
linConsumption4 = predict(linearMod4,data4t)
linConsumption5 = predict(linearMod5,data5t)
linConsumption6 = predict(linearMod6,data6t)
linConsu?ption7 = predict(linearMod7,data7t)
linConsumption8 = predict(linearMod8,data8t)
linConsumption9 = predict(linearMod9,data9t)
linConsumption10 = predict(linearMod10,data10t)
linConsumption11 = predict(linearMod11,data11t)
linConsumption12 = predict(linearM?d12,data12t)
linConsumption13 = predict(linearMod13,data13t)
linConsumption14 = predict(linearMod14,data14t)
linConsumption15 = predict(linearMod15,data15t)
linConsumption16 = predict(linearMod16,data16t)
linConsumption17 = predict(linearMod17,data17t)
lin?onsumption18 = predict(linearMod18,data18t)
linConsumption19 = predict(linearMod19,data19t)
linConsumption20 = predict(linearMod20,data20t)
linConsumption21 = predict(linearMod21,data21t)
linConsumption22 = predict(linearMod22,data22t)
linConsumption23 = p?edict(linearMod23,data23t)



mapeLinHourly = c(mape(data0t$realConsumption,linConsumption0),mape(data1t$realConsumption,linConsumption1),mape(data2t$realConsumption,linConsumption2),mape(data3t$realConsumption,linConsumption3),mape(data4t$realConsumption,?inConsumption4),mape(data5t$realConsumption,linConsumption5),mape(data6t$realConsumption,linConsumption6),mape(data7t$realConsumption,linConsumption7),mape(data8t$realConsumption,linConsumption8),mape(data9t$realConsumption,linConsumption9),mape(data10t$re?lConsumption,linConsumption10),mape(data11t$realConsumption,linConsumption11),mape(data12t$realConsumption,linConsumption12),mape(data13t$realConsumption,linConsumption13),mape(data14t$realConsumption,linConsumption14),mape(data15t$realConsumption,linConsu?ption15),mape(data16t$realConsumption,linConsumption16),mape(data17t$realConsumption,linConsumption17),mape(data18t$realConsumption,linConsumption18),mape(data19t$realConsumption,linConsumption19),mape(data20t$realConsumption,linConsumption20),mape(data21t?realConsumption,linConsumption21),mape(data22t$realConsumption,linConsumption22),mape(data23t$realConsumption,linConsumption23))
print("The MAPE values for the models constructed based on hourly lag48 and lag168 values are listed. Maximum error based on ho?rly consumption is observed for 15:00 and 16:00. In case of 15:00 MAPE is larger than that computed based on the model based on the whle data set.")
print(mapeLinHourly)
print("????wever, when mean is taken it is observed that the error is decreased to 6.45%")
print(mean(mapeLinHourly))

# Task (d)-LASSO
#data frames contain each lag 48 and lag168 values for hourly consumption so all data frames have to be used for predicting consump?ion of 1 hour
#1392 different dates
task3Predictors = data.frame("lag48_0" = data0$lag48,"lag168_0" = data0$lag168,"lag48_1" = data1$lag48,"lag168_1" = data1$lag168,"lag48_2" = data2$lag48,"lag168_2" = data2$lag168,"lag48_3" = data3$lag48,"lag168_3" = data?$lag168,"lag48_4" = data4$lag48,"lag168_4" = data4$lag168,"lag48_5" = data5$lag48,"lag168_5" = data5$lag168,"lag48_6" = data6$lag48,"lag168_6" = data6$lag168,"lag48_7" = data7$lag48,"lag168_7" = data7$lag168,"lag48_8" = data8$lag48,"lag168_8" = data8$lag16?,"lag48_9" = data9$lag48,"lag168_9" = data9$lag168,"lag48_10" = data10$lag48,"lag168_10" = data10$lag168,"lag48_11" = data11$lag48,"lag168_11" = data11$lag168,"lag48_12" = data12$lag48,"lag168_12" = data12$lag168,"lag48_13" = data13$lag48,"lag168_13" = dat?13$lag168,"lag48_14" = data14$lag48,"lag168_14" = data14$lag168,"lag48_15" = data15$lag48,"lag168_15" = data15$lag168,"lag48_16" = data16$lag48,"lag168_16" = data16$lag168,"lag48_17" = data17$lag48,"lag168_17" = data17$lag168,"lag48_18" = data18$lag48,"lag?68_18" = data18$lag168,"lag48_19" = data19$lag48,"lag168_19" = data19$lag168,"lag48_20" = data20$lag48,"lag168_20" = data20$lag168,"lag48_21" = data21$lag48,"lag168_21" = data21$lag168,"lag48_22" = data22$lag48,"lag168_22" = data22$lag168,"lag48_23" = data?3$lag48,"lag168_23" = data23$lag168)

#form datasets for hourly consumption
task3Data_0 = cbind(task3Predictors,realConsumption = data0$realConsumption)
task3Data_1 = cbind(task3Predictors,realConsumption = data1$realConsumption)
task3Data_2 = cbind(task3P?edictors,realConsumption = data2$realConsumption)
task3Data_3 = cbind(task3Predictors,realConsumption = data3$realConsumption)
task3Data_4 = cbind(task3Predictors,realConsumption = data4$realConsumption)
task3Data_5 = cbind(task3Predictors,realConsumption ? data5$realConsumption)
task3Data_6 = cbind(task3Predictors,realConsumption = data6$realConsumption)
task3Data_7 = cbind(task3Predictors,realConsumption = data7$realConsumption)
task3Data_8 = cbind(task3Predictors,realConsumption = data8$realConsumption)
t?sk3Data_9 = cbind(task3Predictors,realConsumption = data9$realConsumption)
task3Data_10 = cbind(task3Predictors,realConsumption = data10$realConsumption)
task3Data_11 = cbind(task3Predictors,realConsumption = data11$realConsumption)
task3Data_12 = cbind(ta?k3Predictors,realConsumption = data12$realConsumption)
task3Data_13 = cbind(task3Predictors,realConsumption = data13$realConsumption)
task3Data_14 = cbind(task3Predictors,realConsumption = data14$realConsumption)
task3Data_15 = cbind(task3Predictors,realCo?sumption = data15$realConsumption)
task3Data_16 = cbind(task3Predictors,realConsumption = data16$realConsumption)
task3Data_17 = cbind(task3Predictors,realConsumption = data17$realConsumption)
task3Data_18 = cbind(task3Predictors,realConsumption = data18$r?alConsumption)
task3Data_19 = cbind(task3Predictors,realConsumption = data19$realConsumption)
task3Data_20 = cbind(task3Predictors,realConsumption = data20$realConsumption)
task3Data_21 = cbind(task3Predictors,realConsumption = data21$realConsumption)
task?Data_22 = cbind(task3Predictors,realConsumption = data22$realConsumption)
task3Data_23 = cbind(task3Predictors,realConsumption = data23$realConsumption)

install.packages("glmnet")
library(glmnet)
#predict lambda with 10fold cv for lasso regression
lambda_? = cv.glmnet(data.matrix(task3Data_0)[,1:48], data.matrix(task3Data_0)[,49],nfolds = 10)
lambda_1 = cv.glmnet(data.matrix(task3Data_1)[,1:48], data.matrix(task3Data_1)[,49],nfolds = 10)
lambda_2 = cv.glmnet(data.matrix(task3Data_2)[,1:48], data.matrix(task?Data_2)[,49],nfolds = 10)
lambda_3 = cv.glmnet(data.matrix(task3Data_3)[,1:48], data.matrix(task3Data_3)[,49],nfolds = 10)
lambda_4 = cv.glmnet(data.matrix(task3Data_4)[,1:48], data.matrix(task3Data_4)[,49],nfolds = 10)
lambda_5 = cv.glmnet(data.matrix(tas?3Data_5)[,1:48], data.matrix(task3Data_5)[,49],nfolds = 10)
lambda_6 = cv.glmnet(data.matrix(task3Data_6)[,1:48], data.matrix(task3Data_6)[,49],nfolds = 10)
lambda_7 = cv.glmnet(data.matrix(task3Data_7)[,1:48], data.matrix(task3Data_7)[,49],nfolds = 10)
la?bda_8 = cv.glmnet(data.matrix(task3Data_8)[,1:48], data.matrix(task3Data_8)[,49],nfolds = 10)
lambda_9 = cv.glmnet(data.matrix(task3Data_9)[,1:48], data.matrix(task3Data_9)[,49],nfolds = 10)
lambda_10 = cv.glmnet(data.matrix(task3Data_10)[,1:48], data.matr?x(task3Data_10)[,49],nfolds = 10)
lambda_11 = cv.glmnet(data.matrix(task3Data_11)[,1:48], data.matrix(task3Data_11)[,49],nfolds = 10)
lambda_12 = cv.glmnet(data.matrix(task3Data_12)[,1:48], data.matrix(task3Data_12)[,49],nfolds = 10)
lambda_13 = cv.glmnet(?ata.matrix(task3Data_13)[,1:48], data.matrix(task3Data_13)[,49],nfolds = 10)
lambda_14 = cv.glmnet(data.matrix(task3Data_14)[,1:48], data.matrix(task3Data_14)[,49],nfolds = 10)
lambda_15 = cv.glmnet(data.matrix(task3Data_15)[,1:48], data.matrix(task3Data_1?)[,49],nfolds = 10)
lambda_16 = cv.glmnet(data.matrix(task3Data_16)[,1:48], data.matrix(task3Data_16)[,49],nfolds = 10)
lambda_17 = cv.glmnet(data.matrix(task3Data_17)[,1:48], data.matrix(task3Data_17)[,49],nfolds = 10)
lambda_18 = cv.glmnet(data.matrix(ta?k3Data_18)[,1:48], data.matrix(task3Data_18)[,49],nfolds = 10)
lambda_19 = cv.glmnet(data.matrix(task3Data_19)[,1:48], data.matrix(task3Data_19)[,49],nfolds = 10)
lambda_20 = cv.glmnet(data.matrix(task3Data_20)[,1:48], data.matrix(task3Data_20)[,49],nfolds?= 10)
lambda_21 = cv.glmnet(data.matrix(task3Data_21)[,1:48], data.matrix(task3Data_21)[,49],nfolds = 10)
lambda_22 = cv.glmnet(data.matrix(task3Data_22)[,1:48], data.matrix(task3Data_22)[,49],nfolds = 10)
lambda_23 = cv.glmnet(data.matrix(task3Data_23)[,1?48], data.matrix(task3Data_23)[,49],nfolds = 10)

#use minimum lambda to construct lasso model
lasso_best_0 <- glmnet(data.matrix(task3Data_0)[,1:48], data.matrix(task3Data_0)[,49], alpha = 1, lambda = lambda_0$lambda.min)
lasso_best_1 <- glmnet(data.matri?(task3Data_1)[,1:48], data.matrix(task3Data_1)[,49], alpha = 1, lambda = lambda_1$lambda.min)
lasso_best_2 <- glmnet(data.matrix(task3Data_2)[,1:48], data.matrix(task3Data_2)[,49], alpha = 1, lambda = lambda_2$lambda.min)
lasso_best_3 <- glmnet(data.matrix?task3Data_3)[,1:48], data.matrix(task3Data_3)[,49], alpha = 1, lambda = lambda_3$lambda.min)
lasso_best_4 <- glmnet(data.matrix(task3Data_4)[,1:48], data.matrix(task3Data_4)[,49], alpha = 1, lambda = lambda_4$lambda.min)
lasso_best_5 <- glmnet(data.matrix(?ask3Data_5)[,1:48], data.matrix(task3Data_5)[,49], alpha = 1, lambda = lambda_5$lambda.min)
lasso_best_6 <- glmnet(data.matrix(task3Data_6)[,1:48], data.matrix(task3Data_6)[,49], alpha = 1, lambda = lambda_6$lambda.min)
lasso_best_7 <- glmnet(data.matrix(t?sk3Data_7)[,1:48], data.matrix(task3Data_7)[,49], alpha = 1, lambda = lambda_7$lambda.min)
lasso_best_8 <- glmnet(data.matrix(task3Data_8)[,1:48], data.matrix(task3Data_8)[,49], alpha = 1, lambda = lambda_8$lambda.min)
lasso_best_9 <- glmnet(data.matrix(ta?k3Data_9)[,1:48], data.matrix(task3Data_9)[,49], alpha = 1, lambda = lambda_9$lambda.min)
lasso_best_10 <- glmnet(data.matrix(task3Data_10)[,1:48], data.matrix(task3Data_10)[,49], alpha = 1, lambda = lambda_10$lambda.min)
lasso_best_11 <- glmnet(data.matri?(task3Data_11)[,1:48], data.matrix(task3Data_11)[,49], alpha = 1, lambda = lambda_11$lambda.min)
lasso_best_12 <- glmnet(data.matrix(task3Data_12)[,1:48], data.matrix(task3Data_12)[,49], alpha = 1, lambda = lambda_12$lambda.min)
lasso_best_13 <- glmnet(dat?.matrix(task3Data_13)[,1:48], data.matrix(task3Data_13)[,49], alpha = 1, lambda = lambda_13$lambda.min)
lasso_best_14 <- glmnet(data.matrix(task3Data_14)[,1:48], data.matrix(task3Data_14)[,49], alpha = 1, lambda = lambda_14$lambda.min)
lasso_best_15 <- glm?et(data.matrix(task3Data_15)[,1:48], data.matrix(task3Data_15)[,49], alpha = 1, lambda = lambda_15$lambda.min)
lasso_best_16 <- glmnet(data.matrix(task3Data_16)[,1:48], data.matrix(task3Data_16)[,49], alpha = 1, lambda = lambda_16$lambda.min)
lasso_best_17?<- glmnet(data.matrix(task3Data_17)[,1:48], data.matrix(task3Data_17)[,49], alpha = 1, lambda = lambda_17$lambda.min)
lasso_best_18 <- glmnet(data.matrix(task3Data_18)[,1:48], data.matrix(task3Data_18)[,49], alpha = 1, lambda = lambda_18$lambda.min)
lasso_?est_19 <- glmnet(data.matrix(task3Data_19)[,1:48], data.matrix(task3Data_19)[,49], alpha = 1, lambda = lambda_19$lambda.min)
lasso_best_20 <- glmnet(data.matrix(task3Data_20)[,1:48], data.matrix(task3Data_20)[,49], alpha = 1, lambda = lambda_20$lambda.min)?lasso_best_21 <- glmnet(data.matrix(task3Data_21)[,1:48], data.matrix(task3Data_21)[,49], alpha = 1, lambda = lambda_21$lambda.min)
lasso_best_22 <- glmnet(data.matrix(task3Data_22)[,1:48], data.matrix(task3Data_22)[,49], alpha = 1, lambda = lambda_22$lamb?a.min)
lasso_best_23 <- glmnet(data.matrix(task3Data_23)[,1:48], data.matrix(task3Data_23)[,49], alpha = 1, lambda = lambda_23$lambda.min)


#form datasets for test hourly consumption
task3PredictorsTest = data.frame("lag48_0" = data0t$lag48,"lag168_0" = d?ta0t$lag168,"lag48_1" = data1t$lag48,"lag168_1" = data1t$lag168,"lag48_2" = data2t$lag48,"lag168_2" = data2t$lag168,"lag48_3" = data3t$lag48,"lag168_3" = data3t$lag168,"lag48_4" = data4t$lag48,"lag168_4" = data4t$lag168,"lag48_5" = data5t$lag48,"lag168_5" ? data5t$lag168,"lag48_6" = data6t$lag48,"lag168_6" = data6t$lag168,"lag48_7" = data7t$lag48,"lag168_7" = data7t$lag168,"lag48_8" = data8t$lag48,"lag168_8" = data8t$lag168,"lag48_9" = data9t$lag48,"lag168_9" = data9t$lag168,"lag48_10" = data10t$lag48,"lag16?_10" = data10t$lag168,"lag48_11" = data11t$lag48,"lag168_11" = data11t$lag168,"lag48_12" = data12t$lag48,"lag168_12" = data12t$lag168,"lag48_13" = data13t$lag48,"lag168_13" = data13t$lag168,"lag48_14" = data14t$lag48,"lag168_14" = data14t$lag168,"lag48_15"?= data15t$lag48,"lag168_15" = data15t$lag168,"lag48_16" = data16t$lag48,"lag168_16" = data16t$lag168,"lag48_17" = data17t$lag48,"lag168_17" = data17t$lag168,"lag48_18" = data18t$lag48,"lag168_18" = data18t$lag168,"lag48_19" = data19t$lag48,"lag168_19" = da?a19t$lag168,"lag48_20" = data20t$lag48,"lag168_20" = data20t$lag168,"lag48_21" = data21t$lag48,"lag168_21" = data21t$lag168,"lag48_22" = data22t$lag48,"lag168_22" = data22t$lag168,"lag48_23" = data23t$lag48,"lag168_23" = data23t$lag168)


task3Data_0t = cb?nd(task3PredictorsTest,realConsumption = data0t$realConsumption)
task3Data_1t = cbind(task3PredictorsTest,realConsumption = data1t$realConsumption)
task3Data_2t = cbind(task3PredictorsTest,realConsumption = data2t$realConsumption)
task3Data_3t = cbind(task?PredictorsTest,realConsumption = data3t$realConsumption)
task3Data_4t = cbind(task3PredictorsTest,realConsumption = data4t$realConsumption)
task3Data_5t = cbind(task3PredictorsTest,realConsumption = data5t$realConsumption)
task3Data_6t = cbind(task3Predict?rsTest,realConsumption = data6t$realConsumption)
task3Data_7t = cbind(task3PredictorsTest,realConsumption = data7t$realConsumption)
task3Data_8t = cbind(task3PredictorsTest,realConsumption = data8t$realConsumption)
task3Data_9t = cbind(task3PredictorsTest,?ealConsumption = data9t$realConsumption)
task3Data_10t = cbind(task3PredictorsTest,realConsumption = data10t$realConsumption)
task3Data_11t = cbind(task3PredictorsTest,realConsumption = data11t$realConsumption)
task3Data_12t = cbind(task3PredictorsTest,rea?Consumption = data12t$realConsumption)
task3Data_13t = cbind(task3PredictorsTest,realConsumption = data13t$realConsumption)
task3Data_14t = cbind(task3PredictorsTest,realConsumption = data14t$realConsumption)
task3Data_15t = cbind(task3PredictorsTest,realC?nsumption = data15t$realConsumption)
task3Data_16t = cbind(task3PredictorsTest,realConsumption = data16t$realConsumption)
task3Data_17t = cbind(task3PredictorsTest,realConsumption = data17t$realConsumption)
task3Data_18t = cbind(task3PredictorsTest,realCon?umption = data18t$realConsumption)
task3Data_19t = cbind(task3PredictorsTest,realConsumption = data19t$realConsumption)
task3Data_20t = cbind(task3PredictorsTest,realConsumption = data20t$realConsumption)
task3Data_21t = cbind(task3PredictorsTest,realConsu?ption = data21t$realConsumption)
task3Data_22t = cbind(task3PredictorsTest,realConsumption = data22t$realConsumption)
task3Data_23t = cbind(task3PredictorsTest,realConsumption = data23t$realConsumption)

#predict test data based on lasso models for each ho?r
predLasso_0 <- predict(lasso_best_0,data.matrix(task3Data_0t)[,1:48], s = lambda_0$lambda.min )
predLasso_1 <- predict(lasso_best_1,data.matrix(task3Data_1t)[,1:48], s = lambda_1$lambda.min )
predLasso_2 <- predict(lasso_best_2,data.matrix(task3Data_2t)[?1:48], s = lambda_2$lambda.min )
predLasso_3 <- predict(lasso_best_3,data.matrix(task3Data_3t)[,1:48], s = lambda_3$lambda.min )
predLasso_4 <- predict(lasso_best_4,data.matrix(task3Data_4t)[,1:48], s = lambda_4$lambda.min )
predLasso_5 <- predict(lasso_be?t_5,data.matrix(task3Data_5t)[,1:48], s = lambda_5$lambda.min )
predLasso_6 <- predict(lasso_best_6,data.matrix(task3Data_6t)[,1:48], s = lambda_6$lambda.min )
predLasso_7 <- predict(lasso_best_7,data.matrix(task3Data_7t)[,1:48], s = lambda_7$lambda.min )
?redLasso_8 <- predict(lasso_best_8,data.matrix(task3Data_8t)[,1:48], s = lambda_8$lambda.min )
predLasso_9 <- predict(lasso_best_9,data.matrix(task3Data_9t)[,1:48], s = lambda_9$lambda.min )
predLasso_10 <- predict(lasso_best_10,data.matrix(task3Data_10t)[?1:48], s = lambda_10$lambda.min )
predLasso_11 <- predict(lasso_best_11,data.matrix(task3Data_11t)[,1:48], s = lambda_11$lambda.min )
predLasso_12 <- predict(lasso_best_12,data.matrix(task3Data_12t)[,1:48], s = lambda_12$lambda.min )
predLasso_13 <- predic?(lasso_best_13,data.matrix(task3Data_13t)[,1:48], s = lambda_13$lambda.min )
predLasso_14 <- predict(lasso_best_14,data.matrix(task3Data_14t)[,1:48], s = lambda_14$lambda.min )
predLasso_15 <- predict(lasso_best_15,data.matrix(task3Data_15t)[,1:48], s = la?bda_15$lambda.min )
predLasso_16 <- predict(lasso_best_16,data.matrix(task3Data_16t)[,1:48], s = lambda_16$lambda.min )
predLasso_17 <- predict(lasso_best_17,data.matrix(task3Data_17t)[,1:48], s = lambda_17$lambda.min )
predLasso_18 <- predict(lasso_best_1?,data.matrix(task3Data_18t)[,1:48], s = lambda_18$lambda.min )
predLasso_19 <- predict(lasso_best_19,data.matrix(task3Data_19t)[,1:48], s = lambda_19$lambda.min )
predLasso_20 <- predict(lasso_best_20,data.matrix(task3Data_20t)[,1:48], s = lambda_20$lambda?min )
predLasso_21 <- predict(lasso_best_21,data.matrix(task3Data_21t)[,1:48], s = lambda_21$lambda.min )
predLasso_22 <- predict(lasso_best_22,data.matrix(task3Data_22t)[,1:48], s = lambda_22$lambda.min )
predLasso_23 <- predict(lasso_best_23,data.matrix(?ask3Data_23t)[,1:48], s = lambda_23$lambda.min )


mapeLinHourly_Lasso = c(mape(data0t$realConsumption,predLasso_0),mape(data1t$realConsumption,predLasso_1),mape(data2t$realConsumption,predLasso_2),mape(data3t$realConsumption,predLasso_3),mape(data4t$realC?nsumption,predLasso_4),mape(data5t$realConsumption,predLasso_5),mape(data6t$realConsumption,predLasso_6),mape(data7t$realConsumption,predLasso_7),mape(data8t$realConsumption,predLasso_8),mape(data9t$realConsumption,predLasso_9),mape(data10t$realConsumption?predLasso_10),mape(data11t$realConsumption,predLasso_11),mape(data12t$realConsumption,predLasso_12),mape(data13t$realConsumption,predLasso_13),mape(data14t$realConsumption,predLasso_14),mape(data15t$realConsumption,predLasso_15),mape(data16t$realConsumptio?,predLasso_16),mape(data17t$realConsumption,predLasso_17),mape(data18t$realConsumption,predLasso_18),mape(data19t$realConsumption,predLasso_19),mape(data20t$realConsumption,predLasso_20),mape(data21t$realConsumption,predLasso_21),mape(data22t$realConsumpti?n,predLasso_22),mape(data23t$realConsumption,predLasso_23))

print("%Dev for all the models is small <~30% but no clear comment can be made for the lambda values.")

#Task (d)-Boxplot

boxplot(mapeLag48,mapeLag168,mapeLinearModel,mapeLinHourly,mapeLinHourl?_Lasso,xlab = "Method", ylab = "MAPE" ,names = c("Lag48","Lag168","LR","LRHourly","LASSO"))
print("Based on the boxplots it can be seen that the best (minimum MAPE) modelling method used for this data set is using Lag168 whereas the most complex method LAS?O gives the highest MAPE value. When linear regression models are constructed hourly or using the whole data set the same MAPEs are obtained (or there is an error!). Using Lag48 gives worse results than linear regression but better than LASSO. ")