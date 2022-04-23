obesity <- read.csv('ObesityDataSet_raw_and_data_sinthetic.csv')

#################################################################################################################
#Backwards Selection (5%)

#Remove missing values
obesity <- obesity[complete.cases(obesity), ]
#Convert categorical to numerical values and convert to dataframe
obesity.num <- data.matrix(obesity)
obesity.num <- data.frame(obesity.num)

#Linear regression with all predictors
lm.model1 <- lm(NObeyesdad~.,data=obesity.num)
summary(lm.model1)

#Remove TUE (time using technology devices)
lm.model2 <- lm(NObeyesdad~Gender+Age+Height+Weight+family_history_with_overweight+FAVC+FCVC+NCP+CAEC+SMOKE+CH2O+SCC+FAF+CALC+MTRANS,data=obesity.num)
summary(lm.model2)

#Remove CALC (Consumption of alcohol)
lm.model3 <- lm(NObeyesdad~Gender+Age+Height+Weight+family_history_with_overweight+FAVC+FCVC+NCP+CAEC+SMOKE+CH2O+SCC+FAF+MTRANS,data=obesity.num)
summary(lm.model3)

#Remove Gender
lm.model4 <- lm(NObeyesdad~Age+Height+Weight+family_history_with_overweight+FAVC+FCVC+NCP+CAEC+SMOKE+CH2O+SCC+FAF+MTRANS,data=obesity.num)
summary(lm.model4) #Most significant predictors are Weight, CAEC, Age, family_history_with_overweight, NCP, FAVC, Height, FCVC
#################################################################################################################
