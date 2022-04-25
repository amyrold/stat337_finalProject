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
#PCA

#PCA of only innately numerical values
obesityPr <- prcomp(obesity[,c(2:4,7,8,11,13,14)], scale = TRUE)
plot(scale(obesity$Weight), scale(obesity$Age))

summary(obesityPr)
plot(obesityPr, type = "l")
biplot(obesityPr, scale = 0)

str(obesityPr)
obesityPr$x
obesity2 <- cbind(obesity, obesityPr$x[,1:2])
head(obesity2)

install.packages("ggplot2")
library(ggplot2)
ggplot(obesity2, aes(PC1,PC2, col = family_history_with_overweight, fill = family_history_with_overweight)) +
  stat_ellipse(geom = 'polygon', col = 'black', alpha = 0.5) +
  geom_point(shape = 21, col = 'black')


