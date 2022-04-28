obesity <- read.csv('ObesityDataSet_raw_and_data_sinthetic.csv')
#obesity <- read.csv('a.data.raw/ObesityDataSet_raw_and_data_sinthetic.csv')

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

#Remove Smoke
lm.model5 <- lm(NObeyesdad~Age+Height+Weight+family_history_with_overweight+FAVC+FCVC+NCP+CAEC+CH2O+SCC+FAF+MTRANS,data=obesity.num)
summary(lm.model5) 
#Most significant predictors are Weight, CAEC, Age, family_history_with_overweight, NCP, FAVC, Height, FCVC

#################################################################################################################
#PCA

#PCA done according to our class code
obesity.data <- obesity[,c(2:4,7,8,11,13,14)]


pr.out <- prcomp(obesity.data, scale = TRUE)
phi <- pr.out$rotation
dim(phi)
Z <- pr.out$x
dim(Z)
summary(pr.out)
plot(Z[,1:2], pch = 19, col = as.numeric(as.factor(obesity$family_history_with_overweight)), xlab = "PC1", ylab = "PC2")
plot(pr.out)
#First two PCs only account for 41% of the variability, so acknowledges the complexity of the issue



#PCA of only innately numerical values done using external methods/packages
obesityPr <- prcomp(obesity[,c(2:4,7,8,11,13,14)], scale = TRUE)
#plot(scale(obesity$Weight), scale(obesity$Age))

summary(obesityPr)
plot(obesityPr, type = "l")
biplot(obesityPr, scale = 0)

str(obesityPr)
obesityPr$x
obesity2 <- cbind(obesity, obesityPr$x[,1:2])
head(obesity2)

install.packages("ggplot2")
library(ggplot2)
ggplot(obesity2, aes(PC1,PC2, col = FAVC, fill = FAVC)) +
  stat_ellipse(geom = 'polygon', col = 'black', alpha = 0.5) +
  geom_point(shape = 21, col = 'black')


###############################################################################
# k-means clustering
obesity <- read.csv('a.data.raw/ObesityDataSet_raw_and_data_sinthetic.csv')
#Remove missing values
obesity <- obesity[complete.cases(obesity), ]
#Convert categorical to numerical values and convert to dataframe
obesity.num <- data.matrix(obesity)
obesity.num <- data.frame(obesity.num)

#select desired traits
obesity.select <- obesity.num[,-c(1,10,14,15)]

obesity.scaled <- scale(obesity.num)
fviz_nbclust(obesity.scaled, kmeans, method = 'wss') #elbow at 9 clusters

#perform kmeans clustering
kmeans.os <- kmeans(obesity.scaled, centers = 3, nstart = 20)
#plot results of final k-means model
fviz_cluster(kmeans.os, data = obesity.select)
#aggregate data based on k-means clustering
aggregate(obesity.select, by=list(cluster=kmeans.os$cluster), mean)

centroids <- data.frame(kmeans.os$centers)

matplot(t(centroids), type = 'l', xaxt = 'n')
legend("top", legend = seq_len(nrow(centroids)),
       col= seq_len(nrow(centroids)),cex=0.8,
       fill=seq_len(nrow(centroids)))
axis(1, at = 1:17, colnames(centroids))

################################################################################
#hierarhical clustering
#use same scaled dataframe as before
#define linkage methods
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

#function to compute agglomerative coefficient
ac <- function(x) {
  agnes(obesity.scaled, method = x)$ac
}

#calculate agglomerative coefficient for each clustering linkage method
print(sapply(m, ac)) #ward produces highest agglomerative coefficient
#perform the clustering
ob.clust <- agnes(obesity.scaled, method='ward')

#produce the dendrogram
pltree(ob.clust, cex=0.6, hang=-1, main='Dendrogram')

#calculate gap statistic for each number of clusters (up to 10 clusters)
gap_stat <- clusGap(obesity.scaled, FUN = hcut, nstart = 25, K.max = 10, B = 50)

#produce plot of clusters vs. gap statistic
fviz_gap_stat(gap_stat)

#compute distance matrix
d <- dist(obesity.scaled, method = "euclidean")

#perform hierarchical clustering using Ward's method
final_clust <- hclust(d, method = "ward.D2" )

#cut the dendrogram into 4 clusters
groups <- cutree(final_clust, k=4)

#find number of observations in each cluster
table(groups)

#append cluster labels to original data
final_data <- cbind(obesity.select, cluster = groups)

#display first six rows of final data
head(final_data)

#find mean values for each cluster
aggregate(final_data, by=list(cluster=final_data$cluster), mean)







##########################################################################################
#Ideas from others
#within cluster variation for number of clusters
#K-means of number of within cluster variation

#hierarchical clustering?


