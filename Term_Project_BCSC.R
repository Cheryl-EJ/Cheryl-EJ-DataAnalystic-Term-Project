rm(list = ls())
options(scipen = 200)
readCSV <- function(dir_dta){
  file_list <- list.files(path=dir_dta,full.names=T)
  varSave_func <- function(x){
    table_x <- read.csv(file=x,sep=",",header = T)
  }
  a<-invisible(lapply(file_list,FUN=varSave_func))
  b<-as.data.frame(a[[1]])
  for (i in 2:length(a)){
    c<-rbind(b,a[[i]])
    b <- c
  }
  return(b)
}
dir_dta <- "/Users/zhangyijie/Desktop/bcsc"
bcsc <-readCSV(dir_dta)
require(ggplot2)
summary(bcsc$age_group_5_years)
summary(bcsc$race_eth)
summary(bcsc$age_menarche)
summary(bcsc$age_first_birth)
summary(bcsc$BIRADS_breast_density)
summary(bcsc$first_degree_hx)
summary(bcsc$current_hrt)
summary(bcsc$menopaus)
summary(bcsc$bmi_group)
summary(bcsc$biophx)
summary(bcsc$breast_cancer_history)
ggplot(bcsc, aes(x = age_group_5_years)) +geom_histogram(binwidth = 1, fill = "lightblue", colour = "black")
ggplot(bcsc, aes(x = race_eth)) +geom_histogram(binwidth = 1, fill = "lightblue", colour = "black")
ggplot(bcsc, aes(x = age_menarche)) +geom_histogram(binwidth = 1, fill = "lightblue", colour = "black")
ggplot(bcsc, aes(x = age_first_birth)) +geom_histogram(binwidth = 1, fill = "lightblue", colour = "black")
ggplot(bcsc, aes(x = BIRADS_breast_density)) +geom_histogram(binwidth = 1, fill = "lightblue", colour = "black")
ggplot(bcsc, aes(x = first_degree_hx)) +geom_histogram(binwidth = 1, fill = "lightblue", colour = "black")
ggplot(bcsc, aes(x = current_hrt)) +geom_histogram(binwidth = 1, fill = "lightblue", colour = "black")
ggplot(bcsc, aes(x = menopaus)) +geom_histogram(binwidth = 1, fill = "lightblue", colour = "black")
ggplot(bcsc, aes(x = bmi_group)) +geom_histogram(binwidth = 1, fill = "lightblue", colour = "black")
ggplot(bcsc, aes(x = biophx)) +geom_histogram(binwidth = 1, fill = "lightblue", colour = "black")
ggplot(bcsc, aes(x = breast_cancer_history)) +geom_histogram(binwidth = 1, fill = "lightblue", colour = "black")
boxplot(bcsc$age_group_5_years)
boxplot(bcsc$age_first_birth,bcsc$age_menarche,bcsc$BIRADS_breast_density,bcsc$bmi_group,bcsc$menopaus,bcsc$race_eth,
        bcsc$biophx,bcsc$current_hrt,bcsc$breast_cancer_history,bcsc$first_degree_hx)
aggregate(bcsc$age_group_5_years,list(bcsc$BIRADS_breast_density),mean)
aggregate(bcsc$race_eth,list(bcsc$BIRADS_breast_density),mean)
aggregate(bcsc$first_degree_hx,list(bcsc$BIRADS_breast_density),mean)
aggregate(bcsc$age_menarche,list(bcsc$BIRADS_breast_density),mean)
aggregate(bcsc$age_first_birth,list(bcsc$BIRADS_breast_density),mean)
aggregate(bcsc$current_hrt,list(bcsc$BIRADS_breast_density),mean)
aggregate(bcsc$menopaus,list(bcsc$BIRADS_breast_density),mean)
aggregate(bcsc$bmi_group,list(bcsc$BIRADS_breast_density),mean)
aggregate(bcsc$biophx,list(bcsc$BIRADS_breast_density),mean)
aggregate(bcsc$breast_cancer_history,list(bcsc$BIRADS_breast_density),mean)

library(dplyr)
BIRADS_breast_density=
  bcsc %>% 
  group_by(bcsc$BIRADS_breast_density) %>% 
  count %>% 
  ungroup

BIRADS_breast_density %>% nrow
View(BIRADS_breast_density)


qqnorm(bcsc$age_group_5_years)
qqline(bcsc$age_group_5_years)
qqnorm(bcsc$race_eth)
qqline(bcsc$race_eth)

bcsc1<-bcsc[,c(-1,-13)]

correlation<-cor(bcsc1)
correlation
bcsc_data<-bcsc[,c(-1,-3,-8,-10,-11,-13)]
bcsc2<-bcsc_data[bcsc_data$BIRADS_breast_density!=9,]
bcsc3<-bcsc2[bcsc2$age_menarche!=9,]
bcsc4<-bcsc3[bcsc3$age_first_birth!=9,]
bcsc5<-bcsc4[bcsc4$first_degree_hx!=9,]
bcsc6<-bcsc5[bcsc5$menopaus!=9,]
bcsc7<-bcsc6[bcsc6$breast_cancer_history!=9,]
bcsc_kmeans<-bcsc7
bcsc_norm <- scale(bcsc7)

bcsc_kmeans2<-bcsc4[,c(-2,-6,-7)]
bcsc_norm2 <- scale(bcsc_kmeans2)

wss <- (nrow(bcsc_norm)-1)*sum(apply(bcsc_norm,2,var))
for (i in 2:30)
  wss[i] <- sum(kmeans(bcsc_norm,centers=i)$withinss)
plot(1:30, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

km<-kmeans(x=bcsc_norm,centers=15,iter.max=1000)
km

table(bcsc_kmeans$BIRADS_breast_density,km$cluster)
table(bcsc_kmeans$age_group_5_years,km$cluster)
table(bcsc_kmeans$age_menarche,km$cluster)
table(bcsc_kmeans$age_first_birth,km$cluster)
table(bcsc_kmeans$first_degree_hx,km$cluster)
table(bcsc_kmeans$menopaus,km$cluster)
table(bcsc_kmeans$breast_cancer_history,km$cluster)

wss <- (nrow(bcsc_norm2)-1)*sum(apply(bcsc_norm2,2,var))
for (i in 2:20)
  wss[i] <- sum(kmeans(bcsc_norm2,centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

km2<-kmeans(x=bcsc_norm2,centers=12,iter.max=1000)
km2

table(bcsc_kmeans2$BIRADS_breast_density,km2$cluster)
table(bcsc_kmeans2$age_group_5_years,km2$cluster)
table(bcsc_kmeans2$age_menarche,km2$cluster)
table(bcsc_kmeans2$age_first_birth,km2$cluster)

bcsc_data2<-bcsc2
bcsc_data2[bcsc_data2$age_menarche==9,"age_menarche"]<-4.606
bcsc_data2[bcsc_data2$age_first_birth==9,"age_first_birth"]<-3.558
bcsc_data2[bcsc_data2$first_degree_hx==9,"first_degree_hx"]<-0.7881
bcsc_data2[bcsc_data2$menopaus==9,"menopaus"]<-2.626
bcsc_data2[bcsc_data2$breast_cancer_history==9,"breast_cancer_history"]<-2.088

bcsc_norm3 <- scale(bcsc_data2)

wss <- (nrow(bcsc_norm3)-1)*sum(apply(bcsc_norm3,2,var))
for (i in 2:30)
  wss[i] <- sum(kmeans(bcsc_norm3,centers=i)$withinss)
plot(1:30, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

km3<-kmeans(x=bcsc_norm3,centers=14,iter.max=1000)
km3

table(bcsc_data2$BIRADS_breast_density,km3$cluster)
table(bcsc_data2$age_group_5_years,km3$cluster)
table(bcsc_data2$age_menarche,km3$cluster)
table(bcsc_data2$age_first_birth,km3$cluster)
table(bcsc_data2$first_degree_hx,km3$cluster)
table(bcsc_data2$menopaus,km3$cluster)
table(bcsc_data2$breast_cancer_history,km3$cluster)

bcsc_data3<-bcsc_data2[,c(-2,-6,-7)]
bcsc_norm4 <- scale(bcsc_data3)
wss <- (nrow(bcsc_norm4)-1)*sum(apply(bcsc_norm4,2,var))
for (i in 2:20)
  wss[i] <- sum(kmeans(bcsc_norm4,centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

km4<-kmeans(x=bcsc_norm4,centers=10,iter.max=1000)
km4

table(bcsc_data3$BIRADS_breast_density,km4$cluster)
table(bcsc_data3$age_group_5_years,km4$cluster)
table(bcsc_data3$age_menarche,km4$cluster)
table(bcsc_data3$age_first_birth,km4$cluster)


require(randomForest)
bcsc_rf_data<-bcsc[bcsc$BIRADS_breast_density!=9,]
bcsc_rf<-bcsc_rf_data[c(1:50000, 200001:250000, 400001:450000, 600001:650000, 800001:850000, 1000001:1050000, 1200001:1250000),c(-1,-13)]
bcsc_rf$BIRADS_breast_density <- as.factor(bcsc_rf$BIRADS_breast_density)
fitBCSC <- randomForest(bcsc_rf$BIRADS_breast_density ~ ., data=bcsc_rf)
print(fitBCSC)
importance(fitBCSC)

bcsc_rf2<-bcsc_rf_data[c(1:50000, 200001:250000, 400001:450000, 600001:650000, 800001:850000, 1000001:1050000, 1200001:1250000),c(-1,-8,-13)]
bcsc_rf2$BIRADS_breast_density <- as.factor(bcsc_rf2$BIRADS_breast_density)
fitBCSC2 <- randomForest(bcsc_rf2$BIRADS_breast_density ~ ., data=bcsc_rf2)
print(fitBCSC2)
importance(fitBCSC2)

bcsc_rf3<-bcsc_rf_data[c(1:50000, 200001:250000, 400001:450000, 600001:650000, 800001:850000, 1000001:1050000, 1200001:1250000),c(-1,-12,-13)]
bcsc_rf3$BIRADS_breast_density <- as.factor(bcsc_rf3$BIRADS_breast_density)
fitBCSC3 <- randomForest(bcsc_rf3$BIRADS_breast_density ~ ., data=bcsc_rf3)
print(fitBCSC3)
importance(fitBCSC3)

bcsc_rf4<-bcsc_rf_data[c(1:50000, 200001:250000, 400001:450000, 600001:650000, 800001:850000, 1000001:1050000, 1200001:1250000),c(-1,-11,-13)]
bcsc_rf4$BIRADS_breast_density <- as.factor(bcsc_rf4$BIRADS_breast_density)
fitBCSC4 <- randomForest(bcsc_rf4$BIRADS_breast_density ~ ., data=bcsc_rf4)
print(fitBCSC4)
importance(fitBCSC4)

bcsc_rf5<-bcsc_rf_data[c(1:50000, 200001:250000, 400001:450000, 600001:650000, 800001:850000, 1000001:1050000, 1200001:1250000),c(-1,-9,-13)]
bcsc_rf5$BIRADS_breast_density <- as.factor(bcsc_rf5$BIRADS_breast_density)
fitBCSC5 <- randomForest(bcsc_rf5$BIRADS_breast_density ~ ., data=bcsc_rf5)
print(fitBCSC5)
importance(fitBCSC5)

bcsc_rf6<-bcsc_rf_data[c(1:50000, 200001:250000, 400001:450000, 600001:650000, 800001:850000, 1000001:1050000, 1200001:1250000),c(-1,-4,-13)]
bcsc_rf6$BIRADS_breast_density <- as.factor(bcsc_rf6$BIRADS_breast_density)
fitBCSC6 <- randomForest(bcsc_rf6$BIRADS_breast_density ~ ., data=bcsc_rf6)
print(fitBCSC6)
importance(fitBCSC6)

bcsc_rf7<-bcsc_rf_data[c(1:50000, 200001:250000, 400001:450000, 600001:650000, 800001:850000, 1000001:1050000, 1200001:1250000),c(-1,-5,-13)]
bcsc_rf7$BIRADS_breast_density <- as.factor(bcsc_rf7$BIRADS_breast_density)
fitBCSC7 <- randomForest(bcsc_rf7$BIRADS_breast_density ~ ., data=bcsc_rf7)
print(fitBCSC7)
importance(fitBCSC7)

bcsc_rf8<-bcsc_rf_data[c(1:50000, 200001:250000, 400001:450000, 600001:650000, 800001:850000, 1000001:1050000, 1200001:1250000),c(-1,-3,-13)]
bcsc_rf8$BIRADS_breast_density <- as.factor(bcsc_rf8$BIRADS_breast_density)
fitBCSC8 <- randomForest(bcsc_rf8$BIRADS_breast_density ~ ., data=bcsc_rf8)
print(fitBCSC8)
importance(fitBCSC8)

bcsc_rf9<-bcsc_rf_data[c(1:50000, 200001:250000, 400001:450000, 600001:650000, 800001:850000, 1000001:1050000, 1200001:1250000),c(-1,-6,-13)]
bcsc_rf9$BIRADS_breast_density <- as.factor(bcsc_rf9$BIRADS_breast_density)
fitBCSC9 <- randomForest(bcsc_rf9$BIRADS_breast_density ~ ., data=bcsc_rf9)
print(fitBCSC9)
importance(fitBCSC9)

bcsc_rf10<-bcsc_rf_data[c(1:50000, 200001:250000, 400001:450000, 600001:650000, 800001:850000, 1000001:1050000, 1200001:1250000),c(-1,-2,-13)]
bcsc_rf10$BIRADS_breast_density <- as.factor(bcsc_rf10$BIRADS_breast_density)
fitBCSC10 <- randomForest(bcsc_rf10$BIRADS_breast_density ~ ., data=bcsc_rf10)
print(fitBCSC10)
importance(fitBCSC10)

bcsc_rf11<-bcsc_rf_data[c(1:50000, 200001:250000, 400001:450000, 600001:650000, 800001:850000, 1000001:1050000, 1200001:1250000),c(-1,-10,-13)]
bcsc_rf11$BIRADS_breast_density <- as.factor(bcsc_rf11$BIRADS_breast_density)
fitBCSC11 <- randomForest(bcsc_rf11$BIRADS_breast_density ~ ., data=bcsc_rf11)
print(fitBCSC11)
importance(fitBCSC11)



bcsc_data1<-bcsc[bcsc$BIRADS_breast_density!=9,c(-1,-13)]
bcsc_data2<-bcsc_data1[bcsc_data1$age_group_5_years!=1,]
bcsc_data3<-bcsc_data2[bcsc_data2$age_group_5_years!=2,]
bcsc_data4<-bcsc_data3[bcsc_data3$age_group_5_years!=11,]
bcsc_data5<-bcsc_data4[bcsc_data4$age_group_5_years!=12,]
bcsc_data6<-bcsc_data5[bcsc_data5$age_group_5_years!=13,]
bcsc_data7<-bcsc_data6[bcsc_data6$breast_cancer_history==0,]
bcsc_data8<-bcsc_data7[,c(-4,-7,-11)]
bcsc_data9<-bcsc_data8[bcsc_data8$race_eth!=9,]
bcsc_data10<-bcsc_data9[bcsc_data9$first_degree_hx!=9,]
bcsc_data11<-bcsc_data10[bcsc_data10$biophx!=9,]

bcsc_data11$BIRADS_breast_density[which(bcsc_data11$BIRADS_breast_density==1)] = 'Low'   
bcsc_data11$BIRADS_breast_density[which(bcsc_data11$BIRADS_breast_density==2)] = 'Low'  
bcsc_data11$BIRADS_breast_density[which(bcsc_data11$BIRADS_breast_density==3)] = 'High'   
bcsc_data11$BIRADS_breast_density[which(bcsc_data11$BIRADS_breast_density==4)] = 'High'
bcsc_rf_data<-bcsc_data11

sub<-sample(1:nrow(bcsc_rf_data),round(nrow(bcsc_rf_data)*2/3)) 
length(sub) 
rf_train<-bcsc_rf_data[sub,]
rf_test<-bcsc_rf_data[-sub,]

rf_train$BIRADS_breast_density <- as.factor(rf_train$BIRADS_breast_density)
rf_test$BIRADS_breast_density <- as.factor(rf_test$BIRADS_breast_density)
fitBCSC <- randomForest(rf_train$BIRADS_breast_density ~ ., data=rf_train)
print(fitBCSC)
importance(fitBCSC)

rf_pred <- predict(fitBCSC,rf_test)
rf_perf <- table(rf_test$BIRADS_breast_density,rf_pred,
                     dnn = c('Actual','Predicted'))
rf_perf

n<-length(names(bcsc_rf_data))
set.seed(9)
min=100
num=0
for (i in 1:(n-1)){
  mtry_fit<- randomForest(bcsc_rf_data$BIRADS_breast_density~., data=bcsc_rf_data, mtry=i)
  err<-mean(mtry_fit$err.rate)
  print(err)
  if(err<min) {    
    min =err     
    num=i }
}
print(min)
print(num)

set.seed(100)
ntree_fit<-randomForest(rf_train$BIRADS_breast_density~.,data=rf_train,mtry=3,ntree=1000)
plot(ntree_fit)

fitBCSC <- randomForest(rf_train$BIRADS_breast_density ~ ., data=rf_train, mtry = 3,ntree = 300, importance = TRUE)
print(fitBCSC)
importance(fitBCSC)

rf_pred <- predict(fitBCSC,rf_test)
rf_perf <- table(rf_test$BIRADS_breast_density,rf_pred,
                 dnn = c('Actual','Predicted'))
rf_perf

hist(treesize(fitBCSC))
MDSplot(fitBCSC,bcsc$BIRADS_breast_density,palette=rep(1,2),pch=as.numeric(bcsc$age_group_5_years))
varImpPlot(fitBCSC)

bcsc_kmeans_data<-bcsc_data11[,c(-4, -6, -8)]
bcsc_kmeans_data[bcsc_kmeans_data$age_first_birth==9,"age_first_birth"]<-3.558
bcsc_kmeans_data[bcsc_kmeans_data$menopaus==9,"menopaus"]<-2.626
bcsc_kmeans_data[bcsc_kmeans_data$bmi_group==9,"bmi_group"]<-4.257
bcsc_norm <- scale(bcsc_kmeans_data)

library(psych)  
library(Hmisc)
principal_components <- princomp(bcsc_norm, cor = TRUE, score = TRUE)
summary(principal_components)
fit <- principal(bcsc_norm, nfactors=5, rotate="varimax",score = TRUE)
fit 

wss <- (nrow(bcsc_norm)-1)*sum(apply(bcsc_norm,2,var))
for (i in 2:30)
  wss[i] <- sum(kmeans(bcsc_norm,centers=i)$withinss)
plot(1:30, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

km<-kmeans(x=bcsc_norm,centers=18,iter.max=1000)
km

table(bcsc_kmeans_data$BIRADS_breast_density,km$cluster)
table(bcsc_kmeans_data$age_group_5_years,km$cluster)
table(bcsc_kmeans_data$race_eth,km$cluster)
table(bcsc_kmeans_data$first_degree_hx,km$cluster)
table(bcsc_kmeans_data$bmi_group,km$cluster)


