rm(list=ls())
library(here)
library(caret)
library(stats)
library(plyr)
library(data.table)
library(tidyverse)
library(randomForest)

set.seed(0927)

#dataset with variables of interest
eoi = read.csv("/Users/zhanna.terechshenko/Fall 2017/SODA 502/Project/gtds_2001.to.may.2014.csv")
head(eoi)

# phoenix data #
phoenix_int = read.csv("/Users/zhanna.terechshenko/pho_international.csv")
head(phoenix_int)


# lag variables by 3 and 6 months
pho_int_lag = phoenix_int %>% 
  group_by(ccode) %>% 
  arrange(ccode, year) %>% 
  do(data.frame(., setNames(shift(.$vcp, c(3,6)), paste("vcp_l", c(3,6), sep=".")))) %>%
  do(data.frame(., setNames(shift(.$mcp, c(3,6)), paste("mcp_l", c(3,6), sep=".")))) %>%
  do(data.frame(., setNames(shift(.$vcf, c(3,6)), paste("vcf_l", c(3,6), sep=".")))) %>%
  do(data.frame(., setNames(shift(.$mcf, c(3,6)), paste("mcf_l", c(3,6), sep=".")))) %>%
  select(ccode, year, month, vcp_l.3, mcp_l.3, vcf_l.3, mcf_l.3,
         vcp_l.6, mcp_l.6, vcf_l.6, mcf_l.6)

pho_int_lag = na.omit(pho_int_lag)
  
# create onset variables 
eoi_int = eoi %>%
  select(ccode, year, month, ic)

eoi_d = eoi_int[eoi_int$ic==1,]
eoi_d$onset <- with(eoi_d, ave(year, month, ccode, FUN = function(x)
  as.integer(c(TRUE, tail(x, -1L) != head(x, -1L) + 1L))))
eoi_d = eoi_d[eoi_d$onset==1,]

eoi_final = merge(eoi_int, eoi_d, all.x=T)
eoi_final$onset[is.na(eoi_final$onset)==T] <-0
eoi_final$ic = NULL

# merge 2 datasets
df = merge(pho_int_lag, eoi_final, by=c("ccode", "year", "month"))
df$onset = as.factor(df$onset)


# split the data
train_data = df[which(df$year<=2005),]
train_data_l3 = subset(train_data, select=c("ccode", "year", "month",
                                            "vcp_l.3", "mcp_l.3", "vcf_l.3", "mcf_l.3", 'onset'))
train_data_l6 = subset(train_data, select=c("ccode", "year", "month",
                                            "vcp_l.6", "mcp_l.6", "vcf_l.6", "mcf_l.6", 'onset'))


test_data = df[which(df$year==2006),]
test_data_l3 = subset(test_data, select=c("ccode", "year", "month",
                                            "vcp_l.3", "mcp_l.3", "vcf_l.3", "mcf_l.3", 'onset'))
test_data_l6 = subset(test_data, select=c("ccode", "year", "month",
                                            "vcp_l.6", "mcp_l.6", "vcf_l.6", "mcf_l.6", 'onset'))




# train the model
rf_model<-train(onset~.,data=train_data_l3,method="rf",
                trControl=trainControl(method="cv",number=5, savePredictions = T),
                prox=TRUE,allowParallel=TRUE)
print(rf_model)

#Random Forest 

#5273 samples
#7 predictor
#2 classes: '0', '1' 

#No pre-processing
#Resampling: Cross-Validated (5 fold) 
#Summary of sample sizes: 4219, 4218, 4219, 4217, 4219  
#Resampling results across tuning parameters:
  
#  mtry  Accuracy   Kappa    
#2     0.9622603  0.3017298
#4     0.9738308  0.6224726
#7     0.9802764  0.7462194

#Accuracy was used to select the optimal model using  the largest value.
#The final value used for the model was mtry = 7.

print(rf_model$finalModel)


testclass <- as.vector(predict(rf_model, newdata = test_data_l3))
cfMatrix <- confusionMatrix(data = testclass, test_data_l3$onset)
print(cfMatrix)
plot(rf_model)



test.probs <- predict(rf_model,test_data_l3,type="prob")

library(pROC)	

pROC::roc(test_data_l3$onset, testclass)

rf.ROC <- roc(predictor=testclass,
               response=test_data_l3$onset,
               levels=rev(levels(test_data_l3$onset)))
rf.ROC$auc
# Area under the curve: 1

plot(rf.ROC,main="RF ROC Phoenix")



# icews data #
icw_int = read.csv("/Users/zhanna.terechshenko/icw_international.csv")
head(icw_int)

# lag variables by 3 and 6 months
icw_int_lag = icw_int %>% 
  group_by(ccode) %>% 
  arrange(ccode, year) %>% 
  do(data.frame(., setNames(shift(.$vcp, c(3,6)), paste("vcp_l", c(3,6), sep=".")))) %>%
  do(data.frame(., setNames(shift(.$mcp, c(3,6)), paste("mcp_l", c(3,6), sep=".")))) %>%
  do(data.frame(., setNames(shift(.$vcf, c(3,6)), paste("vcf_l", c(3,6), sep=".")))) %>%
  do(data.frame(., setNames(shift(.$mcf, c(3,6)), paste("mcf_l", c(3,6), sep=".")))) %>%
  select(ccode, year, month, vcp_l.3, mcp_l.3, vcf_l.3, mcf_l.3,
         vcp_l.6, mcp_l.6, vcf_l.6, mcf_l.6)

icw_int_lag = na.omit(icw_int_lag)

# merge 2 datasets
df2 = merge(icw_int_lag, eoi_final, by=c("ccode", "year", "month"))
df2$onset = as.factor(df2$onset)

# split the data
train_data = df2[which(df2$year<=2005),]
train_data_l3 = subset(train_data, select=c("ccode", "year", "month",
                                            "vcp_l.3", "mcp_l.3", "vcf_l.3", "mcf_l.3", 'onset'))
train_data_l6 = subset(train_data, select=c("ccode", "year", "month",
                                            "vcp_l.6", "mcp_l.6", "vcf_l.6", "mcf_l.6", 'onset'))


test_data = df[which(df2$year==2006),]
test_data_l3 = subset(test_data, select=c("ccode", "year", "month",
                                          "vcp_l.3", "mcp_l.3", "vcf_l.3", "mcf_l.3", 'onset'))
test_data_l6 = subset(test_data, select=c("ccode", "year", "month",
                                          "vcp_l.6", "mcp_l.6", "vcf_l.6", "mcf_l.6", 'onset'))




# train the model
rf_model2<-train(onset~.,data=train_data_l3,method="rf",
                trControl=trainControl(method="cv",number=5, savePredictions = T),
                prox=TRUE,allowParallel=TRUE)
print(rf_model2)

print(rf_model2$finalModel)


testclass <- as.vector(predict(rf_model2, newdata = test_data_l3))
cfMatrix <- confusionMatrix(data = testclass, test_data_l3$onset)
print(cfMatrix)
plot(rf_model)



test.probs <- predict(rf_model2,test_data_l3,type="prob")

library(pROC)	

pROC::roc(test_data_l3$onset, testclass)

rf.ROC <- roc(predictor=testclass,
              response=test_data_l3$onset,
              levels=rev(levels(test_data_l3$onset)))
rf.ROC$auc
# Area under the curve: 1

plot(rf.ROC,main="RF ROC Phoenix")



