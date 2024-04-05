rm(list=ls())
data=read.csv("C:\\Users\\PRIYA\\Desktop\\breast_cancer (1).csv")
attach(data)
head(data)
nrow(data)

install.packages("randomForest")
install.packages("caret")
install.packages("pROC")
install.packages("visdat")
install.packages("ggplot2")

library(randomForest)
library(caret)
library(pROC)
library(visdat)
library(ggplot2)

#checking unique values in class
unique(data$Class)

# replacing the values with 0 and 1 for the purpose of building logistic regression model
data$Class[data$Class == "4"] <- 1
data$Class[data$Class == "2"] <- 0

# checking if any columns have missing values

colSums(is.na(data))

#using vis_miss function to visually identify missing values
vis_miss(data)

#data_visualisation
#bivariate analysis

ggplot(data = data, aes(x = factor(Clump.Thickness), fill = factor(Class))) +
  geom_bar() +
  labs(x = "Clump Thickness", y = "Count", fill = "Class") +
  ggtitle("Bivariate Analysis: Clump Thickness vs. Class")

t1=table(Clump.Thickness,Class)

ggplot(data = data, aes(x = factor(Uniformity.of.Cell.Shape), fill = factor(Class))) +
  geom_bar() +
  labs(x = " Uniformity of Cell Shape ", y = "Count", fill = "Class") +
  ggtitle("Bivariate Analysis:  Uniformity of Cell Shape   vs. Class")

t1=table(Uniformity.of.Cell.Shape,Class)

ggplot(data = data, aes(x = factor(Uniformity.of.Cell.Size), fill = factor(Class))) +
  geom_bar() +
  labs(x = " Uniformity of Cell Size ", y = "Count", fill = "Class") +
  ggtitle("Bivariate Analysis:  Uniformity of Cell Shape   vs. Class")

t1=table(Uniformity.of.Cell.Size,Class)

ggplot(data = data, aes(x = factor(Bare.Nuclei), fill = factor(Class))) +
  geom_bar() +
  labs(x = "Bare.Nuclei", y = "Count", fill = "Class") +
  ggtitle("Bivariate Analysis: Bare Nuclei  vs. Class")

t1=table(Bare.Nuclei,Class)

ggplot(data = data, aes(x = factor(Bland.Chromatin), fill = factor(Class))) +
  geom_bar() +
  labs(x = "Bland Chromatin", y = "Count", fill = "Class") +
  ggtitle("Bivariate Analysis: Bland Chromatin  vs. Class")

t1=table(Bland.Chromatin,Class)


ggplot(data = data, aes(x = factor(Marginal.Adhesion), fill = factor(Class))) +
  geom_bar() +
  labs(x = " Marginal Adhesion ", y = "Count", fill = "Class") +
  ggtitle("Bivariate Analysis:  Marginal Adhesion   vs. Class")

t1=table(Marginal.Adhesion,Class)

ggplot(data = data, aes(x = factor(Mitoses), fill = factor(Class))) +
  geom_bar() +
  labs(x = "Mitoses", y = "Count", fill = "Class") +
  ggtitle("Bivariate Analysis:  Mitoses   vs. Class")

t1=table(Mitoses,Class)

ggplot(data = data, aes(x = factor(Normal.Nucleoli), fill = factor(Class))) +
  geom_bar() +
  labs(x = "Normal Nucleoli", y = "Count", fill = "Class") +
  ggtitle("Bivariate Analysis:  Normal Nucleoli   vs. Class")

t1=table(Normal.Nucleoli,Class)


ggplot(data = data, aes(x = factor(Single.Epithelial.Cell.Size), fill = factor(Class))) +
  geom_bar() +
  labs(x = "Single Epithelial Cell Size", y = "Count", fill = "Class") +
  ggtitle("Bivariate Analysis: Single Epithelial Cell Size   vs. Class")

t1=table(Single.Epithelial.Cell.Size,Class)

#splitting into training and testing datasets
set.seed(123)
train_index = sample(1:nrow(data), 0.8 * nrow(data)) #for splitting data into 80:20 train:test subsets
train_data = data[train_index, ] 
test_data = data[-train_index, ]

rf=randomForest(Class~.,data=train_data,ntree=100)
rf_preds=predict(rf,newdata=test_data,type="response")
rf_predicted=round(rf_preds,digits=0)

importance <- importance(rf)

log_model <- glm(Class ~ .,data=train_data, family = binomial(link="logit"))
summary(log_model)
log_preds=predict(log_model,newdata=test_data,type="response")
log_predicted=ifelse(log_preds>0.5,1,0)


rf_preds=c(rf_preds)
log_roc_auc <- roc(test_data$Class, log_predicted)
rf_roc_auc <- roc(test_data$Class, rf_predicted)


#confusion matrix

confusion_matrix_rf=table(test_data$Class, rf_predicted)
confusion_matrix_log=table(test_data$Class, log_predicted)

#accuracy

acc_rf=((confusion_matrix_rf[1,1] + confusion_matrix_rf[2,2]) / (confusion_matrix_rf[1,1] + confusion_matrix_rf[2,2] + confusion_matrix_rf[2,1] + confusion_matrix_rf[1,2]))*100
acc_log=((confusion_matrix_log[1,1] + confusion_matrix_log[2,2]) / (confusion_matrix_log[1,1] + confusion_matrix_log[2,2] + confusion_matrix_log[2,1] + confusion_matrix_log[1,2]))*100

#precision

precision_rf <- (confusion_matrix_rf[1,1] / (confusion_matrix_rf[1,1] + confusion_matrix_rf[1,2]))*100
precision_log <- (confusion_matrix_log[1,1] / (confusion_matrix_log[1,1] + confusion_matrix_log[1,2]))*100

#recall

recall_rf <- (confusion_matrix_rf[1,1] / (confusion_matrix_rf[1,1] + confusion_matrix_rf[2,1]))*100
recall_log <- (confusion_matrix_log[1,1] / (confusion_matrix_log[1,1] + confusion_matrix_log[2,1]))*100

#F1

F1_rf <- (2*precision_rf * recall_rf) / (precision_rf + recall_rf)
F1_log <- (2*precision_log * recall_log) / (precision_log + recall_log)

#roc curve 

plot(log_roc_auc, col = "blue", main = "ROC Curves")
lines(rf_roc_auc, col = "red")
legend("bottomright", legend = c("Logistic Regression", "Random Forest"),
       col = c("blue", "red"), lty = 1)

