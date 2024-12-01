library(aplore3)
library(ROCR)
library(e1071)
library(MASS)
library(pROC)
library(caret)

data(polypharm)

df <- polypharm[, c(2, 11, 12, 14)]
df$polypharmacy <- ifelse(df$polypharmacy == 'No', 0, 1)
df$gender <- ifelse(df$gender == 'Male', 1, 0)
df$race <- ifelse(df$race == 'White', 0, 1)
df$mhv1 <- ifelse(polypharm$mhv4 == '1-5', 1, 0)
df$mhv2 <- ifelse(polypharm$mhv4 == '6-14', 1, 0)
df$mhv3 <- ifelse(polypharm$mhv4 == '> 14', 1, 0)
df$inptmhv <- ifelse(polypharm$inptmhv3 == 0, 0, 1)

# logistic regression
logit_model <- glm(polypharmacy ~ gender + race + age + mhv1 + mhv2 + mhv3 + inptmhv, 
             data = df, family = binomial)
summary(logit_model)
test_logit.prob <- predict(logit_model, df[-1], type = "response")
test_logit.predict <- prediction(test_logit.prob, df[1])
test_logit.performance <- performance(test_logit.predict, measure = "tpr", x.measure = "fpr")
plot(test_logit.performance, col = "red")



y_true <- df$polypharmacy
test_logit.prob <- predict(logit_model, df, type = "response")
threshold <- 0.5
y_pred <- ifelse(test_logit.prob >= threshold, 1, 0)

conf_matrix <- confusionMatrix(as.factor(y_pred), as.factor(y_true))
accuracy <- conf_matrix$overall['Accuracy']
precision <- conf_matrix$byClass['Pos Pred Value']
recall <- conf_matrix$byClass['Sensitivity']

conf_matrix
accuracy
precision
recall

print(paste("Accuracy:", round(accuracy, 4)))
print(paste("Precision:", round(precision, 4)))
print(paste("Recall:", round(recall, 4)))




beta1 <- read.csv('beta_data1.csv')
beta2 <- read.csv('beta_data2.csv')
beta3 <- read.csv('beta_data3.csv')
beta4 <- read.csv('beta_data4.csv')

#beta <- beta[2501:5000,]
#beta <- read.csv('wsy.csv')
#beta <- beta[-1]


ui <- read.csv('u_i.csv')
#ui <- ui[2501:5000,-501]
ui <- as.data.frame(t(ui))
ui <- ui[rep(1:nrow(ui), each = 7), ]
ui <- cbind(ui, ui, ui, ui, ui, ui, ui, ui, ui, ui)
#ui$mean <- rowMeans(ui)

df <- polypharm[, c(2, 11, 12, 14)]
df$polypharmacy <- ifelse(df$polypharmacy == 'No', 0, 1)
df$gender <- ifelse(df$gender == 'Male', 1, 0)
df$race <- ifelse(df$race == 'White', 0, 1)
df$mhv1 <- ifelse(polypharm$mhv4 == '1-5', 1, 0)
df$mhv2 <- ifelse(polypharm$mhv4 == '6-14', 1, 0)
df$mhv3 <- ifelse(polypharm$mhv4 == '> 14', 1, 0)
df$inptmhv <- ifelse(polypharm$inptmhv3 == 0, 0, 1)

#df <- cbind(ui, df)


####### chain 1 #######
precisions1 <- numeric(length(beta1$V1))
recalls1 <- numeric(length(beta1$V1))
accuracies1 <- numeric(length(beta1$V1))

for (i in 1:length(beta1$V1)) {
  df$y_hat <- beta1[i,1] + beta1[i,2] * df$gender + beta1[i,3] * df$race + beta1[i,4] * df$age + 
    beta1[i,5] * df$mhv1 + beta1[i,6] * df$mhv2 + beta1[i,7] * df$mhv3 + beta1[i,8] * df$inptmhv + ui[i]
  df$mu_hat <- ifelse(df$y_hat < 0, 0, 1)
  accuracy <- mean(df$polypharm == df$mu_hat)
  accuracies1[i] <- accuracy
  tp <- sum(df$mu_hat == 1 & df$polypharm == 1)
  fp <- sum(df$mu_hat == 1 & df$polypharm == 0)
  fn <- sum(df$mu_hat == 0 & df$polypharm == 1)
  precisions1[i] <- tp / (tp + fp)
  recalls1[i] <- tp / (tp + fn)
}

mean_precision1 <- mean(precisions1, na.rm = TRUE)
mean_recall1 <- mean(recalls1, na.rm = TRUE)
mean_accuracy1 <- mean(accuracies1)

mean_accuracy1
mean_precision1
mean_recall1

print(paste("Mean Precision:", mean_precision1))
print(paste("Mean Recall:", mean_recall1))
print(paste("Mean Accuracy:", mean_accuracy1))




actual <- df$polypharm
predicted_prob <- df$mu_hat

# ROC
roc_obj_binary <- roc(df$polypharm, df$mu_hat)
plot(roc_obj_binary, main = "ROC Curve using Binary Predictions", col = "red", lwd = 2)

# AUC
auc_value_binary <- auc(roc_obj_binary)
print(paste("AUC (Binary):", auc_value_binary))












####### chain 2 #######
precisions2 <- numeric(length(beta2$V1))
recalls2 <- numeric(length(beta2$V1))
accuracies2 <- numeric(length(beta2$V1))

for (i in 1:length(beta2$V1)) {
  df$y_hat <- beta2[i,1] + beta2[i,2] * df$gender + beta2[i,3] * df$race + beta2[i,4] * df$age + 
    beta2[i,5] * df$mhv1 + beta2[i,6] * df$mhv2 + beta2[i,7] * df$mhv3 + beta2[i,8] * df$inptmhv + ui[i]
  df$mu_hat <- ifelse(df$y_hat < 0, 0, 1)
  accuracy <- mean(df$polypharm == df$mu_hat)
  accuracies2[i] <- accuracy
  tp <- sum(df$mu_hat == 1 & df$polypharm == 1)
  fp <- sum(df$mu_hat == 1 & df$polypharm == 0)
  fn <- sum(df$mu_hat == 0 & df$polypharm == 1)
  precisions2[i] <- tp / (tp + fp)
  recalls2[i] <- tp / (tp + fn)
}

mean_precision2 <- mean(precisions2, na.rm = TRUE)
mean_recall2 <- mean(recalls2, na.rm = TRUE)
mean_accuracy2 <- mean(accuracies2)

mean_precision2
mean_recall2
mean_accuracy2

print(paste("Mean Precision:", mean_precision2))
print(paste("Mean Recall:", mean_recall2))
print(paste("Mean Accuracy:", mean_accuracy2))




roc_obj_binary <- roc(df$polypharm, df$mu_hat)
plot(roc_obj_binary, main = "ROC Curve using Binary Predictions", col = "red", lwd = 2)
auc_value_binary <- auc(roc_obj_binary)
print(paste("AUC (Binary):", auc_value_binary))


####### chain 3 #######
precisions3 <- numeric(length(beta3$V1))
recalls3 <- numeric(length(beta3$V1))
accuracies3 <- numeric(length(beta3$V1))

for (i in 1:length(beta3$V1)) {
  df$y_hat <- beta3[i,1] + beta3[i,2] * df$gender + beta3[i,3] * df$race + beta3[i,4] * df$age + 
    beta3[i,5] * df$mhv1 + beta3[i,6] * df$mhv3 + beta3[i,7] * df$mhv3 + beta3[i,8] * df$inptmhv + ui[i]
  df$mu_hat <- ifelse(df$y_hat < 0, 0, 1)
  accuracy <- mean(df$polypharm == df$mu_hat)
  accuracies3[i] <- accuracy
  tp <- sum(df$mu_hat == 1 & df$polypharm == 1)
  fp <- sum(df$mu_hat == 1 & df$polypharm == 0)
  fn <- sum(df$mu_hat == 0 & df$polypharm == 1)
  precisions3[i] <- tp / (tp + fp)
  recalls3[i] <- tp / (tp + fn)
}

mean_precision3 <- mean(precisions3, na.rm = TRUE)
mean_recall3 <- mean(recalls3, na.rm = TRUE)
mean_accuracy3 <- mean(accuracies3)

mean_precision3
mean_recall3
mean_accuracy3

print(paste("Mean Precision:", mean_precision3))
print(paste("Mean Recall:", mean_recall3))
print(paste("Mean Accuracy:", mean_accuracy3))




roc_obj_binary <- roc(df$polypharm, df$mu_hat)
plot(roc_obj_binary, main = "ROC Curve using Binary Predictions", col = "red", lwd = 2)
auc_value_binary <- auc(roc_obj_binary)
print(paste("AUC (Binary):", auc_value_binary))



####### chain 4 #######
accuracies4 <- numeric(length(beta4$V1))
for (i in 1:length(beta4$V1)) {
  # i=1
  df$y_hat <- beta4[i,1] + beta4[i,2] * df$gender + beta4[i,3] * df$race + beta4[i,4] * df$age + 
    beta4[i,5] * df$mhv1 + beta4[i,6] * df$mhv3 + beta3[i,7] * df$mhv3 + beta4[i,8] * df$inptmhv + ui[i]
  df$mu_hat <- ifelse(df$y_hat < 0, 0, 1)
  sum(df$polypharmacy)
  sum(df$mu_hat)
  accuracy <- mean(df$polypharm == df$mu_hat)
  #print(paste("Accuracy:", accuracy))
  accuracies4[i] <- accuracy
}

mean(accuracies4)















plot(t(ui[1,]))
