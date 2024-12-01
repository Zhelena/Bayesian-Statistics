library(aplore3)
library(pROC)
data(polypharm)
beta <- read.csv('sample.csv')
ui <- read.csv('u_i.csv')
#ui <- ui[2501:5000,-501]
ui <- as.data.frame(t(ui))
ui <- ui[rep(1:nrow(ui), each = 7), ]
ui <- cbind(ui, ui, ui, ui, ui, ui, ui, ui, ui, ui)


df <- polypharm[, c(2, 11, 12, 14)]
df$polypharmacy <- ifelse(df$polypharmacy == 'No', 0, 1)
df$gender <- ifelse(df$gender == 'Male', 1, 0)
df$race <- ifelse(df$race == 'White', 0, 1)
df$mhv1 <- ifelse(polypharm$mhv4 == '1-5', 1, 0)
df$mhv2 <- ifelse(polypharm$mhv4 == '6-14', 1, 0)
df$mhv3 <- ifelse(polypharm$mhv4 == '> 14', 1, 0)
df$inptmhv <- ifelse(polypharm$inptmhv3 == 0, 0, 1)


precisions <- numeric(length(beta$V1))
recalls <- numeric(length(beta$V1))
accuracies <- numeric(length(beta$V1))

for (i in 1:length(beta$V1)) {
  df$y_hat <- beta[i,1] + beta[i,2] * df$gender + beta[i,3] * df$race + beta[i,4] * df$age + 
    beta[i,5] * df$mhv1 + beta[i,6] * df$mhv2 + beta[i,7] * df$mhv3 + beta[i,8] * df$inptmhv + ui[i]
  df$mu_hat <- ifelse(df$y_hat < 0, 0, 1)
  accuracy <- mean(df$polypharm == df$mu_hat)
  accuracies[i] <- accuracy
  tp <- sum(df$mu_hat == 1 & df$polypharm == 1)
  fp <- sum(df$mu_hat == 1 & df$polypharm == 0)
  fn <- sum(df$mu_hat == 0 & df$polypharm == 1)
  precisions[i] <- tp / (tp + fp)
  recalls[i] <- tp / (tp + fn)
}

mean_precision <- mean(precisions, na.rm = TRUE)
mean_recall <- mean(recalls, na.rm = TRUE)
mean_accuracy <- mean(accuracies)

mean_accuracy
mean_precision
mean_recall

actual <- df$polypharm
predicted_prob <- df$mu_hat

# ROC
roc_obj_binary <- roc(df$polypharm, df$mu_hat)
plot(roc_obj_binary, main = "ROC Curve using Binary Predictions", col = "red", lwd = 2)

# AUC
auc_value_binary <- auc(roc_obj_binary)
print(paste("AUC (Binary):", auc_value_binary))

