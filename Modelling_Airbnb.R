#Remove multi-colinearity
cormat = cor(airbnb_final)
hc = findCorrelation(cormat,cutoff = 0.85)
hc = sort(hc)
hc
airbnb_final = airbnb_final[,-c(hc)]

#split dataset 80/20
set.seed(123)
trainindex = createDataPartition(airbnb_final$price ,p=0.80, list =F)
train_df = airbnb_final[trainindex,]
test_df =  airbnb_final[-trainindex,]

#Scale the numeric data
cols = names(airbnb_final)

pre_proc_val <- preProcess(train_df[,cols], method = c("center", "scale"))

train_df[,cols] = predict(pre_proc_val, train_df[,cols])
test_df[,cols] = predict(pre_proc_val, test_df[,cols])

#Cross-validation 
ctrlspecs = trainControl(method = "cv", number = 10, savePredictions = "all", classProbs = TRUE)
set.seed(1234)

########Baseline Model
baseline = mean(train_df$price)
summary(train_df$price)
TestRmse_Baseline <- sqrt(mean((test_df$price - baseline)^2))
TestMAE_Baseline <- mean(abs(test_df$price-baseline))
TrainRmse_Baseline <- sqrt(mean((train_df$price - baseline)^2))
TrainMAE_Baseline <- mean(abs(train_df$price-baseline))

########Linear Regression
#Train dataset
train_all_lm = train(price~.,data=train_df,method="lm",trcontrol = ctrlspecs)
print(train_all_lm)

#Test Linear Regression
test_all_lm = train(price~.,data=test_df,method="lm",trcontrol = ctrlspecs)
print(test_all_lm)

########Ridge Regression 
#Regularization
#Creating model matrics - glment does not support dataframes
cols_reg = names(airbnb_final)

dummies <- dummyVars(price ~ ., data = airbnb_final[,cols_reg])

train_dummies = predict(dummies, newdata = train_df[,cols_reg])

test_dummies = predict(dummies, newdata = test_df[,cols_reg])

print(dim(train_dummies)); print(dim(test_dummies))

#Build Ridge Regression Model
x = as.matrix(train_dummies)
y_train = train_df$price

x_test = as.matrix(test_dummies)
y_test = test_df$price

lambdas <- 10^seq(2, -3, by = -.1)
ridge_reg = glmnet(x, y_train, nlambda = 25, alpha = 0, family = 'gaussian', lambda = lambdas)

#Model Information
summary(ridge_reg)

#Find Optimal Lambda = 0.019952
cv_ridge <- cv.glmnet(x, y_train, alpha = 0, lambda = lambdas)
optimal_lambda <- cv_ridge$lambda.min
optimal_lambda

# Compute R^2 from true and predicted values
eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  MAE = (sum(abs(predicted-true)))/nrow(df)
  
# Model performance metrics
data.frame(
    RMSE = RMSE,
    Rsquare = R_square,
    MAE = MAE
  )
}

# Prediction and evaluation on train data
predictions_train <- predict(ridge_reg, s = optimal_lambda, newx = x)
eval_results(y_train, predictions_train, train_df)

# Prediction and evaluation on test data
predictions_test <- predict(ridge_reg, s = optimal_lambda, newx = x_test)
eval_results(y_test, predictions_test, test_df)


#####LASSO Regression
lambdas <- 10^seq(2, -3, by = -.1)

# Setting alpha = 1 implements lasso regression
lasso_reg <- cv.glmnet(x, y_train, alpha = 1, lambda = lambdas, standardize = TRUE, nfolds = 5)

# Best 
lambda_best <- lasso_reg$lambda.min 
lambda_best #0.0031622

lasso_model <- glmnet(x, y_train, alpha = 1, lambda = lambda_best, standardize = TRUE)

predictions_train <- predict(lasso_model, s = lambda_best, newx = x)
eval_results(y_train, predictions_train, train_df)

predictions_test <- predict(lasso_model, s = lambda_best, newx = x_test)
eval_results(y_test, predictions_test, test_df)


















