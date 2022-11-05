library(caret)

# Q: 7

# read usap
usap <- read.csv("usap.csv")
# make a regression model for vote share regressed on growth rate, party and interaction between growth rate and party
reg <- lm(vs ~ gr + pp + gr*pp, data = usap)
reg
summary(reg)

# Q: 8
lData <- longley
# take first 12 observations as training data
longley_train <- lData[1:12,]
# take last 4 observations as test data
longley_test <- lData[13:16,]
# get employed as y_train
y_train <- longley_train[, 7]
x_train <- model.matrix(Employed ~ ., data = longley_train)[,-1]
# get employed as y_test
y_test <- longley_test[, 7]
x_test <- model.matrix(Employed ~ ., data = longley_test)[,-1]


ridge_fit <- glmnet(x_train, y_train, alpha = 0, lambda = 0.01)
ridge_fit
ridge_pred <- predict(ridge_fit, s = 0.01, newx = x_test)
ridge_pred

RMSE(y_test, ridge_pred)
# get the r squared value for the fitted model in the test set
caret::R2(y_test, ridge_pred)
ridge_pred