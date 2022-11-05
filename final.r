library(caret)
library(faraway)
# install package faraway
# library(faraway)
install.packages("faraway")
library(faraway)

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
# sum of residuals of y_test and ridge_pred
y_test - ridge_pred
sum(y_test - ridge_pred)
dataMoto <- motorins

# Q: 9

# fit a gamma glm with mu_i as perd_i by Bonus_i
gamma_fit <- glm(Bonus ~ perd, data = dataMoto, family = Gamma(link = "identity"), subset = 1:35)
gamma_fit
# get fitted_log_mu_i
fitted(gamma_fit)
# get fitted_mu_i
exp(fitted(gamma_fit))
# get fitted values
# fit a gamma glm with mu_i as mean of perd_i by Bonus_i, Claims_i and Insured_i
# gamma_fit2 <- glm(Bonus ~ perd + Claims + Insured, data = dataMoto, family = Gamma(link = "identity"))
# gamma_fit2

# fit a gamma glm with mu_i as mean of perd_i by Bonus_i, Claims_i and Insured_i i = 1...35
gamma_fit2 <- glm(Bonus ~ perd + Claims + Insured, data = dataMoto, family = Gamma(link = "identity"), subset = 1:35)
gamma_fit2

# chi square test of gamma_fit2 and gamma_fit
anov <- anova(gamma_fit2, gamma_fit)
# get the p value
anov

# get the fitted values
gamma_pred <- fitted(gamma_fit)
gamma_pred
# get the residuals
gamma_res <- residuals(gamma_fit)
gamma_res
# get the r squared value for the fitted model in the test set
caret::R2(dataMoto$Bonus, gamma_pred)
# get the RMSE value for the fitted model in the test set
RMSE(dataMoto$Bonus, gamma_pred)
# get the MAE value for the fitted model in the test set
MAE(dataMoto$Bonus, gamma_pred)

# Q: 10