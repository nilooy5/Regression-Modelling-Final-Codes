library(caret)
library(faraway)
# install package faraway
# library(faraway)
install.packages("faraway")
install.packages("mlbench")
library(mlbench)
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
ridge_fit$a0
ridge_fit$beta
ridge_pred <- predict(ridge_fit, s = 0.01, newx = x_test)
ridge_pred

RMSE(y_test, ridge_pred)
# get the r squared value for the fitted model in the test set
caret::R2(y_test, ridge_pred)
ridge_pred
# sum of residuals of y_test and ridge_pred
y_test - ridge_pred
sum(y_test - ridge_pred)

# Q: 9
dataMoto <- motorins

# fit a gamma glm with mu_i as perd_i by Bonus_i
gamma_fit <- glm(perd ~ Bonus, data = dataMoto, family = Gamma(link = "log"), subset = 1:35)
gamma_fit
# get fitted values
# fit a gamma glm with mu_i as mean of perd_i by Bonus_i, Claims_i and Insured_i
# gamma_fit2 <- glm(Bonus ~ perd + Claims + Insured, data = dataMoto, family = Gamma(link = "identity"))
# gamma_fit2

# fit a gamma glm with mu_i as mean of perd_i by Bonus_i, Claims_i and Insured_i i = 1...35
gamma_fit2 <- glm(perd ~ Bonus + Claims + Insured, data = dataMoto, family = Gamma(link = "log"), subset = 1:35)
gamma_fit2

# chi square test of gamma_fit2 and gamma_fit
anovTest <- anova(gamma_fit2, gamma_fit, test = "Chisq")

anovTest

# Q: 10
pimaCleaned <- na.omit(PimaIndiansDiabetes2)
# fit a logistic regression model with diabetes as dependent variable and all other variables as independent variables
logit_fit <- glm(diabetes ~ age, data = pimaCleaned, family = binomial(link = "logit"))
logit_fit
logit_fit2 <- glm(diabetes ~ age + mass + pressure, data = pimaCleaned, family = binomial(link = "logit"))
logit_fit2
# predict diabetes for age=53 and mass=32 and pressure=72
predict(logit_fit2, newdata = data.frame(age = 53, mass = 30.5, pressure = 70), type = "response")

# Q: 11
data("iris")
library(caret)
df <- iris[21:150,]
scaled_data <- scale(df[,1:4])
scaled_data <- as.data.frame(scaled_data)
scaled_data$Species <- df$Species

iris_model <- rpart(Species ~ ., data = scaled_data, method = "class")
iris_model
library(rattle)
fancyRpartPlot(iris_model$finalModel)
rpart.plot(iris_model)
