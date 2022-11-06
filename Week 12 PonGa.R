#Week 12 Poisson and Gamma GLM
 
# Poisson

head(warpbreaks)
attach(warpbreaks)
hist(breaks)

? glm

output <-glm(formula = breaks ~ wool + tension,            
             family = poisson, data = warpbreaks)
summary(output)

par(mfrow = c(2,2)) 
plot(output, which = c(1:4))

############
p <- read.csv("poisson_sim.csv")
head(p)

p <- within(p, {
  prog <- factor(prog, levels=1:3, labels=c("General", "Academic", "Vocational"))
  id <- factor(id)
}
)

head(p)

par(mfrow = c(1,1))
hist(p$num_awards)

m1 <- glm(num_awards ~ prog + math, data=p, family="poisson")
summary(m1)

s1 <- data.frame(prog = factor(1:3, 
                               levels = 1:3, 
                               labels = levels(p$prog)),
                 math = mean(p$math)
)

s1

predict(m1, s1, type="response", se.fit=TRUE)

# gamma

? glm

clotting <- data.frame(
  u    = c(5,10,15,20,30,40,60,80,100),
  lot1 = c(118,58,42,35,27,25,21,19,18),
  lot2 = c(69,35,26,21,18,16,13,12,12)) 
summary(glm(lot1 ~ log(u), data = clotting, family = Gamma))
summary(glm(lot2 ~ log(u), data = clotting, family = Gamma))

# simulation data
set.seed(999)
N <- 100
x <- runif(N, -1, 1)
shape <- 10  #shape=alpha, dispersion phi=1/shape
a <- 0.5     #intercept
b <- 1.2     #slope
y_true <- exp(a + b * x)  #mu=mean of y and log(mu)=a+b*x
y <- rgamma(N, shape = shape, rate = shape / y_true) 
#rate=beta, mean=shape/rate, y has gamma random numbers with mean=y_true

par(mfrow = c(1,1))
plot(x, y)
lines(sort(x), y_true[order(x)])

m_glm <- glm(y ~ x, family = Gamma(link = "log"))
coef(m_glm)
summary(m_glm)

library(MASS)
myshape <- gamma.shape(m_glm)
mypred <- predict(m_glm, type="response", se=T, dispersion = 1/myshape$alpha )
mypred

#compare with
m_glm$fitted.values
 
mypred2 <- predict(m_glm, type="response", newdata = data.frame(x=c(-0.5, 0, 0.5)))
mypred2
 