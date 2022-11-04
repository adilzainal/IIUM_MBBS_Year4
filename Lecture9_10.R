#--------------------------------------------------------
# Biostatistic practical using R Software (2022)
#--------------------------------------------------------
# Muhammad Adil ZA
#--------------------------------------------------------
# Regression data analysis
#--------------------------------------------------------
#linear regression
#logistic regression

library(aod)
library(dplyr)
library(ggplot)
library(tidyverse)
library(broom)
library(car)
library(rstan)
library(boot)
library(knitr)
library(stargazer)
library(effects)

#Linear regression when to predict linear outcome that is continuous data 
#correlation is just to test hypothesis whether there is linear relationship between the 2 continuous data
plot(data.sav$wt, data.sav$hcy, main="Scatterplot Example", 
     xlab="Weight ", ylab="Homocysteine ",xlim=c(0,110),ylim=c(0,30), pch=19)
attach(data.sav)
abline(lm(hcy~wt), col="red") # regression line (y~x) 

library(Hmisc)
cor.test(data.sav$hcy, data.sav$wt, method=c("pearson")) #normally distributed data
cor.test(data.sav$hcy, data.sav$wt, method=c("spearman")) #non-normally distributed data
cor.test(data.sav$hba1c, data.sav$wt, method=c("spearman")) #non-normally distributed data

#r <0.3 very weak, 0.3-0.5 is weak, 0.5-0.7 is moderate, >0.7 is strong
#interpret correlation coefficent = 1.-value 2.direction 3.p-value

linearmod1 <- lm(hcy ~ wt, data=data.sav)  # build linear regression model on full data
print(linearmod1)
summary(linearmod1)
y = a + bx
hcy = 1.4 + 0.12(wt)
linearmod2 <- lm(hcy ~ wt + smoking, data=data.sav)  # build linear regression model on full data
summary(linearmod2)
confint(linearmod2)
# how to interpret 
# y = bx + c
# homocysteine = (β∗weight) + Intercept
# homocysteine = 0.1219*weight + 1.3817
# hcy = 0.1219 (1kg) + 1.3817
# hcy = 1.5036
8.6957 = 0.1219(60) + 1.3817

summary(linearmod1)
confint(linearmod1)

linearMod2 <- lm(hcy ~ wt + smoking, data=data.sav)
print(linearMod2)
summary(linearMod2)
confint(linearMod2)

y = a + b1x1 + b2x2 
hcy = a + b1(weight) + b2(smokingstatus)
hcy = 0.1174(wt) + 1.51(smoking) + 1.1876 
9.7416 = 0.1174(60) + 1.51(smokingyes) + 1.1876
7.0576 = 0.1174(50) + 1.51(0) + 1.1876
8.5676 = 0.1174(50) + 1.51(1) + 1.1876

#how to interpret the adjusted R square 0.2405, weight explain 24% variation in homocysteine
#the p value for null hypothesis beta coefficient equal zero is 7.47e-11 which is significant slope is not equal to zero
#p value for the model 7.469e-11 show that the model with the predictor is significantly different compare to model without predictor

linearMod3 <- lm(sbp ~ BMI + hcy, data=data.sav)
print(linearMod3)
summary(linearMod3)
confint(linearMod3)

linearMod4 <- lm(sbp ~ hcy, data=data.sav)
print(linearMod4)
summary(linearMod4)
confint(linearMod4)

(1.395 - 1.06544)/1.395

# Linear regression makes few assumptions about the data 
# 1.Linearity of the data. 2. Normality of residuals 3. Homogeneity of residuals variance 4. Independence of residuals error terms

# 1. The first one is the linearity of the data between predictor x and outcome y
# This can be checked by inspecting Residuals vs Fitted 
# Ideally, the residual plot will show no fitted pattern and the redline should be approximately horizontal at zero.
plot(linearmod2, 1)
# Note that, if the residual plot indicates a non-linear relationship in the data
# Then a simple approach is to use non-linear transformations of the predictors, such as log(x), sqrt(x) and x^2, in the regression model.

# 2. The normality of residuals must be normally distributed. This can be plot using Q-Q Plot.
# It’s good if residuals points follow the straight dashed line.
plot(linearmod2, 2)

# 3. The scale location is used to check the homogeneity of variance of the residuals (homoscedasticity).
# Horizontal line with equally spread points is a good indication of homoscedasticity. 
plot(linearmod2, 3)
# A possible solution to reduce the heteroscedasticity problem is to use a log or square root transformation of the outcome variable (y).
linearmod2a <- lm(log(hcy) ~ wt + smoking, data = data.sav)
plot(linearmod2a, 3)

# 4. No outliers 
# The Residuals vs Leverage plot can help us to find influential observations if any. On this plot, outlying values are generally located at the upper right corner or at the lower right corner. 
# Those spots are the places where data points can be influential against a regression line.
# An outlier is a point that has an extreme outcome variable value.
# Observations whose standardized residuals are greater than 3 in absolute value are possible outliers (James et al. 2014).
# Here we don't have . If we have, we might want to remove that outliers 
plot(linearmod2, 5)

#How to summarize findings using apaTable package
apa.reg.table(linearmod2, filename = "Table1_APA_MLR.doc", table.number = 1)

#-----------------------------------------------------------------------------------------------------------------------------

#logistic regression
#whats the odd of hypertension if you are male
data.sav$hpt <- as.factor(data.sav$hpt)
summary(data.sav$hpt)
mytable <- table(data.sav$hpt, data.sav$sex)
mytable
prop.table(mytable,2)
chisq.test(mytable)
#logistic regression model
chisq.test(data.sav$hpt, data.sav$smoking)
chisq.test(data.sav$hpt, data.sav$exercise)
glm.fit <- glm(hpt ~ smoking, data = data.sav, family = binomial)
summary(glm.fit)
exp(coef(glm.fit))
exp(confint(glm.fit))

library(aod)
data.sav <- within(data.sav, BMIc <- relevel(BMIc, ref = 2))
glm.fit2 <- glm(hpt ~ BMIc + smoking, data = data.sav, family = binomial)
summary(glm.fit2)
exp(coef(glm.fit2))
exp(confint(glm.fit2))
wald.test(b = coef(glm.fit2), Sigma = vcov(glm.fit2), Terms = 2:5)

glm.fit2 <- glm(smoking ~ exercise + sex, data = data.sav, family = binomial)
summary(glm.fit2)
exp(coef(glm.fit2))
exp(confint(glm.fit2))
wald.test(b = coef(glm.fit2), Sigma = vcov(glm.fit2), Terms = 2:3)
# Run multiple logistic regession, outcome is smoking status, factor is gender and exercise 

#interpet being male is 1.4 times higher odd of having hypertension compare to female 
#assessing model accuracy 
probabilities <- glm.fit %>% predict(data.sav, type = "response")
head(probabilities)
contrasts(data.sav$hpt)
predicted.classes <- ifelse(probabilities > 0.5, "1", "2") # predict hpt
head(predicted.classes)
mean(predicted.classes == data.sav$hpt) #assessing model accuracy
#The classification prediction accuracy is about 45%, which is not so good. The misclassification error rate is 55%.

#Eg 2
glm.fit4 <- glm(hpt ~ exercise, data = data.sav, family = binomial)
summary(glm.fit4)
exp(coef(glm.fit4))
exp(confint(glm.fit4))
wald.test(b = coef(glm.fit4), Sigma = vcov(glm.fit4), Terms = 2:3)

glm.fit2 <- glm(hpt ~ exercise + smoking, data = data.sav, family = binomial)
summary(glm.fit2)
exp(coef(glm.fit2))
exp(confint(glm.fit2))
wald.test(b = coef(glm.fit2), Sigma = vcov(glm.fit2), Terms = 2:3)

#Model fit parameter such as AIC , the smaller the better
#-----------------------------------------------------------------------------------------------------------------------------

# Logistic regression assumptions
# 1. Linearity assumption
# Here, we’ll check the linear relationship between continuous predictor variables and the logit of the outcome. 
# This can be done by visually inspecting the scatter plot between each predictor and the logit values.
# Select only numeric predictors
data1 <- data.sav %>% dplyr::select_if(is.numeric)
predictors <- colnames(data1)
# Bind the logit and tidying the data for plot
data1 <- data1 %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
# Create scatter plots
ggplot(data1, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")
# If is has linearity plot than it is ok
# If the scatter plot shows non-linearity, you need other methods to build the model 
# such as including 2 or 3-power terms, fractional polynomials and spline function 

# 2. No influential values
# Influential values are extreme individual data points that can alter the quality of the logistic regression model.
#  The most extreme values in the data can be examined by visualizing the Cook’s distance values.
# Here we label the top 3 largest values:
plot(glm.fit2, which = 4, id.n = 3)
# Note that, not all outliers are influential observations.
# To check whether the data contains potential influential observations, the standardized residual error can be inspected.
# Data points with an absolute standardized residuals above 3 represent possible outliers and may deserve closer attention.
# The following R code computes the standardized residuals (.std.resid) and the Cook’s distance (.cooksd) using the R function augment()
# Extract model results
model.data <- augment(glm.fit2) %>% 
  mutate(index = 1:n()) 
# The data for the top 3 largest values, according to the Cook’s distance, can be displayed as follow:
model.data %>% top_n(3, .cooksd)
# Plot the standardized residuals
ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = hpt), alpha = .5) +
  theme_bw()
# Filter the infuential data points
model.data %>% 
  filter(abs(.std.resid) > 3)
# There is no influential data in our points
# When you have outliers in a continuous predictor, potential solutions include:
# Removing the concerned data
# Transform the data into log scale

# 3. Multicollinearity
glm.fit6 <- glm(hpt ~ exercise + hba1c + age, data=data.sav, family=binomial)
summary(glm.fit6)
car::vif(glm.fit6)
# As a rule of thumb, a VIF value that exceeds 5 or 10 indicates a problematic amount of collinearity. 
# In our example, there is no collinearity: all variables have a value of VIF well below 5.

# Table final result using stargazer
stargazer(glm.fit2, type="text", out="models.txt")
exp(coef(glm.fit2))
stargazer(exp(coef(glm.fit2)), type="text", out="models.txt")
#-----------------------------------------------------------------------------------------------------------------------------


