#linear regression
#logistic regression

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

#logistic regression
#whats the odd of hypertension if you are male
data.sav$hpt <- cut(data.sav$bpc, breaks = c(-Inf,2,Inf), labels = c("1","2"))# 1 is normal, 2 is hpt
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
wald.test(b = coef(glm.fit4``), Sigma = vcov(glm.fit4), Terms = 2:3)

glm.fit2 <- glm(hpt ~ exercise + smoking, data = data.sav, family = binomial)
summary(glm.fit2)
exp(coef(glm.fit2))
exp(confint(glm.fit2))
wald.test(b = coef(glm.fit2), Sigma = vcov(glm.fit2), Terms = 2:3)

