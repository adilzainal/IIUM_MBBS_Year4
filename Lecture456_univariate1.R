# chi square test 
# correlation 
# independent t-test
# pair t-test
# mcnemar test 

library(foreign)  # library to read .sav (SPSS) and .dta (STATA) files
data.sav = read.spss("healthstatus.sav", to.data.frame = TRUE)  #SPSS

#chi square test for categorical variable
tbl = table(data.sav$sex, data.sav$hpt)
tbl
prop.table(tbl,2)
chisq.test(tbl) 

tbl2 = table(data.sav$BMIc, data.sav$hpt)
tbl2
prop.table(tbl2,1)
chisq.test(tbl2) 

#yates correction for correction works when sample size is small 
tbl3 = table(data.sav$sex, data.sav$smoking)
tbl3
prop.table(tbl3, 1)
chisq.test(tbl3)

#fisher exact test when small sample size
fisher.test(data.sav$sex,data.sav$smoking)

#correlation 
plot(data.sav$wt, data.sav$hcy, main="Scatterplot Example", 
     xlab="Weight ", ylab="Hcy ", pch=20)

plot(data.sav$sbp, data.sav$dbp, main="Scatterplot new", 
     xlab="Systolic ", ylab="Diastolic ", pch=20)

attach(data.sav)
abline(lm(data.sav$sbp~data.sav$dbp), col="red") # regression line (y~x) 

library(Hmisc)
cor.test(data.sav$hcy, data.sav$wt, method=c("pearson")) #normally distributed data
cor.test(data.sav$hcy, data.sav$wt, method=c("spearman")) #non-normally distributed data
cor.test(data.sav$sbp, data.sav$dbp, method=c("pearson")) #normally distributed data

#r <0.3 very weak, 0.3-0.5< is weak, 0.5-0.7 is moderate, >0.7 is strong
#interpret correlation coefficent = 1.-value 2.direction 3.p-value

#independent t-test
library(readxl)
library(ggplot2)
library(car)
library(psych)
describeBy(data.sav$sbp, data.sav$sex)
describeBy(data.sav$dbp, data.sav$sex)
leveneTest(data = data.sav, sbp ~ sex) # another assumption for in t test, must have equal variance, homoscedasticity
                                    # null hypothesis, no variance diff btw male and female 
                       # p value not sig, do not reject null, no variance diff
t.test(data = data.sav, sbp ~ sex)
88.8-91.68
124.33 -123.54
62.33012 - 60.91713

t.test(data = data.sav, sbp ~ sex, var.equal=TRUE)
t.test(data = data.sav, sbp ~ sex, var.equal=FALSE)

#pair t-test
describe(data.sav$wt)
sd(data.sav$wt)
describe(data.sav$wt2)
sd(wt2)
t.test(data.sav$wt,data.sav$wt2,paired = TRUE) # p value sig, reject null, there is sig diff of wt before and after 

attach(data.sav)
describe(data.sav$BMI)
describe(data.sav$BMIa)
t.test(BMI, BMIa,paired = TRUE) 

#mcnemar test for paired categorical data
summary(BMIc)
summary(BMIac)
table <- table(data.sav$BMIc, data.sav$BMIac) 
prop.table(table, 1)
test <-mcnemar.test(table(data.sav$BMIc, data.sav$BMIac))
test
data2 <-data.frame(data.sav$BMIc, data.sav$BMIac)
mcnemar.test(table(data2$BMIc, data2$BMIac))
round(table, digits = 1) 

set.seed(150)
data <- data.frame(before = sample(c("Positive",
                                     "Positive",
                                     "Positive",
                                     "Positive",
                                     "Negative"),
                                   300, replace = TRUE),
                   after = sample(c("Positive",
                                    "Positive",
                                    "Positive",
                                    "Positive",
                                    "Negative"),
                                  300, replace = TRUE))
mcnemar.test(table(data$before, data$after))

# or in matrix
Performance <-
  matrix(c(794, 86, 150, 570),
         nrow = 2,
         dimnames = list("1st Survey" = c("alive", "death"),
                         "2nd Survey" = c("alive", "death")))
Performance
mcnemar.test(Performance)


