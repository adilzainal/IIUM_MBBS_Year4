#--------------------------------------------------------
# Biostatistic practical using R Software (2022)
#--------------------------------------------------------
# Muhammad Adil ZA
#--------------------------------------------------------
# Bivariate data analysis
#--------------------------------------------------------

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

#yates correction for correction works when at least one of the cell have less than 5 
t3 = table(data.sav$sex, data.sav$smoking)
t3
prop.table(t3, 1)
chisq.test(t3)

#fisher exact test when small sample size (when more than 20% of cells have exp frequencies less than 5)
fisher.test(data.sav$sex,data.sav$smoking)

#correlation 
plot(data.sav$wt, data.sav$hcy, main="Scatterplot Example", 
     xlab="Weight ", ylab="Hcy ", pch=20)

plot(data.sav$sbp, data.sav$dbp, main="Scatterplot new", 
     xlab="Systolic ", ylab="Diastolic ", pch=20)

# Simple Scatterplot
plot(data.sav$sbp, data.sav$dbp, main="Scatterplot Example", 
     xlab="SBP ", ylab="DBP ", pch=19)

# Add fit lines
abline(lm(data.sav$hcy~data.sav$wt), col="red") # regression line (y~x) 
lines(lowess(data.sav$wt,data.sav$ht), col="blue") # lowess line (x,y)


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
describeBy(data.sav$BMI, data.sav$smoking)
leveneTest(data = data.sav, BMI ~ smoking) # another assumption for in t test, must have equal variance, homoscedasticity
                                    # null hypothesis, no variance diff btw male and female 
                       # p value not sig, do not reject null, no variance diff
t.test(data = data.sav, sbp ~ sex)

t.test(data = data.sav, sbp ~ sex, var.equal=TRUE)
t.test(data = data.sav, BMI ~ smoking, var.equal=FALSE)

#pair t-test
describe(data.sav$wt)
sd(data.sav$wt)
describe(data.sav$wt2)
sd(wt2)
t.test(wt,wt2,paired = TRUE) # p value sig, reject null, there is sig diff of wt before and after 

attach(data.sav)
describe(data.sav$BMI)
describe(data.sav$BMIa)
t.test(BMI, BMIa,paired = TRUE) 

#mcnemar test for binary paired categorical data
HPTbtw <-matrix(c(30,12,40,18),nrow = 2,
         dimnames = list("HPT before" = c("normal", "hypertension"),
                         "HPT after" = c("normal", "hypertension")))
HPTbtw
mcnemar.test(HPTbtw)

#mcnemar bowker test for binary paired categorical data
#package EMT and rcompanion
summary(BMIc)
summary(BMIac)
table(BMIc, BMIac)
Input =("
Before UW   N   OW  O1  O2
UW 8  0  0  0  0           
N  23  30  0  0  0  
OW  0  18  4  0  0
O1  0 1  12  29  0 
O2  0  0  0  7  21
")
Matrix.2 = as.matrix(read.table(textConnection(Input),
                                header=TRUE,
                                row.names=1))
Matrix.2
sum(Matrix.2)
mcnemar.test(Matrix.2)
nominalSymmetryTest(Matrix.2,
                    method="fdr",
                    digits = 3,
                    MonteCarlo = TRUE,
                    ntrial = 100000)
#look at significant result between uw and normal and?


