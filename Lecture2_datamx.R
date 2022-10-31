# Muhammad Adil ZA
#--------------------------------------------------------
# Data management 
#--------------------------------------------------------

# “The greatest value of a picture is when it forces us to notice what we never expected to see.” John Tukey

library(foreign)  # library to read .sav (SPSS) and .dta (STATA) files
data.sav = read.spss("healthstatus.sav", to.data.frame = TRUE)  #SPSS

#make a bmi or composite variable bmi before intervention
data.sav$BMI <- (data.sav$wt)/ (((data.sav$ht)/100)^2)

comsurvey = read_excel("comsurvey.xlsx", sheet = 1)

comsurvey$q1 <- as.factor(comsurvey$q1)
comsurvey$q1 <- factor(comsurvey$q1, levels = c("agree","disagree"), labels = c("1","2"))
comsurvey$q1 <- as.numeric(comsurvey$q1)

comsurvey$q2 <- as.factor(comsurvey$q2)
comsurvey$q2 <- factor(comsurvey$q2, levels = c("agree","disagree"), labels = c("1","2"))
comsurvey$q2 <- as.numeric(comsurvey$q2)

comsurvey$q3 <- as.factor(comsurvey$q3)
comsurvey$q3 <- factor(comsurvey$q3, levels = c("agree","disagree"), labels = c("1","2"))
comsurvey$q3 <- as.numeric(comsurvey$q3)

comsurvey$totalatt <- comsurvey$q1 + comsurvey$q2 + comsurvey$q3
comsurvey$totalattcat <- cut(comsurvey$totalatt, breaks = c(-Inf,3,Inf), labels = c("LA","NA"))

comsurvey$k1f <- as.factor(comsurvey$k1)
comsurvey$k1f <- factor(comsurvey$k1f, levels = c("agree","disagree"), labels = c("1","2"))
comsurvey$k1n <- as.numeric(comsurvey$k1f)

comsurvey$k1 <- as.numeric(comsurvey$k1)
comsurvey$k2 <- as.numeric(comsurvey$k2)
comsurvey$k3 <- as.numeric(comsurvey$k3)
comsurvey$totalk <- (comsurvey$k1 + comsurvey$k2 + comsurvey$k3)
data.sav$BMIc <- cut(data.sav$BMI, breaks = c(-Inf,18.49,22.9,24.9,29.9,Inf), labels = c("UW","N","OW","O1","O2"))
str(data.sav$BMIc) # bmi category before intervention
data.sav$BMIc <- data.sav$BMIcategory
attach(data.sav)
data.sav$BMI2 <- (wt2)/ (((ht)/100)^2)

data.sav$psqic <- cut(data.sav$psqitotscore, breaks = c(-Inf,4,Inf), labels = c("G","P"))


#make a bmi variable after intervention line no 18-21
data.sav$BMIa <- (data.sav$wt2)/ (((data.sav$ht)/100)^2)
str(data.sav)
data.sav$BMIac <- cut(data.sav$BMIa, breaks = c(-Inf,18.49,22.9,24.9,29.9,Inf), labels = c("UW","N","OW","O1","O2"))
str(data.sav$BMIac) # bmi category after intervention

#make hypertension or composite variables
summary(data.sav$sbp)
data.sav$sbpc <- cut(data.sav$sbp , breaks = c(-Inf,129.9,Inf), labels = c("1","2"))
data.sav$sbpc <- as.numeric(data.sav$sbpc)
data.sav$dbpc <- cut(data.sav$dbp , breaks = c(-Inf,89.9,Inf), labels = c("1","2"))
data.sav$dbpc <- as.numeric(data.sav$dbpc)
data.sav$bpc <- data.sav$dbpc + data.sav$sbpc
data.sav$hpt <- cut(data.sav$bpc, breaks = c(-Inf,2,Inf), labels = c("normal","hpt"))
data.sav$hpt <- as.factor(data.sav$hpt)
summary(data.sav$hpt) # hypertension category
summary(data.sav$BMIcategory)
data.sav$BMI <- round(data.sav$BMI,2)

csdata = read_excel("cs.xlsx", sheet = 1)
library("readxl")
csdata$deptotal <- csdata$dep1 + csdata$dep2 + csdata$dep3 + csdata$dep4 + csdata$dep5 + csdata$dep6 + csdata$dep7
csdata$depc <- cut(csdata$deptotal , breaks = c(-Inf,5,Inf), labels = c("normal","depression"))

csdata$anxtotal <- csdata$anx1 + csdata$anx1 + csdata$dep3 + csdata$dep4 + csdata$dep5 + csdata$dep6 + csdata$dep7
csdata$depc <- cut(csdata$deptotal , breaks = c(-Inf,5,Inf), labels = c("normal","depression"))


