#--------------------------------------------------------
# Biostatistic practical using R Software, Year 4 MBBS (2022)
#--------------------------------------------------------
# Muhammad Adil ZA
#--------------------------------------------------------
# Data management 
#--------------------------------------------------------

# “The greatest value of a picture is when it forces us to notice what we never expected to see.” John Tukey

library(foreign)  # library to read .sav (SPSS) and .dta (STATA) files
data.sav = read.spss("healthstatus.sav", to.data.frame = TRUE)  #SPSS

#make a bmi or composite variable bmi before intervention
data.sav$BMI <- (data.sav$wt) / (((data.sav$ht)/100)^2)
#Exercise 2 try to make BMI after intervention 

#Example for your research
comsurvey$totalatt <- comsurvey$q1 + comsurvey$q2 + comsurvey$q3
comsurvey$totalattcat <- cut(comsurvey$totalatt, breaks = c(-Inf,3,Inf), labels = c("LA","NA"))
comsurvey$k1 <- as.numeric(comsurvey$k1)
comsurvey$k2 <- as.numeric(comsurvey$k2)
comsurvey$k3 <- as.numeric(comsurvey$k3)

#Bin variable into vategories
data.sav$BMIc <- cut(data.sav$BMI, breaks = c(-Inf,18.49,22.9,24.9,29.9,Inf), labels = c("UW","N","OW","O1","O2"))
str(data.sav$BMIc) # bmi category before intervention
data.sav$BMIc <- data.sav$BMIcategory
attach(data.sav)

#Exercise 3 try to bin the BMI after intervention

#Example for your research
data.sav$psqic <- cut(data.sav$psqitotscore, breaks = c(-Inf,4,Inf), labels = c("G","P"))

#Make hypertension or composite variables
summary(data.sav$sbp)
data.sav$sbpc <- cut(data.sav$sbp , breaks = c(-Inf,139.9,Inf), labels = c("1","2"))
data.sav$sbpc <- as.numeric(data.sav$sbpc)
data.sav$dbpc <- cut(data.sav$dbp , breaks = c(-Inf,89.9,Inf), labels = c("1","2"))
data.sav$dbpc <- as.numeric(data.sav$dbpc)
data.sav$bpc <- data.sav$dbpc + data.sav$sbpc
data.sav$hpt <- cut(data.sav$bpc, breaks = c(-Inf,2,Inf), labels = c("normal","hpt"))
data.sav$hpt <- as.factor(data.sav$hpt)
summary(data.sav$hpt) # hypertension category

#Shorter code
attach(data.sav)
data.sav$hpt<-(sbp>=140|dbp>=90)
data.sav$hpt <- as.factor(data.sav$hpt)
summary(data.sav$hpt)
data.sav$hpt <- factor(data.sav$hpt, levels = c("FALSE","TRUE"),labels = c("normotensive", "hypertensive"))

#Round the decimal point
summary(data.sav$BMIcategory)
data.sav$BMIa <- round(data.sav$BMIa,2)

#Example in your research
csdata = read_excel("cs.xlsx", sheet = 1)
library("readxl")
csdata$deptotal <- csdata$dep1 + csdata$dep2 + csdata$dep3 + csdata$dep4 + csdata$dep5 + csdata$dep6 + csdata$dep7
csdata$depc <- cut(csdata$deptotal , breaks = c(-Inf,5,Inf), labels = c("normal","depression"))
