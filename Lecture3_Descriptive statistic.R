#--------------------------------------------------------
# Biostatistic practical using R Software, Year 4 MBBS (2020)
#--------------------------------------------------------
# Muhammad Adil ZA
#--------------------------------------------------------
# Exploratory data analysis
#--------------------------------------------------------

# “The greatest value of a picture is when it forces us to notice what we never expected to see.” John Tukey

#missing data
library(Amelia)
library(Rcpp)
data.sav = read.spss("dataedited.sav", to.data.frame = TRUE)
missmap(data.sav, main = "Missing values vs observed")
#You can add the argument na.rm=TRUE to calculate the result while ignoring the missing values.

#Explore dataset
data.sav = read.spss("healthstatus.sav", to.data.frame = TRUE)
summary(data.sav)
summary(data.sav$age)
attach(data.sav)
sapply(data.sav, mean, na.rm=TRUE) #Exclude missing value
summary(data.sav$age) # non-normal
fivenum(data.sav$age) # Tukey min,lower-hinge, median,upper-hinge,max
sd(data.sav$age)
sd(age)
IQR(data.sav$age)
IQR(age)
range(age)
var(data.sav$age)
attach(data.sav)
install.packages("pastecs")
library(pastecs)
stat.desc(data.sav$age) #for normal data
stat.desc(age)
#crosstabulate for continous data
library(psych)
describe(data.sav$age)
describeBy(data.sav$age, data.sav$sex)
describeBy(sbp,sex)

#Table
# 2-Way Frequency Table 
attach(data.sav)
summary(data.sav$sex)
tblsex <- table(exercise)
prop.table(tblsex)
tbmic <- table(BMIc)
prop.table(tbmic)

summary(data.sav$age)
mytable <- table(data.sav$sex, data.sav$exercise) # A will be rows, B will be columns 
mytable # print table 
prop.table(mytable)
prop.table(mytable, 2)
tblsxhpt <- table(sex,data.sav$hpt)
tblsxhpt
t1 <- prop.table(tblsxhpt, 1)
round(t1, digits = 2) 

mytable <- table(data.sav$exercise, data.sav$sex) # A will be rows, B will be columns 
mytable # print table 

summary(data.sav$exercise)
margin.table(mytable, 1) # A frequencies (summed over B) 
margin.table(mytable, 2) # B frequencies (summed over A)

prop.table(mytable) # cell percentages
t2 <- prop.table(mytable, 1) # row percentages 
t3 <- prop.table(mytable, 2) # column percentages
round(t2, digits = 2) # how to make decimal points 
round(t3, digits = 2) 


# 3-Way Frequency Table 
attach(data.sav)
mytable3 <- table(data.sav$sex, data.sav$exercise, data.sav$smoking)
mytable3
mytable3 <- table(sex, exercise, smoking) 
ftable(mytable3)
prop.table(mytable3, 2)

#normality check for a continuous variable can be determine using statistically and graphically
#statistically
shapiro.test(age) # null hypothesis, no difference between age distribution and normal distribution
                  # alternate hypotheses, there is diff btw age and normal.dist
                  # p value, if p value < 0.05, reject null hypothesis, alternate hypothesis , your distribution not normal
                  # p value > 0.05, do not reject null, null hypothesis, your distribution is normal 
describe(data.sav$age) #The acceptable range for skewness or kurtosis below +1.5 and above -1.5 (Tabachnick & Fidell, 2013)
#graphically
#Histogram
hist(data.sav$age)
hist(age)
hist(wt)
hist(ht)
hist(data.sav$age,
     main="Age of patient",
     xlab="Age in years",
     xlim=c(10,80),
     col="darkmagenta",
     freq=FALSE)
hist()
install.packages("moments")
library(moments)
summary(data.sav$age)
attach(data.sav)
skewness(data.sav$age)
skewness(age)
kurtosis(age)

#graphics
library(ggplot2)
#barchart
counts <- table(data.sav$sex)
barplot(counts, 
        main="GENDER OF RESPONDENT",
        xlab="Gender")

propc <- prop.table(counts)
barplot(propc, 
        main="GENDER OF RESPONDENT",
        xlab="Gender")

countsmoking <- table(data.sav$smoking)
p <- barplot(countsmoking, main="Smoking", 
        xlab="Smoking status")

counts <- table(data.sav$smoking, data.sav$sex)
barchart1 <- barplot(counts, main="Smoking by gender",
        xlab="Gender of participants", col=c("Turquoise","brown"),
        legend = rownames(counts), theme(text = element_text(size=20)))

#piechart
library(ggplot2)
df = data.frame("sex" = c("Female","Male"),
                "percentage" = c(.46,.54))
pie = ggplot(df, aes(x="", y=percentage, fill=sex)) + geom_bar(stat="identity", width=1)
pie = pie + coord_polar("y", start=0) + geom_text(aes(label = paste0(round(percentage*100), "%")), position = position_stack(vjust = 0.5))
pie = pie + scale_fill_manual(values=c("#55DDE0", "#999999")) # Add color scale (hex colors)
pie = pie + labs(x = NULL, y = NULL, fill = NULL, title = "Gender") # Remove labels and add title
pie = pie + theme_classic() + theme(axis.line = element_blank(),
                                    axis.text = element_blank(),
                                    axis.ticks = element_blank(),
                                    plot.title = element_text(hjust = 0.5, color = "#666666"))
pie
#example2
df = data.frame("exercise" = c("Low","Moderate","High"),
                "percentage" = c(.40,.40,.20))
pie = ggplot(df, aes(x="", y=percentage, fill=exercise)) + geom_bar(stat="identity", width=1)
pie = pie + coord_polar("y", start=0) + geom_text(aes(label = paste0(round(percentage*100), "%")), position = position_stack(vjust = 0.5))
pie = pie + scale_fill_manual(values=c("#55DDE0", "#999999","#FFC0CB")) # Add color scale (hex colors)
pie = pie + labs(x = NULL, y = NULL, fill = NULL, title = "Exercise") # Remove labels and add title
pie = pie + theme_classic() + theme(axis.line = element_blank(),
                                    axis.text = element_blank(),
                                    axis.ticks = element_blank(),
                                    plot.title = element_text(hjust = 0.5, color = "#666666"))
pie

#boxplot
boxplot(age~sex,
        data=data.sav, 
        main="Age of respondent", 
        xlab="Gender",
        ylab="Age",
        col=c("blue", "green"))

boxplot(age,data=data.sav, main="Age of respondent", 
        ylab="Age")


boxplot(age~sex, data=data.sav,
        col=(c("gold","darkgreen")),
        main="Age according to gender", xlab="Gender")

#error bar 
attach(data.sav)
library(ggplot2)
install.packages(Rmisc)
library(Rmisc)
datas <- summarySE(data.sav, measurevar="age", groupvars=c("sex","exercise"))
datas

ggplot(datas, aes(x=sex, y=age, colour=exercise)) + 
  geom_errorbar(aes(ymin=age-sd, ymax=age+sd), width=.1) +
  geom_line() +
  geom_point()
pd <- position_dodge(0.1)
ggplot(datas, aes(x=sex, y=age, colour=exercise)) + 
  geom_errorbar(aes(ymin=age-ci, ymax=age+ci), width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd)

#only 1 variable
datasa <- summarySE(data.sav, measurevar="age", groupvars=c("sex"))
datasa
ggplot(datasa, aes(x=sex, y=age)) + 
  geom_errorbar(aes(ymin=age-ci, ymax=age+ci), width=.1) +
  geom_line() +
  geom_point()
pd <- position_dodge(0.1)
ggplot(datasa, aes(x=sex, y=age)) + 
  geom_errorbar(aes(ymin=age-ci, ymax=age+ci), width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd)

#example2
attach(data.sav)
datasa <- summarySE(data.sav, measurevar="BMI", groupvars=c("sex"))
datasa
ggplot(datasa, aes(x=sex, y=BMI)) + 
  geom_errorbar(aes(ymin=BMI-ci, ymax=BMI+ci), width=.1) +
  geom_line() +
  geom_point()

