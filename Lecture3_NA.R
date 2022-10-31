#--------------------------------------------------------
# Biostatistic workshop using R Software (2019)
#--------------------------------------------------------
# Muhammad Adil ZA
#--------------------------------------------------------
# Numerical data analysis
#--------------------------------------------------------

#Correlations 

library(Hmisc)
rcorr(data.sav$wt, data.sav$ht, type="spearman") # type can be pearson or spearman
cor(data.sav$wt, data.sav$ht)

# Simple Scatterplot
plot(data.sav$wt, data.sav$ht, main="Scatterplot Example", 
     xlab="Weight of patient ", ylab="Height of patient ", pch=19)

# Add fit lines
abline(lm(data.sav$wt~data.sav$ht), col="red") # regression line (y~x) 
lines(lowess(data.sav$wt,data.sav$ht), col="blue") # lowess line (x,y)

#
scatterplot(wt ~ ht | sex, data=data.sav, 
            xlab="Weight of Car", ylab="Miles Per Gallon", 
            main="Enhanced Scatter Plot", 
            labels=row.names(data.sav))

# Basic Scatterplot Matrix
pairs(~ht+disp+drat+wt,data=data.sav, 
      main="Simple Scatterplot Matrix")

## T-test (Unlike most statistical packages, the default assumes unequal variance and applies the Welsh df modification.
# independent 2-group t-test
t.test(y~x) # where y is numeric and x is a binary factor
t.test(data.sav$wt~data.sav$sex)

# independent 2-group t-test
t.test(data.sav$wt,data.sav$wt2) # where y1 and y2 are numeric

# paired t-test
t.test(data.sav$wt,data.sav$wt2,paired=TRUE) # where y1 & y2 are numeric

# one sample t-test
t.test(data.sav$wt,mu=60) # Ho: mu=60

#Anova 
# One Way Anova (Completely Randomized Design)
fit <- aov(wt ~ exercise, data=data.sav)
summary(fit) 

# Randomized Block Design (B is the blocking factor) 
fit <- aov(wt ~ exercise + sex, data=data.sav)
summary(fit)

# Two Way Factorial Design 
fit <- aov(wt ~ exercise + sex + exercise:sex, data=data.sav)
summary(fit)
fit <- aov(wt ~ exercise*sex, data=data.sav) # same thing
summary(fit)

# Analysis of Covariance 
fit <- aov(wt ~ exercise + age, data=data.sav)
summary(fit)

# Another method evaluate model effects

fit <- aov(data.sav$wt ~ data.sav$exercise, data=data.sav)
summary(fit)# display Type I ANOVA table
TukeyHSD(fit) 
drop1(fit,~.,test="F") # type III SS and F Tests
# Check levene test
library(car)
leveneTest(wt ~ exercise)

#boxplot 
boxplot(wt~exercise,data=data.sav, main="Weight by type of exercise", 
        xlab="Exercise", ylab="Weight")


library(gplots)
plotmeans(wt ~ exercise, frame = FALSE)

## Non parametric

# independent 2-group Mann-Whitney U Test 
wilcox.test(y~A) 
wilcox.test(data.sav$wt~data.sav$sex)
# where y is numeric and A is A binary factor

# independent 2-group Mann-Whitney U Test
wilcox.test(data.sav$wt,data.sav$wt2) # where y and x are numeric

# dependent 2-group Wilcoxon Signed Rank Test 
wilcox.test(data.sav$wt,data.sav$wt2,paired=TRUE) # where y1 and y2 are numeric

# Kruskal Wallis Test One Way Anova by Ranks 
kruskal.test(y~A) # where y1 is numeric and A is a factor
kruskal.test(wt~exercise, data=data.sav)

