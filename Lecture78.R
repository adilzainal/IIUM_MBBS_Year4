#--------------------------------------------------------
# Biostatistic workshop using R Software (2022)
#--------------------------------------------------------
# Muhammad Adil ZA
#--------------------------------------------------------
# Bivariate data analysis continued
#--------------------------------------------------------
#ANOVA
#Non parametric test (mann whitney, kruskall wallis, wilcoxon)

#assumption for anova - normality, equal variance
attach(data.sav)
describeBy(data.sav$sbp,data.sav$exercise,mat=TRUE)
leveneTest(data = data.sav, sbp ~ exercise)
fit <- aov(sbp ~ exercise, data=data.sav)
summary(fit)
TukeyHSD(fit) #post hoc test if p value is significant

describeBy(data.sav$wt2,data.sav$exercise,mat=TRUE)
leveneTest(data = data.sav, wt2 ~ exercise)
fit <- aov(data.sav$wt2 ~ data.sav$exercise,
           data=data.sav)
summary(fit)
TukeyHSD(fit) #post hoc test if p value is significant

library("ggpubr")
ggline(data.sav, x = "exercise", y = "wt2", 
       add = c("mean_se"), 
       order = c("Low", "Moderate", "High"),
       ylab = "Weight (kg)", xlab = "Exercise")

#mann whitney u test (non parametric for independent t-test)
library(psych)
library(purrr)
library(dplyr)
data.sav %>% 
  group_by(data.sav$sex) %>% 
  summary(data.sav$wt)
describeBy(data.sav$wt, data.sav$sex)
stat.desc(wt, sex)
attach(data.sav)
wilcox.test(wt ~ sex, data=data.sav) 

describe.by(data.sav$wt2, data.sav$sex)
wilcox.test(wt2 ~ sex, data=data.sav)

ggboxplot(data.sav, x = "sex", y = "wt2", 
          color = "sex", palette = c("#FF3300", "#E7B800"),
          order = c("Female", "Male"),
          ylab = "Weight after intervention (kg)", xlab = "Sex")

#kruskall wallis (non parametric for anova)
describeBy(data.sav$wt, data.sav$exercise)
kruskal.test(wt ~ exercise, data = data.sav)
attach(data.sav)
describe.by(data.sav$wt, data.sav$exercise)
kruskal.test(wt ~ exercise, data = data.sav)
pairwise.wilcox.test(data.sav$wt, data.sav$exercise,
                     p.adjust.method = "BH")   #post hoc test if the p value is significant

library("ggpubr")
ggboxplot(data.sav, x = "exercise", y = "wt", 
          color = "exercise", palette = c("#FF3300", "#E7B800", "#00FFFF"),
          order = c("Low", "Moderate", "High"),
          ylab = "Weight", xlab = "Exercise")

describe.by(data.sav$sbp, data.sav$exercise)
kruskal.test(sbp ~ exercise, data = data.sav)
pairwise.wilcox.test(data.sav$sbp, data.sav$exercise,
                     p.adjust.method = "BH")   #post hoc test if the p value is significant

library("ggpubr")
ggboxplot(data.sav, x = "exercise", y = "sbp", 
          color = "exercise", palette = c("#FF3300", "#E7B800", "#00FFFF"),
          order = c("Low", "Moderate", "High"),
          ylab = "Systolic", xlab = "Exercise")

#wilcoxon signed rank test (non parametric for pair t -test)
summary(data.sav$wt)
summary(data.sav$wt2)
wilcox.test(data.sav$wt, data.sav$wt2, paired=TRUE) 

