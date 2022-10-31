#--------------------------------------------------------
# Biostatistic practical using R Software, Year 4 MBBS (2020)
#--------------------------------------------------------
# Muhammad Adil ZA 
#--------------------------------------------------------
# Getting started with R
#--------------------------------------------------------
# Learning objectives 
#1.Be familiar with reasons to use R.
#2.Download, install, and load R packages.
#3.Be able to navigate the RStudio interface including the Script, Console, Environment, Help, Files, and Plots windows.
#4.Create an R Project in RStudio using RScript and installing packages.
#5.Set a “working” directory.
#6.Send commands from the Script window to the Console in RStudio and use comments to inform scripts
#7.Create R objects and and assign values to them.
#8.Do simple arithmetic operations in R using values and objects.
#9.Call functions with arguments and change their default options.
#10.Inspect the content of vectors and manipulate their content.
#11.Subset and extract values from vectors.
#12.Correctly define and handle missing values in vectors.
#13.Use the built-in RStudio help interface
#14.Interpret the R help documentation
#15.Provide sufficient information for troubleshooting with the R user community.
#--------------------------------------------------------

# R & RStudio & RCommander

## Installation
# - R @ https://cran.r-project.org/
# - RStudio @ http://www.rstudio.com/

# RStudio Interface

## The windows arrangement
# 1. Script (Command, run, save script)
# 2. Console (Result of command, clear, terminal system shell, output)
# 3. Environment & History (Object, previous command, )
# 4. Files & others (Files directory, plots, package, help, viewer)

# - Open a new R script
#   - type all commands/functions here
#   - comments, start with "#"
#   - run all commands by Ctrl+Enter or click run

## Tasks 
# - Set the working directory (Files you want to use)
# - Install packages a.k.a libraries (Package)
#   - psych, car
install.packages("psych") # this command is to download
install.packages("car") # install car package from cran repository
library(psych)
library(car)
install.packages("readxl")
library(readxl)
library(apaTables)

# Function, Library, Object

## Function, and functions comes with the packages that you install and put in the library.
# - function(), think of MS Excel function

## Library, download from CRAN repositories, dependencies.
library(psych)

## Help
?psych
??psych

## Object , To do useful and interesting things in R, we need to assign values to objects
# - name assigned on left side of "<-" / "="
# - variable, data (data frame, matrix, list)
# - function get more more than 1 input which is the argument. (Example open excel dataset)
# - The return value is the output - z
# - object can be variables
x <- 1
y = 2
z = x + y
z  # type object name, you'll get the value
300/24

# Read data

## Built in data sets
data()
sleep  # view data
?sleep

## Reading data sets
# Always make sure that you set the working directory first!
install.packages("foreign") # one of the method to install a package, another one method is by go to the package tab
library(foreign)  # library to read .sav (SPSS) and .dta (STATA) files
testdata = read.spss("healthstatus.sav") 
data.sav = read.spss("healthstatus.sav")  # most natural way to open data in R
data.sav = read.spss("healthstatus.sav", to.data.frame = TRUE)  # read SPSS dataset
data.sav = read.spss("healthstatus.sav", to.data.frame = TRUE)  # read SPSS dataset
data.dta = read.dta("cholest.dta")  # How to read STATA dataset
install.packages("readxl")
library(readxl)
library(readxl)  # library to read excel files, must install first
datatest = read_excel("comsurvey.xlsx", sheet = 1)
testing1 = read_excel("Book1.xlsx", sheet = 1)
csdata = read_excel("cs.xlsx", sheet = 1)
comsurvey = read_excel("comsurvey.xlsx", sheet = 1)
comsurvey2 <- read_excel("comsurvey2.xlsx", sheet =1)

# Handle data 

## Basics
str(data.sav)  # Basic info
str(data.xls)
str(data)
dim(data.sav)  # Dimension (row/case column/variable)
names(data.sav)  # Variable names
str(data.sav$smoking)
data.sav$sex3 <- as.factor(data.sav$sex)
datacomsurvey$sex5 <- as.factor(datacomsurvey$sex)
str(datacomsurvey)
data.sav$age <- as.numeric(data.sav$age)
data.sav$sex2 <- as.factor(data.sav$sex)
data.sav$sex2 <- as.factor(data.sav$sex2)
data.sav$sex2 <- as.numeric(data.sav$sex)
data.sav$id <- as.numeric(data.sav$id)
data.sav$sex2 <- as.factor(data.sav$sex2)
data.sav$sex3 <- factor(data.sav$sex2, levels = c("Female","Male"), labels = c("Yes","No"))
data.sav$ethnicity <- factor(data.sav$ethnicity, levels = c("Low","Moderate","High"), labels = c("L","M","H"))
comsurvey$ethnic2 <- as.factor(comsurvey$ethnic)
comsurvey$ethnic2 <- factor(comsurvey$ethnic2, levels = c("1","2","3"), labels = c("Malay","Chinese","Indian"))
data.sav$sex2 <- factor(data.sav$sex, levels = c("Female", "Male"), labels = c("2","1"))
data.sav$sex2 <- factor(data.sav$sex2, levels = c("1", "2"), labels = c("female","male"))
data.sav$totalwt <- data.sav$wt + data.sav$wt2

## View data
head(data.sav)  # View data, first 6 rows
tail(data.sav)  # View data, last 6 rows
data.sav  # View all
View(data.sav)  # View, graphical way

# Data structure

## The basic data types
str(data.sav)
# - numeric = numerical
# - factor = categorical
library(labelled)
data.sav$smoking <- as.numeric(data.sav$smoking)
data.sav$smoking2 <- as.factor(data.sav$smoking)
data.sav$smoking <- factor(data.sav$smoking, levels = c("No","Yes"),labels = c("nonsmoker", "smoker"))
# - basically a variable in R is a VECTOR
data_num = c(1,2,3,4,5); str(data_num)
data_cat = factor( c("M", "F", "M", "F", "M") ); str(data_cat)
# - use ";" to write two lines of short commands into one

# Q&A?

library(writexl)
dat <- data.sav
write_xlsx(dat, "data.xlsx")
write_xlsx(comsurvey, "comsurvey.xlsx")

dat = read_excel("data.xlsx", sheet = 1)

write_xlsx(data.sav, "databaru.xlsx")
write_sav(data.sav,"healthstatusedited0822.sav")
data1.sav = read.spss("dataedited.sav", to.data.frame = TRUE)

