#--------------------------------------------------------
# Biostatistic practical using R Software, Year 4 MBBS (2022)
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

## The window pane arrangement
# 1. Script (Command, run, save script)
# 2. Console (Result of command, clear, terminal system shell, output)
# 3. Environment & History (Object, previous command, )
# 4. Files & others (Files directory, plots, package, help, viewer)

# - Open a new R script
#   - type all commands/functions here
#   - comments or for notes start with "#"
#   - run all commands by Ctrl+Enter or click run

## Tasks 
# - Set the working directory (Files you want to use)
# - Install packages a.k.a libraries (Package)
#   - psych, car
install.packages("psych") # this command is to download
install.packages("car") # install car package from cran repository
library(psych)
library(car) # this command to install car package
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
z + y
#Exercise 1 try to calculate 300/25
300/25

# Read data

## Built in data sets
data()
sleep  # view data
?sleep

## Reading data sets
# Always make sure that you set the working directory first!
install.packages("foreign") # one of the method to install a package, another one method is by go to the package tab
library(foreign)  # library to read .sav (SPSS) and .dta (STATA) files
data.sav = read.spss("healthstatus.sav")  # most natural way to open data in R
data.sav = read.spss("healthstatus.sav", to.data.frame = TRUE)  # read SPSS dataset
data.dta = read.dta("cholest.dta")  # How to read STATA dataset
install.packages("readxl")
library(readxl)  # library to read excel files, must install first
datatest = read_excel("comsurvey.xlsx", sheet = 1)

# Handle data 

## Basics
str(data.sav)  # Basic info
str(data.xls)
str(data)
dim(data.sav)  # Dimension (row/case column/variable)
names(data.sav)  # Variable names
str(data.sav$smoking)
data.sav$sex <- as.factor(data.sav$sex) # set a factor variable
str(datacomsurvey)
data.sav$age <- as.numeric(data.sav$age) #set a numerical variable
data.sav$ethnicity <- factor(data.sav$ethnicity, levels = c("Low","Moderate","High"), labels = c("L","M","H"))
data.sav$totalwt <- data.sav$wt + data.sav$wt2
data.sav$wtdiff <- data.sav$wt2 - data.sav$wt
datatest$wtdiff <- datatest$wt2 - datatest$wt
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
write_xlsx(data.sav, "dataedited.xlsx")

dat = read_excel("data.xlsx", sheet = 1)

write_sav(data.sav,"healthstatusedited0822.sav")
data1.sav = read.spss("dataedited.sav", to.data.frame = TRUE)

