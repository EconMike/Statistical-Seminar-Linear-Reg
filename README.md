Statistical-Seminar-Linear-Reg

This repository has been set up to store the R code for the linear regression seminar.

|------------------------| | R software i386 3.2.3 | | LINEAR REGRESSION MODEL| |------------------------| ***************************************************************************************; *libraries
• tseries - arima model
• ggplot2- recession bars
•tis - recession bars
•XLConnect - import xls files
•Car - for scatterplot
•fBasics - Jarque Bera Test (normality) ***************************************************************************************; install.packages("e1071") install.packages("car") install.packages("XLConnect") install.packages("fBasics") install.packages("MASS") install.packages("lmtest") install.packages("psych") install.packages("lattice") install.packages("stargazer")

library(e1071) library(lattice) library(XLConnect) library(car) library(base) library(fBasics) library(MASS) library(psych) library(lmtest) library(stargazer) *************************************************************************************; purpose:

PSYCH | descriptive statistics MASS | Mulitvariate statistics LMTEST | Breusch-Pagan test, durban-watson test,Breush-Godfrey Test

*************************************************************************************;


to remove scientific notation options(scipen=999)

IMPORT

************************************************************************************;

load data

newdata<-read.csv("C:/STATS_COURSE/COURSE_MATERIAL/REG/PRAC_DATA.csv")

data source: BEA.gov

Table 2.5.5. Personal Consumption Expenditures by Function- PCE line 1 [Billions of dollars] Bureau of Economic Analysis Last Revised on: August 06, 2015

Table 2.1. Personal Income and Its Disposition- WAG_S- line 3 [Billions of dollars] Bureau of Economic Analysis Last Revised on: November 24, 2015 - Next Release Date December 22, 2015

*************************************************************************************;

Removing missing observations

mean(airquality$Ozone,na.rm=T)

summary statistics


summary(duration) Min. 1st Qu. Median Mean 3rd Qu. Max. 0.004276 0.341900 0.695700 0.595000 0.881900 0.998500 

normailty

Skewness / Kurtosis

Download the e1071 package for R 32 bit

install.packages(e1071)

library(e1071)

the skewness is a measure of symmetry

data distribution is left-skewed. Positive skewness would indicates that the mean of the data values is larger than the median, and the data distribution is right-skewed.

positive Kurtosis: heavier tail, outlier prone

library(psych)

describe(newdata$WAG_S) describe(newdata$PCE)

Histogram: look at the series hist(newdata$WAG_S) hist(newdata$PCE)

Formal test for normality Jarque Bera Test jarqueberaTest(newdata$WAG_S, title = NULL, description = NULL) jarqueberaTest(newdata$PCE, title = NULL, description = NULL)

Hypothesis Ho: normally distributed Ha: non normal 

QQPLOT


qqnorm(duration) qqline(duration)

SCATTER PLOT/ correlation library(lattice)


plot(newdata$PCE,newdata$WAGES) cor(newdata$PCE,newdata$WAG_S)

(should you or should you not transform data)

linear regression

lm

use lm to fit a regression line through these data

x<-newdata$WAG_S y<-newdata$PCE


model1=lm(y~x, data=newdata) summary(model1)

Call: lm(formula = y ~ x, data = newdata)

Residuals: Min 1Q Median 3Q Max -474.46 -175.05 52.84 141.52 324.15 

Coefficients: Estimate Std. Error t value Pr(>|t|)
 (Intercept) -1.006e+03 1.553e+02 -6.478 1.31e-06 ***

x 1.714e+00 2.977e-02 57.594 < 2e-16 ***

Signif. codes: 0 ‘**’ 0.001 ‘’ 0.01 ‘’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 218.8 on 23 degrees of freedom Multiple R-squared: 0.9931, Adjusted R-squared: 0.9928 F-statistic: 3317 on 1 and 23 DF, p-value: < 2.2e-16


model1

Call: lm(formula = y ~ x, data = newdata)

Coefficients: (Intercept) x
 -1006.259 1.714 

to pull individual statistics


coefficients(model1) (Intercept) x -1006.259401 1.714317 

*************************************************************************************;

Other useful functions from http://www.statmethods.net/stats/regression.html

coefficients(model1) # model coefficients confint(model1, level=0.95) # CIs for model parameters fitted(model1) # predicted values residuals(model1) # residuals anova(model1) # anova table vcov(model1) # covariance matrix for model parameters influence(model1) # regression diagnostics ***************************************************************************************;

Residuals vs fitted

Collect the residuals and put it into a data frame.

resid(model1)

rsd<-as.data.frame(resid(model1)) rsd

resid(model1) 1 132.57464 2 141.51523 3 138.15342 4 198.36419 5 198.70218 6 130.92514 7 74.53329 8 -79.10333 9 -259.32704 10 -329.16415 11 -474.46124 12 -384.05093 13 -175.05223 14 -36.05660 15 -28.59396 16 42.46912 17 -74.04218 18 -206.63834 19 -177.88542 20 136.38039 21 275.40507 22 324.15430 23 176.13083 24 202.22514 25 52.84247

Collect the predicted values and put it into a data frame.


ptd<-as.data.frame(fitted(model1)) ptd fitted(model1) 1 3693.025 2 3818.685 3 4077.547 4 4272.636 5 4542.298 6 4853.275 7 5193.567 8 5639.803 9 6162.327 10 6636.164 11 7266.861 12 7487.151 13 7559.152 14 7801.557 15 8288.594 16 8751.631 17 9378.042 18 9957.138 19 10191.485 20 9710.620 21 9926.795 22 10365.146 23 10874.469 24 11190.075 25 11813.058

*********************************************************************; Test your models assumptions 1.The variables in the model are independent(Test for Heteroscedasticity) 2.Test for Autocorrelation 3. residusal sum to zero (put resids in a data frame use sum function 4. linear

correlation analysis:


cor(newdata$PCE,newdata$WAG_S) [1] 0.996551

Excerises:

library(MASS)

using datasets:

Test for heteordascity

Hypothesis Ho: is if the error variance (homoscedasticity) is equal Ha: Heteroscedasticity 


bptest(model1)
    studentized Breusch-Pagan test


data: model1 BP = 0.056031, df = 1, p-value = 0.8129

*******************************************************************************;

MULITPLE REGRESSION:

nmodel1<-lm(Girth~Height +Volume, data=trees) summary(nmodel1)

Call: lm(formula = Girth ~ Height + Volume, data = trees)

Residuals: Min 1Q Median 3Q Max -1.34288 -0.56696 -0.08628 0.80283 1.11642 

Coefficients: Estimate Std. Error t value Pr(>|t|)
 (Intercept) 10.81637 1.97320 5.482 7.45e-06 *** Height -0.04548 0.02826 -1.609 0.119 

Volume 0.19518 0.01096 17.816 < 2e-16 ***

Signif. codes: 0 ‘**’ 0.001 ‘’ 0.01 ‘’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.7904 on 28 degrees of freedom Multiple R-squared: 0.9408, Adjusted R-squared: 0.9366 F-statistic: 222.5 on 2 and 28 DF, p-value: < 2.2e-16

Checking for Mulitcollinarity


cor(trees) Girth Height Volume Girth 1.0000000 0.5192801 0.9671194 Height 0.5192801 1.0000000 0.5982497 Volume 0.9671194 0.5982497 1.0000000

Use the correlation code on the data frame. If the variables are close to 1 then there is a problem.


bptest(nmodel1)

Hypothesis Ho: is if the error variance (homoscedasticity) is equal Ha: Heteroscedasticity 
    studentized Breusch-Pagan test


data: nmodel1 BP = 3.1411, df = 2, p-value = 0.2079


dwtest(nmodel1)
    Durbin-Watson test


data: nmodel1 DW = 1.2146, p-value = 0.005783 alternative hypothesis: true autocorrelation is greater than 0



anova(nmodel1) Analysis of Variance Table

Response: Girth Df Sum Sq Mean Sq F value Pr(>F)
 Height 1 79.665 79.665 127.53 6.156e-12 *** **Volume 1 198.281 198.281 317.41 < 2.2e-16 *****

Residuals 28 17.491 0.625 

Signif. codes: 0 ‘**’ 0.001 ‘’ 0.01 ‘’ 0.05 ‘.’ 0.1 ‘ ’ 1



***************************************************************************************************; Make nice reports: ;)

library(stargazer)

stargazer(newdata, type = "text", title="Descriptive statistics", digits=1, out="table1.txt")

stargazer(ols, fixed, random, type="html", out="C:/STATS_COURSE/COURSE_MATERIAL/PANEL/DOCS/models.doc")

