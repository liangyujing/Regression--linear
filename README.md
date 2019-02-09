# Linear Regression Assumptions and Diagnostics in R: Essentials 

rm(list=ls())

# Set your working dir as the current dir
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
my_data <- read.csv(file ="R code/regression-assignments 1+ 2/ExperienceSampling_Group7.csv", head=T,sep=";") 

## Standardizing the data:
mydata <- scale(my_data) 

## 1.Building a regression model
#We build a model to predict sales on the basis of advertising budget spent in youtube medias.
model <- lm(y=beepnum ~ x=PA, data = my_data)
model
##Our regression equation is: y = Intercept + PA*x, 
##that is sales = 27.2704 + 0.3316*PA


## 2. Fitted values and residuals
##residual errors: difference between observed (or measured) sale values and the predicted sale values. 
#install.packages("broom")
library.dynam("broom")
model.diag.metrics <- augment(model)
head(model.diag.metrics)

#PA: the invested PA advertising budget
#sales: the observed sale values
#fitted: the fitted sale values
#resid: the residual errors

ggplot(model.diag.metrics, aes(youtube, sales)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = PA, yend = .fitted), color = "red", size = 0.3)

## 3.Regression diagnostics {reg-diag}
##Diagnostic plots
par(mfrow = c(2, 2))
plot(model)

# 3.1 Linearity of the data
#Residuals vs Fitted plot
plot(model, 1)
##Ideally, the residual plot will show no fitted pattern. 

## 3.2 Homogeneity of variance
#spread-location plot
plot(model, 3)

## to reduce the heteroscedasticity problem is to use a log or square root transformation of the outcome variable (y).
model2 <- lm(log(sales) ~ youtube, data = marketing)
plot(model2, 3)

## 3.3 Normality of residuals
plot(model, 2)

## 3.4 Outliers and high levarage points
plot(model, 5)
