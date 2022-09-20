## Goal of this assignment
# to review basic stats functions and assumption checking in R

## How to complete this assignment
# read through the comments and run the code below, writing and running your own code where indicated
# whenever you see a comment line that begins with Q, discuss that question with your partner and answer it together in a one or two line comment
# turn in your modified code by emailing the R file to me at ojensen@wisc.edu before the next class meeting (due 9am Monday, September 19)
# Hat tip: Modified from original assignment by Rae Winfree

## SOME HANDY THINGS TO KNOW, IF YOU ARE NEW TO R STUDIO
# you can set your working directory under Files, by clicking on the file path you want one folder at a time, then choosing 'more' and 'set as working directory'
# to run code from the script window, highlight it and hit control[command]+enter
# to run code in the console, just hit return
# R studio will highlight the opening parenthesis of the closing parenthesis you are typing
# if you want to put some text in quotes, highlight the text and type quote mark (single and double work the same)
# when you type the first parenthesis following a function, a yellow box will show you the arguments of that function

## Outline
# correlation
# regression
# anova
# multiple regression and ancova
# extra credit exercises

# preliminaries
# clear R's brain
rm(list = ls())
# load libraries
library(readr)
library(ggplot2) 
library(dplyr)
library(datasets)
# get data
# mtcars data are from 1974 Motor Trend car road tests
# click on mtcars in environment pane to see the row names (observations), which in this case are types of cars
# search help pane for mtcars to get more info on the data
data(mtcars)
glimpse(mtcars)

## correlation

# our correlation question: are car weight and mpg correlated?

# first, data picture
ggplot(mtcars, aes(x = wt, y = mpg)) +
        geom_point()

# second, stats test
cor(mtcars$mpg, mtcars$wt, method = "pearson")

# third, check assumptions, in this case that both x and y are normally distributed
qqnorm(mtcars$mpg)
qqnorm(mtcars$wt)

# Q: put in words how the wt distribution differs from a normal distribution, along the lines of what was discussed in lecture
# check your intuition
#Weight seems to fit almost a normal distribution except when the weight is greater than 5. 
#Those three outliers pull the distribution away from normality.
#A Shapiro-Wilk normality test has a p-value more than 0.05, so we can assume normal distribution.
shapiro.test(mtcars$wt)
shapiro.test(mtcars$mpg)

# Q: Is the Spearman or Pearson correlation a better choice here and why?
#Pearson correlation would be a better choice here since both variables have a p-value greater than 0.05 for 
#the Shapiro-Wilk test

hist(mtcars$wt)
# while you're at it, check your other variable. should always look at histogram along with qq plot
hist(mtcars$mpg)
# Q: why do you think the histograms look only iffily normal, while the qq plots look pretty good?
#If you adjust the breaks on the histogram it matches more the qqplots which are a visual comparison to a standard 
#normal distribution

# fourth, interpret results
# oops, we forgot to get a p value!
# which is actually good, because we shouldn't be thinking about the results until after checking assumptions
cor.test(mtcars$mpg, mtcars$wt)
#With a p-value less than 0.05, there is a statistically significant negative relationship between the weight of a car and
#the miles per gallon. 
        
## regression

# preliminaries
# clear R's brain
rm(list = ls())
# load libraries
library(readr)
library(ggplot2) # autoplot
library(ggfortify) # ggplot needs this to use autoplot for lm
library(dplyr)
library(datasets)
# get data
data(mtcars)
glimpse(mtcars)
# 'tidy data' means that each column is one variable, and each row is one observation
# Q: are these data tidy? why or why not?
#These data are not tidy since the rows have car names.

# our regression question: does car weight predict mpg?

# first, data picture
ggplot(mtcars, aes(x = wt, y = mpg)) +
        geom_point()
# think about the analysis before doing it
# Q: what do you expect the slope to be, roughly?
#-3
# Q: the intercept?
#35
# Q: the df for the error?  Why?
#32 cars -1 = 31
# you should also use the data picture to check assumptions informally to decide what model to run
# linearity and homoskedasticity look pretty good, so try lm()
# note, you will need to run and save the regression model in order to get more detailed checks on assumptions

# second, run the linear model
# R reads the lm function as saying, 'fit a linear model, where we predict mpg as a function of car weight, using variables from the mtcars data frame'
car_mod <- lm (mpg ~ wt, data = mtcars)

# third, check assumptions
# first look at a histogram of the residuals
hist(car_mod$residuals)
# autoplot() produces same figures that plot() does in base R
autoplot(car_mod)
# the default blue lines are distracting and don't mean much, remove them next time
autoplot(car_mod, smooth.colour = NA)
# Q: for each of the 4 figures, write down what you are looking for and what you found 
#Residuals vs. Fitted- The residuals seem to bounce around randomly suggesting the relationship is linear.
#Normal Q-Q- both variables seem to come from the same distribution as it roughly forms a straight line.
#Scale-Location- Here it seems the spread of magnitude is lowest in the fitted values closest to 0 but the fitted values are roughly 
#the same suggesting the data might not be heteroskedastic. 
#Residuals vs. Leverage- the high leverage might have a greater influence on the model. 
#The residual's spread shouldn't change as a function of leverage.

# fourth, interpret stats
# summary() gives you the regression-type output including the coefficients
summary(car_mod)
# for more details look under environment / values
# Q: are the slope, intercept, and error df roughly what you expected?
#They are roughly what I expected: slope = -5, intercept = 37 and df = 30 (i calculated the total not the error).
# anova() gives you anova-type output such as SS
anova(car_mod) 
# Q: compare the mean square for wt versus residuals. what does this result mean, and how does it lead to the p value?
#The mean square of the residuals is an estimator of variance. The results mean there is a difference in the means with a p-value less than 0.05.

# fifth, plot the model back onto the data 
# first extract the coefficients from the model output
# look at the output under Environment / Values first, to make sure you are extracting the right stuff
car_mod_intercept = coef(car_mod)[1]
car_mod_slope = coef(car_mod)[2]
# Q: what could happen if you didn't define the slope and intercept terms like this, but just typed in the numerical values instead?
#It might not have been the same significant figures as the model output, however, this example it is the same.
ggplot(mtcars, aes(x = wt, y = mpg)) + 
  geom_point() + 
  geom_abline(intercept = 37.2851, slope = -5.3445) +
  xlim(0,6) + ylim(10,40)

# use geom_abline to plot using slope and intercept
# use manual x and y limits to override the defaults and see the intercept
ggplot(mtcars, aes(x = wt, y = mpg)) + 
        geom_point() + 
        geom_abline(intercept = car_mod_intercept, slope = car_mod_slope) +
        xlim(0,6) + ylim(10,40)



	
# add the confidence intervals
mtcars$lwr =  predict.lm(car_mod, interval="confidence", level = 0.95)[,"lwr"]
mtcars$upr =  predict.lm(car_mod, interval="confidence", level = 0.95)[,"upr"]
# Q: what is the interpretation of the confidence interval you just created?
#95% confidence defines a range of values taht you can be 95% certain contains the population mean.
# Q: if you used interval="prediction" instead, what would be the interpretation? would the ribbon be wider or narrower?
#Wider
# make the data + model results picture
ggplot(mtcars, aes(x = wt, y = mpg)) + 
        geom_point() + 
        geom_abline(intercept = car_mod_intercept, slope = car_mod_slope) +
        geom_ribbon(data=mtcars,aes(ymin=lwr,ymax=upr),alpha=0.3)  # alpha makes things transparent



## some extra tools for doing regression

# extra tool 1:  standardizing variables
# let's center the wt variable, so that we can interpret the intercept as the mpg of a car of average weight, as opposed to the mpg of a car of weight=0 
# Q optional: why will the slope, p value, R2 all be the same?
# you basically just slide the data to the mean of center so you don't change the slope.

mtcars$wt_centered = scale(mtcars$wt, center=TRUE, scale=FALSE) # center=true subtracts the mean from all data values, scale=true divides all data values by the sd 
car_mod2 = lm(mpg ~ wt_centered, data = mtcars)
summary(car_mod2)

ggplot(mtcars, aes(x = wt_centered, y = mpg)) + 
        geom_point() + 
        geom_abline(intercept = coef(car_mod2)[1], slope = coef(car_mod2)[2]) +
        geom_vline(xintercept = 0, linetype="dashed") 

# say we want to standardize both variables, i.e., do the analysis in terms of z scores
# clearly now the slope and intercept will both be different, but the R2 and significance should be the same
# Q optional: what are the units of a z score?  why? how does this make a z score particularly useful (or not)?
#z scores does not have any units. It just represents the number of st. deviations from the mean. Z scores can be
#useful when doing comparisons from different normal distributions.
mtcars$wt_z = scale(mtcars$wt, center = TRUE, scale = TRUE) 
mtcars$mpg_z = scale(mtcars$mpg, center = TRUE, scale = TRUE)
car_mod3 = lm(mpg_z ~ wt_z, data = mtcars)
summary(car_mod3)

ggplot(mtcars, aes(x = wt_z, y = mpg_z)) + 
        geom_point() + 
        geom_abline(intercept = coef(car_mod3)[1], slope = coef(car_mod3)[2]) +
        geom_vline(xintercept = 0, linetype="dashed") +
        geom_hline(yintercept = 0, linetype="dashed")



## anova 
# preliminaries
rm(list = ls())
library(datasets)
library(ggplot2)
library(ggfortify)
library(car) # for levene's test  
library(mosaic) # for contrasts. library(multicomp) is another option
glimpse(iris)

# our anova question: do irises of different species have different sepal widths?

# first, data picture
ggplot(iris, aes(x = Species, y = Sepal.Width)) +
        geom_boxplot() +
        geom_point(color = "purple", alpha = 0.5) +
        theme_bw()
# Q: what assumptions of anova can you roughly check by eye, and how do they look?
#Equal variance. They look fairly equivalent.

# second, run model
iris_mod <- lm (Sepal.Width ~ Species, data = iris)

# third, check assumptions
autoplot(iris_mod, smooth.colour = NA)
# Q: for each of the four plots, say what assumption you were checking, how you checked it (i.e. what you looked for in the plot), and what you found
# good idea to check the variances of the groups separately, since that is the assumption of anova actually and autoplot doesn't do it
#Residuals Vs. Fitted: Homogeneity of variances; no obvious relationship between residuals and fitted values; assume homogeneity.
#Normal Q-Q: Normal distribution; they seem to fit the line and so would assume normality
#Scale-Location; Homoscedasticity; spread of residuals is roughly equal to all fitted values/no clear pattern; assumption met 
#Constant leverage: shows how influential certain data points are; the spread looks even; results don't seem to be heavily influenced by one point.
# levene's test for homogeneity of variances
leveneTest(iris_mod)
# looks okay

# fourth, interpret results
summary(iris_mod)
# Q: what does the estimate for the intercept mean? what do the t values mean? what does the R2 tell you?  the F and its p value?
#Intercept- estimate of the dependent variable when all the independent variables are 0.
#T-value- difference between two means from the null value; A large t-score shows the groups are different while a small shows similarity.
#R2- proportion of the variation in the dependent variable accounted by the explanatory variables.
#F and p-value: F is the ratio the mean square value of factor and error. A higher F would be higher the variation between sample means relative to the variation within the samples.
#The higher the F-value, the lower the p-value.

# if you wanted to force R to compare the group means to the grand mean instead of using a reference group, you could do that by suppressing the intercept and centered the outcome variable on zero
# it's a bit complicated though so we are not going to do that here
anova(iris_mod)
# Q: why are the df what they are? what do the two MS values tell you?
#Df for species would be 3 species - 1 = 2
#Df for residuals is the total number of observations or rows of the dataset substracted by the number of variables actually being estimated.
#150-3 = 147
# the null hypothesis we have just rejected is that all groups come from populations with the same mean
# that is, the F value suggests this is unlikely
# but anova itself does not tell you which group mean(s) are different from which other(s)
# need a tukey test for comparing all pairwise means
TukeyHSD(iris_mod, ordered = TRUE)
# Q: interpret your results
#All three species have statistically different means.
# check your results, at least the diff column
by_sp <- group_by(iris, Species)
summarize(by_sp, meanSW = mean(Sepal.Width))

# remember R orders levels, and thus chooses the reference level, alphabetically
# check the ordered list of levels:
levels(iris$Species)
# you can reorder the factor levels by giving it a new list
iris$Species = factor(iris$Species, levels = c("virginica", "versicolor", "setosa"))
levels(iris$Species)
# now your reference level and therefore some of your output will be different, although the F and p value should be the same of course
iris_mod <- lm (Sepal.Width ~ Species, data = iris)
summary(iris_mod)
# Q:  what components of the summary() output change when you reorder levels, and why?  what components stay the same and why?
#Nothing changed
# CHECK need to sort out answer to the above Q 


# fifth, plot model results back onto the data picture
# we can easily get the mean for each level and the associated se using the predict() function
iris_mean_se = predict(iris_mod, se=T, newdata=data.frame(Species=c("virginica", "versicolor", "setosa")))
# make a new dataframe to hold the mean and se output from the anova
output= data.frame(Species = c("virginica", "versicolor", "setosa"), 
	mean = iris_mean_se[[1]],
	se = iris_mean_se[[2]])
output
# now that we made a new dataframe, R has assigned the species order as alphabetical again, so...
output$Species = factor(output$Species, levels = c("versicolor", "virginica", "setosa"))
levels(output$Species)
# plot the means with error bars (+/- 1 SE)
ggplot(output, aes(x = Species, y = mean)) +
	geom_point(size=3) +
	geom_errorbar(data=output, aes(x=Species, ymin=mean-se, ymax=mean+se), width=.25)+
	ylab("model estimated mean +/- 1 SE") +
	theme_bw()
# plot the means with 95% CIs
ggplot(output, aes(x = Species, y = mean)) +
	geom_point(size=3) +
	geom_errorbar(data=output, aes(x=Species, ymin=mean-1.96*se, ymax=mean+1.96*se), width=.25)+
	ylab("model estimated mean and 95% CI") +
	theme_bw()
# check that you got the right numbers by comparing the means in your boxplots to the below
# we did this already above, but now let's do the same thing using piping from dplyr
check_means <- iris %>%
        group_by(Species) %>%
        summarize(meanSW = mean(Sepal.Width))
check_means
# we still need to add anova results to indicate which group mean(s) are different from which other(s)
# but, the significance stars and letters are easier to add in keynote/powerpoint/adobe than R, so we won't do that here





## multiple regression 

# preliminaries
# clear R's brain
rm(list = ls())
# load libraries
library(readr)
library(ggplot2) 
library(ggfortify) 
library(dplyr)
library(datasets)

# get data
data(mtcars)
mtcars

# our multiple regression question: do car weight and horsepower predict mpg?

# first, data pictures
ggplot(mtcars, aes(x = wt, y = mpg)) +
        geom_point()
ggplot(mtcars, aes(x = hp, y = mpg)) +
        geom_point()
ggplot(mtcars, aes(x = hp, y = wt)) +
        geom_point()

# second, run model
car_mod2 <- lm(mtcars$mpg ~ wt + hp, data=mtcars)
summary(car_mod2)

# third, check assumptions the same as simple linear regression
hist(car_mod2$residuals)
autoplot(car_mod2, smooth.colour = NA)
# Q: for each plot, write down what you would conclude
#Residuals vs Fitted: residuals and fitted values are uncorrelated
#Normal Q-Q: normal distribution for all variables
#Scale-Location: homoskedastic
#Residuals vs. Leverage: There are a few outliers that might pull the model  (~4)

# for multiple regression, also very important to check for multicollinearity
# first have a look at correlation of the two predictors:
# anything less than 0.7 is usually viewed as nothing to worry about, but bivariate correlations like this don't really determine how the model will be affected
cor(mtcars$wt, mtcars$hp)
# a better metric is the variance inflation factor, which actually measures how the model is affected
# as a guideline, vif should be less than 10 for each predictor
vif(car_mod2)


# fourth, interpret results
# Q: interpret your results
#VIF is less than 10 for each predictor suggesting there is no multicollinearity
        
# fifth, plot results onto data picture
# this is difficult with 2 predictors--the most straightforward way to visualize would be a 3d plot:
library(scatterplot3d)
plot3d <-scatterplot3d(mtcars$wt,mtcars$hp,mtcars$mpg, pch=16, type="h", main="3D Scatterplot")
plot3d$plane3d(car_mod2)

# another useful method is a partial residual plot, which plots the effect of each predictor after controlling for the effect of the others
# warning, partial residual plots are not recommended when dealing with interactions, although this is sometimes done anyway, and I don't know exactly what the issues are
par(mfrow=c(1,2))
termplot(car_mod2, partial.resid=TRUE, col.res = "black")
# or equivalently:
library(car)
crPlots(car_mod2)
# set the par back now in case people run all the following stuff without closing their graph window
par(mfrow=c(1,1))



## ancova
# this exercise is modified from Beckerman et al 2017, Getting started with R
# preliminaries
# clear R's brain
rm(list = ls())
# load libraries
library(readr)
library(ggplot2) 
library(ggfortify) 
library(dplyr)
library(datasets)
# get some data on egg-laying by limpets
limpet <- read_csv("data/limpets.csv") #Note: need to have a valid path here
# click dataset name in environment / data to take a look
glimpse(limpet)

# our ancova question: does the mean number of eggs laid by limpets vary by limpet density and/or season?

# first, data picture
ggplot(limpet, aes(x = DENSITY, y = EGGS, color = SEASON)) +
        geom_point() +
        scale_color_manual(values = c(spring="green", summer="red")) +  # this is how you set colors manually in ggplot
        theme_bw()
# Q: what is your prediction about the answer to the ancova question?
#Looks like there are more eggs at lower densities and more eggs in the spring versus the summer across the density gradient.
# Q: use the picture to estimate the slope for each season. what are the units of the slope?
#Spring slope = -0.04 change in eggs/change in density
#Summer slope = -0.04
# Q: do you expect the effect of density depends on season? that is, do you predict an interaction? explain why or why not
#We did not predict an interaction because the seasonality seemed to have the same effect across all densities.

# second, run model
limpet_mod <- lm(EGGS ~ DENSITY + SEASON + DENSITY:SEASON, data = limpet)
# look at all the output info R stores for you, under Values
names(limpet_mod)

# third, check assumptions
autoplot(limpet_mod, smooth.colour = NA)

# fourth, interpret results
# this gets a bit hairy, for ancova. both summary() and anova() are relevant
anova(limpet_mod)
# here is an interpretation of the anova table from the top row down
# R estimated a joint slope for both seasons, and it explained lots of variance, with MS 5.0
# R then estimated different intercepts for the two seasons, and that explained a bit more variance, for a MS of 3.3
# Last, R allowed the slopes to vary (the interaction), but doing so didn't explain much; MS 0.1

summary(limpet_mod)
# coefficients table: here is where we get our intercept and slope
# Q: write the model equation for egg production in the spring in y = a + bx form, filling in all coefficients and variable names
#y = 2.664166 (eggs) -.033565 (change in density)x
# SEASONsummer estimate is the difference between spring and summer in terms of egg production. in other words, a shift in the intercept
# Q: write the model equation for egg production in the summer in y = a + bx form, filling in all coefficients and variable names
#y = 1.85 (eggs) -.033565 (change in density)x

# you could use the DENSITY:SEASONsummer coefficient to flatten the slope by 0.003, but it won't make much difference, and anyway, wasn't significant
# the bottom line: R2 of 0.67, p<0.0001
# t test asks if the difference between 2 values is different from zero. so, what two values? is the question
# intercept: is intercept different from 0. yes
# density: is slope of density different from 0. yes
# SEASONsummer: is difference between the intercept of summer and spring different 0. yes
# DENSITY:SEASONsummer: is difference between the slopes of density between spring and summer different from 0. no


# fifth, plot results onto data picture
# we can just plot the estimated lines on the original data picture, being sure to add the coefficients correctly
ggplot(limpet, aes(x = DENSITY, y = EGGS, color = SEASON)) +
        geom_point() +
        scale_color_manual(values = c(spring="green", summer="red")) +  
		geom_abline(intercept = coef(limpet_mod)[1], slope = coef(limpet_mod)[2], colour="green") +
		geom_abline(intercept = (coef(limpet_mod)[1]+coef(limpet_mod)[3]), slope = (coef(limpet_mod)[2]+coef(limpet_mod)[4]), colour="red") +
        theme_bw()







