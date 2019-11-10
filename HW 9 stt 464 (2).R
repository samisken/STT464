#STT 464 HW 9
hw9_data <- read.table(file = "clipboard", header=TRUE)
Strain_3DOK1_mean <- mean(hw9_data$Nitrogen[hw9_data$Strain=="3DOK1"])


x <- read.table(file = "clipboard", header=TRUE)
x$Strain <- as.factor(x$Strain) 
boxplot(x$Nitrogen ~ x$Strain)
anova(lm(x$Nitrogen ~ x$Strain))

#####################################
#####Check Assumptions
# get residuals

# this will ask the output of the model fitting to be written in the new data set, e.g., 'fitresults'
fitresults<-lm(x$Nitrogen ~ x$Strain)

# the 'residuals' function will extract the residuals from the data set 'fitresults'
# we will call the variable that contains residuals 'res1'
res1<-residuals(fitresults)

#if we want to do a plot of residuals vs predicted values we also need to get predicted values
# the function that we will use for that is 'predict'
pred1<-predict(fitresults)
#########

# check normality
# we can look at
#histograms
hist(res1)

#boxplots
boxplot(res1)

#normal probability plot of the residuals
qqnorm(res1)
# qqline will add a line corresponding to the normal distribution to the plot
qqline(res1)
#########

# check equal variance assumption
#side-by-side box plot of the residuals
boxplot(res1 ~ x$Strain)

#plot of residuals vs predicted values
plot(pred1,res1)

# Levene's test for equal variances
# create squared residuals, here we are creating a new variable res1sq by squaring the original residulas res1
res1sq<-res1*res1
#run ANOVA for squared residuals as the response variable
anova(lm(res1sq ~ x$Strain))
#########

library(emmeans)
library(multcompView)
# need to run ANOVA first and write the outcome of the model fitting
trt.model = lm(Nitrogen ~ Strain, data=x)

#Tukey's HSD  
trt.means.HSD <- emmeans(trt.model, "Strain", adjust='tukey') #write the means in a separate data set
pairs(trt.means.HSD, adjust='tukey') #get p-values for all pair-wise comparisons
cld(trt.means.HSD,alpha=0.05,  Letters=letters, adjust='tukey')  #get letters for mean separations       

#LSD
trt.means.LSD <- emmeans(trt.model, "Strain", adjust='none') 
pairs(trt.means.LSD, adjust='none')
cld(trt.means.LSD,alpha=0.05,  Letters=letters,  adjust="none") 
