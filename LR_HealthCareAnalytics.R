#Install Packages (devtools,broom,arm,gvlma,ggplot2,ggchart,nlme)
analytic = read.csv(file="/Users/akshayhirapara/Downloads/Ex_Files_Healthcare_Regression_R/Ch00/00_02/analytic.csv",
                    header = TRUE)

SLPTMWREG = lm(analytic$SLEPTIM2 ~ analytic$ALCGRP)
SLPTMWREG

summary(SLPTMWREG)

#Define a matric to discuss layut of graph, else only one will come
layout(matrix(c(1,2,3,4),2,2))
plot(SLPTMWREG, main = "Alcohol by Sleep Duration")


#########################################################################################################
#Build Model 1: Base Model

model1= lm(analytic$SLEPTIM2~analytic$DRKMONTHLY + analytic$DRKWEEKLY)

#Check the Summary

summary(model1)

#P-value of f-stat is 0.001657 hence it is statistically significant to use the model.We cannot use if incase
#value was above 0.05


#DRKMonthly is also highly significant, DRKWEEKLY is not

#Now Save Output as csv
library("devtools")
library(broom)

Tidy_Model1 = tidy(model1) #tidy up the model

write.csv(Tidy_Model1,file = "LinearRegressionModel1.csv")


#########################################################################################################
#Build Model 2: Adjusted for Sex and Age

model2 = lm(analytic$SLEPTIM2 ~ analytic$DRKMONTHLY + analytic$DRKWEEKLY + analytic$MALE + analytic$AGE2 
            + analytic$AGE3 + analytic$AGE4 + analytic$AGE5 + analytic$AGE6)

#Lets look at Summary Now
summary(model2)

#lets save the model
Tidy_Model2 = tidy(model2)

write.csv(Tidy_Model2,file = "LinearRegressionModel2.csv" )

#########################################################################################################
#Build Model 3: Forward Stelwise Modelling Proces

model3 = lm(analytic$SLEPTIM2~ analytic$DRKWEEKLY + analytic$AGE4 +analytic$AGE5 +analytic$AGE6)
summary(model3)


#Add smoke
model4 = lm(analytic$SLEPTIM2~ analytic$DRKWEEKLY + analytic$AGE4 +analytic$AGE5 +analytic$AGE6 + analytic$SMOKER)
summary(model4)


#ADD HISPANIC
model5 = lm(analytic$SLEPTIM2~ analytic$DRKWEEKLY + analytic$AGE4 +analytic$AGE5 +analytic$AGE6 
            + analytic$SMOKER + analytic$HISPANIC)
summary(model5)


#Add race
model6 = lm(analytic$SLEPTIM2~ analytic$DRKWEEKLY + analytic$AGE4 +analytic$AGE5 +analytic$AGE6 
            + analytic$SMOKER + analytic$HISPANIC + analytic$BLACK +analytic$ASIAN + analytic$OTHRACE)
summary(model6)

#Add Married
model7 = lm(analytic$SLEPTIM2~ analytic$DRKWEEKLY + analytic$AGE4 +analytic$AGE5 +analytic$AGE6 
            + analytic$SMOKER + analytic$HISPANIC + analytic$BLACK +analytic$ASIAN + analytic$OTHRACE
            +analytic$FORMERMAR +analytic$NEVERMAR)
summary(model7)

#Add FInal Model now

FinalModel = lm(analytic$SLEPTIM2~ analytic$DRKMONTHLY + analytic$DRKWEEKLY + analytic$AGE3 + analytic$AGE4 +analytic$AGE5 +analytic$AGE6 
                + analytic$SMOKER + analytic$HISPANIC + analytic$BLACK +analytic$ASIAN + analytic$OTHRACE
                +analytic$FORMERMAR +analytic$NEVERMAR + analytic$LOWED + analytic$SOMECOLL + analytic$INC2 +analytic$INC7
                +analytic$OVWT + analytic$OBESE +analytic$FAIRHLTH + analytic$POORHLTH)
summary(FinalModel)

#Save the Model
FinalModel_tidy =tidy(FinalModel)
write.csv(FinalModel_tidy, "FinalModelLinearRegression.csv")


#Plot the Coefficients
library(arm)

coefplot(FinalModel)

#Model Diagnostics
layout(matrix(c(1,2,3,4),2,2))
plot(FinalModel, main = "FinalModelPlot")


#We will use GVLMA now
library(gvlma)

gvmodel = gvlma(FinalModel)
summary(gvmodel)

#########################################################################################################
#Build Model 1: Base Model
library(e1071)

LOR_base = glm(analytic$ASTHMA4 ~ analytic$DRKMONTHLY + analytic$DRKWEEKLY, family = "binomial")
summary(LOR_base)

Tidy_LORbase = tidy(LOR_base)
write.csv(Tidy_LORbase," Base Model LOgistic Regression.csv")

#lets add the needed coeficients
#Add Odds Ratio
Tidy_LORbase$OddsRatio = exp(Tidy_LORbase$estimate)

#Add Lower Band and Upper Band (Log Lower Limit and Log Upper Limit)
Tidy_LORbase$LL = exp(Tidy_LORbase$estimate) - (1.96 * Tidy_LORbase$std.error)

#add Upper Band
Tidy_LORbase$UL = exp(Tidy_LORbase$estimate) + (1.96 * Tidy_LORbase$std.error)

#########################################################################################################
#Build Model 2: With Exposure Variables

LOR_exposure = glm(analytic$ASTHMA4 ~ analytic$DRKMONTHLY + analytic$DRKWEEKLY
                    +analytic$MALE +analytic$AGE2 +analytic$AGE3 +analytic$AGE4 
                   +analytic$AGE5 +analytic$AGE6 ,family = "binomial")
summary(LOR_exposure)

#Tidy it up

Tidy_LOR2 = tidy(LOR_exposure)

Tidy_LOR2$OddsRatio = exp(Tidy_LOR2$estimate)
Tidy_LOR2$LL = exp(Tidy_LOR2$estimate) - (1.96*Tidy_LOR2$std.error) 
Tidy_LOR2$UL = exp(Tidy_LOR2$estimate) + (1.96*Tidy_LOR2$std.error) 

write.csv(Tidy_LOR2,"Logistic Regression Exposure Model.csv")

#########################################################################################################
#Build Model 3: Step Forward Model
#Remove Age

lor1 = glm(analytic$ASTHMA4~ analytic$DRKWEEKLY + analytic$MALE, family = "binomial")
summary(lor1)

#Add Smoker and Hispanic
lor2 = glm(analytic$ASTHMA4~ analytic$DRKWEEKLY + analytic$MALE +analytic$SMOKER +analytic$HISPANIC,family = "binomial")
summary(lor2)

#Hispanic is not relevant

#LOR3
lor3 = glm(analytic$ASTHMA4~ analytic$DRKWEEKLY + analytic$MALE +analytic$SMOKER 
           +analytic$OTHRACE,family = "binomial")
summary(lor3)


#Lets run Fimnal Model Directly now

FinalLOR = glm(analytic$ASTHMA4~ analytic$DRKMONTHLY + analytic$DRKWEEKLY + analytic$MALE 
              +analytic$AGE2 +analytic$AGE5 +analytic$LOWED +analytic$NEVERMAR
              +analytic$INC1 +analytic$INC2 +analytic$INC3 +analytic$INC4 +analytic$OVWT +analytic$OBESE
              +analytic$SMOKER +analytic$NOEXER +analytic$FAIRHLTH +analytic$POORHLTH +analytic$OTHRACE,family = "binomial")

summary(FinalLOR)


#TIdy it
Tidy_FinalLOR = tidy(FinalLOR)
Tidy_FinalLOR$OddsRatio = exp(Tidy_FinalLOR$estimate)

Tidy_FinalLOR$LL = exp(Tidy_FinalLOR$estimate) - (1.96 * Tidy_FinalLOR$std.error)

Tidy_FinalLOR$UL = exp(Tidy_FinalLOR$estimate) + (1.96 * Tidy_FinalLOR$std.error)

#Estimate in Chart
library(ggplot2)
plot = ggplot(data = Tidy_FinalLOR, aes(x= term, y =OddsRatio , ymin = LL, ymax= UL))+
  geom_pointrange(aes(col = factor(term),position = position_dodge(width = 0.30)), +ylab(" ODDS RATIO at 95% CI")+
                    geom_hline(aes(yintercept = 1))+ scale_color_discrete(name = "Term")+
                    xlab("")
  

summary(FinalModel)






