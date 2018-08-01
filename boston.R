library(ggplot2)
library(dplyr)
library(reshape2)
library(Metrics)
library(car)
library(MASS)


########################################
## Reading the Boston Housing Dataset ##
########################################

boston <- read.csv("boston.csv", header = T)

################################################################################
# CRIM : per capita crime rate by town.                                        #
# ZN : proportion of residential land zoned for lots over 25,000 sq.ft.        #
# INDUS : proportion of non-retail business acres per town.                    #
# CHAS : Charles River dummy variable (= 1 if tract bounds river; 0 otherwise).#
# NOX : nitrogen oxides concentration (parts per 10 million).                  #
# RM : average number of rooms per dwelling.                                   #
# AGE : proportion of owner-occupied units built prior to 1940.                #
# DIS : weighted mean of distances to five Boston employment centres.          #
# RAD : index of accessibility to radial highways.                             #
# TAX : full-value property-tax rate per $10,000.                              #
# PT : pupil-teacher ratio by town.                                            #
# B : 1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town.          #
# LSTAT : lower status of the population (percent).                            #
# MV : median value of owner-occupied homes in $1000s.                         #
################################################################################


########################################
## IF column name of CRIM is X...CRIM ##
########################################
names(boston)[1] <- "CRIM"
########################################

summary(boston$MV*1000)

quantile(boston$MV*1000, seq(0,1,0.01))

####################################################
# Minimum 1st Qu.  Median    Mean 3rd Qu.  Maximum #
# 5000   17025   21200   22533   25000   50000     #
####################################################

#######################
## Checking for NA's ##
#######################

length(which(is.na(boston)))


############
# Plots    #
############

## Scatter Plots
  melted_boston <- melt(boston[,-4], id.vars = "MV")

  ggplot(melted_boston, aes(x=value, y=MV, colour=variable)) +
  geom_point(alpha = 0.6) +
  stat_smooth() +
  facet_wrap(~variable, scales="free", ncol=3) +
  theme_minimal()+
  labs(x="Variable Value", y="Median House Price in $1000s")
  
## Bar Plot of CHAS
  ggplot(boston, aes(x=as.factor(CHAS)))+
    geom_bar(fill="lightseagreen")+
    theme_minimal() +
    labs(x="CHAS", y="Frequency")

## Density Plot ##
ggplot(boston, aes(x=MV*1000)) +
  stat_density(fill="lightsalmon1", alpha=0.6) +
  theme_minimal() +
  labs(x="Median Housing Price", y="Density")

######################
## Correlation Plot ##
######################


boston_cor <- round(cor(boston[,-4]),2)

melted_cormat <- melt(boston_cor)

ggplot(melted_cormat,aes(x=Var1,y=Var2,fill=value)) + 
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) + 
  scale_fill_gradient2(limits=c(-1, 1)) +
  labs(x="", y="")

  

##########################
## REGRESSION MODELLING ##
##########################

set.seed(100)

# Creating test indices
trainindices <- sample(1:nrow(boston), 0.75*nrow(boston))

# Split data
train <- boston[trainindices, ]
test <- boston[-trainindices, ]


############################
# Single Linear Regression #
############################
## For LSTAT and RM       ##
############################


#############
### LSTAT ###
#############

# Training Model For LSTAT
SLR_Model1 <- lm(MV~LSTAT, data = train)
summary(SLR_Model1)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -10.192  -4.322  -1.383   2.102  24.228 

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 35.30147    0.66288   53.25   <2e-16 ***
#   LSTAT       -0.99994    0.04718  -21.19   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 6.362 on 377 degrees of freedom
# Multiple R-squared:  0.5437,	Adjusted R-squared:  0.5424 
# F-statistic: 449.1 on 1 and 377 DF,  p-value: < 2.2e-16

## Residual Scatter Plot
ggplot(SLR_Model1)+
  geom_point(aes(x=.fitted*1000, y=.resid), color="slateblue1", alpha=0.8) +
  geom_hline(yintercept = 0, color="slateblue4") +
  theme_minimal() +
  labs(x="Predicted Price", y="Residual")


## Scatter plot
ggplot(test, aes(x=MV*1000, y=predict(SLR_Model1, test[,-14])*1000)) +
  geom_point(alpha=0.6, color="aquamarine3") +
  geom_abline(alpha=0.8, color="aquamarine4") +
  theme_minimal() +
  labs(x="Median Housing Price", y="Predicted Price")


# Calculating r and r-squared values

# r
cor(test$MV, predict(SLR_Model1, test[,-14]))
## 0.7283513

# r-squared
cor(test$MV, predict(SLR_Model1, test[,-14]))^2
## 0.5304956

##########
### RM ###
##########

# Training Model For RM
SLR_Model2 <- lm(MV~RM, data = train)
summary(SLR_Model2)

# Residuals:
#     Min      1Q  Median      3Q     Max 
# -24.669  -2.393   0.130   2.986  39.708 

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -37.0288     3.0242  -12.24   <2e-16 ***
# RM            9.5213     0.4761   20.00   <2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 6.56 on 377 degrees of freedom
# Multiple R-squared:  0.5148,	Adjusted R-squared:  0.5135 
# F-statistic:   400 on 1 and 377 DF,  p-value: < 2.2e-16


## Residual Scatter Plot
ggplot(SLR_Model2)+
  geom_point(aes(x=.fitted*1000, y=.resid), color="orchid3", alpha=0.8) +
  geom_hline(yintercept = 0, color="orchid4") +
  theme_minimal() +
  labs(x="Predicted Price", y="Residual")

## Scatter plot
ggplot(test, aes(x=MV*1000, y=predict(SLR_Model2, test[,-14])*1000)) +
  geom_point(alpha=0.6, color="dodgerblue3") +
  geom_abline(alpha=0.8, color="dodgerblue4") +
  theme_minimal() +
  labs(x="Median Housing Price", y="Predicted Price")

# Calculating r and r-squared values

# r
cor(test$MV, predict(SLR_Model2, test[,-14]))
## 0.7451573

# r-squared
cor(test$MV, predict(SLR_Model2, test[,-14]))^2
## 0.5552594


##############################
# Multiple Linear Regression #
##############################

model_1 <- lm(MV~., data=train)
summary(model_1)


# Performing StepAIC on model_1
stepAIC(model_1, direction = "both") 

# Removing AGE and INDUS as stepAIC suggests

model_2 <- lm(formula = MV ~ CRIM + ZN + CHAS + NOX + 
                RM + DIS + RAD + TAX + 
                PT + B + LSTAT, 
              data = train)

summary(model_2)

sort(vif(model_2), decreasing = T)

#===================================#
# variable  | vif        | p-value  #
#===================================#
# TAX       | 7.309419   | 0.018244 #
#===================================#

model_3 <- lm(formula = MV ~ CRIM + ZN + CHAS + NOX + 
                RM + DIS + RAD +
                PT + B + LSTAT, 
              data = train)

summary(model_3)

sort(vif(model_3), decreasing = T)

#===================================#
# variable  | vif        | p-value  #
#===================================#
# NOX       | 3.511627   | 3.35e-07 #
#===================================#
# DIS       | 3.295402   | 1.19e-10 #
#===================================#


## Since p-value is lowest for the higher vifs

## Checking for correlation between NOX and DIS
cor(boston$NOX, boston$DIS)
##  -0.7692301
## High negative correlation

## Checking vifs by removing one of them at a time.
### Removing NOX
model_3.1 <- lm(formula = MV ~ CRIM + ZN + CHAS +
                RM + DIS + RAD +
                PT + B + LSTAT, 
              data = train)

summary(model_3.1)

sort(vif(model_3.1), decreasing = T)

### Removing DIS
model_3.2 <- lm(formula = MV ~ CRIM + ZN + CHAS + NOX + 
                RM + RAD +
                PT + B + LSTAT, 
              data = train)

summary(model_3.2)

sort(vif(model_3.2), decreasing = T)

## VIF is lowered more by removing NOX
## R-squared and Adj R-Squared change is less by removing NOX

#===================================#
# variable  | vif        | p-value  #
#===================================#
# NOX       | 3.511627   | 3.35e-07 #
#===================================#

model_4 <- lm(formula = MV ~ CRIM + ZN + CHAS +
                  RM + DIS + RAD +
                  PT + B + LSTAT, 
                data = train)

summary(model_4)

sort(vif(model_4), decreasing = T)

#===================================#
# variable  | vif        | p-value  #
#===================================#
# RAD       | 2.256700   | 0.075527 #
#===================================#

model_5 <- lm(formula = MV ~ CRIM + ZN + CHAS +
                RM + DIS +
                PT + B + LSTAT, 
              data = train)

summary(model_5)

sort(vif(model_5), decreasing = T)

#===================================#
# variable  | vif        | p-value  #
#===================================#
# CRIM      | 1.421117   | 0.010167 #
#===================================#

model_6 <- lm(formula = MV ~ ZN + CHAS +
                RM + DIS +
                PT + B + LSTAT, 
              data = train)

summary(model_6)

sort(vif(model_6), decreasing = T)

#===================================#
# variable  | vif        | p-value  #
#===================================#
# ZN        | 2.027475   | 0.019812 #
#===================================#

model_7 <- lm(formula = MV ~ CHAS +
                RM + DIS +
                PT + B + LSTAT, 
              data = train)

summary(model_7)

sort(vif(model_7), decreasing = T)

#===================================#
# variable  | vif        | p-value  #
#===================================#
# B         | 1.183324   | 0.002921 #
#===================================#

model_8 <- lm(formula = MV ~ CHAS +
                RM + DIS +
                PT + LSTAT, 
              data = train)

summary(model_8)

sort(vif(model_8), decreasing = T)


## Predicting the prices

test$Predicted.Price <- predict(model_8, test[,-14])

# R and R-Squared for predicted values
r <- cor(test$MV, test$Predicted.Price)
r
rSquared <- cor(test$MV, test$Predicted.Price)^2
rSquared

## Residual Scatter Plot
ggplot(model_8)+
  geom_point(aes(x=.fitted*1000, y=.resid), color="mediumorchid4", alpha=0.8) +
  geom_hline(yintercept = 0, color="mediumpurple") +
  theme_minimal() +
  labs(x="Predicted Price", y="Residual")

## Scatter Plot of Predicted Price
ggplot(test, aes(MV*1000, Predicted.Price*1000)) +
  geom_point(alpha=0.6, color="turquoise") +
  geom_abline(color="turquoise") +
  theme_minimal() +
  labs(x="Median Housing Price", y="Predicted Price")
