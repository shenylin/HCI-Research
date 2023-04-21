#Fireside Exploration Analysis (0915 updated)

install.packages("psych")
install.packages("dplyr")
library(psych)
library(dplyr)


#Setup Working Directory
setwd("~/Downloads/ACTION Lab")

#Read in Data
library(readr)
FEData.raw <- read_csv("Fireside Exploration Dataset - Combined Dataset_0913updated.csv", col_names = TRUE)
head(FEData.raw) 
View(FEData.raw) 


#Cronbach alpha value - likeability, intelligence, trust
install.packages("ltm")
library(ltm)
cronbach.alpha(FEData.raw[c(18:22)]) #likeability
cronbach.alpha(FEData.raw[c(24,25,27,28,29)]) #perceived_intelligence
cronbach.alpha(FEData.raw[c(31,32,34)]) #perceived_trust

#Check Variable Names
FEData<-FEData.raw[c(17,23,30,35)]
FEData
summary(FEData)
names(FEData)

#Check Data structure
str(FEData) 
class(FEData$Formality_Group)

#Check Missing Data
sum(is.na(FEData))  #No Missing Data


#one-way MANOVA: main effects. Report F values, p values, SE, and effect size.
dependent_vars <- cbind(FEData$likeability, FEData$perceived_intelligence, FEData$perceived_trust)
independent_var <- FEData$Formality_Group


FEData.manova_model <- manova(dependent_vars ~ independent_var, data = FEData)
summary(FEData.manova_model)


### effect size
install.packages("effectsize")
library(effectsize)
eta_squared(FEData.manova_model)


## T-test: age group
install.packages("vtable")
library(vtable)
FE_AGE <- FEData.raw[c(7,9)]
FE_AGE
str(FE_AGE)
t.test(FE_AGE$Age ~ FE_AGE$age_group, var.equal=TRUE)
st(FE_AGE, group = 'age_group', group.test = TRUE)


#MANOVA: Using both "formality" and "age_group" as IVs
FEData2<-FEData.raw[c(7,17,23,30,35)]
FEData2
dependent_vars <- cbind(FEData2$likeability, FEData2$perceived_intelligence, FEData2$perceived_trust)
independent_var <- cbind(FEData2$Formality_Group, FEData2$age_group)

FEData2.manova_model <- manova(dependent_vars ~ independent_var, data = FEData2)
summary(FEData2.manova_model)

### effect size
install.packages("effectsize")
library(effectsize)
eta_squared(FEData2.manova_model)


#ANOVA: formality -> likeability
AOV_L <- FEData.raw[c(17,23)]
summary(AOV_L)
str(AOV_L)
AOV_L$Formality_Group <- as.factor(AOV_L$Formality_Group)
class(AOV_L$Formality_Group)
library(psych)
describe(AOV_L)

bartlett.test(likeability ~ Formality_Group, data=AOV_L)
AOV_L.op <- aov(likeability ~ Formality_Group, data=AOV_L)
summary(AOV_L.op)

#ANOVA: formality -> intelligence
AOV_I <- FEData.raw[c(17,30)]
summary(AOV_I)
str(AOV_I)
AOV_I$Formality_Group <- as.factor(AOV_I$Formality_Group)
class(AOV_I$Formality_Group)
library(psych)
describe(AOV_I)

bartlett.test(perceived_intelligence ~ Formality_Group, data=AOV_I)
AOV_I.op <- aov(perceived_intelligence ~ Formality_Group, data=AOV_I)
summary(AOV_I.op)

#ANOVA: formality -> perceived_trust
AOV_T <- FEData.raw[c(17,35)]
summary(AOV_T)
str(AOV_T)
AOV_T$Formality_Group <- as.factor(AOV_T$Formality_Group)
class(AOV_T$Formality_Group)
library(psych)
describe(AOV_T)

bartlett.test(perceived_trust ~ Formality_Group, data=AOV_T)
AOV_T.op <- aov(perceived_trust ~ Formality_Group, data=AOV_T)
summary(AOV_T.op)