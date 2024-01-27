## Setup for Survival Analysis ----

pkgs <- c(
    
    "survival",
    "ranger",
    "ggfortify"
)

install.packages(pkgs)
install.packages("ggfortify")

library(survival)
library(ranger)
library(tidyverse)
library(ggfortify)

glimpse(dataFModel)

ranger::au

## Load the data set ----

dataSurv <- readRDS("./data/data_modelling_V6_no_missing.rds")


dataSurv <- readRDS("./data/dataforsurvivalanalysis.rds")

glimpse(dataFModel)

dataSurv <- dataSurv %>%
    mutate(yearsactive = ifelse(is.na(maxdaysactive), 99, as.numeric(maxdaysactive/365)) )  %>%
    filter(XLABEL==1)  %>%
    mutate(yearsactive =as.numeric(maxdaysactive/365))


dataSurv <- dataFModel

View(dataSurv)

#To begin our analysis, we use the formula Surv(futime, status) ~ 1 and the survfit() function to 
#produce the Kaplan-Meier estimates of the probability of survival over time. The times parameter of 
#the summary() function gives some control over which times to print. Here, it is set to print the 
#estimates for 1, 30, 60 and 90 days, and then every 90 days thereafter. This is the simplest possible model. 
#It only takes three lines of R code to fit it, and produce numerical and graphical summaries.

dataSurv$XSTATUSCODE

km_fit <- survfit(Surv(XDURATION, XLABEL) ~ 1, data=dataSurv)
#plot(km_fit, xlab="Days", main = 'Kaplan Meyer Plot') #base graphics is always ready
autoplot(km_fit,title = "Survival Plot Lapse", xlab = "Time in Years", ylab = "Survival")

write.csv(rdatanSec,file="rdataSec.csv")

write.csv(dataFModel,file="dataFModel.csv")

glimpse(rdatanSec)

survfit(Surv(yearsactive, XSTATUSCODE) ~ 1, data=dataSurv)




km_trt_fit <- survfit(Surv(XDURATION, XLABEL) ~ PRIMGENDER, data=dataSurv)
autoplot(km_trt_fit,title = "Survival Plot Lapse", xlab = "Time in Years", ylab = "Survival")


plot(survfit(Surv(yearsactive, XSTATUSCODE) ~ 0, data = dataSurv), 
     xlab = "Years", 
     ylab = "Overall survival probability")

glimpse(dataSurvAge)
skimr::skim(dataSurvAge)
dataSurvAge <- dataSurv %>%
    mutate(AGE = ifelse((XPRIMAGE1 < 38), "Millenials", "Gen X & Elder"),
              AGE = factor(AGE))

km_AG_fit <- survfit(Surv(XDURATION, XLABEL) ~ AGE, data=dataSurvAge)
#https://www.rdocumentation.org/packages/ggfortify/versions/0.4.7/topics/autoplot.survfit
autoplot(km_AG_fit,main=c('Survival Analysis Plot'),ylab=c("Survival Probability"),xlab=c("Time in Years"))

    km_AG_fit2 <- survfit(Surv(XDURATION, XLABEL) ~ AGE +,PRODTYPE  data=dataSurvAge)
#https://www.rdocumentation.org/packages/ggfortify/versions/0.4.7/topics/autoplot.survfit
autoplot(km_AG_fit2,main=c('Survival Analysis Plot'),ylab=c("Survival Probability"),xlab=c("Time in Years"))

summary(dataSurvAge)

dataSurvAge %>%
    filter(X1PREMPERYEAR > 0) %>%
    summarise()


library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)



# Fit Cox Model
cox <- coxph(Surv(XDURATION, XLABEL) ~ AGE +PRODTYPE , data = dataSurvAge)
summary(cox)
cox_fit <- survfit(cox)
#plot(cox_fit, main = "cph model", xlab="Days")
autoplot(cox_fit)


# Fit Cox Model
cox <- coxph(Surv(XDURATION, XLABEL) ~  MXPRIMAGEINYEARS+COCODE + XPRIMRISKSTDCLASS + XSECPOLICYHOLDER + PRIMGENDER  + XSECGIND, data = dataSurvAge)
summary(cox)
cox_fit <- survfit(cox)
#plot(cox_fit, main = "cph model", xlab="Days")
autoplot(cox_fit)


aa_fit <-aareg(Surv(XDURATION, XLABEL) ~ AGE +PRODTYPE, data = dataSurvAge)
aa_fit



# ranger model
r_fit <- ranger(Surv(XDURATION, XLABEL)  ~ AGE + XMEDHINCOME + XUMEMPRT + XPRIMRISKSTDCLASS + XSECPOLICYHOLDER + PRIMGENDER   + XANNPLANNEDPREM,
                data = dataSurvAge,
                mtry = 4,
                importance = "permutation",
                splitrule = "extratrees",
                verbose = TRUE)


vi <- data.frame(sort(round(r_fit$variable.importance, 4), decreasing = TRUE))
names(vi) <- "importance"
head(vi)
