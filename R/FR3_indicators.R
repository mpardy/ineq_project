# ------------------------------------------------------------------------
#
# Part3: Indicators
# Group FR
#
# -------------------------------------------------------------------------

library(dplyr)
library(survey)
library(convey)
library(srvyr)

l#oad("data/p1.rda")
#load("data/p2.rda")


# Creating Survey Objects -------------------------------------------------

svy.p1 <- svydesign(ids =  ~ id_h,
                    strata = ~rb020,
                    weights = ~rb050,
                    data = silc.p1) %>% convey_prep()

svy.p2 <- svydesign(ids =  ~ id_h, 
                    strata = ~rb020,
                    weights = ~rb050,
                    data = silc.p2) %>% convey_prep()

# Indicators --------------------------------------------------------------

## P1: EQUAL SHARING OF RESOURCES; WHOLE SAMPLE-----------------------------

# Mean

mean.p1 <- svyby(~y11 + ~y12 + y13, ~rb010, svy.p1, svymean, keep.var = FALSE)

# Median

median.p1 <- svyby(~y11 + ~y12 + y13, ~rb010, svy.p1, svyquantile, quantiles=0.5, keep.var = FALSE)

# Gini

gini11 <- svyby(~y11, ~rb010, svy.p1, svygini, keep.var = FALSE)

gini12 <- svyby(~y12, ~rb010, svy.p1, svygini, keep.var = FALSE)

gini13 <- svyby(~y13, ~rb010, svy.p1, svygini, keep.var = FALSE)

gini.p1 <- as.data.frame(cbind(gini11[,2], gini12[,2], gini13[,2]))

# P80/20

p80.p1 <- svyby(~y11 + ~y12 + y13, ~rb010, svy.p1, svyqsr)

# Top 10% share

#create svydesign for top 10%:

svy.p1.top <- subset(svy.p1, y11 >= as.numeric(
  svyquantile(~y11, svy.p1, quantile=c(0.9))))

topnum <- svyby(~y11, ~rb010, svy.p1.top, svytotal)

topden <- svyby(~y11, ~rb010, svy.p1, svytotal)

top11 <- topnum / topden


## P2: PARTIAL SHARING OF RESOURCES, RESTRICTED SAMPLE (>=20 YEARS)---------


#Mean

mean.p2 <- svyby(~y21 + ~y22 + y23, ~rb010, svy.p2, 
                 svymean, keep.var = FALSE)

# Median

median.p2 <- svyby(~y21 + ~y22 + y23, ~rb010, svy.p2, 
                   svyquantile, quantiles=0.5, keep.var = FALSE)

#Gini

gini21 <- svygini(~y21, svy.p2)

gini22 <- svygini(~y22, svy.p2)

gini23 <- svygini(~y23, svy.p2)

gini.p2 <- as.data.frame(cbind(gini21[,2], gini22[,2], gini23[,2]))

# P80/20

p80.p2 <- svyby(~y21 + ~y22 + y23, ~rb010, svy.p2, svyqsr)

# Top 10% share

# create svydesign for top 10%:

svy.p2.top <- subset(svy.p2, y21 >= as.numeric(svyquantile(~y21, svy.p2, quantile=c(0.9))))

topnum <- svyby(~y21, ~rb010, svy.p2.top, svytotal)

topden <- svyby(~y21, ~rb010, svy.p2, svytotal)

top21 <- topnum / topden

## Overview ---------------------------------------------------

var <-c("Mean", "Median", "Gini","P80/20","Top 10% share")

inc <- c("Pre-tax factor income","Pre-tax national income", 
                    "Post-tax disposable income")

# P1: EQUAL SHARING, WHOLE SAMPLE

# P2: PARTIAL SHARING, RESTRICTED SAMPLE