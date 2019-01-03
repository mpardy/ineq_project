# ------------------------------------------------------------------------
#
# Part3: Indicators
# Group FR
#
# -------------------------------------------------------------------------

library(dplyr)
library(survey)
library(convey)
#library(srvyr)
library(eurostat)

load("data/p1.rda")
load("data/p2.rda")

# Creating Survey Objects -------------------------------------------------

svy.p1 <- svydesign(ids =  ~id_h,
                    strata = ~rb020,
                    weights = ~rb050,
                    data = silc.p1) %>% convey_prep()

svy.p2 <- svydesign(ids =  ~id_h, 
                    strata = ~rb020,
                    weights = ~rb050,
                    data = silc.p2) %>% convey_prep()

# Inflation ---------------------------------------------------------------

vpi <- get_eurostat("prc_hicp_aind", time_format = "raw")

vpi <- vpi %>% filter(unit == "INX_A_AVG", 
                      coicop == "CP00", 
                      geo == "FR",
                      time %in% 2004:2017)

vpi <- vpi %>% select(time, values) %>% 
  mutate(time = as.numeric(time)) %>% 
  rename(rb010 = time, VPI = values)

# Indicators --------------------------------------------------------------

## P1: EQUAL SHARING OF RESOURCES; WHOLE SAMPLE-----------------------------

# Mean

mean.p1 <- svyby(~y11 + ~y12 + y13, ~rb010, svy.p1, 
                 svymean, keep.var = FALSE)

# inflation-adjustment (2015 = 100%)

mean.p1 <- left_join(mean.p1, vpi) %>% 
  group_by(rb010) %>% 
  mutate_at(vars(statistic.y11 : statistic.y13), funs(. * 100 / VPI))
  

# Median

median.p1 <- svyby(~y11 + ~y12 + y13, ~rb010, svy.p1, 
                   svyquantile, quantiles=0.5, keep.var = FALSE)

# inflation-adjustment (2015 = 100%)

median.p1 <- left_join(median.p1, vpi) %>% 
  group_by(rb010) %>% 
  mutate_at(vars(statistic1 : statistic3), funs(. * 100 / VPI))

# Gini

gini11 <- svyby(~y11, ~rb010, svy.p1, svygini, keep.var = FALSE)

gini12 <- svyby(~y12, ~rb010, svy.p1, svygini, keep.var = FALSE)

gini13 <- svyby(~y13, ~rb010, svy.p1, svygini, keep.var = FALSE)

gini.p1 <- as.data.frame(cbind(gini11[,1], gini11[,2],
                               gini12[,2], gini13[,2]))

# S80/S20

s80_11 <- svyby(~y11, ~rb010, svy.p1, svyqsr)

s80_12 <- svyby(~y12, ~rb010, svy.p1, svyqsr)

s80_13 <- svyby(~y13, ~rb010, svy.p1, svyqsr)

s80.p1 <- as.data.frame(cbind(s80_11[,1], s80_11[,2], 
                              s80_12[,2], s80_13[,2]))

# Top 10% share

#top11

#create svydesign for top 10%:

svy.p11.top <- subset(svy.p1, y11 >= as.numeric(
  svyquantile(~y11, svy.p1, quantile=c(0.9))))

topnum <- svyby(~y11, ~rb010, svy.p11.top, svytotal)

topden <- svyby(~y11, ~rb010, svy.p1, svytotal)

top11 <- topnum / topden

#top12

svy.p12.top <- subset(svy.p1, y12 >= as.numeric(
  svyquantile(~y12, svy.p1, quantile=c(0.9))))

topnum <- svyby(~y12, ~rb010, svy.p12.top, svytotal)

topden <- svyby(~y12, ~rb010, svy.p1, svytotal)

top12 <- topnum / topden

#top13

svy.p13.top <- subset(svy.p1, y13 >= as.numeric(
  svyquantile(~y13, svy.p1, quantile=c(0.9))))

topnum <- svyby(~y13, ~rb010, svy.p13.top, svytotal)

topden <- svyby(~y13, ~rb010, svy.p1, svytotal)

top13 <- topnum / topden

top.p1 <- as.data.frame(cbind(top11[,2], top12[,2], top13[,2]))


## P2: PARTIAL SHARING OF RESOURCES, RESTRICTED SAMPLE (>=20 YEARS)---------


#Mean

mean.p2 <- svyby(~y21 + ~y22 + y23, ~rb010, svy.p2, 
                 svymean, keep.var = FALSE)

# inflation-adjustment (2015 = 100%)

mean.p2 <- left_join(mean.p2, vpi) %>% 
  group_by(rb010) %>% 
  mutate_at(vars(statistic.y21 : statistic.y23), funs(. * 100 / VPI))

# Median

median.p2 <- svyby(~y21 + ~y22 + y23, ~rb010, svy.p2, 
                   svyquantile, quantiles=0.5, keep.var = FALSE)

# inflation-adjustment (2015 = 100%)

median.p2 <- left_join(median.p2, vpi) %>% 
  group_by(rb010) %>% 
  mutate_at(vars(statistic1 : statistic3), funs(. * 100 / VPI))

#Gini

gini21 <- svyby(~y21, ~rb010, svy.p2, svygini, keep.var = FALSE)

gini22 <- svyby(~y22, ~rb010, svy.p2, svygini, keep.var = FALSE)

gini23 <- svyby(~y23, ~rb010, svy.p2, svygini, keep.var = FALSE)

gini.p2 <- as.data.frame(cbind(gini21[,1], gini21[,2],
                               gini22[,2], gini23[,2]))

# S80/S20

s80_21 <- svyby(~y21, ~rb010, svy.p2, svyqsr)

s80_22 <- svyby(~y22, ~rb010, svy.p2, svyqsr)

s80_23 <- svyby(~y23, ~rb010, svy.p2, svyqsr)

s80.p2 <- as.data.frame(cbind(s80_21[,1], s80_21[,2], 
                              s80_22[,2], s80_23[,2]))

# Top 10% share

#top21

svy.p21.top <- subset(svy.p2, y21 >= as.numeric(
  svyquantile(~y21, svy.p2, quantile=c(0.9))))

topnum <- svyby(~y21, ~rb010, svy.p21.top, svytotal)

topden <- svyby(~y21, ~rb010, svy.p2, svytotal)

top21 <- topnum / topden

#top22

svy.p22.top <- subset(svy.p2, y22 >= as.numeric(
  svyquantile(~y22, svy.p2, quantile=c(0.9))))

topnum <- svyby(~y22, ~rb010, svy.p22.top, svytotal)

topden <- svyby(~y22, ~rb010, svy.p2, svytotal)

top22 <- topnum / topden

#top23

svy.p23.top <- subset(svy.p2, y23 >= as.numeric(
  svyquantile(~y23, svy.p2, quantile=c(0.9))))

topnum <- svyby(~y23, ~rb010, svy.p23.top, svytotal)

topden <- svyby(~y23, ~rb010, svy.p2, svytotal)

top23 <- topnum / topden

top.p2 <- as.data.frame(cbind(top21[,2], top22[,2], top23[,2]))

## Overview ---------------------------------------------------

var <-c("Mean", "Median", "Gini","P80/20","Top 10% share")

inc <- c("Pre-tax factor income","Pre-tax national income", 
                    "Post-tax disposable income")

# P1: EQUAL SHARING, WHOLE SAMPLE

# P2: PARTIAL SHARING, RESTRICTED SAMPLE