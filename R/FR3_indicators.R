# ------------------------------------------------------------------------
#
# Part3: Indicators
# Group FR
#
# -------------------------------------------------------------------------

library(dplyr)
library(survey)
library(convey)
library(eurostat)

load("data/p1.rda")
load("data/p2.rda")

# Creating Survey Objects -------------------------------------------------

svy.p1 <- svydesign(ids =  ~id_h,
                    strata = ~rb020,
                    weights = ~rb050,
                    data = silc.rph) %>% convey_prep()

svy.p2 <- svydesign(ids =  ~id_h, 
                    strata = ~rb020,
                    weights = ~rb050,
                    data = silc.rph20) %>% convey_prep()

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

## P1: EQUAL SHARING OF RESOURCES; WHOLE SAMPLE ---------------------------

# Mean

mean.p1 <- svyby(~y11 + ~y12 + ~y13, ~rb010, svy.p1, 
                 svymean, keep.var = FALSE)

# inflation-adjustment (2015 = 100%)

mean.p1 <- left_join(mean.p1, vpi) %>%
  mutate_at(vars(statistic.y11 : statistic.y13), funs(. * 100 / VPI)) %>%
  select(-c(VPI))

mean.p1 <- mean.p1 %>% 
  rename("Mittelwert Faktoreinkommen vor Steuern" = statistic.y11, 
         "Mittelwert Nationaleinkommen vor Steuern" = statistic.y12,
         "Mittelwert verfügbares Äquivalenzeinkommen" = statistic.y13)

# Median

median.p1 <- svyby(~y11 + ~y12 + ~y13, ~rb010, svy.p1, 
                   svyquantile, quantiles=0.5, keep.var = FALSE)

# inflation-adjustment (2015 = 100%)

median.p1 <- left_join(median.p1, vpi) %>% 
  mutate_at(vars(statistic1 : statistic3), funs(. * 100 / VPI)) %>%
  select(-c(VPI))

median.p1 <- median.p1 %>% 
  rename("Median Faktoreinkommen vor Steuern" = statistic1, 
         "Median Nationaleinkommen vor Steuern" = statistic2,
         "Median verfügbares Äquivalenzeinkommen" = statistic3)

# Gini

gini11 <- svyby(~y11, ~rb010, svy.p1, svygini, keep.var = FALSE)

gini12 <- svyby(~y12, ~rb010, svy.p1, svygini, keep.var = FALSE)

gini13 <- svyby(~y13, ~rb010, svy.p1, svygini, keep.var = FALSE)

gini.p1 <- as.data.frame(cbind(gini11, gini12[,2], gini13[,2]))

gini.p1 <- gini.p1 %>% 
  rename("Gini Faktoreinkommen vor Steuern" = statistic, 
         "Gini Nationaleinkommen vor Steuern" = "gini12[, 2]",
         "Gini verfügbares Äquivalenzeinkommen" = "gini13[, 2]")

# S80/S20

# S80/S20 not defined for pre-tax factor income (division by 0)

s80_12 <- svyby(~y12, ~rb010, svy.p1, svyqsr, keep.var = FALSE)

s80_13 <- svyby(~y13, ~rb010, svy.p1, svyqsr, keep.var = FALSE)

s80.p1 <- as.data.frame(cbind(s80_12, s80_13[,2]))

s80.p1 <- s80.p1 %>% 
  rename("S80/S20 Nationaleinkommen vor Steuern" = statistic,
         "S80/S20 verfügbares Äquivalenzeinkommen" = "s80_13[, 2]")

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

rb010 <- c(2004 : 2017)

top.p1 <- as.data.frame(cbind(rb010, top11[,2], top12[,2], top13[,2]))

top.p1 <- top.p1 %>% 
  rename("Anteil Top 10% Faktoreinkommen vor Steuern" = V2, 
         "Anteil Top 10% Nationaleinkommen vor Steuern" = V3,
         "Anteil Top 10% verfügbares Äquivalenzeinkommen" = V4)

## P2: PARTIAL SHARING OF RESOURCES, RESTRICTED SAMPLE (>=20 YEARS)---------


#Mean

mean.p2 <- svyby(~y21 + ~y22 + ~y23, ~rb010, svy.p2, 
                 svymean, keep.var = FALSE)

# inflation-adjustment (2015 = 100%)

mean.p2 <- left_join(mean.p2, vpi) %>%
  mutate_at(vars(statistic.y21 : statistic.y23), funs(. * 100 / VPI)) %>%
  select(-c(VPI))

mean.p2 <- mean.p2 %>% 
  rename("Mittelwert Faktoreinkommen vor Steuern" = statistic.y21, 
         "Mittelwert Nationaleinkommen vor Steuern" = statistic.y22,
         "Mittelwert verfügbares Äquivalenzeinkommen" = statistic.y23)

# Median

median.p2 <- svyby(~y21 + ~y22 + ~y23, ~rb010, svy.p2, 
                   svyquantile, quantiles=0.5, keep.var = FALSE)

# inflation-adjustment (2015 = 100%)

median.p2 <- left_join(median.p2, vpi) %>% 
  mutate_at(vars(statistic1 : statistic3), funs(. * 100 / VPI)) %>%
  select(-c(VPI))

median.p2 <- median.p2 %>% 
  rename("Median Faktoreinkommen vor Steuern" = statistic1, 
         "Median Nationaleinkommen vor Steuern" = statistic2,
         "Median verfügbares Äquivalenzeinkommen" = statistic3)

#Gini

gini21 <- svyby(~y21, ~rb010, svy.p2, svygini, keep.var = FALSE)

gini22 <- svyby(~y22, ~rb010, svy.p2, svygini, keep.var = FALSE)

gini23 <- svyby(~y23, ~rb010, svy.p2, svygini, keep.var = FALSE)

gini.p2 <- as.data.frame(cbind(gini21, gini22[,2], gini23[,2]))

gini.p2 <- gini.p2 %>% 
  rename("Gini Faktoreinkommen vor Steuern" = statistic, 
         "Gini Nationaleinkommen vor Steuern" = "gini22[, 2]",
         "Gini verfügbares Äquivalenzeinkommen" = "gini23[, 2]")

# S80/S20

# again, S80/S20 not defined for pre-tax factor income (division by 0)

s80_22 <- svyby(~y22, ~rb010, svy.p2, svyqsr, keep.var = FALSE)

s80_23 <- svyby(~y23, ~rb010, svy.p2, svyqsr, keep.var = FALSE)

s80.p2 <- as.data.frame(cbind(s80_22, s80_23[,2]))

s80.p2 <- s80.p2 %>% 
  rename("S80/S20 Nationaleinkommen vor Steuern" = statistic,
         "S80/S20 verfügbares Äquivalenzeinkommen" = "s80_23[, 2]")

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

top.p2 <- as.data.frame(cbind(rb010, top21[,2], top22[,2], top23[,2]))

top.p2 <- top.p2 %>% 
  rename("Anteil Top 10% Faktoreinkommen vor Steuern" = V2, 
         "Anteil Top 10% Nationaleinkommen vor Steuern" = V3,
         "Anteil Top 10% verfügbares Äquivalenzeinkommen" = V4)


## Summary Dataframes -----------------------------------------------------

library(tidyverse)

# P1: EQUAL SHARING, WHOLE SAMPLE

indicators.p1 <- list(mean.p1, median.p1, gini.p1, s80.p1, top.p1) %>%
  reduce(left_join, by = "rb010") %>%
  round(digits = 4) %>%
  rename(Jahr = rb010)

save(indicators.p1, file = "data/indicators.p1.rda")
write.csv(indicators.p1, file = "data/table_p1.csv")

# P2: PARTIAL SHARING, RESTRICTED SAMPLE

indicators.p2 <- list(mean.p2, median.p2, gini.p2, s80.p2, top.p2) %>% 
  reduce(left_join, by = "rb010") %>%
  round(digits = 4) %>%
  rename(Jahr = rb010)

save(indicators.p2, file = "data/indicators.p2.rda")
write.csv(indicators.p2, file = "data/table_p2.csv")


# create tables for paper ------------------------------------------------------

mean.median.p1 <- indicators.p1 %>% 
  dplyr::select(Jahr, starts_with("Median"), starts_with("Mittelwert"))

mean.median.p1[,2:7] <- round(mean.median.p1[,2:7], digits = 0)

write.csv(mean.median.p1, file = "data/meanmedian_p1.csv")

gini.p1 <- indicators.p1 %>% 
  select(Jahr, starts_with("Gini"))

write.csv(gini.p1, file = "data/gini_p1.csv")

s80.p1 <- indicators.p1 %>% 
  select(Jahr, starts_with("S80"))

write.csv(s80.p1, file = "data/s80_p1.csv")

top.p1 <- indicators.p1 %>% 
  select(Jahr, starts_with("Anteil"))

write.csv(top.p1, file = "data/top_p1.csv")
