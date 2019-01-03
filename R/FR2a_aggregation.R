# ------------------------------------------------------------------------
#
# Part 2: Income aggregation MODIFIED FOR FRENCH HY020 / y13 calculation
# Group FR
#
# -------------------------------------------------------------------------

#rm(list = ls())
#setwd("./EconIneq/ineq_project")
#load("data/rph_a.rda")
library(dplyr)

## P1: EQUAL SHARING OF RESOURCES; WHOLE SAMPLE-----------------------------

# (1.1) Pre-tax factor income = y11 ----------------------------------------

#sum up per person
silc.rph <- silc.rph %>%
  mutate(p11 = py010g + py050g + py080g)

# sum up per household
silc.rph <- silc.rph %>% group_by(id_h) %>%
  mutate(sp11 = sum(p11))

# equivalise, allocate p.p.
silc.rph <- silc.rph %>% 
  mutate(y11 = (sp11 + hy110g + hy040g + hy090g) / hx050)

# (1.2) Pre-tax national income = y12 ---------------------------------------

#sum up per person
silc.rph <- silc.rph %>% 
  mutate(p12 = py090g + py100g)

#sum up per household
silc.rph <- silc.rph %>% group_by(id_h) %>%
  mutate(sp12 = sum(p12))

# equivalise, allocate p.p.
silc.rph <- silc.rph %>%
  mutate(y12 = y11 + (sp12 / hx050))

# (1.3) Post-tax disposable income, = y13 --------------------------------------

#we use the formula indicated in quality reports from France

#sum up per person
silc.rph <- silc.rph %>% 
  mutate(p13 = py010n + py050n + py080n + py090n + py100n
         + py110n + py120n + py130n + py140n)

#sum up per household
silc.rph <- silc.rph %>% group_by(id_h) %>%
  mutate(sp13 = sum(p13))

# equivalise, allocate p.p.
silc.rph <- silc.rph %>%
  mutate(y13 = ((sp13 + hy040n + hy050n + hy060n + hy070n
                 + hy080n + hy090n + hy110n 
                 - hy120n - hy130n - hy145n) / hx050))


silc.rph <- silc.rph %>% mutate(y13true = (hy020 / hx050))

identical(silc.rph$y13, silc.rph$y13true)

silc.rph %>% group_by(rb010) %>% summarise(mean(abs(y13 - y13true)))


#remove superfluous columns

silc.rph <- silc.rph %>% 
  select(-c(p11, sp11, p12, sp12, p13, sp13))

## P2: PARTIAL SHARING OF RESOURCES, RESTRICTED SAMPLE (>=20 YEARS)------------

# subset to age >= 20 years
#n = number of remaining household members

silc.rph20 <- silc.rph %>% 
  filter(age >= 20) %>% add_count(id_h)

# (2.1) Pre-tax factor income = y21 ------------------------------------------

silc.rph20 <- silc.rph20 %>%
  mutate(y21 = py010g + py050g + py080g + 
           (hy110g + hy040g + hy090g) / n)

# (2.2) Pre-tax national income = y22 -----------------------------------------

silc.rph20 <- silc.rph20 %>%
  mutate(y22 = y21 + py090g + py100g)

# (2.3) Post-tax disposable income = y23 --------------------------------------

silc.rph20 <- silc.rph20 %>%
  mutate(y23 = y22 + py110g + py120g + py130g + py140g + 
           (hy050g + hy060g + hy070g + hy080g 
            - hy120g - hy130g - hy140g) / n)

# Restrict samples to positive income ---------------------------------------

silc.p1 <- silc.rph %>% filter(y11 > 0, y12 > 0, y13 > 0)

silc.p2 <- silc.rph20 %>% filter(y21 > 0, y22 > 0, y23 > 0)

# Store to disk ------------------------------------------------------------

#save(silc.p1, file="data/p1.rda",compress = 'xz')

#save(silc.p2, file="data/p2.rda",compress = 'xz')

# Fin ---------------------------------------------------------------------