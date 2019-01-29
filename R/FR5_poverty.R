# -------------------------------------------------------------------------
#
# Part5: Armutsindikatoren
# Group FR
#
# --------------------------------------------------------------------------


#1) At risk of poverty rate (AROP)
#1a) AROP by gender
#1b) AROP by age
#2) Severe material deprivation rate


#1) At risk of poverty rate (AROP) ------------------------------------------

#=percentage of people living in a household with an equivalised net disposable income below the at-risk- of-poverty threshold, set at 60% of the national median (after social transfers)

#1) AROP total
library(laeken)

silc.p07 <- silc.rph %>% filter(rb010 > 2006)

silc.p105 <- silc.p07

arop1 <- laeken::arpr(silc.p105$y13, weights=silc.p105$rb050, years=silc.p105$rb010)
arop1


#1a) GENDER: At risk of poverty rate -------------------------------------------

# age >=18 

silc.p118 <- silc.p07 %>% filter(age>=18)

arop2 <- laeken::arpr(silc.p118$y13, weights=silc.p118$rb050, breakdown = silc.p118$rb090, years=silc.p118$rb010)

arop2


#1b) AGE:  At risk of poverty rate  ---------------------------------------------

#AGE: <18, 17-65, >64

silc.p07 <- silc.p07 %>% mutate(agecl1=cut(age,c(0,17,65,120)))

arop3 <- arpr(silc.p07$y13, weights=silc.p07$rb050, breakdown = silc.p07$agecl1, years=silc.p07$rb010)

arop3
  
#2) Severe material deprivation (SMD) rate ---------------------------------------

# Use only data between 2011-2017
silc.rph1 <- silc.rph %>% filter(rb010>2010)

#as there are too many NAs concerning variable hs010 in the years before

# Create a column that fulfills criteria of SMD
silc.rph1 <- silc.rph1 %>% 
  mutate(
    matdep.items=(hh050 == 2)+(hs050 == 2)+(hs040 == 2)+(hs060 == 2)+(hs070 == 2)+(hs080 == 2)+(hs100 == 2)+(hs110 == 2)+(hs011 == 2),
    matdep=(matdep.items>=4)*1)

smd <- silc.rph1 %>% 
  group_by(rb010) %>% 
  count(matdep==1) 

# Rename columns
colnames(smd) <- c("Jahr", "Condition", "n")

smd <- smd %>% group_by(Jahr) %>% 
  filter(Condition %in% "TRUE" | Condition %in% "FALSE") %>% 
  mutate(all=sum(n))

# Calculate SMD for all years
smd <- smd %>% group_by(Jahr) %>% 
  filter(Condition %in% "TRUE") %>% 
  summarise(n/all)

smd

# Fin ---------------------------------------------------------------------