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


#1) Use laeken package using y13
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

#AGE: <18, 18-64, >64

silc.p07 <- silc.p07 %>% mutate(agecl1=cut(age,c(0,17,65,120)))

arop3 <- arpr(silc.p07$y13, weights=silc.p07$rb050, breakdown = silc.p07$agecl1, years=silc.p07$rb010)

arop3

#--------------------------------
  
#AGE: 0-5, 5-11, 11-17

silc.p106 <- silc.p106 %>% mutate(agecl2=cut(age,c(0,5,11,17)))

# At risk of poverty rate aged 0-5
arop4 <- arpr(silc.p106$y13, weights=silc.p106$rb050, breakdown=silc.p106$agecl2, years=silc.p106$rb010)

arop4

  
#2) Severe material deprivation rate ---------------------------------------


#Dataset for 2007
silc.07 <- silc.rph %>% filter(hb010==2007)

#silc.07 <- silc.10 %>%  select(-c(hb010, hb030, px030, pl060, py050g, py010g, rb050, rb080, rb010, rb020, rb030, rb090, rx030,id_p, id_h, age, pb010, pb020, pb030, hx040, hx050))

silc.07 <- silc.07 %>% 
  mutate(
    matdep.items=(hh050 == 2)+(hs050 == 2)+(hs040 == 2)+(hs060 == 2)+(hs070 == 2)+(hs080 == 2)+(hs100 == 2)+(hs110 == 2),#hs011 == 2
    matdep=(matdep.items>=4)*1)

#hh050 <- silc.07$hh050 == 2
#hs050 <- silc.07$hs050 == 2
#hs040 <- silc.07$hs040 == 2
#hs060 <- silc.07$hs060 == 2
#hs070 <- silc.07$hs070 == 2
#hs080 <- silc.07$hs080 == 2
#hs100 <- silc.07$hs100 == 2
#hs110 <- silc.07$hs110 == 2
#hs011 <- silc.07$hs011 == 2

#silc.h00c <-  silc.07 %>% bind_cols(hh050 = hh050, hs050=hs050,hs040 =hs040, hs060=hs060, hs070=hs070, hs080=hs080, hs100=hs100, hs110=hs110, hs011 = hs011)

#silc.h00c[is.na(silc.h00c)] <- 0

#silc.h00c <- silc.h00c  %>% mutate(x = hh0501+hs0501+hs0401+hs0601+hs0701+hs0801+hs1001+hs1101+hs0111)

#silc.h00c <- silc.h00c %>% filter(x>=4)

#n07smd <- nrow(silc.h00c)
#n07 <- nrow(silc.07)

#smd07 <- n07smd/n07

#smd07

# Fin ---------------------------------------------------------------------