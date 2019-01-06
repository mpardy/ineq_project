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

silc.p105 <- silc.p1 %>% filter(rb010 > 2006)

arop <- arpr(silc.p105$y13, weights=silc.p105$rb050, years=silc.p105$rb010)
arop

arop <- as.data.frame(arop$value)


#1a) GENDER: At risk of poverty rate -------------------------------------------

# age >=18 

silc.p118 <- silc.p105 %>% filter(age>=18)

arop1 <- arpr (silc.p118$y13, weights=silc.p118$rb050, breakdown = silc.p118$rb090, years=silc.p118$rb010)

arop1


#1b) AGE:  At risk of poverty rate  ---------------------------------------------

#AGE: <18, 18-64, >64

#Prepare data
silc.p0 <- silc.p105 %>% filter(age<18)

silc.p1864  <- silc.p105 %>% filter(age >= 18 & age <= 64)

silc.p65 <- silc.p105 %>% filter(age>64)

# At risk of poverty rate aged 0-17 
arop2 <- arpr(silc.p0$y13, weights=silc.p0$rb050, years=silc.p0$rb010)

arop2

# At risk of poverty rate aged 18-64
arop3 <- arpr(silc.p1864$y13, weights=silc.p1864$rb050, years=silc.p1864$rb010)

arop3

# At risk of poverty rate aged 65+
arop4 <- arpr(silc.p65$y13, weights=silc.p65$rb050, years=silc.p65$rb010)

arop4

#--------------------------------
  
#AGE: 0-5, 6-11, 12-17

#Prepare data
  
silc.p11  <- silc.p105 %>% filter(age >= 6 & age <= 11)

silc.p17  <- silc.p105 %>% filter(age >= 12 & age <= 17)

silc.p5 <- silc.p105 %>% filter(age<6)

# At risk of poverty rate aged 0-5
arop5 <- arpr(silc.p5$y13, weights=silc.p5$rb050, years=silc.p5$rb010)

arop5

# At risk of poverty rate aged 6-11
arop6 <- arpr(silc.p11$y13, weights=silc.p11$rb050, years=silc.p11$rb010)

arop6

# At risk of poverty rate aged 12-17
arop7 <- arpr(silc.p17$y13, weights=silc.p17$rb050, years=silc.p17$rb010)

arop7

  
#2) Severe material deprivation rate ---------------------------------------

"country" <- "FR"

silc.h <- tbl(pg, "hh") %>%
  filter(hb020 %in% country) %>%
  dplyr::select(hb010, hb020, hb030, hs110, hh050, hs040, hs050, hs060, hs070, hs080, hs100) %>%
  collect(n = Inf)

# Only 8 variables downloaded, as object 'hs011' not found
# Need to download seperately: The Living conditions Working Group agreed during its 2010 meeting to suppress HS010 and to
#keep only HS011 starting from the 2011 operation. 
# difference hs011 & hs010: hs010 only asks if you have ever had arrears on mortage and rent payments 

#Dataset for 2007
silc.07 <- silc.rph %>% filter(hb010==2007)

#silc.12 <- silc.10 %>%  select(-c(hb010, hb030, px030, pl060, py050g, py010g, rb050, rb080, rb010, rb020, rb030, rb090, rx030,id_p, id_h, age, pb010, pb020, pb030, hx040, hx050))

hh050 <- silc.07$hh050 == 2
hs050 <- silc.07$hs050 == 2
hs040 <- silc.07$hs040 == 2
hs060 <- silc.07$hs060 == 2
hs070 <- silc.07$hs070 == 2
hs080 <- silc.07$hs080 == 2
hs100 <- silc.07$hs100 == 2
hs110 <- silc.07$hs110 == 2
hs011 <- silc.07$hs011 == 2

silc.h00c <-  silc.07 %>% bind_cols(hh050 = hh050, hs050=hs050,hs040 =hs040, hs060=hs060, hs070=hs070, hs080=hs080, hs100=hs100, hs110=hs110, hs011 = hs011)

silc.h00c[is.na(silc.h00c)] <- 0

silc.h00c <- silc.h00c  %>%
  mutate(x = hh0501+hs0501+hs0401+hs0601+hs0701+hs0801+hs1001+hs1101+hs0111)

silc.h00c <- silc.h00c %>% filter(x>=4)

n07smd <- nrow(silc.h00c)
n07 <- nrow(silc.07)

smd07 <- n06smd/n07

smd07

# Fin ---------------------------------------------------------------------