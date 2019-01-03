# -------------------------------------------------------------------------
#
# Part5: Armutsindikatoren
# Group FR
#
# --------------------------------------------------------------------------


#1) At risk of poverty rate (AROP)
#2) Severe material deprivation rate
#3) Population living in very low intensity (quasijobless) households


#1) At risk of poverty rate (AROP) --------------------------------------------------

#=percentage of people living in a household with an equivalised net disposable income below the at-risk- of-poverty threshold, set at 60% of the national median (after social transfers)


#1) Use laeken package using y13
library(laeken)

silc.p105 <- silc.p1 %>% filter(rb010 > 2005)

arop <- arpr(data$y13, weights=data$rb050, years=data$rb010)
arop


#Manual computation using y13
#silc.p105 <- silc.p105 %>% mutate(x05=y13/median)
#nindivpoverty05 <- sum(silc.p105$x05<0.6)
#n05 <- nrow(silc.p105)
#nindivpoverty05/n05 #=12.94
#nindivpoverty16/n16 #=12.57

#Use laeken package using hy020
#arpr(inc = silc.p105$hy020, weights = silc.p105$rb050) =15.23
#arpr(inc = silc.p116$hy020, weights = silc.p116$rb050) =18.26


#1a) GENDER: At risk of poverty rate ---------------------------------------------------

# age >=18 

data18 <- data %>% filter(age>=18)
arop1 <- arpr (data$y13, weights=data$rb050, breakdown = data$rb090, years=data$rb010)
arop1


#1b) AGE:  At risk of poverty rate  ----------------------------------------------------

#Prepare data
data0 <- data %>% subset(age<18)
data1865  <- data %>% subset(age >= 18 & age <= 64)
data65 <- data %>% subset(age>64)

# At risk of poverty rate aged 0-17 
arop2 <- arpr(data0$y13, weights=data0$rb050, years=data0$rb010)
arop2

# At risk of poverty rate aged 18-64
arop3 <- arpr(data1865$y13, weights=data1865$rb050, years=data1865$rb010)
arop3

# At risk of poverty rate aged 65+
arop4 <- arpr(data65$y13, weights=data65$rb050, years=data65$rb010)
arop4


#2) Severe material deprivation rate ------------------------------------------------------------------------

"country" <- "FR"

silc.h <- tbl(pg, "hh") %>%
  filter(hb020 %in% country) %>%
  dplyr::select(hb010, hb020, hb030, hs110, hh050, hs040, hs050, hs060, hs070, hs080, hs100) %>%
  collect(n = Inf)

# Only 8 variables downloaded, as object 'hs011' not found
# Need to download seperately: The Living conditions Working Group agreed during its 2010 meeting to suppress HS010 and to
#keep only HS011 starting from the 2011 operation. 
# difference hs011 & hs010: hs010 only asks if you have ever had arrears on mortage and rent payments 


#Dataset for 2012
silc.06 <- silc.rph %>% filter(hb010==2012)

silc.06 <- silc.06 %>%  select(-c(hb010, hb030, px030, pl060, py050g, py010g, rb050, rb080, rb010, rb020, rb030, rb090, rx030,id_p, id_h, age, pb010, pb020, pb030, hx040, hx050))

hh050 <- silc.06$hh050 == 2
hs050 <- silc.06$hs050 == 2
hs040 <- silc.06$hs040 == 2
hs060 <- silc.06$hs060 == 2
hs070 <- silc.06$hs070 == 2
hs080 <- silc.06$hs080 == 2
hs100 <- silc.06$hs100 == 2
hs110 <- silc.06$hs110 == 2
hs011 <- silc.06$hs011 == 2

silc.h00c <-  silc.06 %>% bind_cols(hh050 = hh050, hs050=hs050,hs040 =hs040, hs060=hs060, hs070=hs070, hs080=hs080, hs100=hs100, hs110=hs110, hs011 = hs011)

silc.h00c[is.na(silc.h00c)] <- 0

silc.h00c <- silc.h00c  %>%
  mutate(x = hh0501+hs0501+hs0401+hs0601+hs0701+hs0801+hs1001+hs1101+hs0111)

silc.h00c <- silc.h00c %>% filter(x>=4)

n06smd <- nrow(silc.h00c)
n06 <- nrow(silc.06)

smd06<-n06smd/n06



#3) Population living in very low intensity (quasijobless) households ---------------------------------------

#People aged 0-59, living in households, where working-age adults (18-59) work less than 20% of their total work potential during the past year
