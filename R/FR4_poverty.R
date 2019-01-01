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

#get data for hs011
c06h <- tbl(pg, "c06h") %>% filter(hb020 %in% country) %>% select(hb010, hb020, hb030,  
                                                                  hs010) %>% collect(n = Inf)
c07h <- tbl(pg, "c07h") %>% filter(hb020 %in% country) %>% select(hb010, hb020, hb030,  
                                                                  hs010) %>% collect(n = Inf)
c08h <- tbl(pg, "c08h") %>% filter(hb020 %in% country) %>% select(hb010, hb020, hb030,  
                                                                  hs010) %>% collect(n = Inf)
c09h <- tbl(pg, "c09h") %>% filter(hb020 %in% country) %>% select(hb010, hb020, hb030,  
                                                                  hs010) %>% collect(n = Inf)
c10h <- tbl(pg, "c10h") %>% filter(hb020 %in% country) %>% select(hb010, hb020, hb030,  
                                                                  hs010) %>% collect(n = Inf)
c11h <- tbl(pg, "c11h") %>% filter(hb020 %in% country) %>% select(hb010, hb020, hb030,  
                                                                  hs011) %>% collect(n = Inf)
c12h <- tbl(pg, "c12h") %>% filter(hb020 %in% country) %>% select(hb010, hb020, hb030,  
                                                                  hs011) %>% collect(n = Inf)
c13h <- tbl(pg, "c13h") %>% filter(hb020 %in% country) %>% select(hb010, hb020, hb030,  
                                                                  hs011) %>% collect(n = Inf)
c14h <- tbl(pg, "c14h") %>% filter(hb020 %in% country) %>% select(hb010, hb020, hb030,  
                                                                  hs011) %>% collect(n = Inf)
c15h <- tbl(pg, "c15h") %>% filter(hb020 %in% country) %>% select(hb010, hb020, hb030,  
                                                                 hs011) %>% collect(n = Inf)
c16h <- tbl(pg, "c16h") %>% filter(hb020 %in% country) %>% select(hb010, hb020, hb030,  
                                                                  hs011) %>% collect(n = Inf)
c17h <- tbl(pg, "c17h") %>% filter(hb020 %in% country) %>% select(hb010, hb020, hb030,  
                                                                  hs011) %>% collect(n = Inf)

hs010 <- bind_rows(c06h, c07h, c08h, c09h, c10h, c11h, c12h, c13h, c14h, c15h, c16h, c17h)

silc.h <- left_join(silc.h, hs010 %>% select(hs011, hs010, hb010, hb030))
rm(hs010, c07h, c08h, c09h, c10h, c11h, c12h, c13h, c14h, c15h, c16h, c17h)

#dataset for 2005 
silc.h05 <- silc.h %>% filter(hb010==2005)
silc.h00 <- silc.h05 %>%  dplyr::select(-c(hb010, hb030))

hh050 <- silc.h00$hh050 == 2
hs050 <- silc.h00$hs050 == 2
hs040 <- silc.h00$hs040 == 2
hs060 <- silc.h00$hs060 == 2
hs070 <- silc.h00$hs070 == 2
hs080 <- silc.h00$hs080 == 2
hs100 <- silc.h00$hs100 == 2
hs110 <- silc.h00$hs110 == 2

silc.h00c <-  bind_cols(hh050 = hh050, hs050=hs050,hs040 =hs040, hs060=hs060, hs070=hs070, hs080=hs080, hs100=hs100, hs110=hs110)
View(silc.h00c)

silc.h00c[is.na(silc.h00c)] <- 0

silc.h00c <- silc.h00c  %>%
  mutate(x = hh050+hs050+hs040+hs060+hs070+hs080+hs100+hs110)

silc.h00c %>% filter(x>=4)

n05dp <- nrow(silc.h00c)
n05 <- nrow(silc.h05)

sdm<-n05dp/n05

#3) Population living in very low intensity (quasijobless) households ---------------------------------------

#People aged 0-59, living in households, where working-age adults (18-59) work less than 20% of their total work potential during the past year
