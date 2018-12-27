
#Armutsindikatoren --------------------------------------------------------------------------------------

#1) At risk of poverty and social exclusion rate (AROPE)
#2) Severe material deprivation rate
#3) Population living in very low intensity (quasijobless) households


#1) At risk of poverty rate (AROP) --------------------------------------------------

#=percentage of people living in a household with an equivalised net disposable income below the at-risk- of-poverty threshold, set at 60% of the national median (after social transfers)


#1) Use laeken package using y13
library(laeken)

arop <- arpr(silc.p1$y13, weights=silc.p1$rb050, years=silc.p1$rb010)
arop


#2) Manual computation using y13
#silc.p105 <- silc.p105 %>% mutate(x05=y13/median)
#nindivpoverty05 <- sum(silc.p105$x05<0.6)
#n05 <- nrow(silc.p105)
#nindivpoverty05/n05 #=12.94

#silc.p116 <- silc.p116 %>% mutate(x16=y13/median)
#nindivpoverty16 <- sum(silc.p116$x16<0.6)
#n16 <- nrow(silc.p116)
#nindivpoverty16/n16 #=12.57

#Check - use hy020
#1) Use laeken package using hy020
#arpr(inc = silc.p105$hy020, weights = silc.p105$rb050) =15.23
#arpr(inc = silc.p116$hy020, weights = silc.p116$rb050) =18.26


#1a) GENDER: At risk of poverty rate ---------------------------------------------------

#above 18


#1b) AGE:  At risk of poverty and social exclusion rate  ----------------------------------------------------
#0-17 (0-5; 6-11; 12-17), 18-64, 65+

#2) Severe material deprivation rate ------------------------------------------------------------------------

"country" <- "FR"

silc.h <- tbl(pg, "hh") %>%
  filter(hb020 %in% country) %>%
  select(hb010, hb020, hb030, hs011, hs110, hh050, hs040, hs050, hs060, hs070, hs080, hs100) %>%
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

#silc.h00 <- dplyr::select(c(hs110, hh050, hs040, hs050, hs060, hs070, hs080, hs100)) %>% add_count() 

#silc.h00 <- silc.h00 %>% count(c(hs110, hh050, hs040, hs050, hs060, hs070, hs080, hs100)==2) 

#silc.h00 <- add_count(silc.h00) %>% filter(n==2)

View(silc.h00)
n05 <- nrow(silc.h05)


#3) Population living in very low intensity (quasijobless) households ---------------------------------------

#People aged 0-59, living in households, where working-age adults (18-59) work less than 20% of their total work potential during the past year
