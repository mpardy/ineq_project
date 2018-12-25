
#Armutsindikatoren --------------------------------------------------------------------------------------

#1) At risk of poverty and social exclusion rate (AROPE)
#2) Population living in very low intensity (quasijobless) households
#3) Severe material deprivation rate

#1) At risk of poverty and social exclusion rate (AROPE) --------------------------------------------------

#=percentage of people living in a household with an equivalised net disposable income below the at-risk- of-poverty threshold, set at 60% of the national median (after social transfers)

#create datasets for different years
silc.p105 <- silc.p1 %>% filter(rb010==2005)
silc.p106 <- silc.p1 %>% filter(rb010==2006)
silc.p107 <- silc.p1 %>% filter(rb010==2007)
silc.p108 <- silc.p1 %>% filter(rb010==2008)
silc.p109 <- silc.p1 %>% filter(rb010==2009)
silc.p110 <- silc.p1 %>% filter(rb010==2010)
silc.p112 <- silc.p1 %>% filter(rb010==2012)
silc.p116 <- silc.p1 %>% filter(rb010==2016)


#add median for each year
silc.p105 <- silc.p105 %>% mutate(median=15878)
silc.p116 <- silc.p116 %>% mutate(median=21993.85)

#1) Use laeken package using y13
library(laeken)
arpr(inc = silc.p105$y13, weights = silc.p105$rb050) #=12.25

arpr(inc = silc.p116$y13, weights = silc.p116$rb050) #=13.06

#2) Manually computation using y13
silc.p105 <- silc.p105 %>% mutate(x05=y13/median)
nindivpoverty05 <- sum(silc.p105$x05<0.6)
n05 <- nrow(silc.p105)
nindivpoverty05/n05 #=12.94

silc.p116 <- silc.p116 %>% mutate(x16=y13/median)
nindivpoverty16 <- sum(silc.p116$x16<0.6)
n16 <- nrow(silc.p116)
nindivpoverty16/n16 #=12.57

#Check - use hy020
#1) Use laeken package using hy020
arpr(inc = silc.p105$hy020, weights = silc.p105$rb050) #=15.23
arpr(inc = silc.p106$hy020, weights = silc.p106$rb050) #=15.88
arpr(inc = silc.p107$hy020, weights = silc.p107$rb050) #=18.63
arpr(inc = silc.p108$hy020, weights = silc.p108$rb050) #=17.74
arpr(inc = silc.p110$hy020, weights = silc.p110$rb050) #=19.00
arpr(inc = silc.p112$hy020, weights = silc.p112$rb050) #=19.43
arpr(inc = silc.p116$hy020, weights = silc.p116$rb050) #=18.26 should be the right value, as same value as https://ec.europa.eu/eurostat/statistics-explained/index.php/People_at_risk_of_poverty_or_social_exclusion

#2) Manually computation using hy020
median.p1 <- svyby(~hy020, ~rb010, svy.p1, svyquantile, quantiles=0.5, keep.var = FALSE)
median.p1

#add median for each year
silc.p105 <- silc.p105 %>% mutate(median=32242)
silc.p116 <- silc.p116 %>% mutate(median=39760)

silc.p105 <- silc.p105 %>% mutate(x2=hy020/median)
sum(silc.p105$x2<0.6)
nrow(silc.p105)
2464/19020 #= 12.95

silc.p116 <- silc.p116 %>% mutate(x2=hy020/median)
sum(silc.p116$x2<0.6)
nrow(silc.p116)
4189/25617 #= 16.35

#1a) GENDER: At risk of poverty and social exclusion rate ---------------------------------------------------

#above 18


#1b) AGE:  At risk of poverty and social exclusion rate  ----------------------------------------------------
#0-17 (0-5; 6-11; 12-17), 18-64, 65+

#2) Population living in very low intensity (quasijobless) households ---------------------------------------

#People aged 0-59, living in households, where working-age adults (18-59) work less than 20% of their total work potential during the past year
