# ------------------------------------------------------------------------
#
# Part 4: Setup poverty
# Group FR
#
# -------------------------------------------------------------------------

library(dplyr)
library(tidyr)

"country" <- "FR"

# Prepare Data ------------------------------------------------------------

#Severe material deprivation rate 

# Download data 2004-2013 -------------------------------------------------

silc.p <- tbl(pg, "pp") %>%
  filter(pb020 %in% country) %>%
  dplyr::select(pb010, pb020, pb030, py010g, py050g, pl060, px030) %>%
  collect(n = Inf)

silc.h <- tbl(pg, "hh") %>%
  filter(hb020 %in% country) %>%
  dplyr::select(hb010, hb020, hb030, hs110, hh050, hs040, hs050, hs060, hs070, hs080, hs100, hx040, hx050) %>%
  collect(n = Inf)

silc.r <- tbl(pg, "rr") %>%
  filter(rb020 %in% country) %>%
  dplyr::select(rb010, rb020, rb030, rb050, rb080, rb090, rx030) %>%
  collect(n = Inf)


# Download recent data 2014-2017 ------------------------------------------

#p

silc.p14 <- tbl(pg, "c14p") %>%
  filter(pb020 %in% country) %>%
  dplyr::select(pb010, pb020, pb030, py010g, py050g, pl060, px030) %>%
  collect(n = Inf)

silc.p15 <- tbl(pg, "c15p") %>%
  filter(pb020 %in% country) %>%
  dplyr::select(pb010, pb020, pb030, py010g, py050g, pl060, px030) %>%
  collect(n = Inf)

silc.p16 <- tbl(pg, "c16p") %>%
  filter(pb020 %in% country) %>%
  dplyr::select(pb010, pb020, pb030, py010g, py050g, pl060, px030) %>%
  collect(n = Inf)

silc.p17 <- tbl(pg, "c17p") %>%
  filter(pb020 %in% country) %>%
  dplyr::select(pb010, pb020, pb030, py010g, py050g, pl060, px030) %>%
  collect(n = Inf)

#h

silc.h14 <- tbl(pg, "c14h") %>%
  filter(hb020 %in% country) %>%
  dplyr::select(hb010, hb020, hb030, hs110, hh050, hs040, hs050, hs060, hs070, hs080, hs100, hx040, hx050) %>%
  collect(n = Inf)

silc.h15 <- tbl(pg, "c15h") %>%
  filter(hb020 %in% country) %>%
  dplyr::select(hb010, hb020, hb030, hs110, hh050, hs040, hs050, hs060, hs070, hs080, hs100, hx040, hx050) %>%
  collect(n = Inf)

silc.h16 <- tbl(pg, "c16h") %>%
  filter(hb020 %in% country) %>%
  dplyr::select(hb010, hb020, hb030, hs110, hh050, hs040, hs050, hs060, hs070, hs080, hs100, hx040, hx050) %>%
  collect(n = Inf)

silc.h17 <- tbl(pg, "c17h") %>%
  filter(hb020 %in% country) %>%
  dplyr::select(hb010, hb020, hb030, hs110, hh050, hs040, hs050, hs060, hs070, hs080, hs100, hx040, hx050) %>%
  collect(n = Inf)

#r

silc.r14 <- tbl(pg, "c14r") %>%
  filter(rb020 %in% country) %>%
  dplyr::select(rb010, rb020, rb030, rb050, rb080, rb090, rx030) %>%
  collect(n = Inf)

silc.r15 <- tbl(pg, "c15r") %>%
  filter(rb020 %in% country) %>%
  dplyr::select(rb010, rb020, rb030, rb050, rb080, rb090, rx030) %>%
  collect(n = Inf)

silc.r16 <- tbl(pg, "c16r") %>%
  filter(rb020 %in% country) %>%
  dplyr::select(rb010, rb020, rb030, rb050, rb080, rb090, rx030) %>%
  collect(n = Inf)

silc.r17 <- tbl(pg, "c17r") %>%
  filter(rb020 %in% country) %>%
  dplyr::select(rb010, rb020, rb030, rb050, rb080, rb090, rx030) %>%
  collect(n = Inf)

#merge years

silc.p <- bind_rows(silc.p, silc.p14, silc.p15, silc.p16, silc.p17)

silc.h <- bind_rows(silc.h, silc.h14, silc.h15, silc.h16, silc.h17)

silc.r <- bind_rows(silc.r, silc.r14, silc.r15, silc.r16, silc.r17)

#r, p: exclude observations with personal id = NA

silc.p <- silc.p %>% drop_na(pb030)

silc.r <- silc.r %>% drop_na(rb030)


# Need to download hs011 seperately: The Living conditions Working Group agreed during its 2010 meeting to suppress HS010 and to
# keep only HS011 starting from the 2011 operation. 
# difference hs011 & hs010: hs010 only asks if you have ever had arrears on mortage and rent payments and not once or twice as hs011 does

# Get data for hs011 --------------------------------------------------------------------------

c06h <- tbl(pg, "c06h") %>% 
  filter(hb020 %in% country) %>% 
  dplyr::select(hb010, hb020, hb030, hs010, hx040, hx050) %>% 
  collect(n = Inf)

c07h <- tbl(pg, "c07h") %>% 
  filter(hb020 %in% country) %>% 
  dplyr::select(hb010, hb020, hb030, hs010, hx040, hx050) %>% 
  collect(n = Inf)

c08h <- tbl(pg, "c08h") %>% 
  filter(hb020 %in% country) %>% 
  dplyr::select(hb010, hb020, hb030, hs010, hx040, hx050) %>% 
  collect(n = Inf)

c09h <- tbl(pg, "c09h") %>% 
  filter(hb020 %in% country) %>% 
  dplyr::select(hb010, hb020, hb030, hs010, hx040, hx050) %>% 
  collect(n = Inf)

c10h <- tbl(pg, "c10h") %>% 
  filter(hb020 %in% country) %>% 
  dplyr::select(hb010, hb020, hb030, hs010, hx040, hx050) %>% 
  collect(n = Inf)

c11h <- tbl(pg, "c11h") %>% 
  filter(hb020 %in% country) %>% 
  dplyr::select(hb010, hb020, hb030, hs011, hx040, hx050) %>% 
  collect(n = Inf)

c12h <- tbl(pg, "c12h") %>% 
  filter(hb020 %in% country) %>% 
  dplyr::select(hb010, hb020, hb030, hs011, hx040, hx050) %>% 
  collect(n = Inf)

c13h <- tbl(pg, "c13h") %>% 
  filter(hb020 %in% country) %>% 
  dplyr::select(hb010, hb020, hb030, hs011, hx040, hx050) %>% 
  collect(n = Inf)

c14h <- tbl(pg, "c14h") %>% 
  filter(hb020 %in% country) %>% 
  dplyr::select(hb010, hb020, hb030, hs011, hx040, hx050) %>% 
  collect(n = Inf)

c15h <- tbl(pg, "c15h") %>% 
  filter(hb020 %in% country) %>% 
  dplyr::select(hb010, hb020, hb030, hs011, hx040, hx050) %>% 
  collect(n = Inf)

c16h <- tbl(pg, "c16h") %>% 
  filter(hb020 %in% country) %>% 
  dplyr::select(hb010, hb020, hb030, hs011, hx040, hx050) %>% 
  collect(n = Inf)

c17h <- tbl(pg, "c17h") %>% 
  filter(hb020 %in% country) %>% 
  dplyr::select(hb010, hb020, hb030, hs011, hx040, hx050) %>% 
  collect(n = Inf)

c06h <- c06h %>% rename(hs011=hs010)
c07h <- c07h %>% rename(hs011=hs010)
c08h <- c08h %>% rename(hs011=hs010)
c09h <- c09h %>% rename(hs011=hs010)
c10h <- c10h %>% rename(hs011=hs010)

#join all years of hs011
hs011 <- bind_rows(c06h, c07h, c08h, c09h, c10h, c11h, c12h, c13h, c14h, c15h, c16h, c17h)

# Merge hs011 and silc.h
silc.h <- full_join(silc.h, hs011)

rm(hs011, c06h, c07h, c08h, c09h, c10h, c11h, c12h, c13h, c14h, c15h, c16h, c17h)


#Both datasets -----------------------------------------------------------

#r, p: exclude observations with personal id = NA

silc.p <- silc.p %>% drop_na(pb030)

silc.r <- silc.r %>% drop_na(rb030)


# Unique ids on personal level

silc.p <- silc.p %>% mutate(id_p = paste0(pb010, pb030))

silc.r <- silc.r %>% mutate(id_p = paste0(rb010, rb030))

# Merge r with p

silc.rp <- left_join(silc.r, silc.p)

# Unique ids on household level

silc.rp <- silc.rp %>% 
  mutate(id_h = paste0(rb010, rx030), age = rb010 - rb080)

silc.h <- silc.h %>% mutate(id_h = paste0(hb010, hb030))

# Merge rp with h

silc.arop <- left_join(silc.rp, silc.h)

# Use only data between 2007-2017
silc.arop <- silc.arop %>% filter(rb010>2006)

save(silc.arop, file="data/silc.arop.rda",compress = 'xz')


# Fin ---------------------------------------------------------------------
