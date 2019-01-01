# ------------------------------------------------------------------------
#
# Part 4: Setup poverty
# Group FR
#
# -------------------------------------------------------------------------

library(dplyr)
library(tidyr)

"country" <- "FR"

if(!exists(c("country"))) {
  stop("please specify country and year")
}


# Prepare Data ------------------------------------------------------------

#1) Severe material deprivation rate & Population living in very low intensity (quasijobless) households

# Download data 2004-2013:

silc.p <- tbl(pg, "pp") %>%
  filter(pb020 %in% country) %>%
  dplyr::select(pb010, pb020, pb030, py010g, py050g, pl031, pl060, pl073, pl074, pl075, pl076, px030) %>%
  collect(n = Inf)

#pl100 and pl035 not included

silc.h <- tbl(pg, "hh") %>%
  filter(hb020 %in% country) %>%
  select(hb010, hb020, hb030, hs110, hh050, hs040, hs050, hs060, hs070, hs080, hs100, hx040, hx050) %>%
  collect(n = Inf)

silc.r <- tbl(pg, "rr") %>%
  filter(rb020 %in% country) %>%
  select(rb010, rb020, rb030, rb050, rb080, rb090, rx030) %>%
  collect(n = Inf)

# Need to download hs011 seperately: The Living conditions Working Group agreed during its 2010 meeting to suppress HS010 and to
#keep only HS011 starting from the 2011 operation. 
# difference hs011 & hs010: hs010 only asks if you have ever had arrears on mortage and rent payments and not once or twice as hs011 does

#get data for hs011

c06h <- tbl(pg, "c06h") %>% 
  filter(hb020 %in% country) %>% 
  select(hb010, hb020, hb030, hs010, hx040, hx050) %>% 
  collect(n = Inf)

c07h <- tbl(pg, "c07h") %>% 
  filter(hb020 %in% country) %>% 
  select(hb010, hb020, hb030, hs010, hx040, hx050) %>% 
  collect(n = Inf)

c08h <- tbl(pg, "c08h") %>% 
  filter(hb020 %in% country) %>% 
  select(hb010, hb020, hb030, hs010, hx040, hx050) %>% 
  collect(n = Inf)

c09h <- tbl(pg, "c09h") %>% 
  filter(hb020 %in% country) %>% 
  select(hb010, hb020, hb030, hs010, hx040, hx050) %>% 
  collect(n = Inf)

c10h <- tbl(pg, "c10h") %>% 
  filter(hb020 %in% country) %>% 
  select(hb010, hb020, hb030, hs010, hx040, hx050) %>% 
  collect(n = Inf)

c11h <- tbl(pg, "c11h") %>% 
  filter(hb020 %in% country) %>% 
  select(hb010, hb020, hb030, hs011, hx040, hx050) %>% 
  collect(n = Inf)

c12h <- tbl(pg, "c12h") %>% 
  filter(hb020 %in% country) %>% 
  select(hb010, hb020, hb030, hs011, hx040, hx050) %>% 
  collect(n = Inf)

c13h <- tbl(pg, "c13h") %>% 
  filter(hb020 %in% country) %>% 
  select(hb010, hb020, hb030, hs011, hx040, hx050) %>% 
  collect(n = Inf)

c14h <- tbl(pg, "c14h") %>% 
  filter(hb020 %in% country) %>% 
  select(hb010, hb020, hb030, hs011, hx040, hx050) %>% 
  collect(n = Inf)

c15h <- tbl(pg, "c15h") %>% 
  filter(hb020 %in% country) %>% 
  select(hb010, hb020, hb030, hs011, hx040, hx050) %>% 
  collect(n = Inf)

c16h <- tbl(pg, "c16h") %>% 
  filter(hb020 %in% country) %>% 
  select(hb010, hb020, hb030, hs011, hx040, hx050) %>% 
  collect(n = Inf)

c17h <- tbl(pg, "c17h") %>% 
  filter(hb020 %in% country) %>% 
  select(hb010, hb020, hb030, hs011, hx040, hx050) %>% 
  collect(n = Inf)

hs010 <- bind_rows(c06h, c07h, c08h, c09h, c10h, c11h, c12h, c13h, c14h, c15h, c16h, c17h)

silc.h <- left_join(silc.h, hs010 %>% select(hs011, hs010, hb010, hb030))
rm(hs010, c07h, c08h, c09h, c10h, c11h, c12h, c13h, c14h, c15h, c16h, c17h)