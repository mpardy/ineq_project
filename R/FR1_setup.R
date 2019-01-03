# ------------------------------------------------------------------------
#
# Part 1: Setup
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

# Download data 2004-2013:
silc.p <- tbl(pg, "pp") %>%
  filter(pb020 %in% country) %>%
  select(pb010, pb020, pb030, py010g, py050g, py080g, 
         py090g, py100g, py110g, py120g, py130g, py140g, px030) %>%
  collect(n = Inf)

# py021 (company car) included in py010 (FR-specific)

silc.h <- tbl(pg, "hh") %>%
  filter(hb020 %in% country) %>%
  select(hb010, hb020, hb030, hy010, hy020, hy030g, hy040g, 
         hy050g, hy060g, hy070g, hy080g, hy090g, hy110g, 
         hy120g, hy130g, hy140g, hx040, hx050) %>%
  collect(n = Inf)

silc.r <- tbl(pg, "rr") %>%
  filter(rb020 %in% country) %>%
  select(rb010, rb020, rb030, rb050, rb080, rb090, rx030) %>%
  collect(n = Inf)

#Download data 2014-2016

#p

silc.p14 <- tbl(pg, "c14p") %>%
  filter(pb020 %in% country) %>%
  select(pb010, pb020, pb030, py010g, py050g, py080g, 
         py090g, py100g, py110g, py120g, py130g, py140g, px030) %>%
  collect(n = Inf)

silc.p15 <- tbl(pg, "c15p") %>%
  filter(pb020 %in% country) %>%
  select(pb010, pb020, pb030, py010g, py050g, py080g, 
         py090g, py100g, py110g, py120g, py130g, py140g, px030) %>%
  collect(n = Inf)

silc.p16 <- tbl(pg, "c16p") %>%
  filter(pb020 %in% country) %>%
  select(pb010, pb020, pb030, py010g, py050g, py080g, 
         py090g, py100g, py110g, py120g, py130g, py140g, px030) %>%
  collect(n = Inf)

silc.p17 <- tbl(pg, "c17p") %>%
  filter(pb020 %in% country) %>%
  select(pb010, pb020, pb030, py010g, py050g, py080g, 
         py090g, py100g, py110g, py120g, py130g, py140g, px030) %>%
  collect(n = Inf)

#h

silc.h14 <- tbl(pg, "c14h") %>%
  filter(hb020 %in% country) %>%
  select(hb010, hb020, hb030, hy010, hy020, hy030g, hy040g, 
         hy050g, hy060g, hy070g, hy080g, hy090g, hy110g, 
         hy120g, hy130g, hy140g, hx040, hx050) %>%
  collect(n = Inf)

silc.h15 <- tbl(pg, "c15h") %>%
  filter(hb020 %in% country) %>%
  select(hb010, hb020, hb030, hy010, hy020, hy030g, hy040g, 
         hy050g, hy060g, hy070g, hy080g, hy090g, hy110g, 
         hy120g, hy130g, hy140g, hx040, hx050) %>%
  collect(n = Inf)

silc.h16 <- tbl(pg, "c16h") %>%
  filter(hb020 %in% country) %>%
  select(hb010, hb020, hb030, hy010, hy020, hy030g, hy040g, 
         hy050g, hy060g, hy070g, hy080g, hy090g, hy110g, 
         hy120g, hy130g, hy140g, hx040, hx050) %>%
  collect(n = Inf)

silc.h17 <- tbl(pg, "c17h") %>%
  filter(hb020 %in% country) %>%
  select(hb010, hb020, hb030, hy010, hy020, hy030g, hy040g, 
         hy050g, hy060g, hy070g, hy080g, hy090g, hy110g, 
         hy120g, hy130g, hy140g, hx040, hx050) %>%
  collect(n = Inf)

#r

silc.r14 <- tbl(pg, "c14r") %>%
  filter(rb020 %in% country) %>%
  select(rb010, rb020, rb030, rb050, rb080, rb090, rx030) %>%
  collect(n = Inf)


silc.r15 <- tbl(pg, "c15r") %>%
  filter(rb020 %in% country) %>%
  select(rb010, rb020, rb030, rb050, rb080, rb090, rx030) %>%
  collect(n = Inf)


silc.r16 <- tbl(pg, "c16r") %>%
  filter(rb020 %in% country) %>%
  select(rb010, rb020, rb030, rb050, rb080, rb090, rx030) %>%
  collect(n = Inf)

silc.r17 <- tbl(pg, "c17r") %>%
  filter(rb020 %in% country) %>%
  select(rb010, rb020, rb030, rb050, rb080, rb090, rx030) %>%
  collect(n = Inf)

# Merge all datasets -----------------------------------------

#merge years

silc.p <- bind_rows(silc.p, silc.p14, silc.p15, silc.p16, silc.p17)

silc.h <- bind_rows(silc.h, silc.h14, silc.h15, silc.h16, silc.h17)

silc.r <- bind_rows(silc.r, silc.r14, silc.r15, silc.r16, silc.r17)

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

silc.rph <- left_join(silc.rp, silc.h)


# remove superfluous columns

silc.rph <- silc.rph %>% select(-c(pb010, pb020, pb030, px030, hb010,
                                    hb020, hb030, rb030, rb080, rx030))

# exclude observations with unknown hh-size

silc.rph <- silc.rph %>% drop_na(hx040)

# Convert NA of income variables to 0 to avoid aggregation issues

silc.rph[is.na(silc.rph)] <- 0

# Store mutated datasets to disk --------------------------------

save(silc.rph, file="data/rph.rda",compress = 'xz')
