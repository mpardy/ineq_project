# -------------------------------------------------------------------------
#
# Part6: Plots
# Group FR
#
# --------------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(eurostat)

#1) Mean and Median --------------------------------------------------------

indic <- indicators.p1[5:14,]

png("meanplot.png", height = 5, width = 7.2,
    units="in", res = 300)

ggplot(indic, aes(x = Jahr)) + 
  geom_line(aes(y = indic[,4], col = "Mittelwert")) + 
  geom_line(aes(y = indic[,7], col = "Median")) +
  labs(title="Median und Mittelwert", 
       subtitle="des verfügbaren Äquivalenzeinkommens in Frankreich, 2008 bis 2017", 
       caption="Eigene Ausarbeitung, EU-SILC Daten, Eurostat-Konzept, inflationsbereinigt (2015=100%",
       y ="reale Euro",
       x = "Jahr",
       col="Legende") +
  scale_x_continuous(breaks=c(2008 : 2017))

dev.off()

# 2) Gini-index -------------------------------------------------------------

png("Giniplot.png", height = 5, width = 7.2, units="in", res = 300)

ggplot(indic, aes(x = Jahr)) + 
  geom_line(aes(y = indic[,8], col = "Faktoreinkommen vor Steuern")) +
  geom_line(aes(y = indic[,9], col = "Nationaleinkommen vor Steuern")) +
  geom_line(aes(y = indic[,10], col = "verfügbares Äquivalenzeinkommen")) + 
  labs(title="Gini-Index", 
       subtitle="in Frankreich, 2008 bis 2017", 
       caption="Eigene Ausarbeitung, EU-SILC Daten, Eurostat-Konzept", 
       y ="Gini-Index",
       x = "Jahr",
       col = "Legende") +
  scale_x_continuous(breaks=c(2008 : 2017))

dev.off()

# 3) S80/S20 ----------------------------------------------------------------

png("s80plot.png", height = 5, width = 7.2, units="in", res = 300)

ggplot(indic, aes(x = Jahr)) + 
  geom_line(aes(y = indic[,11], col = "Nationaleinkommen vor Steuern")) + 
  geom_line(aes(y = indic[,12], col = "Verfügbares Einkommen")) + 
  labs(title = "S80/S20 Einkommensquintilverhältnis", 
       subtitle = "in Frankreich, 2008 bis 2017", 
       caption ="Eigene Ausarbeitung, EU-SILC Daten, Eurostat-Konzept", 
       y ="S80/S20 Verhältnis",
       x = "Jahr",
       col = "Legende") +
  scale_x_continuous(breaks=c(2008 : 2017)) +
  scale_y_continuous(breaks=c(4.5, 5, 5.5, 6, 6.5, 7, 7.5, 8))

dev.off()


# 4) Anteil der Top 10% -----------------------------------------------------

png("top10plot.png", height = 5, width = 7.2, units="in", res = 300)

ggplot(indic, aes(x = Jahr)) + 
  geom_line(aes(y = indic[,13], col = "Faktoreinkommen vor Steuern")) + 
  geom_line(aes(y = indic[,14], col = "Nationaleinkommen vor Steuern")) + 
  geom_line(aes(y = indic[,15], col = "Verfügbares Einkommen")) + 
  labs(title = "Anteil der Top 10 Prozent",
       subtitle = "am Gesamteinkommen in Frankreich, 2008 bis 2017", 
       caption="Eigene Ausarbeitung, EU-SILC Daten, Eurostat-Konzept", 
       y ="Anteil in %",
       x = "Jahr",
       col="Legende") +
  scale_x_continuous(breaks=c(2008 : 2017))

dev.off()
        
# 5) At risk of poverty rate, TOTAL ------------------------------------------

# Prepare data
arop1 <- data.frame(arop1$value, arop1$threshold)
arop1 <- arop1 %>% mutate(rb010 = 2008:2017)

# Create plot

png("arop_total.png", height = 5, width = 7.2, units="in", res = 300)

ggplot(arop1, aes(x=rb010)) + 
  geom_line(aes(y=arop1.value, color = "Anteil Armutsgefährdung")) + 
  labs(title="Armutsgefährdungsquote", 
       subtitle="in Frankreich von 2008 bis 2017", 
       caption="Eigene Ausarbeitung, EU-SILC Daten, Eurostat-Konzept", 
       y ="Armutsgefährdung in %",
       x = "Jahre",
       col="Legende") +
  scale_x_continuous(breaks=c(2008:2017))

dev.off()

#6) At risk of poverty rate, by gender ---------------------------------------

# Prepare data
arop2 <- data.frame(arop2$valueByStratum)

arop12 <- arop2 %>% filter(stratum==2) %>% 
  rename("female"=value, "stratum1"=stratum)

arop11 <- arop2 %>% filter(stratum==1) %>% 
  rename("male"=value)

arop2 <- left_join(arop12, arop11)

# Create plot

png("arop_gender.png", height = 5, width = 7.2, units="in", res = 300)

ggplot(arop2, aes(x=year)) + 
  geom_line(aes(y=female, color="Frauen ab 18 Jahren")) +
  geom_line(aes(y=male, color="Männer ab 18 Jahren")) +
  labs(title="Armutsgefährdungsquote nach Geschlecht", 
       subtitle="in Frankreich von 2008 bis 2017", 
       caption="Eigene Ausarbeitung, EU-SILC Daten, Eurostat-Konzept", 
       y ="Armutsgefährdung in %",
       x = "Jahre",
       col="Legende") +
  scale_x_continuous(breaks=c(2008:2017))

dev.off()

#7) At risk of poverty rate, by age ---------------------------------------

# AGE: 0-17, 18-64, >65

# Prepare Data

arop3 <- data.frame(arop3$valueByStratum)

arop31 <- arop3 %>% filter(stratum=="(0,17]") %>% 
  rename("until17"=value, "stratum1"=stratum)

arop32 <- arop3 %>% filter(stratum=="(17,65]") %>% 
  rename("17to65"=value)

arop33 <- arop3 %>% filter(stratum=="(65,120]") %>% 
  rename("65to120"=value)

arop3 <- bind_cols(arop31, arop32, arop33)

arop3 <- arop3[-1,]

# Create plot

png("arop_age.png", height = 5, width = 7.2, units="in", res = 300)

ggplot(arop3, aes(x=year)) + 
  geom_line(aes(y = arop3[,3], color="Unter 18")) +
  geom_line(aes(y = arop3[,6], color="17-65")) +
  geom_line(aes(y = arop3[,9], color="Über 64")) +
  labs(title="Armutsgefährdungsquote nach Alter", 
       subtitle="in Frankreich von 2008 bis 2017", 
       caption="Eigene Ausarbeitung, EU-SILC Daten, Eurostat-Konzept", 
       y ="Armutsgefährdung in %",
       x = "Jahre",
       col="Legende") +
  scale_x_continuous(breaks=c(2008 : 2017))

dev.off()

# Fin --------------------------------------------------------------------------

