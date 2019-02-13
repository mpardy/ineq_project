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

png("meanplot.png", height = 5, width = 7.2,
    units="in", res = 500)

ggplot(indicators.p1, aes(x = Jahr)) + 
  geom_line(aes(y = indicators.p1[,4], col = "Mittelwert")) + 
  geom_line(aes(y = indicators.p1[,7], col = "Median")) +
  labs(title="Median und Mittelwert", 
       subtitle="des verfügbaren Äquivalenzeinkommens in Frankreich, 2004-2017", 
       caption="Eigene Ausarbeitung, EU-SILC Daten, Eurostat-Konzept
       \n*Zeitreihenbruch von 2007 auf 2008",
       y ="reale Euro",
       x = "Jahr",
       col="Legende")

dev.off()

# 2) Gini-index -------------------------------------------------------------

png("Giniplot.png", height = 5, width = 7.2, units="in", res = 500)

ggplot(indicators.p1, aes(x = Jahr)) + 
  geom_line(aes(y = indicators.p1[,8], col = "Faktoreinkommen vor Steuern")) +
  geom_line(aes(y = indicators.p1[,9], col = "Nationaleinkommen vor Steuern")) +
  geom_line(aes(y = indicators.p1[,10], col = "verfügbares Äquivalenzeinkommen")) + 
  labs(title="Gini-Index", 
       subtitle="in Frankreich, 2004-2017", 
       caption="Eigene Ausarbeitung, EU-SILC Daten, Eurostat-Konzept
       \n*Zeitreihenbruch von 2007 auf 2008", 
       y ="Gini-Index",
       x = "Jahr",
       col = "Legende")

dev.off()

# 3) S80/S20 ----------------------------------------------------------------

png("s80plot.png", height = 5, width = 7.2, units="in", res = 500)

ggplot(indicators.p1, aes(x = Jahr)) + 
  geom_line(aes(y = indicators.p1[,11], col = "Nationaleinkommen vor Steuern")) + 
  geom_line(aes(y = indicators.p1[,12], col = "Verfügbares Einkommen")) + 
  labs(title = "S80/S20 Einkommensquintilverhältnis", 
       subtitle = "in Frankreich, 2004-2017", 
       caption ="Eigene Ausarbeitung, EU-SILC Daten, Eurostat-Konzept
       \n*Zeitreihenbruch von 2007 auf 2008", 
       y ="S80/S20 Verhältnis",
       x = "Jahr",
       col = "Legende")

dev.off()


# 4) Anteil der Top 10% -----------------------------------------------------

png("top10plot.png", height = 5, width = 7.2, units="in", res = 500)

ggplot(indicators.p1, aes(x = Jahr)) + 
  geom_line(aes(y = indicators.p1[,13], col = "Faktoreinkommen vor Steuern")) + 
  geom_line(aes(y = indicators.p1[,14], col = "Nationaleinkommen vor Steuern")) + 
  geom_line(aes(y = indicators.p1[,15], col = "Verfügbares Einkommen")) + 
  labs(title = "Anteil der Top 10 Prozent",
       subtitle = "am Gesamteinkommen in Frankreich, 2004-2017", 
       caption="Eigene Ausarbeitung, EU-SILC Daten, Eurostat-Konzept
       \n*Zeitreihenbruch von 2007 auf 2008", 
       y ="Anteil in %",
       x = "Jahr",
       col="Legende")

dev.off()
        
# 5) At risk of poverty rate, TOTAL ------------------------------------------

# Prepare data
arop1 <- data.frame(arop1$value, arop1$threshold)
arop1 <- arop1 %>% mutate (rb010 = 2007:2017)

# Create plot

library(ggplot2)

ggplot5 <- ggplot(arop1, aes(x=rb010)) + 
  geom_line(aes(y=arop1.value, color = "Anteil Armutsgefährdung")) + 
  labs(title="Armutsgefährdungsquote", 
       subtitle="in Frankreich von 2007-2017", 
       caption="Eigene Ausarbeitung, EU-SILC Daten", 
       y ="Armutsgefährdung in %",
       x = "Jahre",
       col="Legende") +
  scale_x_continuous(breaks=c(2007:2017))

ggplot5

#6) At risk of poverty rate, by gender ---------------------------------------

# Prepare data
arop2 <- data.frame(arop2$valueByStratum)

arop12 <- arop2 %>% filter(stratum==2) %>% 
  rename("female"=value, "stratum1"=stratum)

arop11 <- arop2 %>% filter(stratum==1) %>% 
  rename("male"=value)

arop2 <- left_join(arop12, arop11)

arop2

# Create plot
ggplot6 <- ggplot(arop2, aes(x=year)) + 
  geom_line(aes(y=female, color="Frauen")) +
  geom_line(aes(y=male, color="Männer")) +
  labs(title="Armutsgefährdungsquote nach Geschlecht", 
       subtitle="in Frankreich von 2007-2017", 
       caption="Eigene Ausarbeitung, EU-SILC Daten", 
       y ="Armutsgefährdung in %",
       x = "Jahre",
       col="Legende") +
  scale_x_continuous(breaks=c(2007:2017))

ggplot6

#7) At risk of poverty rate, by age ---------------------------------------

# AGE: 0-17, 18-64, >65

# Prepare Data
arop31 <- arop3 %>% filter(stratum=="(0,17]") %>% 
  rename("until17"=value, "stratum1"=stratum)

arop32 <- arop3 %>% filter(stratum=="(17,65]") %>% 
  rename("17to65"=value)

arop33 <- arop3 %>% filter(stratum=="(65,120]") %>% 
  rename("65to120"=value)

arop3 <- bind_cols(arop31, arop32, arop33)

# Create plot
ggplot7 <- ggplot(arop3, aes(x=year)) + 
  geom_line(aes(y=aropunder18, color="Unter 18")) +
  geom_line(aes(y=aropbetween, color="17-65")) +
  geom_line(aes(y=aropover64, color="Über 64")) +
  labs(title="Armutsgefährdungsquote nach Alter", 
       subtitle="in Frankreich von 2007-2017", 
       caption="Eigene Ausarbeitung, EU-SILC Daten", 
       y ="Armutsgefährdung in %",
       x = "Jahre",
       col="Legende") +
  scale_x_continuous(breaks=c(2007 : 2017)) +
  scale_y_continuous(limits=c(7,16), breaks=seq(7,16, by = 1))

ggplot7

# Fin --------------------------------------------------------------------------

