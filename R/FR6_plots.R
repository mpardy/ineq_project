# -------------------------------------------------------------------------
#
# Part6: Plots
# Group FR
#
# --------------------------------------------------------------------------

library(dplyr)
library(ggplot2)

#1) Mean and Median --------------------------------------------------------

ggplot1 <- ggplot(indicators.p1, aes(x = indicators.p1[,1])) + 
  geom_line(aes(y = indicators.p1[,4], col = "Mittelwert")) + 
  geom_line(aes(y = indicators.p1[,7], col = "Median")) +
  labs(title="Median und Mittelwert", 
       subtitle="des verfügbaren Äquivalenzeinkommen in Frankreich, 2004-2017", 
       caption="Eigene Ausarbeitung, EU-SILC Daten", 
       y ="reale Euro",
       x = "Jahre",
       col="Legende")

ggplot1

# 2) Gini coefficient

ggplot2 <- ggplot(indicators.p1, aes(x = indicators.p1[,1])) + 
  geom_line(aes(y = indicators.p1[,8], col = "Faktoreinkommen vor Steuern")) +
  geom_line(aes(y = indicators.p1[,9], col = "Nationaleinkommen vor Steuern")) +
  geom_line(aes(y = indicators.p1[,10], col = "verfügbares Äquivalenzeinkommen")) + 
  labs(title="Gini Koeffizient", 
       subtitle="des verfügbaren Äquivalenzeinkommen in Frankreich, 2004-2017", 
       caption="Eigene Ausarbeitung, EU-SILC Daten", 
       y ="Gini-Koeffizient",
       x = "Jahre",
       col = "Legende")

ggplot2

# 3) S80/S20

ggplot3 <- ggplot(indicators.p1, aes(x = indicators.p1[,1])) + 
  geom_line(aes(y = indicators.p1[,12], col = "Nationaleinkommen vor Steuern")) + 
  geom_line(aes(y = indicators.p1[,13], col = "Verfügbares Einkommen")) + 
  labs(title = "S80/S20 Einkommensquintilverhältnis", 
       subtitle = "in Frankreich, 2004-2017", 
       caption="Eigene Ausarbeitung, EU-SILC Daten", 
       y ="S80/S20 Verhältnis",
       x = "Jahre",
       col="Legende")

ggplot3


# 4) Anteil der Top 10%

ggplot4 <- ggplot(indicators.p1, aes(x = indicators.p1[,1])) + 
  geom_line(aes(y = indicators.p1[,14], col = "Faktoreinkommen vor Steuern")) + 
  geom_line(aes(y = indicators.p1[,15], col = "Nationaleinkommen vor Steuern")) + 
  geom_line(aes(y = indicators.p1[,16], col = "Verfügbares Einkommen")) + 
  labs(title = "Anteil der Top 10%", 
       subtitle = "am Gesamteinkommen in Frankreich, 2004-2017", 
       caption="Eigene Ausarbeitung, EU-SILC Daten", 
       y ="Anteil in %",
       x = "Jahre",
       col="Legende")

ggplot4
        
#3) At risk of poverty rate, TOTAL ------------------------------------------

# Prepare data
arop <- data.frame(arop$value, arop$threshold)
arop <- arop %>% mutate (rb010 = 2007:2017)

# Create plot
ggplot5 <- ggplot(arop, aes(x=rb010)) + 
  geom_line(aes(y=arop.value, color="Anteil Armutsgefaehrdung")) + 
  labs(title="Armutsgefaehrdungsquote", 
       subtitle="in Frankreich von 2007-2017", 
       caption="Eigene Ausarbeitung, EU-SILC Daten", 
       y ="Armutsgefaehrdung in %",
       x = "Jahre",
       col="Legende") +
  scale_x_continuous(breaks=c(2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)) +
  scale_y_continuous(limits=c(11,15), breaks=seq(11,15, by = 0.5))

ggplot5

library(eurostat)

#4) At risk of poverty rate, by gender ---------------------------------------

# Prepare data
arop11 <- data.frame(arop1$valueByStratum)
arop12 <- arop11 %>% filter(stratum==2) %>% rename("female"=value, "stratum1"=stratum)
arop11 <- arop11 %>% filter(stratum==1) %>% rename("male"=value)
arop1 <- left_join(arop12, arop11)

# Create plot
ggplot6 <- ggplot(arop1, aes(x=year)) + 
  geom_line(aes(y=female, color="Armutsgefaehrdung Frauen")) +
  geom_line(aes(y=male, color="Armutsgefaehrdung Maenner")) +
  labs(title="Armutsgefaehrdungsquote nach Geschlecht", 
       subtitle="in Frankreich von 2007-2017", 
       caption="Eigene Ausarbeitung, EU-SILC Daten", 
       y ="Armutsgefaehrdung in %",
       x = "Jahre",
       col="Legende") +
  scale_x_continuous(breaks=c(2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)) +
  scale_y_continuous(limits=c(10,15), breaks=seq(10,15, by = 1))

ggplot6
