# -------------------------------------------------------------------------
#
# Part6: Plots
# Group FR
#
# --------------------------------------------------------------------------

library(dplyr)
library(ggplot2)

#1) Mean and Median --------------------------------------------------------

# Prepare data
meanmedian <- left_join(median.p1, mean.p1)
meanmedian <- meanmedian %>% rename(Mittelwert = statistic.y13, Median = statistic3)

# Create plot
ggplot1 <- ggplot(meanmedian, aes(x=rb010)) + 
  geom_line(aes(y=Mittelwert, col = "Mittelwert")) + 
  geom_line(aes(y=Median, col = "Median")) +
  labs(title="Median und Mittelwert", 
       subtitle="in Frankreich von 2004-2017", 
       caption="Eigene Ausarbeitung, EU-SILC Daten", 
       y ="Verfügbares Äquivalenzeinkommen",
       x = "Jahre",
       col="Legende")

ggplot1

#2) Share Top 10% ----------------------------------------------------------
        
# Prepare data
top11 <- top11 %>% mutate(rb010 = 2004:2017)

# Create plot
ggplot2 <- ggplot(top11, aes(x=rb010)) + 
  geom_line(aes(y=y11, col = "Anteil Top 10%")) + 
  labs(title="Anteil der Top 10% am Gesamtbruttoeinkommen", 
       subtitle="in Frankreich von 2004-2017", 
       caption="Eigene Ausarbeitung, EU-SILC Daten", 
       y ="Anteil am Gesamteinkommen in %",
       x = "Jahre",
       col="Legende")

ggplot2
        
#3) At risk of poverty rate, TOTAL ------------------------------------------

# Prepare data
arop <- data.frame(arop$value, arop$threshold)
arop <- arop %>% mutate (rb010 = 2007:2017)

# Create plot
ggplot3 <- ggplot(arop, aes(x=rb010)) + 
  geom_line(aes(y=arop.value, color="Anteil Armutsgefaehrdung")) + 
  labs(title="Armutsgefaehrdungsquote", 
       subtitle="in Frankreich von 2007-2017", 
       caption="Eigene Ausarbeitung, EU-SILC Daten", 
       y ="Armutsgefaehrdung in %",
       x = "Jahre",
       col="Legende") +
  scale_x_continuous(breaks=c(2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)) +
  scale_y_continuous(limits=c(11,15), breaks=seq(11,15, by = 0.5))

ggplot3

library(eurostat)

#4) At risk of poverty rate, by gender ---------------------------------------

# Prepare data
arop11 <- data.frame(arop1$valueByStratum)
arop12 <- arop11 %>% filter(stratum==2) %>% rename("female"=value, "stratum1"=stratum)
arop11 <- arop11 %>% filter(stratum==1) %>% rename("male"=value)
arop1 <- left_join(arop12, arop11)

# Create plot
ggplot4 <- ggplot(arop1, aes(x=year)) + 
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

ggplot4
