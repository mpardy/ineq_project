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
        
#2) At risk of poverty rate --------------------------------------------------

# Prepare data
arop <- data.frame(arop$value, arop$threshold)
arop <- arop %>% mutate (rb010 = 2006:2017)

# Create plot
ggplot3 <- ggplot(arop, aes(x=rb010)) + 
  geom_line(aes(y=arop.value, col = "Armutsgefaehrdung")) + 
  labs(title="Armutsgefaehrdungsquote", 
       subtitle="in Frankreich von 2006-2017", 
       caption="Eigene Ausarbeitung, EU-SILC Daten", 
       y ="Armutsgefaehrdung in %",
       x = "Jahre",
       col="Legende") +
  scale_x_continuous(breaks=c(2006, 2008, 2010, 2012, 2014, 2016, 2017)) +
  scale_y_continuous(breaks=c(10, 11.5, 12, 12.5, 13, 13.5, 14, 14.5, 15))

ggplot3

