library(ggplot2)
library(plotrix)
library(dplyr)
library(multcompView)
library(multcomp)
library(agricolae)

data=read.csv('GasProd2.csv')

cols = c(2,3)

#order the data by adding a column
sub <- data2[]
data$Strain <- c("S.cerevisiae","L. plantarum","L.brevis","L.casei","L.rossiae","A.malorum","L. plantarum","L.brevis","L.casei","L.rossiae","A.malorum")
data$Strain <- factor(data2$Strain, levels = c("A.malorum","L.rossiae","L.casei","L.brevis","L. plantarum","S.cerevisiae"))
data$label1 <- factor(c(2,1,1,1,1,1,2,2,2,2,2))

#calculate mean and sd for each strain
data2= group_by(mydata, Strain) %>%
  summarise(
    count = n(),
    mean2 = mean(mean1, na.rm = TRUE),
    sd2 = sd(sd1, na.rm = TRUE)
  )

#one way ANOVA
ANOVA=aov(mean1 ~ Strain, data = mydata)
summary(ANOVA)

myTukey=TukeyHSD(x=ANOVA, conf.level = 0.95, return=TRUE)
myTukey

comparison = HSD.test(ANOVA, "Strain", group=TRUE)
groups1 <- as.vector(comparison$groups)
groups1$groups

#graph and add HSD labels
ggplot(data = data2,aes(x=StrainCom, y=mean1, fill=label1, width=.4)) + 
  geom_bar(position="dodge",stat="identity",colour = "black") +
  geom_errorbar(aes(ymin=mean1, ymax=mean1+sd), linetype="solid", position = "dodge") +
  labs(x="Strain combinations", y = "ml CO2 / g dough") +
  theme_classic() +  
  theme(axis.text.x = element_text(size=10,vjust=0.55,hjust=1),
        axis.title.y=element_text(size=10,angle=90),
        title = element_text(size=10, face='bold', hjust=0.5)) +
  scale_fill_manual(labels=c("without","with"),values = c("white","black"))+
  coord_flip() + labs(fill="S.cerevisiae")
