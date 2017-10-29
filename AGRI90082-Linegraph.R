library(ggplot2)
data=read.csv('YBratio.csv')

cols = c(3,4,5)

#transform data structure to means and standard deviations
data2 <- transform(data, mean1 =rowMeans(data[,cols]), sd=apply(data[,cols],1, sd))[,-(3:5)]

#plot line graph
ggplot(data = data2,aes(x=Time, y=mean1, group=1)) +
  geom_line(aes(group=Sample), color = "#666666", linetype = "solid") + 
  geom_point(aes(shape=factor(Sample)))+ 
  scale_x_continuous(breaks=c(0,6,12,24,48), labels=c("0","6","12","24","48")) +
  scale_shape_manual(values=c(0,1,24,15,16,3)) + scale_size() +theme_classic() +
  theme(legend.position=c(0.9,0.8),legend.text=element_text(size=8), 
        legend.title=element_blank()) + 
  labs(x="Time(min)", y = "g CO2 / time") +
  theme(title = element_text(size=10, face='bold', hjust=0.5))