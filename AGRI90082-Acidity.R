library(ggplot2)
library(dplyr)
data=read.csv('TTA.csv')

#data for 2 rows, 3 columns
data$Dough = factor(data$Dough, levels=c("S5","S4","S2","S6","S1","S3"))
data$Sample = factor(data$Sample, levels =c("Control","Lb.plantarum","Lb.brevis",
                                            "Lb.rossiae", "Lb.casei","A.malorum"))

#give a color and shape to each species
group.colors = c("Control" = "#333333", "Lb.plantarum" = "#000099", 
                 "Lb.brevis" ="#9933CC", "Lb.rossiae" = "#3399FF", 
                 "Lb.casei" = "#9966CC", "A.malorum" = "#3399CC")
group.shapes = c("Control" = 8, "Lb.plantarum" = 0, 
                 "Lb.brevis" =1, "Lb.rossiae" = 6, 
                 "Lb.casei" = 15, "A.malorum" = 17)

#plot 6 graphs with facetwrap
ggplot(data,aes(x=Time, y=Measurement, color=factor(Sample))) + 
  geom_point(aes(shape=factor(Sample))) +
  geom_line(aes(group=Sample)) +
  facet_wrap(~ Dough, ncol = 3,scales = "free_x", 
             labeller = as_labeller(c(S1="Yeast + Bacteria + Gliadin",
                                      S2="Bacteria + Gliadin + Papain",
                                      S3="Yeast + Bacteria + Gliadin + Papain",
                                      S4="Bacteria + Gliadin",
                                      S5="Bacteria",
                                      S6="Yeast + Bacteria"))) +
  labs(x="Time(min)", y = "Acidity (%)") + theme_classic() +
  scale_x_continuous(breaks = c(0,6,12,24,48)) +
  scale_shape_manual(values = group.shapes) + 
  scale_color_manual(values = group.colors) +
  theme(legend.position="right",legend.text=element_text(size=9),
        legend.title=element_blank(),strip.text = element_text(size=12))


ggsave("TTA.png", width=7, height=11, dpi=600) 