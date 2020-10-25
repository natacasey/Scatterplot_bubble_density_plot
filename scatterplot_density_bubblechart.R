#importing libraries
library(ggplot2)
library(treemapify) 
library(tidyverse)
library(plyr)
library(reshape2)
library(ggplotify)
library("readxl")
library(ggExtra)
library(dplyr)
library(scales)
library(ggrepel)
library(grid)
#install.packages('gridExtra')
library(gridExtra)
#reading data to a data frame
df<-read.csv("crimerates-by-state-2005.csv")
head(df)
df <- df[-c(1, 10), ]

#xreating a scatterplot

p<-ggplot(df, aes(x=murder, y=burglary)) + geom_point(size = 2, color = 'orange')+
  geom_smooth(method = "loess",formula = y ~ x, size = 1,span =1.6,color = 'gray', se=FALSE)+
  
  labs(x = '\n     Murders (per 100,000 population)', y = 'Burglaries (per 100,000 population)\n')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line.x = element_line(size = 0.5, colour = "gray"),axis.line.y = element_line(size = 0.5, colour = "gray"), axis.ticks.y = element_blank(), axis.ticks.x = element_blank(), axis.text=element_text(size=12),axis.text.x = element_text(size = 11, colour = 'dimgray'), axis.text.y = element_text(size = 11, colour = 'dimgray'),  axis.title.y=element_text(size=13, colour ='dimgray'), axis.title.x=element_text(size=13, colour = 'dimgray'))+
  theme(plot.title = element_text(color = "dimgray"))+
  scale_x_continuous(limits=c(0,10),breaks=c(0,1, 2,3,4,5,6,7,8,9,10))+
  scale_y_continuous(limits=c(0,1300),breaks=c(0, 200,400,600,800,1000, 1200))
title.grob <- textGrob(
  label = "\nMURDER VERSUS BURGLARY IN THE US\n",
  x = unit(0, "lines"), 
  y = unit(0, "lines"),
  hjust = 0, vjust = 0,
  gp = gpar(fontsize = 14, col="dimgray"))

p1<- arrangeGrob(p, top = title.grob)
grid.draw(p1)

#density plot

plot<-ggplot(df, aes(x=burglary)) + 
  geom_density(fill = 'orange',colour = 'darkorange', alpha = 0.5)+
  scale_x_continuous(limits=c(0,1500), breaks = c(0,200,400,600,800,1000,1200,1400))+
  labs(x = 'Burglaries (per 100,000 population)\n', y = 'Density')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.ticks.y = element_blank(), axis.ticks.x = element_blank(), axis.text=element_text(size=12),axis.text.x = element_text(size = 11, colour = 'dimgray'), axis.text.y = element_text(size = 11, colour = 'dimgray'),  axis.title.y=element_text(size=13, colour ='dimgray'), axis.title.x=element_text(size=13, colour = 'dimgray'))+
  theme(plot.title = element_text(color = "dimgray"))
  
title.grob <- textGrob(
  label = '\nDISTRIBUTION OF BURGLARIES\n',
  x = unit(0, "lines"), 
  y = unit(0, "lines"),
  hjust = 0, vjust = 0,
  gp = gpar(fontsize = 14, col="dimgray"))


p2<- arrangeGrob(plot, top = title.grob)
grid.draw(p2)

#bubble chart

install.packages(ggrepel)
library(scales)
plt<-ggplot(df, aes(x=murder, y = burglary, size = population))+
  geom_point(alpha = 0.5, fill = 'orange', color = 'darkorange')+
  ggrepel::geom_text_repel(aes(label =state), colour = I(alpha("dimgray", 0.85)), size = 3 )+
  
  scale_size(range = c(0.5,23), labels = unit_format(unit = "M", scale = 1e-6))+
  
  scale_x_continuous(expand = c(0, 0), limits = c(0, 12),breaks=c(0,2,4,6,8,10))+
  
  scale_y_continuous(breaks=c(0,200,400,600,800,1000, 1200))+
  labs(x = '\n     Murders (per 100,000 population)', y = 'Burglaries (per 100,000 population)\n')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.ticks.y = element_blank(), axis.ticks.x = element_blank(), axis.text=element_text(size=12),axis.text.x = element_text(size = 11, colour = 'dimgray'), axis.text.y = element_text(size = 11, colour = 'dimgray'),  axis.title.y=element_text(size=13, colour ='dimgray'), axis.title.x=element_text(size=13, colour = 'dimgray'))+
  theme(plot.title = element_text(color = "black"))+
  theme(legend.position = 'right', legend.title = element_text(color = "dimgray", size = 9),
        legend.text = element_text(color = "dimgray", size =9))

title.grob <- textGrob(
  label = '\nMURDERS VERSUS BURGLARIES IN THE US SIZED BY POPULATION\n',
  x = unit(0, "lines"), 
  y = unit(0, "lines"),
  hjust = 0, vjust = 0,
  gp = gpar(fontsize = 14, col="dimgray"))

p3<- arrangeGrob(plt, top = title.grob)
grid.draw(p3)
