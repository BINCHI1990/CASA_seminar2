library(ggplot2)

# read in the first dataset
housedata1<-read.csv("data1.csv")
# read in the second dataset
housedata2 <- read.csv("data2.csv")
result1 <- data.frame()
ms <- 0
autodata <-0
j <- 1
list <-unique(housedata2$yearchi)
for(ms in list)
{
  print(ms)
  print(j)
  autodata <- housedata2[housedata2$yearchi==ms,]
  autodata$ranknew <- rank(-autodata$priceper)
  autodata$ranknew1 <- rank(autodata$priceper)
  result1 <- rbind(result1,autodata)
  j <- j+1
  
}


ggplot(data=result1,aes(x=yearchi, y=ranknew,group=region,color=region))+
  geom_line(aes(color = region, alpha = 1), size = 0.7) +
  geom_point(data = result1[(result1$yearchi == "2009"|result1$yearchi == "2016"),],
             aes(x=yearchi, y=ranknew,group=region,color = region),
             size = 7)+
  geom_text(data = result1[result1$yearchi == "2009",], 
            aes(label = region) , 
            position = position_nudge(x = -3.7),
            hjust = 0, 
            size = 5) +
  geom_text(data = result1[result1$yearchi == "2016",], 
            aes(label = region) , 
            position = position_nudge(x = 0.4),
            hjust = 0,
            size = 5) +
  geom_label(data = result1[(result1$yearchi == "2009"|result1$yearchi == "2016"),],
             aes(label = ranknew), 
             color="white",
             alpha = 0,
             size = 5,
             label.padding = unit(0.00, "lines"), 
             label.size = 0.0)+
  theme_bw()+
  theme(legend.position = "none",
        panel.border= element_blank(),
        axis.line = element_blank(),
        axis.text.y  = element_blank(),
        axis.title = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks = element_blank()
  ) +
  scale_y_reverse( breaks = unique(result1$ranknew))+
  scale_x_continuous(breaks = c(2009,2010,2011,2012,2013,2014,2015,2016))+
  expand_limits(x=c(2009,2020))+
  theme(axis.title = element_text(size=18), axis.text = element_text(size=15),strip.text.x = element_text(size=20),legend.text = element_text(size=18),legend.title = element_text(size=18))