library(ggplot2)

library("RPostgreSQL")
#library("lme4") 
library("lmerTest")
library("optimx")
#library("dplyr")
drv <- dbDriver("PostgreSQL")
###creates a connection to the postgres database
### note that "con" will be used later in each connection to the database

con <- dbConnect(drv, dbname = "epc",port=5432, user="postgres",password=232323)

tran2<- dbGetQuery(con,"select * from ch5 where yearchi=2009")
head(tran2)
unique(tran2$yearchi)

data1<-

tran2 <- tran
yearui <- sort(unique(tran2$yearchi))
resutlla <- data.frame()
yy <- 0

setwd("D:/R/work/chapter2")


mlist <-unique(tran2$laua)
  
  ms <- 0
  j <- 1
  resultp <- data.frame()
  for(ms in mlist)
  {
    print(ms)
    print(j)
    autodata2 <- tran2[tran2$laua==ms,]
    group<- group_by(autodata2,laua)
    addsummary<- summarize(group,cor(price,tfarea,method="pearson"),n(),cor.test(price,tfarea,method="pearson")$conf.int[1],cor.test(price,tfarea,method="pearson")$conf.int[2],cor.test(price,tfarea,method="pearson")$p.value)
    temp <- data.frame()
    temp[1,"la"] <- addsummary[1,1]
    temp[1,"corr"]<- round(addsummary[1,2], digits=2)
    temp[1,"count"] <- addsummary[1,3]
    temp[1,"conf1"] <- addsummary[1,4]
    temp[1,"couf2"] <- addsummary[1,5]
    temp[1,"pvalue"] <- addsummary[1,6]
    resultp<- rbind( resultp,temp) 
    j <- j+1
    
  }
#E07000242
houseprice1 <-tran2[tran2$laua=="E09000001"|tran2$laua=="E07000177",]
unique(houseprice1$laua)
# setwd("D:/R/tran2")
# geo <- read.csv("geo1.csv")
# 
# head(geo)
# geo1 <- sqldf("select  distinct gor,RGN11NM from geo
#               ", drv="SQLite", dbname=":memory:" )
# 
# 
# geo5 <- sqldf("select  distinct  laua,ldnm,RGN11NM,gor from geo
#               ", drv="SQLite", dbname=":memory:" )
getwd()

houseprice1<-merge(houseprice1,geo5,by="laua")
unique(houseprice1$laua)
unique(houseprice1$ldnm)
head(houseprice1)
houseprice1<-houseprice1[,c("price","dateoftransfer","yearchi","tfarea","laua","ldnm","RGN11NM")]
write.csv(houseprice1,"housedata1.csv")



ggplot(houseprice1,aes(x=tfarea,y=price/1000))+
  geom_point(color="#56b4e9",size = 1.2)+
  facet_wrap(~ ldnm)+
  scale_y_continuous(name = "Transaction Price (in £1000s)",labels = scales::comma)+
  xlab(expression("Total floor area (" ~ m^2 ~ ")"))+
  theme_bw()+
  theme(axis.title = element_text(size=15),axis.text = element_text(size=13),strip.text = element_text(size=15))
 
data1<-tran2[tran2$laua=="E07000242",]
cor(data1$price,data1$tfarea, method = "pearson")

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