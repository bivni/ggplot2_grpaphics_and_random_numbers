install.packages("WDI")
install.packages("tidyverse")

library(WDI)
WDIsearch("population")

dat <- WDI(indicator = c("gdp_per_capita" = "NY.GDP.PCAP.KD",
"population" = "SP.POP.TOTL"))
library(dplyr)
library(ggplot2)
str(dat)
dat2<- dat[dat$iso2c %in% c('RU','BR','US','CN'),]
dat2$name <- factor(dat2$iso2c)


gr1 <- ggplot(data=dat2,mapping=aes(x=year, y=population,col=name))

(gr2 <- gr1+geom_line())

print(gr2+geom_point(shape=2))

gr2+scale_y_log10()+theme_bw()


ggplot(data=dat2, mapping=aes(x=year,col=name,y=population))+
  geom_line()+
  xlim(NA,2015)+
  coord_flip()+
    theme_dark()


ggplot(data=dat2, mapping=aes(x=year,col=iso2c,y=population)) + geom_point()

gr=ggplot(data=dat2,
        mapping=aes(x=year,col=iso2c,y=gdp_per_capita)) +
        geom_point()
print(gr)
suppressWarnings(print(gr))
print(gr)
suppressWarnings(print(gr))
gr+xlim(c(1995,2010))
gr+xlim(c(1995,2010))+geom_line()
gr+xlim(c(1995,2010))+ylim(c(3000,45000)) + geom_line()
gr+xlim(c(1995,2010))+ylim(NA,45000) + geom_line()

big2010 <- dat[dat$iso2c %in% c("US","BR","RU","CN")
                                   &dat$year==2010 ,]

library(forcats)
str(big2010)
big2010$name<- fct_reorder(
    big2010$iso2c,
    big2010$population,.desc = T)
str(big2010)
ggplot(big2010,
       mapping = aes(x=name,y=population))+
      geom_bar(stat="identity",fill="wheat",
               col="red")+
     coord_flip()
  
  #geom_bar(stat="identity",fill="wheat",col="red")
big5years <-  dat[dat$iso2c %in% c("US","BR","RU","CN")
                  &dat$year %in% c(1970 ,1980, 1990,2000,2010),]
big5years$name <- factor(big5years$iso2c,levels=c("CN","US","BR","RU"),ordered=T)


ggplot(big5years,mapping = aes(x=name,y=population))+
  geom_bar(stat="identity",fill="wheat",col="red")+facet_wrap(~year,ncol = 2)


