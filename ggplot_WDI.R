dat <- WDI(indicator = c("gdp_per_capita" = "NY.GDP.PCAP.KD",
"population" = "SP.POP.TOTL"))
library(dplyr)
library(ggplot2)
dat2=dat[dat$iso2c %in% c('RU','BR','US'),]
ggplot(data=dat2, mapping=aes(x=year,col=iso2c,y=population)) + geom_point()
gr=ggplot(data=dat2, mapping=aes(x=year,col=iso2c,y=gdp_per_capita)) + geom_point()
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
str(big2010)
big2010$name<- fct_reorder(big2010$iso2c,big2010$population,.desc = T)
str(big2010)
ggplot(big2010,mapping = aes(x=name,y=population))+
  geom_bar(stat="identity",fill="wheat",col="red")
big5years <-  dat[dat$iso2c %in% c("US","BR","RU","CN")
                  &dat$year %in% c(1970 ,1980, 1990,2000,2010),]
big5years$name <- factor(big5years$iso2c,levels=c("CN","US","BR","RU"),ordered=T)


ggplot(big5years,mapping = aes(x=name,y=population))+
  geom_bar(stat="identity",fill="wheat",col="red")+facet_wrap(~year,ncol = 2)


