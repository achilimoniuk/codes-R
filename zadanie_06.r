# https://ec.europa.eu/eurostat/web/population-demography/demography-population-stock-balance/database?node_code=demomwk
# https://appsso.eurostat.ec.europa.eu/nui/submitViewTableAction.do
# https://appsso.eurostat.ec.europa.eu/nui/setupDownloads.do
rm(list=ls())
dataDir    <- file.path(getwd(),"data")
library("tidyverse")
# rozpakowywanie danych  
unzip(file.path(dataDir,"demo_r_mwk_ts.zip"),exdir=file.path(dataDir),setTimes=T)

rm(list=ls())

d <- read.table(file="demo_r_mwk_ts_1_Data.csv",sep=",",dec=",",header=T,stringsAsFactors=F)
print(head(d))
print(str(d))

# foramtowanie kolumny Value z char to integer 
d$Value <- as.integer(gsub(",|:","",d$Value))
print(head(d,10))

print(sort(unique(d$GEO)))

d$rok <- substr(d$TIME, 1, 4)
d <- d[d$rok==c(2018,2019, 2020, 2021), c("rok", "GEO", "SEX", "Value", "TIME")]
d <- subset(d, SEX!="Total")
d <- d[!(d$TIME=="2018W49" | d$TIME=="2018W50"| d$TIME=="2018W51"| d$TIME=="2018W52"| d$TIME=="2019W49"| d$TIME=="2019W50"| d$TIME=="2019W51"| d$TIME=="2019W52'| d$TIME=='2020W49"| d$TIME=="2020W50"| d$TIME=="2020W51"| d$TIME=="2020W52"| d$TIME=="2020W53"), ]
d <- d[ , c("rok", "GEO", "SEX", "Value")]
d <- d %>% drop_na() 


d2 <- d[d$rok==c(2018,2019), ]
d2 <- d2 %>%
  group_by(GEO, SEX) %>%
  summarise(sum = sum(Value))
names(d2)[3] <- "sum18/19"


d3 <- d[d$rok==c(2020,2021), ]
d3 <- d3 %>%
  group_by(GEO, SEX) %>%
  summarise(sum = sum(Value))
names(d3)[3] <- "sum20/21"


dane <- merge(d2, d3, by=c("GEO", "SEX"), all = T)
dane <- dane %>% drop_na()

dane$zmiana <- (dane$`sum20/21`- dane$`sum18/19`)/dane$`sum18/19`    

dane_k <- dane[dane$SEX=="Females", ]
dane_m <- dane[dane$SEX=="Males", ]


x11()
op <- par(mfrow=c(2,1))
barplot(
  dane_k$zmiana,
  names=dane_k$GEO,
  col=c(heat.colors(11), "black"),
  ylab="Wzglêdna zmiana",
  xlab="Pañstwo",
  main="Wzglêdna zmiana œmiertelnoœci kobiet na grype w latach 2018-2019 i 2020-2021 "
)

barplot(
  dane_m$zmiana,
  names=dane_m$GEO,
  col=c(heat.colors(11), "black"),
  ylab="Wzglêdna zmiana",
  xlab="Pañstwo",
  main="Wzglêdna zmiana œmiertelnoœci mezyczyzn na grype w latach 2018-2019 i 2020-2021 "
)
par(op)
