#PSE Operator na biezace publikuje dane o generacji zrodel wiatrowych i fotowoltaicznych 
#Dane te sa dosepne na stronie 
#https://www.pse.pl/dane-systemowe/funkcjonowanie-kse/raporty-dobowe-z-pracy-kse/generacja-zrodel-wiatrowych


#przygotowanie dat
rm(list=ls())
daty <- lapply(gsub("-","",as.character(seq(from=Sys.Date()-6,to=Sys.Date(),by="1 day"))),function(d){d})
data1 <- (daty[[c(1)]])
data2 <- (daty[[c(2)]])
data3 <- (daty[[c(3)]])
data4 <- (daty[[c(4)]])
data5 <- (daty[[c(5)]])
data6 <- (daty[[c(6)]])
data7 <- (daty[[c(7)]])


#sciaganie danych
#1
download.file(url=paste("https://www.pse.pl/getcsv/-/export/csv/PL_GEN_WIATR/data/", data1, sep=''), 
                paste(destfile=data1, '.csv', sep=''),method="curl")

d1 <- read.table(paste(file=data1, '.csv', sep=''),sep=";",dec=",",header=T,fileEncoding="CP1250")
 
#2
download.file(url=paste("https://www.pse.pl/getcsv/-/export/csv/PL_GEN_WIATR/data/", data2, sep=''), 
              paste(destfile=data2, '.csv', sep=''),method="curl")


d2 <- read.table(paste(file=data2, '.csv', sep=''),sep=";",dec=",",header=T,fileEncoding="CP1250")

#3
download.file(url=paste("https://www.pse.pl/getcsv/-/export/csv/PL_GEN_WIATR/data/", data3, sep=''), 
              paste(destfile=data3, '.csv', sep=''),method="curl")


d3 <- read.table(paste(file=data3, '.csv', sep=''),sep=";",dec=",",header=T,fileEncoding="CP1250")

#4
download.file(url=paste("https://www.pse.pl/getcsv/-/export/csv/PL_GEN_WIATR/data/", data4, sep=''), 
              paste(destfile=data4, '.csv', sep=''),method="curl")

d4 <- read.table(paste(file=data4, '.csv', sep=''),sep=";",dec=",",header=T,fileEncoding="CP1250")

#5
download.file(url=paste("https://www.pse.pl/getcsv/-/export/csv/PL_GEN_WIATR/data/", data5, sep=''), 
              paste(destfile=data5, '.csv', sep=''),method="curl")

d5 <- read.table(paste(file=data5, '.csv', sep=''),sep=";",dec=",",header=T,fileEncoding="CP1250")

#6
download.file(url=paste("https://www.pse.pl/getcsv/-/export/csv/PL_GEN_WIATR/data/", data6, sep=''), 
              paste(destfile=data6, '.csv', sep=''),method="curl")


d6 <- read.table(paste(file=data6, '.csv', sep=''),sep=";",dec=",",header=T,fileEncoding="CP1250")

#7
download.file(url=paste("https://www.pse.pl/getcsv/-/export/csv/PL_GEN_WIATR/data/", data7, sep=''), 
              paste(destfile=data7, '.csv', sep=''),method="curl")


d7 <- read.table(paste(file=data7, '.csv', sep=''),sep=";",dec=",",header=T,fileEncoding="CP1250")


#laczenie danych
dane <- rbind(d1, d2, d3, d4, d5, d6, d7)

colnames(dane)
names(dane)[3] <- "wiatrowe"
names(dane)[4] <- "foto"

dane$datagodzina <- paste0(dane$Data," ",dane$Godzina)
dane$Data <- as.Date(dane$Data, format =  "%Y-%m-%d")
dane$dt2 <- strptime(dane$datagodzina, format = "%Y-%m-%d %H")
dane$dt2 <- as.POSIXct(dane$dt2, "%Y-%m-%d %H:%M:%S", tz="CET")    


dane2 <- dane[c("dt2","wiatrowe","foto")]
names(dane2)[1] <- "Timestamp"
names(dane2)[2] <- "Wiatr"
names(dane2)[3] <- "PV"

#wykresy
library(reshape)
library(lattice)
library(ggplot2)
x11()
par(mfrow=c(2,1))
xyplot(
  wiatrowe~dt2, data=dane,
  type ="l",
  xlab = list("Data", cex = 1), 
  ylab = list("Zrodla wiatrowe", cex=1),
  main = "Tygodniowa generacja zrodel wiatrowych")

xyplot(
  foto~dt2, data=dane,
  type ="l",
  xlab = list("Data", cex = 1), 
  ylab = list("Zrodla fotowoltaiczne", cex=1),
  main = "Tygodniowa generacja zrodel fotowoltaicznych")


#tabela
kable( head(dane), caption="Podpis tabeli")






  
