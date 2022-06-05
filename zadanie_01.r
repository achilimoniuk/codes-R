rm(list=ls())
options(width=250)

#odczyt danych
L <- read.table(
  file="data/PZHGrypaZachorowania.csv",sep=";",dec=",",
  header=T,stringsAsFactors=F, fileEncoding = "CP1250")
daneRobocze <- L[L$Region=="POLSKA",]

daneRobocze[is.na(daneRobocze)] <- 0 
daneRobocze$Nr <- gsub("A|B|C|D","",daneRobocze$Nr)
daneRobocze$Nr <- gsub("([0-9]{1}|[0-9]{2})\\(|\\)","",daneRobocze$Nr)
daneRobocze$Nr <- as.integer(daneRobocze$Nr)


######################################wykres nr1 (strona2)- wykres ko³owy
#przygotowanie danych

library(dplyr)
cos <- daneRobocze[, -c(1:5)]
nowe <- cos %>%
  summarise_each(funs(sum))

procent1 = round(nowe$liczba_0_4/nowe$liczba_ogolem, 4)*100 
procent2 = round(nowe$liczba_5_14/nowe$liczba_ogolem, 4)*100 
procents3 = round(nowe$liczba_15_64/nowe$liczba_ogolem, 4)*100 
procent4 = round(nowe$liczba_65_inf/nowe$liczba_ogolem, 4)*100 

slices <- c(nowe$liczba_0_4, nowe$liczba_5_14, nowe$liczba_15_64, nowe$liczba_65_inf)

#wykres
x11();
pie(slices, labels = "", clockwise = TRUE, 
    main="Grypa- rozk³ad po segmentach wiekowych dla lat 2010-2021",
    col= c("grey", "green", "yellow", "red"))

legend(
  "topright",
  c("0-4 (23.33%)","5-14 (22.07%)","15-64 (45.68%)","65-Inf (8.92%)"),
  fill=c("grey","green","yellow","red"), 
  inset=c(-0.6,0.0),
  box.lwd = 0, box.col="white", 
  bg = "white"
)
mtext("Zrodlo danych: PZH",side=4,line=0,adj=0)
mtext("http://wwwold.pzh.gov.pl/oldpage/epimeld/grypa/index.htm",side=4,line=1,adj=0) 


###############################################wykres nr 2 (strona 4)
#przygotowanie danych
rok <- daneRobocze[, -c(2:5)]

nowe2 <- rok %>%
  group_by(Rok) %>%
  summarise_each(funs(sum))

rok2<-nowe2[,c(5, 7, 9, 11)]


grouped_barplot <- nowe2[, c(1, 5, 7, 9,11)]
grouped_barplot$Rok <- factor(grouped_barplot$Rok)
grouped_barplot <- as.data.frame(t(as.matrix(grouped_barplot)))
grouped_barplot
names(grouped_barplot) <- as.matrix(grouped_barplot[1, ])
grouped_barplot <- grouped_barplot[-1, ]
grouped_barplot[] <- lapply(grouped_barplot, function(x) type.convert(as.character(x)))
grouped_barplot
my_mat <- as.matrix(grouped_barplot)
my_mat

#wykres
x11()
op <- par(mfrow=c(2,1))
barplot(
  nowe2$liczba_ogolem,
  names=nowe2$Rok,
  col=c(heat.colors(11), "black"),
  ylab="Liczba przypadkow",
  xlab="Rok",
  main="Grypa-dane zagregowane z lat 2010-2021"
)

barplot(my_mat, 
        xlab="Rok", ylab="Liczba przypadkow",
        col=c("grey","green", "yellow", "red"),
        beside=T,
) 

legend(
  "topleft",
  c("0-4","5-14","15-64","65-Inf"),
  fill=c("grey","green","yellow","red"), 
  box.lwd = 0, box.col="white", 
  bg = "white"
)
mtext("Zrodlo danych: PZH",side=4,line=0,adj=0)
mtext("http://wwwold.pzh.gov.pl/oldpage/epimeld/grypa/index.htm",side=4,line=1,adj=0) 
par(op)


##################################################wykres 3 (strona5)-boxploty
#przygotowanie danych
miesiac <- daneRobocze[, -c(3:5)]
miesiac$Miesiac <- factor(miesiac$Miesiac , levels=c("styczeñ", "luty", "marzec", "kwiecieñ",
                                                     "maj", "czerwiec", "lipiec", "sierpieñ", "wrzesieñ",
                                                     "paŸdziernik", "listopad", "grudzieñ"))

#wykresy
x11()
attach(mtcars)
layout(matrix(c(1,1,2,3,4,5), 3, 2, byrow = TRUE))

boxplot(
  miesiac$liczba_ogolem ~ miesiac$Miesiac,
  col=heat.colors(12),
  ylab=" ",
  xlab=" ",
  main="Ogolem",
  frame=F
  ) 

title("Grypa-boxploty miesieczne z danych tygodniowych za lata 2010-2021", line=3)
boxplot(
  miesiac$liczba_0_4 ~ miesiac$Miesiac,
  col=heat.colors(12),
  ylab=" ",
  xlab=" ",
  main="0-4",
  frame=F
) 

boxplot(
  miesiac$liczba_5_14 ~ miesiac$Miesiac,
  col=heat.colors(12),
  ylab=" ",
  xlab=" ",
  main="5-14",
  frame=F
) 

boxplot(
  miesiac$liczba_15_64 ~ miesiac$Miesiac,
  col=heat.colors(12),
  ylab=" ",
  xlab=" ",
  main="15-64",
  frame=F
) 

boxplot(
  miesiac$liczba_65_inf ~ miesiac$Miesiac,
  col=heat.colors(12),
  ylab=" ",
  xlab=" ",
  main="65-Inf",
  frame=F
) 
mtext("Zrodlo danych: PZH",side=4,line=0,adj=0)
mtext("http://wwwold.pzh.gov.pl/oldpage/epimeld/grypa/index.htm",side=4,line=1,adj=0) 







 
  
  
  

