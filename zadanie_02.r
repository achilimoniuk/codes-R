rm(list=ls())
options(width=250)
#library(colorout)
library(reshape)
library(lattice)

getNBPData <- function(year=2021){
    
  ret <- data.frame()

  if(year>=2013){

    fileName <- paste0(year,"_NBP_data.csv")
  
    try({
      if(file.exists(fileName)){
        if(as.Date(file.info(fileName)$mtime)==Sys.Date()){
          cat(paste("Reading data from local file\n"))
          ret<-read.table(file=fileName,sep=";",dec=",",header=T,stringsAsFactor=F)
    	  colnames(ret) <- gsub("X","",colnames(ret))
	  return(ret)
	}
      }
    })
  
    cat(paste("Downloading data\n"))
  
    res <- try({
  
      d <- readLines(paste0("https://www.nbp.pl/kursy/Archiwum/archiwum_tab_a_",year,".csv"))
      d <- d[-2]
      d <- d[-c((length(d)-3):length(d))]
      tmpColnames <- strsplit(d[1],";",useBytes=T)[[1]]
      tmpColnames <- tmpColnames[-c((length(tmpColnames)-1):length(tmpColnames))]
      d <- do.call("rbind",
        lapply(strsplit(d[-1],";"),
        function(x){
          matrix(as.numeric(gsub(",",".",x[-c((length(x)-1):length(x))])),nrow=1)
        })
      )
      colnames(d) <- tmpColnames
      d <- as.data.frame(d)
      
      d$data <- as.Date(as.character(d$data),format="%Y%m%d")
      ret <- d
      write.table(ret,file=fileName,sep=";",dec=",",row.names=F)
    
    },silent=T)
  
    if(inherits(res,"try-error")){
      cat(paste("An error occurred while downloading data!!!\n")) 
    }
  

  }

  return(ret)

}

#---------scalenie danych
ret <- getNBPData(2013)
ret <- ret[,grep("data|EUR|USD",colnames(ret))] 
ret1 <- getNBPData(2014)
ret1 <- ret1[,grep("data|EUR|USD",colnames(ret1))] 
ret2 <- getNBPData(2015)
ret2 <- ret2[,grep("data|EUR|USD",colnames(ret2))] 
ret3 <- getNBPData(2016)
ret3 <- ret3[,grep("data|EUR|USD",colnames(ret3))] 
ret4 <- getNBPData(2017)
ret4 <- ret4[,grep("data|EUR|USD",colnames(ret4))] 
ret5 <- getNBPData(2018)
ret5 <- ret5[,grep("data|EUR|USD",colnames(ret5))] 
ret6 <- getNBPData(2019)
ret6 <- ret6[,grep("data|EUR|USD",colnames(ret6))] 
ret7 <- getNBPData(2020)
ret7 <- ret7[,grep("data|EUR|USD",colnames(ret7))] 
ret8 <- getNBPData(2021)
ret8 <- ret8[,grep("data|EUR|USD",colnames(ret8))] 

ret1$data <- as.Date(ret1$data, format =  "%Y-%m-%d")
class(ret1$data)
ret$data <- as.Date(ret$data, format =  "%Y-%m-%d")
class(ret$data)
library(dplyr)
total <- rbind(ret,ret1,ret2,ret3,ret4, ret5,ret6,ret7,ret8)
names(total)[2] <- "USD"
names(total)[3] <- "EUR"



#######wykres

 img <- xyplot(
   EUR+USD~data, data=total,
   type ="l",
   xlab = list("Rok", cex = 1), 
   ylab = list("Kurs waluty", cex=1),
   main = "Kurs dolara(USD) i euro(EUR) wzglêdem z³otówki",
   auto.key  = list(space="right", points=F,lines=T))
  
 x11()
 print(img)
 
 



                                                                                
