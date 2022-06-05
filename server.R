library(shiny)
library(data.table)
library(googleVis)
library(DT)

shinyServer(function(input, output) {

    outVar <- reactiveValues(
        selectYearVar = "2021"
    )
    
    observeEvent(input$selectYear,{
      outVar$selectYearVar <- input$selectYear
    })

    dataIn <- reactive({
        try({
            d <- (
              read.table(file=input$fileInPath$datapath,
                sep=";",dec=",",header=T,stringsAsFactors=FALSE, 
                fileEncoding="UTF-8"
              )
            )

            d <- d[c("Rok","Region","liczba_ogolem")]
            d <- d[d$Region!="POLSKA",]
            d$Region <- tolower(d$Region)
            d <- aggregate(liczba_ogolem~Rok+Region,data=d,FUN="sum")
            colnames(d)[3] <- "Liczba"
            d <- d[order(d$Rok,d$Region),]
            d <- d[as.character(d$Rok)==as.character(outVar$selectYearVar),]
 
            return(d)
        },silent=T)
        return(data.frame())
    })

     output$dataSample <- DT::renderDataTable({
           DT::datatable(  
                       dataIn(), 
                       rownames = FALSE,
                       options = list(
                           scrollX = TRUE,
                           pageLength = 16,
                           lengthMenu = seq(from=2,by=2,to=16) 
                         )
         )
     })
     
     output$Geo <- renderGvis({gvisGeoChart(dataIn(),
                              "Region",
                              "Liczba", 
                              options = list(region="PL", 
                                             displayMode="regions",
                                             resolution="provinces", as.is=T))
        
    
     })
     
     
   
     
     output$Gauge <-  renderGvis({gvisGauge(dataIn()[,2:3],
                        labelvar= "Region",
                        numvar= "Liczba",
                         options=list(min=0, max=1000000,
                                      redFrom=700000, redTo=1000000,
                                      yellowFrom=400000, yellowTo=700000,
                                      greenFrom=0, greenTo=400000,
                                      width=800, height=800, as.is=T))
     })
     
     
help("gvisGauge")

    
    
    
    output$plainText <- renderPrint({
        return(outVar$selectYearVar)
    })



})
