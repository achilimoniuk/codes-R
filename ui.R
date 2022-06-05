
library(shiny)
library(data.table)
library(googleVis)
library(DT)

shinyUI(fluidPage(

    titlePanel("PiWD/shiny/sgh/grypa"),

    sidebarLayout(

        sidebarPanel(
            fileInput("fileInPath", 
                label= h4("Import danych")
            ),

            selectInput("selectYear",
                label = "Rok danych",
                choices = as.vector(as.character(2021:2010),mode="list")
            )
        ),

        mainPanel(
            tabsetPanel(type = "tabs",
                tabPanel("Moja tabela", DT::dataTableOutput("dataSample")),
#                tabPanel("Moja tabela", tableOutput("dataSample")),
                tabPanel("Mapa", htmlOutput("Geo")),
                tabPanel("Liczniki", htmlOutput("Gauge")),
                tabPanel("Test",verbatimTextOutput("plainText"))
            )
        

        )


    )

))
