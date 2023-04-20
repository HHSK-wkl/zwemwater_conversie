#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(readxl)
library(glue)
library(DT)

selectie <- c(#"Monster.identificatie" = "Id text",
              # "Meetobject.Namespace" = "",
              "Meetobject.lokaalid" = "Sample name",
              "Typering.code" = "Typering code",
              # "Typering.omschrijving" = "Typering omschrijving",
              "Grootheid.code" = "Grootheid code",
              # "Grootheid.omschrijving" = "Grootheid omschrijving",
              "Parameter.code" = "Parameter code",
              "Parameter.groep" = "Parameter groep",
              # "Parameter.omschrijving" = "Parameter omschrijving",
              "Eenheid.code" = "Units",
              "Hoedanigheid.code" = "Hoedanigheid code",
              "AnalyseCompartiment.code" = "Compartiment code",
              "Waardebewerkingsmethode.code" = "Waardepalingsmethode code",
              "Waardebepalingsmethode.code" = "Waardebewerkingsmethode code",
              "Begindatum" = "Zwem begindatum",
              "Begintijd" = "Zwem begintijd",
              "Einddatum" = "Zwem einddatum",
              "Eindtijd" = "Zwem eindtijd",
              "Limietsymbool" = "Limietsymbool",
              "Numeriekewaarde" = "Numeriekewaarde",
              "Alfanumeriekewaarde" = "Alfanumeriekewaarde",
              "Kwaliteitsoordeel.code" = "Kwaliteits oordeel")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Conversie zwemwaterbestanden"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(

            # h3("Uitvouwen"),
            fileInput("bestand", label = h3("Selecteer een bestand voor conversie")),
            downloadButton("downloadData", "Geconverteerd bestand"),
            
            # h3("Samenvouwen"),
            # fileInput("file_samenvouwen", label = h3("OMS Excelbestand om samen te vouwen")),
            # downloadButton("downloadData_samengevouwen", "Download"),
            # 
            
            hr(),
            
            p("Deze applicatie kan worden gebruikt om bestanden uit het LIMS van Aquon geschikt te maken voor het zwemwaterregister."),
            p("Voor een correcte werking moeten alle benodigde kolommen aanwezig zijn. In de inkijkfunctie is dat mogelijk met 'Show all columns'"),
            
            
            width = 4
            
            
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
          
          h1(textOutput("error")),
          
          h3("Missende data"),
          
           tableOutput("missend"),
          
          h3("Overschrijdingen"),
           tableOutput("overschrijdend")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    zwemdata <- reactive({
        req(input$bestand)

           pad <- input$bestand[[1, 4]]
           # print(filename)
           readxl::read_excel(pad)
           
        })
    
    output$error <- renderText({
      if (all(unname(selectie) %in% names(zwemdata()) )) {
        ""
      } else {
        "Er ontbreken kolommen in het databestand!"
      }
      
    })

    output$missend <- renderTable({
        req(input$bestand)
        zwemdata() %>% 
          missende_data() 
      })
    
    output$overschrijdend <- renderTable({
      req(input$bestand)
      zwemdata() %>% 
        overschrijdingen()
    })


    output$downloadData <- downloadHandler(
        filename = function() {
            glue("{format(now(), '%Y-%m-%d %H%M')} import_zwemwaterportaal.csv")
        },
        content = function(file) {
            # openxlsx::write.xlsx(uitgevouwen(), file)
          write_delim(zwemdata() %>% conversiefunctie(),
                      file = file, delim = ";", na = "") 
        }
    )

   
   
}

# Run the application 
shinyApp(ui = ui, server = server)
