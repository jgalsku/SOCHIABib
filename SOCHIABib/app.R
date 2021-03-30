library(shiny)
library(dplyr)
library(shinyWidgets)
library(shinythemes)
library(DT)
library(shinyjs)

load("biblio.Rdata")

#original vectors
autores_vector <- sort(unique(trimws(unlist(strsplit(biblio$autor, split=";")), which="left")))
key_vector <- sort(unique(unlist(strsplit(biblio$key, split=";"))))
geo_vector <- sort(unique(unlist(strsplit(biblio$geo, split=","))))
met_vector <- sort(unique(unlist(strsplit(biblio$met, split=","))))
pub_type_vector <- sort(unique(unlist(biblio$pub_type)))




ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            
            titlePanel("Biblioteca de enlaces publicaciones Antropología Biológica en Chile"),
            
            sliderInput(inputId = "yearInput", label = "Año de publicación", min = 1950, max = 2030, value = c(1980, 2021), dragRange = TRUE, sep=""),
            
            pickerInput(inputId = "metInput", label = "Área metodológica", multiple = TRUE, choices = met_vector, options = pickerOptions(`actions-box` = TRUE, noneSelectedText = "All selected")),
            
            pickerInput(inputId = "geoInput", label = "Región natural Chile", multiple = TRUE, choices = geo_vector, options = pickerOptions(`actions-box` = TRUE, noneSelectedText = "All selected")),
            
            pickerInput(inputId = "autorInput", label = "Nombre autores", multiple = TRUE, choices = autores_vector, options = pickerOptions(`actions-box` = TRUE, `live-search`=TRUE, noneSelectedText = "All selected")),
            
            pickerInput(inputId = "keywordInput", label = "Palabras clave", multiple = TRUE, choices = key_vector, options = pickerOptions(`actions-box` = TRUE, `live-search`=TRUE, noneSelectedText = "All selected")),
            
            pickerInput(inputId = "pubTypeInput", label = "Tipo de Publicación", multiple = TRUE, choices = pub_type_vector, options = pickerOptions(`actions-box` = TRUE, noneSelectedText = "All selected")),
            
            
            
            br(), 
            
            actionButton(inputId = "buscarInput", label = "Buscar"),
            
            actionButton("refresh", "Limpiar búsqueda"),
            
            hr(),
            
            downloadButton("download", "Descargar tabla resultados")
        ),
        
        
        mainPanel(
            DT::dataTableOutput(outputId = "tableOutput")
        )
    )
)





server <- function(input, output, session) {
    
    ### all observant on year
    
    
    
    ##dep on year
    
    
    observeEvent(input$yearInput, {
        biblio <- biblio %>%
            filter(pub_year >= input$yearInput[1] & pub_year <= input$yearInput[2]) %>% 
            droplevels()
        
        
        # met
        updatePickerInput(session = session, inputId = "metInput",
                          choices = sort(unique(unlist(strsplit(biblio$met, split=",")))))
        # geo
        updatePickerInput(session = session, inputId = "geoInput",
                          choices = sort(unique(unlist(strsplit(biblio$geo, split=",")))))
        # autor
        updatePickerInput(session = session, inputId = "autorInput",
                          choices = sort(unique(trimws(unlist(strsplit(biblio$autor, split=";")), which="left"))))
        # key
        updatePickerInput(session = session, inputId = "keywordInput",
                          choices = sort(unique(unlist(strsplit(biblio$key, split=";")))))
        # put_type
        updatePickerInput(session = session, inputId = "pubTypeInput",
                          choices = sort(unique(unlist(biblio$pub_type))))
        
    })
    
    
    
    
    ##dep on met
    
    observeEvent(input$metInput, {
        biblio <- biblio %>% 
            filter(pub_year >= input$yearInput[1] & pub_year <= input$yearInput[2]) %>%
            filter(grepl(paste(input$metInput, collapse = "|"), met)) %>%
            filter(grepl(paste(input$geoInput, collapse = "|"), geo)) %>%
            filter(grepl(paste(input$autorInput, collapse = "|"), autor)) %>%
            filter(grepl(paste(input$keywordInput, collapse = "|"), key)) %>%
            filter(grepl(paste(input$pubTypeInput, collapse = "|"), pub_type))
        
        
        
        # geo
        updatePickerInput(session = session, inputId = "geoInput",
                          choices = sort(unique(unlist(strsplit(biblio$geo, split=",")))), selected = input$geoInput)
        # autor
        updatePickerInput(session = session, inputId = "autorInput",
                          choices = sort(unique(trimws(unlist(strsplit(biblio$autor, split=";")), which="left"))), selected = input$autorInput)
        # key
        updatePickerInput(session = session, inputId = "keywordInput",
                          choices = sort(unique(unlist(strsplit(biblio$key, split=";")))), selected = input$keyInput)
        # put_type
        updatePickerInput(session = session, inputId = "pubTypeInput",
                          choices = sort(unique(unlist(biblio$pub_type))), selected = input$pubTypeInput)
        
        
        
    })
    
    
    
    
    
    
    ##dep on geo
    
    observeEvent(input$geoInput, {
        biblio <- biblio %>% 
            filter(pub_year >= input$yearInput[1] & pub_year <= input$yearInput[2]) %>%
            filter(grepl(paste(input$metInput, collapse = "|"), met)) %>%
            filter(grepl(paste(input$geoInput, collapse = "|"), geo)) %>%
            filter(grepl(paste(input$autorInput, collapse = "|"), autor)) %>%
            filter(grepl(paste(input$keywordInput, collapse = "|"), key)) %>%
            filter(grepl(paste(input$pubTypeInput, collapse = "|"), pub_type))
        
        
        # met
        updatePickerInput(session = session, inputId = "metInput",
                          choices = sort(unique(unlist(strsplit(biblio$met, split=",")))), selected = input$metInput)
        # autor
        updatePickerInput(session = session, inputId = "autorInput",
                          choices = sort(unique(trimws(unlist(strsplit(biblio$autor, split=";")), which="left"))), selected = input$autorInput)
        # key
        updatePickerInput(session = session, inputId = "keywordInput",
                          choices = sort(unique(unlist(strsplit(biblio$key, split=";")))), selected = input$keyInput)
        # put_type
        updatePickerInput(session = session, inputId = "pubTypeInput",
                          choices = sort(unique(unlist(biblio$pub_type))), selected = input$pubTypeInput)
        
    })
    
    
    
    
    ##dep on author
    
    observeEvent(input$autorInput, {
        biblio <- biblio %>% 
            filter(pub_year >= input$yearInput[1] & pub_year <= input$yearInput[2]) %>%
            filter(grepl(paste(input$metInput, collapse = "|"), met)) %>%
            filter(grepl(paste(input$geoInput, collapse = "|"), geo)) %>%
            filter(grepl(paste(input$autorInput, collapse = "|"), autor)) %>%
            filter(grepl(paste(input$keywordInput, collapse = "|"), key)) %>%
            filter(grepl(paste(input$pubTypeInput, collapse = "|"), pub_type))
        
        
        # met
        updatePickerInput(session = session, inputId = "metInput",
                          choices = sort(unique(unlist(strsplit(biblio$met, split=",")))), selected = input$metInput)
        # geo
        updatePickerInput(session = session, inputId = "geoInput",
                          choices = sort(unique(unlist(strsplit(biblio$geo, split=",")))), selected = input$geoInput)
        # key
        updatePickerInput(session = session, inputId = "keywordInput",
                          choices = sort(unique(unlist(strsplit(biblio$key, split=";")))), selected = input$keyInput)
        # put_type
        updatePickerInput(session = session, inputId = "pubTypeInput",
                          choices = sort(unique(unlist(biblio$pub_type))), selected = input$pubTypeInput)
        
    })
    
    
    
    
    ## dep on keyword
    
    
    # met dependent on keyword
    
    observeEvent(input$keywordInput, {
        biblio <- biblio %>% 
            filter(pub_year >= input$yearInput[1] & pub_year <= input$yearInput[2]) %>%
            filter(grepl(paste(input$metInput, collapse = "|"), met)) %>%
            filter(grepl(paste(input$geoInput, collapse = "|"), geo)) %>%
            filter(grepl(paste(input$autorInput, collapse = "|"), autor)) %>%
            filter(grepl(paste(input$keywordInput, collapse = "|"), key)) %>%
            filter(grepl(paste(input$pubTypeInput, collapse = "|"), pub_type))
        
        
        # met
        updatePickerInput(session = session, inputId = "metInput",
                          choices = sort(unique(unlist(strsplit(biblio$met, split=",")))), selected = input$metInput)
        # geo
        updatePickerInput(session = session, inputId = "geoInput",
                          choices = sort(unique(unlist(strsplit(biblio$geo, split=",")))), selected = input$geoInput)
        # autor
        updatePickerInput(session = session, inputId = "autorInput",
                          choices = sort(unique(trimws(unlist(strsplit(biblio$autor, split=";")), which="left"))), selected = input$autorInput)
        # put_type
        updatePickerInput(session = session, inputId = "pubTypeInput",
                          choices = sort(unique(unlist(biblio$pub_type))), selected = input$pubTypeInput)
    })
    
    
    ## dep on pubType
    
    observeEvent(input$pubTypeInput, {
        biblio <- biblio %>% 
            filter(pub_year >= input$yearInput[1] & pub_year <= input$yearInput[2]) %>%
            filter(grepl(paste(input$metInput, collapse = "|"), met)) %>%
            filter(grepl(paste(input$geoInput, collapse = "|"), geo)) %>%
            filter(grepl(paste(input$autorInput, collapse = "|"), autor)) %>%
            filter(grepl(paste(input$keywordInput, collapse = "|"), key)) %>%
            filter(grepl(paste(input$pubTypeInput, collapse = "|"), pub_type))
        
        
        # met
        updatePickerInput(session = session, inputId = "metInput",
                          choices = sort(unique(unlist(strsplit(biblio$met, split=",")))), selected = input$metInput)
        # geo
        updatePickerInput(session = session, inputId = "geoInput",
                          choices = sort(unique(unlist(strsplit(biblio$geo, split=",")))), selected = input$geoInput)
        # autor
        updatePickerInput(session = session, inputId = "autorInput",
                          choices = sort(unique(trimws(unlist(strsplit(biblio$autor, split=";")), which="left"))), selected = input$autorInput)
        # key
        updatePickerInput(session = session, inputId = "keywordInput",
                          choices = sort(unique(unlist(strsplit(biblio$key, split=";")))), selected = input$keyInput)
        
    })
    
    

    
    dataInput <- eventReactive(input$buscarInput,{
        
        data <- biblio %>% 
            filter(pub_year >= input$yearInput[1] & pub_year <= input$yearInput[2]) %>%
            filter(grepl(paste(input$metInput, collapse = "|"), met)) %>%
            filter(grepl(paste(input$geoInput, collapse = "|"), geo)) %>%
            filter(grepl(paste(input$autorInput, collapse = "|"), autor)) %>%
            filter(grepl(paste(input$keywordInput, collapse = "|"), key)) %>%
            filter(grepl(paste(input$pubTypeInput, collapse = "|"), pub_type))
    })
    
    
    
    observeEvent(input$refresh, {
        session$reload();
    })
    
    
    # print table with database info queried above
    output$tableOutput <- DT::renderDataTable({
        dataInput()
    },
    
    escape = FALSE,
    colnames=c("Autor/a/es", "Año publicación", "Título", "Fuente", "Enlace", "pub_type", 
               "DOI", "Pages", "Issue", "Volume", "Conference.Name", "Publisher", 
               "Editor", "Place", "geo", "met", "key"),
    options = list(
        autoWidth = FALSE,
        columnDefs = list(list(targets = c(6:17),visible=FALSE))
    ))
    
    
    
    output$download <- downloadHandler(
        filename = function() {
            "SOCHIABib.csv"
        },
        content = function(con) {
            write.csv(dataInput(), con)
        }
    )
    
}



# Run the application 
shinyApp(ui = ui, server = server)
