library(shiny)
library(dplyr)
library(shinyWidgets)
library(shinythemes)
library(DT)
library(shinyjs)

load("biblio.Rdata")

#original vectors
autores_vector <- sort(unique(trimws(unlist(strsplit(biblio$autor, split=";")), which="left")))
key_vector <- sort(unique(trimws(unlist(strsplit(biblio$key, split=";")))))
geo_vector <- sort(unique(unlist(strsplit(biblio$geo, split=","))))
met_vector <- sort(unique(unlist(strsplit(biblio$met, split=","))))
pub_type_vector <- sort(unique(unlist(biblio$pub_type)))

min_year <- round(min(biblio$pub_year),0)
max_year <- round(max(biblio$pub_year),0)

radio_vector <- c("Solo Chile", "Sí, de socies SOCHIAB")



ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            
            style = "position:fixed;width:inherit;",
            
            tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap_custom.css")),
            div(style = "margin-top:-5px"),
            
            tags$h1(strong("SOCHIABib"), style = "font-size:45px;"),
            
            tags$h4("Biblioteca de enlaces Antropología Biológica de Chile"),
            div(style = "margin-bottom:-11px"),
            
            tags$hr(style = "border-top: 1px solid #000000;"),
            
            radioButtons(inputId = "radioInput", label = "¿Incluir publicaciones ajenas a Chile?     ", choices = radio_vector, selected = "Solo Chile", inline = TRUE),
                
            sliderInput(inputId = "yearInput", label = "Año de publicación", min = min_year, max = max_year, value = c(min_year, max_year), dragRange = TRUE, sep=""),
            div(style = "margin-top:-8px"),
            
            pickerInput(inputId = "metInput", label = "Área metodológica", multiple = TRUE, choices = met_vector, options = pickerOptions(`actions-box` = TRUE, noneSelectedText = "All selected")),
            div(style = "margin-top:-8px"),
            
            pickerInput(inputId = "geoInput", label = "Región natural Chile", multiple = TRUE, choices = geo_vector, options = pickerOptions(`actions-box` = TRUE, noneSelectedText = "All selected")),
            div(style = "margin-top:-8px"),
            
            pickerInput(inputId = "autorInput", label = "Nombre autores", multiple = TRUE, choices = autores_vector, options = pickerOptions(`actions-box` = TRUE, `live-search`=TRUE, noneSelectedText = "All selected")),
            div(style = "margin-top:-8px"),
            
            pickerInput(inputId = "keywordInput", label = "Palabras clave", multiple = TRUE, choices = key_vector, options = pickerOptions(`actions-box` = TRUE, `live-search`=TRUE, noneSelectedText = "All selected")),
            div(style = "margin-top:-8px"),
            
            pickerInput(inputId = "pubTypeInput", label = "Tipo de Publicación", multiple = TRUE, choices = pub_type_vector, options = pickerOptions(`actions-box` = TRUE, noneSelectedText = "All selected")),
            

           

            actionButton(inputId = "buscarInput", label = "Buscar"),

            actionButton("refresh", "Limpiar búsqueda"),

            downloadButton("download", "Descargar resultados"),

            tags$br(),
            tags$br(),
            
            div(style="font-size:13px", span("Descarga la "),
                a("biblioteca completa", href = "Bioantro_Chile.bib"), " para tu manejador de referencias."),

            div(style="font-size:13px", span("Agrega referencias "),
                a("aquí.", href = "https://docs.google.com/forms/d/e/1FAIpQLSdhplY5vG5KClkDnyWZpOZfVfAEWJs4V1pHquGryzLbsXgPag/viewform?usp=sf_link", target="_blank"),
                span("Creado por "),
                a("jgalsku ", href = "https://github.com/jgalsku/SOCHIABib", target="_blank"),
                span("para "),
                a("SOCHIAB.", href = "http://www.sochiab.cl", target="_blank")),



        ),


        mainPanel(
            
            setBackgroundImage(src = 'logo4.png', shinydashboard = FALSE),
            DT::dataTableOutput(outputId = "tableOutput")
        )
    )
)





server <- function(input, output, session) {
    

    observeEvent(input$radioInput, {
        
        if(input$radioInput == "Solo Chile") {
            
            biblio <- biblio %>%
                filter(geo != "Fuera de Chile")
        }
        
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
                          choices = sort(unique(trimws(unlist(strsplit(biblio$key, split=";"))))))
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
                          choices = sort(unique(trimws(unlist(strsplit(biblio$key, split=";"))))), selected = input$keyInput)
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
                          choices = sort(unique(trimws(unlist(strsplit(biblio$key, split=";"))))), selected = input$keyInput)
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
                          choices = sort(unique(trimws(unlist(strsplit(biblio$key, split=";"))))), selected = input$keyInput)
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
                          choices = sort(unique(trimws(unlist(strsplit(biblio$key, split=";"))))), selected = input$keyInput)
        
    })
    
    
})
    
    
    
    dataInput <- eventReactive(input$buscarInput,{
        
        
        if(input$radioInput == "Solo Chile") {
            
            biblio <- biblio %>%
                filter(geo != "Fuera de Chile")
        }

        
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
    colnames=c("Autor/a/es", "Año publicación", "Título", "Publicación", "Enlace", "pub_type", 
               "DOI", "Pages", "Issue", "Volume", "Conference.Name", "Publisher", 
               "Editor", "Place", "geo", "met", "key", "abstract"),
    options = list(
        autoWidth = FALSE,
        columnDefs = list(list(targets = c(6:18),visible=FALSE))
    ))
    
    
    
    output$download <- downloadHandler(
        filename = function() {
            "SOCHIABib.csv"
        },
        content = function(con) {
            
            biblio <- dataInput()
            
            #remove html tags
            biblio$Url <- gsub("<a href='", "", biblio$Url)
            biblio$Url <- gsub("' target='_blank'>URL</a>", "", biblio$Url)
            biblio$pub_title <- gsub("<b>", "", biblio$pub_title)
            biblio$pub_title <- gsub("</b>", "", biblio$pub_title)
            biblio <- biblio %>% select(!abstract)

            
            write.csv(biblio, con, fileEncoding = "latin1")
        }
    )
    
    
    
    
    
}



# Run the application 
shinyApp(ui = ui, server = server)
