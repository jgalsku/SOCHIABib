library(shiny)
library(dplyr)
library(shinyWidgets)
library(shinythemes)
library(DT)
library(shinyjs)



load("biblio.Rdata")

#original input choice vectors
autores_vector <- sort(unique(trimws(unlist(strsplit(biblio$autor, split=";")), which="left")))
key_vector <- sort(unique(trimws(unlist(strsplit(biblio$key, split=";")))))
geo_vector <- sort(unique(unlist(strsplit(biblio$geo, split=","))))
met_vector <- sort(unique(unlist(strsplit(biblio$met, split=","))))
pub_type_vector <- sort(unique(unlist(biblio$pub_type)))

min_year <- round(min(biblio$pub_year),0)
max_year <- round(max(biblio$pub_year),0)

radio_vector <- c("Solo Chile", "Sí, de autores comunidad SOCHIAB")



ui <- fluidPage(
  
    tags$head(
        # Note the wrapping of the string in HTML()
        tags$style(HTML("
    
      .well {
        background-color: #F1F3F5;
      }
      * {
        font-family: 'Trebuchet MS';
      }

      "))
    ),
    
    sidebarLayout(
        sidebarPanel(
            titlePanel(windowTitle ="SOCHIABib", 
                       h1(strong("SOCHIABib"), 
                            style='margin-top: -30px;
                                   font-size:45px')),
        
        

            # address bar icon (favicon)
            tags$head(tags$link(rel="shortcut icon", href="favicon.png")),

            #attempt to add favicons of all sizes, does not work
            # tags$head(
            #           tags$link(rel="apple-touch-icon", sizes="180x180", href="apple-touch-icon.png"),
            #           tags$link(rel="icon", type="image/png", sizes="32x32", href="favicon-32x32.png"),
            #           tags$link(rel="icon", type="image/png", sizes="16x16", href="favicon-16x16.png"),
            #           tags$link(rel="manifest", href="site.webmanifest"),
            #           tags$link(rel="mask-icon", href="safari-pinned-tab.svg", color="#5bbad5"),
            #           tags$link(rel="mask-icon", href="safari-pinned-tab.svg", color="#5bbad5"),
            #           tags$meta(name="msapplication-TileColor", content="#00aba9"),
            #           tags$meta(name="theme-color", content="#ffffff")
            #           ),

            
            # subtitle set with html tags
            tags$h4("Biblioteca de enlaces Antropología Biológica Chile"),
            div(style = "margin-bottom:-20px"),
            
            #space on top border
            tags$hr(style = "border-top: 1px solid #000000;"),
            
            # inputs/filters with reduced space in between using html 
    
            radioButtons(inputId = "radioInput", label = "¿Incluir publicaciones ajenas a Chile?     ", choices = radio_vector, selected = "Solo Chile", inline = TRUE),
            
            # setSliderColor("#FFD54F", 1),
            
            sliderInput(inputId = "yearInput", label = "Año de publicación", min = min_year, max = max_year, value = c(min_year, max_year), dragRange = TRUE, sep=""),
            div(style = "margin-top:-10px"),
            
            pickerInput(inputId = "metInput", label = "Área temático-metodológica", multiple = TRUE, choices = met_vector, 
                        options = pickerOptions(`actions-box` = TRUE, noneSelectedText = "All selected")),
            div(style = "margin-top:-10px"),
            
            pickerInput(inputId = "geoInput", label = "Región natural Chile", multiple = TRUE, choices = geo_vector, 
                        options = pickerOptions(`actions-box` = TRUE, noneSelectedText = "All selected")),
            div(style = "margin-top:-10px"),
            
            pickerInput(inputId = "autorInput", label = "Nombre autores", multiple = TRUE, choices = autores_vector, 
                        options = pickerOptions(`actions-box` = TRUE, `live-search`=TRUE, liveSearchNormalize = TRUE, noneSelectedText = "All selected")),
            div(style = "margin-top:-10px"),
            
            pickerInput(inputId = "keywordInput", label = "Palabras clave", multiple = TRUE, choices = key_vector, 
                        options = pickerOptions(`actions-box` = TRUE, `live-search`=TRUE, liveSearchNormalize = TRUE, noneSelectedText = "All selected")),
            div(style = "margin-top:-10px"),
            
            pickerInput(inputId = "pubTypeInput", label = "Tipo de Publicación", multiple = TRUE, choices = pub_type_vector, 
                        options = pickerOptions(`actions-box` = TRUE, noneSelectedText = "All selected")),
            

           
            #search button
            actionButton(inputId = "buscarInput", label = "Buscar"),

            # reload session button (too slow)
            # actionButton("refresh", "Limpiar búsqueda"),
            
            # reset inputs button
            actionButton("reset_input", "Limpiar filtros"),
            
            # download search results button
            downloadButton("download", "Descargar resultados"),

            tags$br(),
            tags$br(),
            
            # bottom links
            div(style="font-size:13px", span("Descarga la "),
                a("biblioteca completa", href = "Bioantro_Chile.bib"), " para tu manejador de referencias."),

            div(style="font-size:13px", span("Agrega referencias "),
                a("aquí.", href = "https://docs.google.com/forms/d/e/1FAIpQLSdhplY5vG5KClkDnyWZpOZfVfAEWJs4V1pHquGryzLbsXgPag/viewform?usp=sf_link", target="_blank"),
                span("Escrito por "),
                a("jgalsku ", href = "https://github.com/jgalsku/SOCHIABib", target="_blank"),
                span("para "),
                a("SOCHIAB.", href = "http://www.sochiab.cl", target="_blank")),



        ),


        mainPanel(
            
            setBackgroundImage(src = 'logo.png', shinydashboard = FALSE),
            DT::dataTableOutput(outputId = "tableOutput", height = "100%")
        )
    )
)





server <- function(input, output, session) {
    

    observeEvent(input$radioInput, {
        
        #filter out all that is not Chile when "solo chile" selected (default)
        if(input$radioInput == "Solo Chile") {
            
            biblio <- biblio %>%
                filter(geo != "Fuera de Chile")
        }
        
    # filter input options displayed    
        
    ### inputs all observant on year
    
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

        # filter search results depending on inputs 
        
        data <- biblio %>% 
            filter(pub_year >= input$yearInput[1] & pub_year <= input$yearInput[2]) %>%
            filter(grepl(paste(input$metInput, collapse = "|"), met)) %>%
            filter(grepl(paste(input$geoInput, collapse = "|"), geo)) %>%
            filter(grepl(paste(input$autorInput, collapse = "|"), autor)) %>%
            filter(grepl(paste(input$keywordInput, collapse = "|"), key)) %>%
            filter(grepl(paste(input$pubTypeInput, collapse = "|"), pub_type)) 
            # mutate(autor = case_when(nchar(autor) >= 150 ~ paste(substring(autor, 1, 150), "...", sep = ""), 
            # TRUE ~ as.character(autor))) # cut author names to 150 characters, better aesthetically but not content-wise, find better solution
            
    })
    
    
    #reload session (was too slow)
    # observeEvent(input$refresh, {
    #     session$reload();
    # })
    # 
    
    # reset inputs
    observeEvent(input$reset_input, {
        
        # filter options displayed after reseting input 
        
        if(input$radioInput == "Solo Chile") {
            
            biblio <- biblio %>%
                filter(geo != "Fuera de Chile")
        }
        
        #chile
        updateRadioButtons(session, "radioInput", selected = "Solo Chile")
        
        #year
        updateSliderInput(session, "yearInput", value = c(min_year, max_year))
        
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
        # pub_type
        updatePickerInput(session = session, inputId = "pubTypeInput",
                          choices = sort(unique(unlist(biblio$pub_type))))
        

    })
            

    
    # print table with database info queried above
    
    output$tableOutput <- DT::renderDataTable({
        dataInput()
    },
    
    escape = FALSE,
    colnames=c("Autor/a/es", "Año", "Título", "Publicación", "Enlace", "pub_type", 
               "DOI", "Pages", "Issue", "Volume", "Conference.Name", "Publisher", 
               "Editor", "Place", "geo", "met", "key", "abstract"),
    options = list(
        columnDefs = list(list(targets = c(6:18),visible=FALSE)),
        scrollResize = T,
        scrollY = 580,
        scrollCollapse = T,
        paging = F
    ))
    
    # fillcontainer = T, paging = F
    
    # download search results in csv format, modify some columns before creating csv
    
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
