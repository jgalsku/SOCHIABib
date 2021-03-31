
library(dplyr)
library(tidyr)
library(stringr)

setwd("C:/Users/yo/Dropbox/sochiab/biblio")


biblio <- read.csv("C:/Users/yo/Dropbox/sochiab/biblio/Bioantro_Chile.csv", encoding = "UTF-8", na.strings=c("","NA"))

#separate column Extra into geo and met
biblio <- separate(data = biblio, col = "Extra", into = c("geo", "met"), ";")


dput(names(biblio))

biblio$key <- ifelse(is.na(biblio$Automatic.Tags), paste(biblio$Manual.Tags),paste(biblio$Manual.Tags,biblio$Automatic.Tags))

biblio$key


biblio <- biblio %>% 
  rename(
    pub_type = Item.Type, 
    pub_year = Publication.Year,
    pub_title = Title,
    pub_venue = Publication.Title,
    autor = Author,
    conferencia = Conference.Name
    
  )

dput(names(biblio))




myvars <- c("autor", "pub_year", "pub_title", "pub_venue",  
            "Url", "pub_type", "DOI", "Pages", "Issue", 
            "Volume", "conferencia", "Publisher", "Editor", 
            "Place", "geo", "met", "key"
)
biblio <- biblio[myvars]


dput(names(biblio))


#add hyperlink to url
biblio$Url <- ifelse(is.na(biblio$Url), NA, paste0("<a href='",biblio$Url,"' target='_blank'>URL</a>"))

#make title bold
biblio$pub_title <- paste0("<b>",biblio$pub_title,"</b>")

#coalesce pub_venue con publisher priorizanto pub_venue, para agregar info de memorias
biblio$pub_venue <- dplyr::coalesce(biblio$pub_venue, biblio$Publisher, biblio$conferencia)

#rename pub_type
dput(levels(as.factor(biblio$pub_type)))

biblio <-
  biblio %>%
  mutate_at("pub_type", str_replace, "bookSection", "Capítulo")

biblio <- 
  biblio %>% 
  mutate_at("pub_type", str_replace, "conferencePaper", "Presentación")

biblio <- 
  biblio %>% 
  mutate_at("pub_type", str_replace, "journalArticle", "Artículo")

biblio <- 
  biblio %>% 
  mutate_at("pub_type", str_replace, "manuscript", "Manuscrito")

biblio <- 
  biblio %>% 
  mutate_at("pub_type", str_replace, "thesis", "Tesis")

biblio <- 
  biblio %>% 
  mutate_at("pub_type", str_replace, "book", "Libro")


#eliminate black space left author list & order alphabetic
biblio$autor <- trimws(biblio$autor, which= c("left"))
biblio <- biblio[order(biblio$autor),]




setwd("C:/Users/yo/Dropbox/shiny/SOCHIABib/SOCHIABib")

save(biblio, file = "biblio.Rdata")

