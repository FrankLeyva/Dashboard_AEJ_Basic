library(shiny)
library(bslib)
library(ggplot2)
library(plotly)
library(thematic)
library(lubridate)
library(dplyr)
library(wordcloud)
library(RColorBrewer)
library(sf)
library(tm)
library(DT)
library(tokenizers)
thematic::thematic_shiny(font = "auto")

function(input, output, session) {
  map <- read_sf("map.geojson")
  DSum <- read.csv('District_summary.csv')
  DSum$DISTRICT <- as.character(DSum$DISTRICT)
  texas <- read_sf("EP.geojson")
  nc <-left_join(map,DSum,by='DISTRICT')
  AEP <- read.csv('Base3.csv')
  district_filter <- reactive({input$District_filter})
  if(is.null(district_filter)){
    district_filter <-     c( 
      "Distrito 1", 
      "Distrito 2", 
      "Distrito 3",
      "Distrito 4",
      "Distrito 5",
      "Distrito 6",
      "Distrito 7",
      "Distrito 8")
  }
  AEPf <-  reactive({
  district_filter <- input$District_filter
  if(is.null(district_filter)){
      district_filter <-c( 
        "Distrito 1", 
        "Distrito 2", 
        "Distrito 3",
        "Distrito 4",
        "Distrito 5",
        "Distrito 6",
        "Distrito 7",
        "Distrito 8")
    }
  AEP %>% 
      filter(AEP$Q4...Di %in% district_filter)
  })
  
## TOKENIZATION

filtered_text<- reactive({
  test2 <-  data.frame(AEPf())$Q7...Si
  #test2 <-  AEP$Q7...Si
 test2[test2 == 'DK/NA'] <- NA
  test2[test2 == 'None'] <- NA
  test2[test2 == 'Nc'|test2 == 'NC'|test2 == 'nc'|test2=='nç'] <- NA
  test2[test2 == 'no sé'|test2 == 'No sé'] <- NA
  test2 <- na.omit(test2)
  tr_test <- test2
   corpus <- Corpus(VectorSource(tr_test))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords,stopwords(kind= 'en'))
  filtered_text <- unlist(sapply(corpus, as.character))
  tokens <- unlist(tokenize_words(filtered_text))
  tokens <- na.omit(tokens)
#  filtered_text <- as.matrix(TermDocumentMatrix(corpus))
 # filtered_text <- sort(rowSums(filtered_text), decreasing = TRUE)
 # filtered_text <- data.frame(word = names(filtered_text), freq = as.numeric(filtered_text))
  tokens
  })
  
#### OUTPUTS  
  output$Q1.NA.Perc <- renderText({
  Q1_low_perc <- paste0(round((sum(is.na(data.frame(AEPf())$Q5...En)) / 
                                 length(data.frame(AEPf())$Q5...En) * 100),2),'%')
  Q1_low_perc
})  
  output$Q1.1Perc <- renderText({

    Q1_low_perc <- paste0(round((sum(table(data.frame(AEPf())$Q5...En)[1:2], na.rm=T) / 
                                   length(data.frame(AEPf())$Q5...En) * 100),2),'%')
    Q1_low_perc
  })
  output$Q1.2Perc <- renderText({
    Q1_mid_total <- sum(table(data.frame(AEPf())$Q5...En)[3], na.rm=T)
    Q1_mid_perc <- paste0(round((Q1_mid_total / length(data.frame(AEPf())$Q5...En) * 100),2),'%')
    Q1_mid_perc
  })
  output$Q1.3Perc <- renderText({
    Q1_mid_perc <- paste0(round((sum(table(data.frame(AEPf())$Q5...En)[4:5], na.rm=T) / length(data.frame(AEPf())$Q5...En) * 100)
                                ,2),'%')
    Q1_mid_perc })
  output$Q1.plot <- renderPlotly({ggplotly( ggplot(data.frame(AEPf()), mapping = aes(x= Q5...En, fill=as.factor(Q5...En))) +
    geom_bar()+
      labs(x='Cambio en la situación Económica', y= 'Frecuencia')+
      theme(legend.position = "none"))})
  output$Q1.table <- renderDataTable({
    bind <- cbind(data.frame(AEPf())$Q4...D, data.frame(AEPf())$Q5...En) 
    bind <- as.data.frame(bind)
    colnames(bind)<- c('Distrito', 'Cambio_Situación_Económica')
    bind <- bind %>% arrange(Distrito)
    bind
    })
  output$D1.table <- downloadHandler(
    'data.csv',content= function(file){
    bind <- cbind(data.frame(AEPf())$Q4...D, data.frame(AEPf())$Q5...En) 
    colnames(bind)<- c('Distrito', 'Cambio en la situación Económica')  
    write.csv(bind,file)
      })
  
  
# Question 2
output$Q2.1 <- renderText({
  
out <-paste0(round((sum(table(data.frame(AEPf())$Q6...Co )[3:4], na.rm=T) / 
      length(data.frame(AEPf())$Q6...Co) * 100),2),'%')
out
})
output$Q2.2 <- renderText({
  
  paste0(round((sum(table(data.frame(AEPf())$Q6...Co )[1:2], na.rm=T) / 
                  length(data.frame(AEPf())$Q6...Co) * 100),2),'%')
})
output$Q2.NA <- renderText({
paste0(round((sum(is.na(data.frame(AEPf())$Q6...Co)) / 
                                 length(data.frame(AEPf())$Q6...Co) * 100),2),'%')
})
output$Q2.plot <- renderPlot({
  wordcloud(words = as.character(filtered_text()), min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35,colors=brewer.pal(5, "Dark2"))
})
output$Q2.table <- renderDataTable({
  bind <- cbind(data.frame(AEPf())$Q4...D, data.frame(AEPf())$Q6...Co) 
  bind <- as.data.frame(bind)
  colnames(bind)<- c('Distrito', 'Alcanza')
  bind <- bind %>% arrange(Distrito)
  bind
})
output$D2.table <- downloadHandler(
  'data.csv',content= function(file){
    bind <- cbind(data.frame(AEPf())$Q4...D, data.frame(AEPf())$Q6...Co) 
    colnames(bind)<- c('Distrito', 'Alcanza')  
    write.csv(bind,file)
  })

## Question 3
# Question 2
output$Q3.1 <- renderText({
  
  out <-paste0(round((sum(table(data.frame(AEPf())$Q8...En )[4], na.rm=T) / 
                        length(data.frame(AEPf())$Q8...En) * 100),2),'%')
  out
})
output$Q3.2 <- renderText({
  
paste0(round((sum(table(data.frame(AEPf())$Q8...En )[1:3], na.rm=T) / 
                  length(data.frame(AEPf())$Q8...En) * 100),2),'%')
})
output$Q3.NA <- renderText({
  paste0(round((sum(is.na(data.frame(AEPf())$Q8...En)) / 
                  length(data.frame(AEPf())$Q8...En) * 100),2),'%')
})

output$Q3.plot <- renderPlotly({
  ggplotly(
    ggplot(texas) +
    geom_sf(data=nc,aes(fill = IrseDeEP)) +
    theme(legend.position = "none")
)
                                         })
output$Q3.table <- renderDataTable({
  bind <- cbind(data.frame(AEPf())$Q4...D, data.frame(AEPf())$Q8...En) 
  bind <- as.data.frame(bind)
  colnames(bind)<- c('Distrito', 'IrseDeEP')
  bind <- bind %>% arrange(Distrito)
  bind
})
output$D3.table <- downloadHandler(
  'data.csv',content= function(file){
    bind <- cbind(data.frame(AEPf())$Q4...D, data.frame(AEPf())$QQ8...En) 
    colnames(bind)<- c('Distrito', 'IrseDeEP')  
    write.csv(bind,file)
  })
}