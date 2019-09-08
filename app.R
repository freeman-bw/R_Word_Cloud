#This is a shiny web app designed to take a url as an input and produce 
# a word cloud from the content of the web page

library(shiny)
library(httr)
library(rvest)
library(magrittr)
library(stringr)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

get_data <- function(url) {
    #url <- "http://httpbin.org/get"
    r <- GET(url)
    print(r)
    r
}

get_info <- function(url) {
    #https://towardsdatascience.com/scrape-data-and-build-a-webapp-in-r-using-rvest-and-shiny-f20d84dd1b74
    #https://github.com/bradlindblad/shiny_scraper/blob/master/gainer/app.R
    #http://bradleyboehmke.github.io/2015/12/scraping-html-text.html#
    
    scraping_data <- xml2::read_html(url)
    
    substr(scraping_data, start = nchar(scraping_data)-700, stop = nchar(scraping_data))
    
    scraping_data %>% 
        html_nodes("div") %>%
        html_text() %>% 
        strsplit(split = "\n") %>%
        unlist() %>%
        .[. != ""] %>%
        str_replace_all(pattern = "\n", replacement = " ") %>%
        str_replace_all(pattern = "[\\^]", replacement = " ") %>%
        str_replace_all(pattern = "\"", replacement = " ") %>%
        str_replace_all(pattern = "\\s+", replacement = " ") %>%
        str_trim(side = "both") %>%
        substr(start = nchar(scraping_data)-700, stop = nchar(scraping_data))
    
}

create_wc <- function(docs) {
    print("here")
    
    toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
    docs <- tm_map(docs, toSpace, "/")
    docs <- tm_map(docs, toSpace, "@")
    docs <- tm_map(docs, toSpace, "\\|")
    
    # Convert the text to lower case
    docs <- tm_map(docs, content_transformer(tolower))
    # Remove numbers
    docs <- tm_map(docs, removeNumbers)
    # Remove english common stopwords
    docs <- tm_map(docs, removeWords, stopwords("english"))
    # Remove your own stop word
    # specify your stopwords as a character vector
    docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
    # Remove punctuations
    docs <- tm_map(docs, removePunctuation)
    # Eliminate extra white spaces
    docs <- tm_map(docs, stripWhitespace)
    # Text stemming
    # docs <- tm_map(docs, stemDocument)
    
    
    dtm <- TermDocumentMatrix(docs)
    m <- as.matrix(dtm)
    v <- sort(rowSums(m),decreasing=TRUE)
    d <- data.frame(word = names(v),freq=v)
    head(d, 10)
    
    set.seed(1234)
    wordcloud(words = d$word, freq = d$freq, min.freq = 1,
              max.words=200, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"))
    
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Web Page Word Cloud"),

    textInput("words", "Source URL"),
    #actionButton("go", "go"),
    
    actionButton("search", "Create Word Cloud"),
    
    verbatimTextOutput("value"),s
    
    plotOutput("plt")
    
    #textOutput("display_text")
    

)

# Define server logic required to draw a histogram
server <- function(input, output) {
    

    
  #output$value <- renderPrint({
      
   #   input$go
    #  isolate(input$words)
      
  #})
  

      
 # output$display_text <- renderPrint({
 #     input$search
 #     isolate(get_data(input$words))
 #     
  #})
  
  #reactive expression
  #on click of search button, get_data is called and
  # the results are put into df
  
     df <- eventReactive(input$search, {
         #this works but is limited
        get_info(input$words)
         #get_info("http://bradleyboehmke.github.io/2015/12/scraping-html-text.html")
         
         
       
     })
     
     #the above works to show text, now trying to get it to display the word cloud
     cloud <- eventReactive(input$search, {
         
         
         wds <- " here here there funny love love live live alive alive alive"
         create_wc(wds)
         
     })
     
     #text output
     #when there is something in df(after the button is clicked)
     # the results will be rendered
     #keep this for debugging
     #output$display_text <- renderPrint({
      # df()
    # })
     
     
     
     output$plt <- renderPlot({
         #wds <- " here here there funny love love live live alive alive alive"
         wds <- df()
         docs <- Corpus(VectorSource(wds))
                        
         toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
         docs <- tm_map(docs, toSpace, "/")
         docs <- tm_map(docs, toSpace, "@")
         docs <- tm_map(docs, toSpace, "\\|")
         docs <- tm_map(docs, toSpace, "<\\/span>")
         
         # Convert the text to lower case
         docs <- tm_map(docs, content_transformer(tolower))
         # Remove numbers
         docs <- tm_map(docs, removeNumbers)
         # Remove english common stopwords
         docs <- tm_map(docs, removeWords, stopwords("english"))
         # Remove your own stop word
         # specify your stopwords as a character vector
         docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
         # Remove punctuations
         docs <- tm_map(docs, removePunctuation)
         # Eliminate extra white spaces
         docs <- tm_map(docs, stripWhitespace)
         
         
         dtm <- TermDocumentMatrix(docs)
         m <- as.matrix(dtm)
         v <- sort(rowSums(m),decreasing=TRUE)
         d <- data.frame(word = names(v),freq=v)
         head(d, 10)
         
         set.seed(1234)
         wordcloud(words = d$word, freq = d$freq, min.freq = 1,
                   max.words=200, random.order=FALSE, rot.per=0.35, 
                   colors=brewer.pal(8, "Dark2"))
        
     })


}

# Run the application 
shinyApp(ui = ui, server = server)
