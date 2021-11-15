library(shiny)
library(tidyverse)
library(wordcloud)
library(ggplot2)
library(shinythemes)
library(RColorBrewer)
library(tidytext)


# The list of valid books
  books <- list("A Mid Summer Night's Dream" = "summer",
                "The Merchant of Venice" = "merchant",
                "Romeo and Juliet" = "romeo")

# task4: add in getFreq function for pre-processing
  getFreq <- function(book, stopwords = TRUE) {
    # check that only one of three books is selected
    if (!(book %in% books))
      stop("Unknown book")

    text <-  tibble(text = readLines(sprintf("./data/%s.txt", book), encoding="UTF-8"))

    # could also pass column of text/character instead
    text <- text %>%
      unnest_tokens(word, text) %>%
      count(word, sort = TRUE)

    if(stopwords){
      text <- text %>%
        anti_join(stop_words)
    }

    return(text)
  }



# task6: add in shinythemes function

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Shakespeare's Plays Word Frequencies"), # Application title

  # task1: add in the sidebarLayout with sidebarPanel and mainPanel

    sidebarLayout(

      sidebarPanel(
        # task2: add in the inputs in the sidebarPanel
        selectInput(inputId = "bookChoice", label = "Choose a Book:", choices = books),

        checkboxInput(inputId = "stopWords", label = "Stop Words:", value = TRUE),

        actionButton(inputId = "rerun", label = "Rerun"),

        hr(),

        h3("Word Cloud Settings"),

        sliderInput(inputId = "maxwords", label = "Max # of Words:", min = 10,
                    max = 200, value = 100, step = 20),

        sliderInput(inputId = "largestWords", label = "Size of Largest Words",
                    min = 1, max = 8, value = 4),

        sliderInput(inputId = "smallestWords", label = "Size of Smallest Words",
                    min = 0.1, max = 4, value = 0.5),

        hr(),

        h3("Word Count Settings"),

        sliderInput(inputId = "minWords", label = "Minimum Words for Counts Chart:",
                    min = 10, max = 100, value = 25),

        sliderInput(inputId = "wordSize", label = "Word Size for Counts Chart:",
                    min = 8, max = 30, value = 14)
      ),

      mainPanel(
        # task1: within the mainPanel, create two tabs (Word Cloud and Frequency)
        tabsetPanel(
          # task3: add in the outputs in the sidebarPanel
          tabPanel("Word Clouds", plotOutput("cloud", height = "600px")),
          tabPanel("Word Counts", plotOutput("freq", height = "600px"))
        )
      )
    )

  # task6: and modify your figure heights
)

server <- function(input, output) {

  freq <- eventReactive( input$rerun, {

    withProgress({
      setProgress(message = "Processing corpus...")
      getFreq(input$bookChoice,
              input$stopWords)})
    })

# task5: add in reactivity for getFreq function based on inputs
  output$cloud <- renderPlot({
    v <- freq()
    pal <- brewer.pal(8,"Dark2")
    v %>%
      with(
        wordcloud(
          word,
          n,
          scale = c(input$largestWords, input$smallestWords),
          random.order = FALSE,
          max.words = input$maxwords,
          colors=pal))
  })

  output$freq <- renderPlot({
    v <- freq()
    v %>%
      filter(n > input$minWords) %>%
      ggplot(aes(x=reorder(word, n), y=n)) +
      geom_col() +
      theme(axis.text = element_text(size = input$wordSize),
            axis.title.x = element_blank(),
            axis.title.y = element_blank()) +
      coord_flip()
})

}

shinyApp(ui = ui, server = server)
