library(shiny)

shinyUI(fluidPage(
    titlePanel("Predict next word from input words"),
    sidebarLayout(
      sidebarPanel(
        textInput("text", "This app will predict three next word from the below input text data. Please input the text data, then push Submit bottom.", ""),
        submitButton("Sbumit")
      ),
      mainPanel(
        h3("Predicted next word 1:"),
        textOutput("pred1"),
        h3("Predicted next word 2:"),
        textOutput("pred2"),
        h3("Predicted next word 3:"),
        textOutput("pred3")
      )
  )
))