library(shiny)

shinyServer(function(input, output, session) {
    
    PrediictNext1 <- reactive({
        input <- tolower(input$text)
        input <- unlist(strsplit(as.character(input), ' '))
        n <- length(input)
        if(n >= 1 ){
            New <- DT2[DT2$base == input[n],]
            return(New$predict[1])
        } else{
            return("Unknown")
        }
    })
    
    PrediictNext2 <- reactive({
        input <- tolower(input$text)
        input <- unlist(strsplit(as.character(input), ' '))
        n <- length(input)
        if(n >= 1 ){
            New <- DT2[DT2$base == input[n],]
            return(New$predict[2])
        } else{
            return("Unknown")
        }
    })
    
    PrediictNext3 <- reactive({
        input <- tolower(input$text)
        input <- unlist(strsplit(as.character(input), ' '))
        n <- length(input)
        if(n >= 1 ){
            New <- DT2[DT2$base == input[n],]
            return(New$predict[3])
        } else{
            return("Unknown")
        }
    })
    
    output$pred1 <- renderText({PrediictNext1()})
    output$pred2 <- renderText({PrediictNext2()})
    output$pred3 <- renderText({PrediictNext3()})
})