library(DT)
library(shiny)

ui <- fluidPage(
    fluidRow(
        column(4,selectizeInput("var1", label = "Var 1", choices = NULL, multiple = TRUE)),
        column(4,selectizeInput("var2", label = "Var 2", choices = NULL, multiple = TRUE)),
        column(4,selectizeInput("var3", label = "Var 3", choices = NULL, multiple = TRUE)),
        column(4,DT::dataTableOutput("dt")
        )
    )
)

server <- function(input, output, session) {
    
    getwhich<-function(){
        whichs<-which(df$var3 == df$var3)
        
        if(!is.null(input$var1)){
            whichs<-intersect(whichs,which(df$var1 %in% input$var1))
        }
        if(!is.null(input$var2)){
            whichs<-intersect(whichs,which(df$var2 %in% input$var2))
        }
        if(!is.null(input$var3)){
            whichs<-intersect(whichs,which(df$var3 %in% input$var3))
        }
        return(whichs)
    }
    filterData <- function(dataset){
        df <- dataset
        if (!is.null(input$var1)){
            df <- df[which(df$var1 %in% input$var1),]
        }
        if (!is.null(input$var2)){
            df <- df[which(df$var2 %in% input$var2),]
        }
        if (!is.null(input$var3)){
            df <- df[which(df$var3 %in% input$var3),]
        }
        df
    }
    
    df <- data.frame(var1 = c(rep("A",3),rep("B",3)), var2 = c("x","y","x","z","x","s"), var3 = c(1:6))
    
    updateSelectizeInput(session, 'var1', choices = sort(unique(df$var1)), server = TRUE)
    updateSelectizeInput(session, 'var2', choices = sort(unique(df$var2)), server = TRUE)
    updateSelectizeInput(session, 'var3', choices = sort(unique(df$var3)), server = TRUE)
    
    output$dt <- renderDataTable({
        DT::datatable(
            filterData(df)
            )
    })
    
    observe({
        w<-getwhich()
        if(is.null(input$var1)){
            updateSelectizeInput(session,"var1",choices=sort(unique(df$var1[w])))
        }
        
    })
    
    observe({
        w<-getwhich()
        if(is.null(input$var2)){
            updateSelectizeInput(session,"var2",choices=sort(unique(df$var2[w])))
        }
        
    })
    
    observe({
        w<-getwhich()
        if(is.null(input$var3)){
            updateSelectizeInput(session,"var3",choices=sort(unique(df$var3[w])))
        }
        
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)