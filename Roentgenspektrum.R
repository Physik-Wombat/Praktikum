
library(imager)
library(shiny)



ui <- fluidPage(

   br(),
    titlePanel("Röntgenspektrum"),

    
    
        
        fluidRow(
            column(8,
        
           plotOutput("plot")),
           
           column(4,
           imageOutput("img")),
        
   
        
    
        
        fluidRow(
            
            column(5,offset=1,
                   
           
           
           h3("Blaues Spektrum"),
           
           sliderInput("U1",
                       "Roehrenspannung in kV",
                       min = 25,
                       max = 35,
                       value = 35,
                       step = 5),
           sliderInput("Strom1",
                        "Heizstrom in mA (blau)",
                        min = 0.1,
                        max = 1.0,
                        value = 1.0,
                        step = 0.05)
           ),
            
        
         column(5,
           
           h3("Rotes Spektrum"),
           
           
           sliderInput("U2",
                       "Roehrenspannung in kV",
                       min = 25,
                       max = 35,
                       value = 25,
                       step = 5),
            sliderInput("Strom2",
                        "Heizstrom in mA (rot)",
                        min = 0.1,
                        max = 1.0,
                        value = 0.1,
                        step = 0.05)
           )
           
           )
            
        
        
       
    
))

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$plot <- renderPlot({
       
        
        x    <- read.table("Roentgenwerte.csv",header = TRUE, sep = ";")
        m    <- ((input$U1-25)/5)*102+2        ### Umrechnung um auf die richtigen Zeilen der Daten zuzugreifen
        n    <- ((input$U2-25)/5)*102+2         
        p    <- 12-input$Strom1*10               ### Umrechnung um auf die richtigen Spalten der Daten zuzugreifen
        q    <- 12-input$Strom2*10
        
            {plot(x[2:102,1], x[206:306,2], type="l", xlab = "Wellenlänge [pm]", lwd=5, 
                  ylab = "Intensität", ylim = c(0,1050), col="white")
                par(new=TRUE)
                plot(x[2:102,1], x[m:(m+100),p], type="l", xlab = "Wellenlänge [pm]", lwd=5, 
             ylab = "Intensität", ylim = c(0,1050), col="blue")
            par(new=TRUE)
            plot(x[2:102,1], x[n:(n+100),q], type="l", xlab = "Wellenlänge [pm]", lwd=5, 
             ylab = "Intensität", ylim = c(0,1050), col="red")
            
            }
        
        
    })
    
    output$img<- renderPlot({
        
        im <-load.image("xray.jpeg")
        plot(im, axes="FALSE")
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
