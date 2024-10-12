library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Collatz conjecture"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
           
            
            sliderInput("bigNumber",
                        "Number of iterations:",
                        min = 0,
                        max = 10000,
                        value = 3000),
            numericInput("oddDegChange",
                         "Change in degrees for odd numbers:",
                         value = 1.2),
            numericInput("evenDegChange",
                         "Change in degrees for even numbers:",
                         value = -0.54),
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("distPlot"),
          HTML("<p>For details see: <a href='https://ebaker.me.uk/notes/collatz-conjecture'>Collatz conjecture</a>.</p>")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
      bigNumber <- input$bigNumber
      oddDegChange <- input$oddDegChange
      evenDegChange <- input$evenDegChange
      
      oddRadChange <- oddDegChange * pi / 180
      evenRadChange <- evenDegChange * pi / 180
      
      l <- weight <- vector('integer', bigNumber)
      collatz <- function(n) {
        weight[[n]] <<- weight[[n]] + 1
        if (n == 1) {
          return()
        }
        if (n %% 2 == 0) {
          l[[n]] <<- n/2
          collatz(n/2)
        } else {
          l[[n]] <<- 3*n+1
          if (3*n + 1 <= bigNumber) {
            collatz(3*n+1)
          }
        }
      }
      
      for (n in 1:bigNumber) {
        collatz(n)
      }
      
      startx <- starty <- endx <- endy <- vector('numeric', bigNumber)
      
      drawCollatz <- function(n, x1, y1, rad1) {
        startx[[n]] <<- x1
        starty[[n]] <<- y1
        if (n %% 2 == 0) {
          rad2 <- rad1 + evenRadChange
        } else {
          rad2 <- rad1 + oddRadChange
        }
        endx[[n]] <<- x2 <- x1 + cos(rad2)
        endy[[n]] <<- y2 <- y1 + sin(rad2)
        
        nextValue <- which(l == n)
        if (length(nextValue) > 0) {
          for (i in 1:length(nextValue)) {
            drawCollatz(nextValue[[i]], x2, y2, rad2)
          }
        }
      }
      
      drawCollatz(2, 0, 0, 0)
      
      plot.new()
      plot.window(xlim=c(min(endx),max(endx)), ylim=c(min(endy),max(endy)))
      
      segments(startx,starty,endx,endy,lwd=0.5, col="purple")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
