#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Fitting data using the exponential function Y = A +B*exp(k*t)"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("k",
                        "Exponential rate constant k:",
                        min = -3,
                        max = 3,
                        value = 0, step = 0.01),
            sliderInput("B",
                        "Multiplicative constant B:",
                        min = -100,
                        max = 100,
                        value = 0),
            sliderInput("A",
                        "Additive constant A:",
                        min = -100,
                        max = 100,
                        value = 0),
            hr(),
            helpText("Select parameters for the function Y = A +Bexp(kt)")
        ),        

        # Show a plot of the generated distribution
        mainPanel(
            h4("Plot of the KaiC data set with the exponential function fit"),
           plotOutput("KaiCPlot"),
           textOutput("LLS")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    KaiCdata <- read_tsv("KaiC_data.txt")
    output$KaiCPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        #x    <- faithful[, 2]
        #bins <- seq(min(x), max(x), length.out = input$bins + 1)
        # draw the histogram with the specified number of bins
        #hist(x, breaks = bins, col = 'darkgray', border = 'white')
        plot(KaiCdata$Time, KaiCdata$Amount, xlab= "time (s)", ylab = "KaiC (a.u.)")
        curve(input$A + input$B*exp(input$k*x), add = TRUE)
        leg.txt=c("KaiC data", "exponential fit")
        legend("bottomright", leg.txt, col=c(1,1), pch=c(1,NA), lty=c(0,1), lwd=1)
    })
    output$LLS <- renderText({
        t <- KaiCdata$Time # the time points
        pred <- input$A + input$B*exp(input$k*t) # vector of predicted values 
        SSE <- sum((pred-KaiCdata$Amount)^2) # sum of squared errors
        paste("Sum of squared errors: ", SSE)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
