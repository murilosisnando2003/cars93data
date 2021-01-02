#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(datasets)
library(MASS)
library(tidyr)
library(tibble)
library(ggplot2)

aq.no.missing <-drop_na(Cars93)



options <- c(" Car Price" = "Price",
             "Car Passengers"= "Passengers",
             "Car EngineSize" = "EngineSize",
             "Horsepower" = "Horsepower",
             "Car Width"= "Width",
             "Car RPM"= "RPM")

df.options <- data.frame(options)

df.lv <- rownames_to_column(df.options)

colnames(df.lv) <- c("label","value")


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Cars 93 Data"),
    
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("X","X Variable:", options),
            
            selectInput("Y","Y Variable:", options)
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("scatter")
        )
    )
)


# Define server logic required to draw a histogram
# Define server logic required to draw a histogram
server <- function(input, output) {
    
    selections <- reactive({aq.no.missing[,c(input$X,input$Y)]
        
    })
    
    output$scatter <- renderPlot({
        
        x_column <- selections()[,1]
        y_column <- selections()[,2]
        
        correlation <- cor(x_column,y_column)
        regression <- lm(y_column ~ x_column)
        intercept <- regression$coefficients[1]
        slope <- regression$coefficients[2]
        
        X_Label <- df.lv$label[which(df.lv$value == input$X)]
        Y_Label <- df.lv$label[which(df.lv$value == input$Y)]
        
        ggplot(selections(),aes(x=x_column, y=y_column))+
            geom_point(size=3)+
            labs(x = X_Label, y= Y_Label,
                 title = paste(Y_Label, "vs", X_Label,
                               "\n r = ",round(correlation,3),"Y' = ", round(intercept,3),"+",round(slope,3),"X"))+
            theme(axis.title.x = element_text(size=18),
                  axis.text.x = element_text(size=17),
                  axis.title.y = element_text(size=17),
                  axis.text.y = element_text(size=17),
                  plot.title = element_text(hjust = 0.5,size=20))+
            geom_smooth(method="lm",col="blue")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
