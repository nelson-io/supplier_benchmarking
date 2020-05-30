
library(shiny)
library(dplyr)
library(janitor)
library(ggplot2)
library(rio)
library(purrr)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Supplier Benchmarking"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            fileInput("df",
                      "Choose your file"),
            uiOutput("sliders")
            
            # sliderInput("bins",
            #             "Number of bins:",
            #             min = 1,
            #             max = 50,
            #             value = 30)
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           tableOutput("table_1", width = "50%"),
           plotOutput("plot_1", width = "50%")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #Import DATA
    data <- reactive({
        validate(
            need(input$df, message = F)
        )
        infile <- input$df
        import(input$df$datapath)
    })
    
    #Set Scores
    scores_vect <- reactive({
        map2_df(data(), map_dbl(data(),mean), ~ .x/.y) %>% 
            transmute(scores = rowSums(.)) %>% pull()
    })
    
    #Bind Scores
    data_scored <- reactive({
        cbind(Provider = seq_along(scores_vect()), data(), Score = scores_vect())
    })
        
    #Generate sliders
    output$sliders <- renderUI({
        pvars <- names(data())
        lapply(seq(pvars), function(i) {
            sliderInput(inputId = paste0("range", pvars[i]),
                        label = paste0("Weight of: ", pvars[i]),
                        min = 1, max = 100, value = 100)
        })
        
    })
        

    #Render table
    output$table_1 <- renderTable({
       data_scored()
    })
    
    output$plot_1 <- renderPlot({
        ggplot(data_scored())+
            geom_col(aes(x = as.factor(Provider), y = Score), fill = 'steelblue')+
            theme_bw()+
            ggtitle("Score by provider")+
            xlab("Provider")+
            ylab("Score")
    })

    


    

    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)
