
library(shiny)
library(dplyr)
library(janitor)
library(ggplot2)
library(rio)
library(purrr)
library(stringr)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Supplier Benchmarking with standardized scores"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            fileInput("df",
                      "Choose your file (.xlsx, .xls or .csv)"),
            uiOutput("sliders")
            
            # sliderInput("bins",
            #             "Number of bins:",
            #             min = 1,
            #             max = 50,
            #             value = 30)
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Score", plotOutput("plot_1"),
                         textOutput("subtitle"),tableOutput("test")),
                tabPanel("Original table", dataTableOutput("table_2")),
                
                tabPanel("Standardized table", dataTableOutput("table_1"))
            )
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
    
    data_standardized <- reactive({
        data() %>% 
            mutate_all(~(.-mean(.))/sd(.))
    })
    
    datanames <- reactive({
        names(data())
    })
    
    #set weights
    
    rel_imp <- reactive({
       
       map_dbl(names(data()), ~ input[[paste0("var",.)]])
        # map_dbl(paste0("input$var",names(data())), get) 
    })
    
    weights <- reactive({
      
          rel_imp()/sum(rel_imp())
    })
    
    #Set Scores
    scores_vect <- reactive({
        req(weights())
        map2_df(data_standardized(), weights(), ~ (.x*.y)) %>% 
            transmute(scores = rowSums(.)) %>% pull() 
    })
    
    #Bind Scores
    data_scored <- reactive({
        cbind(Provider = seq_along(scores_vect()), data_standardized(), Score = scores_vect())
    })
    
    
        
        
    #Generate sliders
    output$sliders <- renderUI({
        pvars <- names(data())
        lapply(seq(pvars), function(i) {
            sliderInput(inputId = paste0("var", pvars[i]),
                        label = paste0("Relative importance of:", pvars[i]),
                        min = 0, max = 1, value = 1)
        })

    })
        

    #Render table
    output$table_1 <- renderDataTable({
       data_scored() %>% mutate_at(vars(-Provider),~round(.,4))
    })
    
    output$plot_1 <- renderPlot({
        ggplot(data_scored())+
            geom_col(aes(x = as.factor(Provider), y = Score), fill = 'steelblue')+
            theme_bw()+
            ggtitle("Standardized score by provider")+
            xlab("Provider")+
            ylab("Score")
    })
    
    output$test <- renderTable({
        table_2 <- weights() %>% t() %>% data.frame() 
        names(table_2) <- datanames()
        table_2
    })
    
    output$subtitle <- renderText({
        req(weights())
        "Weights"
    })

    output$table_2 <- renderDataTable({
        data()
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
