
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
                tabPanel("Introducción", 
                         p(""),
                         p("The goal of this app is to use data from your suppliers to estimate flexible indexes. It lets you adjust the relative importance of every feature and automatically standardize features (substracting the mean and dividing by the standard deviation of each variable) in order to make them comparable. The obtained score corresponds to the weighted sum of standardized attributes. It's open source and all the files are hosted on my github. Here is a sample .xlsx file to try the app"),
                         h4(strong("Input file")),
                         p("Choose a file from your system with suppliers data. \n Each column must be an attribute and the first column", strong("MUST"), " contain supplier names (or id's)."),
                         h4(strong("Sliders")),
                         p( "Use the automatically generated sliders to adjust the relative importance of each attribute."),
                         h4(strong("Tabs")),
                         p(code("Score"), "- This tab shows with a barplot the standardized scores obtained by each supplier. It also shows the weight of every variable (which depends on the relative importance of each attribute)."),
                         p(code("Original table"),"- This tab shows the original data. It can be filtered and arranged by multiple variables"),
                         p(code("Standardized table"), "- This tab shows the standardized data. It can be filtered and arranged by multiple variables")
                         ),
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
        import(input$df$datapath) %>% 
          rename(Supplier = 1)
    })
    
    data_standardized <- reactive({
        data() %>% 
            mutate_at(vars(-Supplier),~(.-mean(.))/sd(.))
    })
    
    datanames <- reactive({
        names(data())[-1]
    })
    
    #set weights
    
    rel_imp <- reactive({
       
       map_dbl(datanames(), ~ input[[paste0("var",.)]])
        # map_dbl(paste0("input$var",names(data())), get) 
    })
    
    weights <- reactive({
      
          rel_imp()/sum(rel_imp())
    })
    
    #Set Scores
    scores_vect <- reactive({
        req(weights())
        map2_df(data_standardized() %>% select(-Supplier), weights(), ~ (.x*.y)) %>% 
            transmute(scores = rowSums(.)) %>% pull() 
    })
    
    #Bind Scores
    data_scored <- reactive({
        cbind(data_standardized(), Score = scores_vect())
    })
    
    
        
        
    #Generate sliders
    output$sliders <- renderUI({
        pvars <- datanames()
        lapply(seq(pvars), function(i) {
            sliderInput(inputId = paste0("var", pvars[i]),
                        label = paste0("Relative importance of:", pvars[i]),
                        min = 1, max = 100, value = 100)
        })

    })
        

    #Render table
    output$table_1 <- renderDataTable({
       data_scored() %>% mutate_at(vars(-Supplier),~round(.,4))
    })
    
    output$plot_1 <- renderPlot({
        ggplot(data_scored(),aes(x = as.factor(Supplier), y = Score))+
            geom_col(fill = 'steelblue')+
            theme_bw()+
            ggtitle("Standardized score by Supplier")+
            xlab("Supplier")+
            ylab("Score")+
            geom_text(aes(label = round(Score,2), y = .5*Score), size = 5, fontface = 2)
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
    
    # output$intro_1 <- renderText({
    #   
    # })
    # 
    # output$intro_2 <- renderText({
    #  
    # })
    # 
    # 


    

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
