library(DT)
library(shinyWidgets) 
library(dplyr)
library(shinycssloaders)


ui <- fluidPage(
  titlePanel("Linear Regression Analysis"),
  sidebarPanel(
    
    h3(HTML(paste0("<b>","About:"))),
    tags$hr(),
    
    h6("Use this app to perform linear regression analysis. If you need a sample dataset, you may download from:"),
    h6(HTML('<a href="https://drive.google.com/file/d/1AM0bL2ab-d9RwCpllmtUG6QGFkK1l9aN/view?usp=sharing" target="_blank">HERE</a>')),
    h6("This sample dataset is available in :"),
    h6(HTML('<a href="https://www.kaggle.com/grosvenpaul/family-income-and-expenditure" target="_blank">Kaggle</a>')),
    tags$hr(),
    
    h6("For more info, please contact:"),
    h6(HTML('<a href="https://www.linkedin.com/in/yong-poh-yu/">Yong Poh Yu </a>')),

    tags$hr(),
    fileInput(
      inputId = "filedata",
      label = "Please upload a csv. Click the RUN button to run the linear regression.",
      multiple = FALSE,
      accept = c(".csv"),
      buttonLabel = "Upload",
      placeholder = "No files selected yet"
    ),
    uiOutput("xvariable"),
    uiOutput("yvariable"),
    actionButton("runButton", "Run")
  ),
  
  mainPanel( 
    
    uiOutput("results"),
    fluidRow(verbatimTextOutput('lmSummary')%>% withSpinner(color="#0dc5c1")) , 
    fluidRow(plotOutput('diagnosticPlot')),
    uiOutput("message"),
    
    dataTableOutput("tbl")
  )
) 

options(shiny.maxRequestSize=300*1024^2)
server <- function(input, output) {
  
  yourdata <- reactive({
    req(input$filedata)
    inData <- input$filedata
    if (is.null(inData)){ return(NULL) }
    mydata <- read.csv(inData$datapath, 
                       header = TRUE, 
                       sep=",",
                       stringsAsFactors = T)
  })
  output$message <- renderText({
    req(lmModel())
    
    "<b>The Selected Data Frame:</b>"
    
  })
  
  output$xvariable <- renderUI({
    req(yourdata())
    xa<-colnames(yourdata())
    pickerInput(inputId = 'xvar',
                label = 'Select one or more independent variable(s).',
                choices = c(xa[1:length(xa)]), selected=xa[1],
                options = list(`style` = "btn-info"),
                multiple = TRUE)
    
  })
  output$yvariable <- renderUI({
    req(yourdata())
    ya<-colnames(yourdata()) 
    pickerInput(inputId = 'yvar',
                label = 'Select a dependent variable.',
                choices = c(ya[1:length(ya)]), selected=ya[length(ya)],
                options = list(`style` = "btn-info"),
                multiple = FALSE)
    
  })

  
  
  lmModel <- eventReactive(input$runButton, {

    req(yourdata(),input$xvar,input$yvar)

    x <- yourdata()[[as.name(input$xvar)]]
    y <- yourdata()[[as.name(input$yvar)]]
    current_formula <- paste0(input$yvar, " ~ ", paste0(input$xvar, collapse = " + "))
    f <- as.formula(current_formula)
    model <- lm(f, data = yourdata(), na.action=na.exclude)
   
    return(model)
  })
  output$results <- renderUI({
    req(lmModel())
    withMathJax(
      h3("Summary:"),
      paste(
        
        "Adj. \\( R^2 = \\) ", round(summary(lmModel())$adj.r.squared, 3),
        " ,\\( \\beta_0 = \\) ", round(lmModel()$coef[[1]], 3),
        ", and more details: ")
    )
  })
  
  
  output$tbl <- renderDataTable({
 
    req(lmModel())
  
    datatable(data.frame(yourdata()%>% select(input$xvar), yourdata()%>% select(input$yvar)),
                  extensions = "Buttons",
                  options = list(
                    lengthChange = FALSE,
                    dom = "Blfrtip",
                    buttons = c("copy", "csv", "pdf", "print")
                  )
    )
  })
  
  
  
  
  
  output$lmSummary <- renderPrint({

    req(lmModel())
    summary(lmModel())
  })
  
  output$diagnosticPlot <- renderPlot({
    req(lmModel())
    par(mfrow = c(2,2))
    plot(lmModel())
  })
}

shinyApp(ui = ui, server = server)
