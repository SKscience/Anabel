list.of.packages <- c("anabel", "shiny","tidyverse","openxlsx","plotly")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)){
  conf <- readline(prompt=paste0('The following dependencies (packages) are missing: ',  paste(new.packages, collapse = ", "), '. Do you want to install them (Y/y): '))
  if(conf == "Y" | conf == "y"){
    install.packages(new.packages)
  } else{
    stop(paste0("App start was abborded, the following packages are missing:   ", paste(new.packages, collapse = ", ")))
  }
}

library(shiny)
library(tidyverse)
library(anabel)
library(openxlsx)
library(plotly)

# Define UI for application that draws a histogram
ui <- navbarPage("AnabelApp",
    tabPanel("App",
       tags$head(
         tags$link(rel = "stylesheet", type = "text/css", href = "anabel_theme.css")
       ),
      # Application title
       titlePanel(
         div(img(src = "anabel_logo.png", width = 300), align = "center"),
         "AnabelApp"
       ),
  
      # File upload input
      div(class = "container-fluid mt-6 page-container",
      fluidRow(align = "center",
        h3("Step 1: Use your own data or use the example data"),
        fileInput("data_file", "Accepted file formats: .xlsx, .csv or .tsv",
                accept = c(".csv",
                           ".tsv",
                           ".xlsx")),
        checkboxInput("example_data", "Or use our example data", value = FALSE),
        )),
      div(class = "container-fluid mt-6 page-container",
      fluidRow(align = "center",
        h3("Step 2: Select the mode of analysis "),
        column(2, offset = 3, div(img(src = "SCA.png", width = 150), align = "center")),
        column(2, div(img(src = "SCK.png", width = 150), align = "center")),
        column(2, div(img(src = "MCK.png", width = 150), align = "center")),
      ),
      fluidRow(align = "center",
        column(12, 
         radioButtons("mode", label = NULL, choices = list("Single Curve Analysis" = "SCA", "Single Cycle Kinetic" = "SCK", "Multi Cycle Kinetic" = "MCK"),inline=TRUE, selected = character(0))
        )
      )
      ),
      div(class = "container-fluid mt-6 page-container",
      fluidRow(align = "center",
         h3("Step 3: Supply all the additional information"),
         column(2, textInput("conc", "Concentraion [nM]:", value = "")),
         column(2, textInput("tass", "Start time of association:", value = "")),
         column(2, textInput("tdiss", "Start time of dissociation:", value = "")),
         column(2, checkboxInput("drift", "Apply linear drift correction", value = FALSE)),
         column(2, checkboxInput("decay", "Apply decay correction", value = FALSE))
               ),
      fluidRow(align = "center",
        uiOutput("options_ui")
        ),
      fluidRow(align = "center",
        plotlyOutput("plot")
      )
      ),
      div(class = "container-fluid mt-6 page-container",
      fluidRow(align="center",
       h3("Step 4: Run anabel and Download the Results"),
       actionButton("run_anabel", "Run Anabel"),
       uiOutput("download_kinetic_ui"),
       uiOutput("download_plot_ui"),
      )
    )
    ),
    tabPanel("Help",
             includeHTML("help.html")
    )
)

# Define server logic
server <- function(input, output, session) {
  
  # Render custom images above radio buttons
  output$image1 <- renderUI({
    tags$img(src = "image1.jpg", height = 100, width = 100)
  })
  
  output$image2 <- renderUI({
    tags$img(src = "image2.jpg", height = 100, width = 100)
  })
  
  output$image3 <- renderUI({
    tags$img(src = "image3.jpg", height = 100, width = 100)
  })

  data = reactive({
    req(input$mode)
    x=NULL
    if(input$example_data){
      if(input$mode == "SCA"){
        x = SCA_dataset
        updateTextInput(session, "conc", value = "100")
        updateTextInput(session, "tass", value = "50")
        updateTextInput(session, "tdiss", value = "200")
      } else if(input$mode == "SCK"){
        x = SCK_dataset
        updateTextInput(session, "conc", value = "0.617, 1.85, 5.56, 16.7, 50")
        updateTextInput(session, "tass", value = "50, 200, 400, 560, 740")
        updateTextInput(session, "tdiss", value = "150, 330, 490, 670, 840")
      }else if(input$mode == "MCK"){
        x = MCK_dataset
        updateTextInput(session, "conc", value = "50, 16.7, 5.56, 1.85, 0.617")
        updateTextInput(session, "tass", value = "50")
        updateTextInput(session, "tdiss", value = "150")
      }
    }
    
    if(!is.null(input$data_file)){
        ext <- tools::file_ext(input$data_file$name)
        x = switch(ext,
               csv = vroom::vroom(input$data_file$datapath, delim = ","),
               tsv = vroom::vroom(input$data_file$datapath, delim = "\t"),
               xlsx = read.xlsx(input$data_file$datapath),
               validate("Invalid file; Please upload a .csv, .tsv or xlsx file")
        )
    }
    if(!is.null(x)){
      names(x)[1] = "Time"
    }
    x
  })
  
  output$plot <- renderPlotly({
      req(data(), input$mode, rv$results$mode)
      
      df = data()
        p = df %>%
          pivot_longer(!Time, names_to = "Name", values_to = "values") %>%
          ggplot(aes(x=Time, y=values, color=Name)) + 
            geom_line() + 
            theme_minimal()
        
        if(!is.null(input$tass)){
          tass = as.numeric(gsub("[^[:digit:]., ]", "", strsplit(input$tass,",")[[1]]))
          p = p + geom_vline(xintercept = tass, color="green")
        }
        if(!is.null(input$tdiss)){
          tdiss = as.numeric(gsub("[^[:digit:]., ]", "", strsplit(input$tdiss,",")[[1]]))
          p = p + geom_vline(xintercept = tdiss, color="red")
        }
        
        if(!is.null(rv$results$fit_data) & rv$results$mode == input$mode){
          r = rv$results$fit_data
          p = p + geom_line(aes(y = r$fit, x = r$Time, colour=r$Name))
        }
        
        ggplotly(p)
    
  })
  
  rv <- reactiveValues(results = list(kinetics = NULL, fit_data = NULL, p = NULL))
  
  observeEvent(input$mode,{
    rv$results = list(kinetics = NULL, fit_data = NULL, p = NULL, mode=input$mode)
  })
  
  observeEvent(input$run_anabel, {
    req(data())
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Calculating", value = 0)
    
    tass = as.numeric(gsub("[^[:digit:]., ]", "", strsplit(input$tass,",")[[1]]))
    tdiss = as.numeric(gsub("[^[:digit:]., ]", "", strsplit(input$tdiss,",")[[1]]))
    
    conc_M = convert_toMolar(val = as.numeric(gsub("[^[:digit:]., ]", "", strsplit(input$conc,",")[[1]])), unit = "nM")
    
    r = run_anabel(input = data(),
                        tass = tass,
                        tdiss = tdiss,
                        conc = conc_M,
                        method = input$mode,
                        drift = input$drift,
                        decay = input$decay,
                        quiet = FALSE
                        )
    rv$results$kinetics = r$kinetics
    rv$results$fit_data = r$fit_data
    rv$results$mode = input$mode
    
    if(input$mode == "MCK"){
      p = ggplot(r$fit_data, aes(x = Time, colour = Name)) +
        geom_point(aes(y = Response)) +
        geom_path(aes(y = fit)) +
        theme_light()
    } else{
      p = ggplot(r$fit_data, aes(x = Time)) +
        geom_point(aes(y = Response), col = "#A2C510") +
        geom_path(aes(y = fit)) +
        facet_wrap(~Name, ncol = 2, scales = "free") +
        theme_light()
    }
    
    rv$results$p = p
    
    
  })
  
  output$download_kinetic_ui <- renderUI({
    if(!is.null(rv$results$kinetics)) {
      downloadButton('kinetic_out', 'Download Kinetic Table')
    }
  })
  
  output$download_plot_ui <- renderUI({
    if(!is.null(rv$results$kinetics)) {
      downloadButton('plot_out', 'Download Fit Graphs')
    }
  })
  
  output$kinetic_out <- downloadHandler(
    filename = function() { "kinetic_table.xlsx"},
    content = function(file) {write.xlsx(x=rv$results$kinetics, file = file)}
  )
  
  output$plot_out <- downloadHandler(
    filename = function() { "fit_plots.pdf"},
    content = function(file) {
      ggsave(file, rv$results$p, width=15, device="pdf")
    }
  )
  
  
}

# Run the application
shinyApp(ui = ui, server = server)