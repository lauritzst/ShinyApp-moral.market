
# ==============================================================================
# Make sure your device has internet connection before running the code!
# ==============================================================================



# Library ======================================================================

library(plotly)
library(dplyr)
library(readr)
library(htmlwidgets)
library(shinythemes)
library(DT)
library(data.table)
library(shiny)



### locally ====================================================================

# #########################################################
# ##  Please comment out if the code should run locally  ##
# #########################################################
# 
# # Turn off Warnings, clear Console, and Global Environment
# options(warn=0)
# rm(list=ls())
# cat("\014")
# 
# 
# # # Set working directory
# setwd("~/MMM Dropbox/MMM/Analysis")
# under_path = getwd()
# 
# 
# # Presettings part
# setwd(paste0(under_path, "/Choropleth Map/Green_Finance_Indicators"))



### Pre-settings ===============================================================

# add current working directory to temporary user
addResourcePath("tmpuser", getwd())


# Get indicator names (vector)
input_choice <- c(gsub(".html","", gsub("^.*?- ","",
                      list.files('./html files', full.names = FALSE,
                                 recursive = FALSE))))


# Get year span of the data set (vector)
years <- suppressWarnings(as.numeric(gsub("([0-9]+).*$", "\\1",
                          c(gsub(".csv","", gsub("data_ranking_","",
                          list.files('./data', full.names = FALSE,recursive = FALSE)))))))
years <- rev(years[!is.na(years)]) # Drop NAs


# # data loop for data ranking (third Panel)
# for (i in seq(years)){
#   assign(paste0("data_ranking_",years[i]), read.csv(paste0(getwd(),"/data/data_ranking_",years[i],".csv"),
#                                                     check.names=FALSE))
#   data_ranking_columns <- which(colnames(get(paste0("data_ranking_",years[i]))) %in% input_choice)
#   assign(paste0("indicator_ranking_",years[i]),
#          colnames(get(paste0("data_ranking_",years[i]))[,data_ranking_columns]))
# }



# data loop for data ranking (third Panel)
for (i in seq(years)){

  data <- read.csv(paste0(getwd(),"/data/data_ranking_",years[i],".csv"),
                   check.names=FALSE)
  data <- data %>%
    mutate_if(is.numeric, round, digits=2)

  data_ranking_columns <- which(colnames(data) %in% input_choice)
  assign(paste0("indicator_ranking_",years[i]),
         colnames(data[,data_ranking_columns]))

  assign(paste0("data_ranking_",years[i]), data)
}


# data loop for time series (second Panel)
for (value in input_choice) {
  assign(paste0("data_",value), read.csv(paste0(getwd(),"/data/longpanel_",value,".csv"),
                                         check.names=FALSE))
  assign(paste0("choice_",value), c(colnames(get(paste0("data_",value))[,4:ncol(get(paste0("data_",value)))])))
}

# Color vector for World/OECD/non-OECD time series
color_vector <- c('#777a80','#68ba32','#f2a138')


# Selection for Area Checkbox (Timeseries)
choice_checkbox <- c("World" = "World","OECD" = "OECD","non-OECD" = "non_OECD")


### UI function ================================================================



ui <- navbarPage(

    # Application title
    "Green Finance Indicators",
    theme = shinytheme("flatly"),
    
    
    
    # Choropleth Map Tab Panel ==============
    
    
    tabPanel(
      "Maps",
      fluidRow(
        column(1), # Spacing column
        
        
        # Indicator Dropdown
        column(3, selectInput("variable_input1", "Select Value:", 
                              choices = input_choice
                              ),
               offset = 0, style='padding-left:70px; padding-right:0px; padding-top:0px; padding-bottom:0px'
               )
        ),
      
      
      # HTML output (choropleth maps)
      htmlOutput("choropleth", align = "center", style = "margin-top: 20px"),
      br() # spacing
    )
    ,
    
    
    # Time Series Tab Panel =================

    
    tabPanel(
      "Time Series",
      fluidRow(
        column(1), # Spacing column
        
        
        # Indicator Dropdown
        column(3, selectInput("variable_input2", "Select Value:",
                              choices = input_choice
                              ),
               
               # styling options
               offset = 0, style='padding-left:70px; padding-right:0px; padding-top:0px; padding-bottom:0px'
               ),
        
        
        # World/OECD/non-OECD Checkbox
        column(2, checkboxGroupInput("Area", label = "Select",
                                    choices = choice_checkbox, selected = c("World")
                                    ),
               
               # styling options 
               offset = 1, style='padding-left:0px; padding-right:0px; padding-top:0px; padding-bottom:0px'
               ), 
        
        
        # Country Selection (Dropdown with multiple selection)
        column(5, selectInput("countries", "Select Countries:",
                              choices = get(paste0("choice_",input_choice[1])), multiple = T
                              ),
               
               # styling options 
               offset = 0, style='padding-left:30px; padding-right:250px; padding-top:0px; padding-bottom:0px'
               )
        ),
      
      
      # Time Series output
      div(plotlyOutput("time_series", height = "100%", width = "100%"), align = "center", style = "margin-top: -15px"),
      br() # spacing
    )
    ,
    
    
    # Country Ranking Tab Panel =============

    
    tabPanel("Country Ranking",
      fluidRow(
        column(1), # Spacing column
        
        
        # Year Selection (Dropdown with single selection)
        column(5, offset = 1, selectInput("years", "Select Year",
                                          choices = years
                                          ),
               
               # styling options 
               style='padding-left:13px; padding-right:0px; padding-top:0px; padding-bottom:50px'
               ),
        
        
        # Data Table output
        column(11, offset = 1, textOutput("tab_name_rankings"),
              br(), # spacing
              
              # Data table
              wellPanel(
                dataTableOutput("rank_table_top")),
              
              # styling options 
              style='padding-left:00px; padding-right:100px; padding-top:0px; padding-bottom:0px'
              )
        )
      )
)



### Server function ============================================================



server <- function(input, output, session){
  
  
  # Choropleth map functions ==============
  
  
  
  ### Render HTML file ###
  output$choropleth <- renderUI({
    tags$iframe(seamless="seamless",
                
                # Input changes according to dropdown selection (input$variable_input1)
                src= paste0("tmpuser","/html files/choropleth - ",input$variable_input1,".html"),
                frameborder="0",
                width=1600*0.63, 
                height=900*0.63)
  })

  
  
  # Time Series functions =================
  
  
  ### Time series Country selection ###
  observeEvent(input$variable_input2,{
    
    # Get column index in data set for input$variable_input2 selection
    i <- which(input$variable_input2 == input_choice)
    
    
    # Create according updatet Country vector
    choice_vector <- c(var = get(paste0("choice_",input$variable_input2)))[1:(length(get(paste0("choice_",input$variable_input2)))-9)]
    name_vector <- c(gsub("\\."," ",get(paste0("choice_",input$variable_input2))))[1:(length(get(paste0("choice_",input$variable_input2)))-9)]
    inputvector <-  sort(c(replace(choice_vector,
                                   name_vector,
                                   choice_vector))[(length(name_vector)+1):(length(name_vector)*2)])
    
    
    # Country Selection updates according to dropdown selection (input$variable_input2)
    updateSelectInput(session, "countries", choices = inputvector)
  })

  
  
  ### Create subset depending on input$variable_input2 ###
  data <- reactive({
    criteria <- input$variable_input2
    get(paste0("data_",criteria))
  })

  
  
  ### Create time series plot via plotly ###
  output$time_series <- renderPlotly({
    
    
    # Load data
    data_plotly <- data()
    
    
    # Create plotly figure
    fig <- plot_ly(data_plotly, type = 'scatter', mode = 'lines', width = 16*63, height = 9*63)
    
    
    # If no country or area (World/OECD/non-OECD) is selected return nothing
    if(is.null(input$Area) && is.null(input$countries)){return(NULL)}
    
    
    # Add all ts plots for selected areas (World/OECD/non-OECD)
    invisible(lapply(seq_along(input$Area), function(i){
      
      # Mean
      fig <<- get("fig") %>%  add_trace(x = ~year_ts,
                                        y = as.formula(paste0("~`", input$Area[i],"_mean`")),
                                        name = paste0(names(choice_checkbox)[choice_checkbox == input$Area[i]], " Mean"),
                                        line = list(color = color_vector[i],
                                                    width = 4, legendgroup="group")) %>%
      
        # 25% Quantile
        add_trace(x = ~year_ts,
                  y = as.formula(paste0("~`", input$Area[i],"_bottom`")),
                  name = paste0(names(choice_checkbox)[choice_checkbox == input$Area[i]], " 25% Q"),
                  line = list(color = color_vector[i],
                              width = 2, dash = 'dash'), showlegend = F) %>%
        
        # 75% Quantile
        add_trace(x = ~year_ts,
                  y = as.formula(paste0("~`", input$Area[i],"_top`")),
                  name = paste0(names(choice_checkbox)[choice_checkbox == input$Area[i]], " 75% Q"),
                  line = list(color = color_vector[i],
                              width = 2, dash = 'dash'), showlegend = F)
      
      # Return updatet figure
      return(fig)
    }))


    # Add all ts plots for selected countries
    invisible(lapply(seq(input$countries), function(i){
      
      fig <<- get("fig") %>%  add_trace(x = ~year_ts,
                                        y = as.formula(paste0("~`", input$countries[i],"`")),
                                        name = sub("^.*?-", " ",input$countries[i]), # Replace . with space
                                        line = list(width = 2.5),
                                        mode = 'lines+markers')
      
      # Return updatet figure
      return(fig)
    }))
    
    
    # Layout settings
    fig <- fig %>% layout(showlegend = T, 
                          
                          # x-axis settings
                          xaxis = list(ticks = "inside", 
                                       ticklen = 10,
                                       zerolinecolor = '#ffff',
                                       zerolinewidth = 0,
                                       gridcolor = '#cdcfd1',
                                       title = "",
                                       showline = T,
                                       mirror = T
                                       ),
                          
                          # y-axis settings
                          yaxis = list(zerolinecolor = '#d3d4db',
                                       zerolinewidth = 2,
                                       gridcolor = '#cdcfd1',
                                       title = "",
                                       showline = T,
                                       mirror = T
                                       ),
                          
                          # Legend settings
                          legend = list(x = 100,
                                        y = 0.5,
                                        title= list(text='Select <br>'),
                                        xanchor = "right"),
                      
                              
                      # Annotions to explain dashed and solid lines    
                      annotations = list(text = 
                                                 
                                "Solid lines: Mean Values <br>Dashed lines: Quantile Values",
                                          
                                         showarrow=FALSE, align = "left", xref="paper", yref="paper",
                                         textangle=0 , x=-0.005, y=-0.205, font = list(size = 15)),
                                         plot_bgcolor = 'white', margin = 0.1) %>%
      
      # Disable ModeBar                      
      config(displayModeBar = F)
  })
  
  
  
  # Country Ranking functions =============

  
  ### Create data set depending on input$years ###
  data_ranking <- reactive({    
    data <- get(paste0("data_ranking_",input$years))
    # input_choice_2 <- get(paste0("indicator_ranking_",input$years))
    return(data)
    # data %>%
    #   select(Country, !!input_choice_2, !!paste0("score_",input_choice_2)) #%>%
      #mutate_if(is.numeric, round, digits=4) # Round values to 4 digits 
    
  })

  
  # https://stackoverflow.com/questions/73083652/sorting-rows-in-a-shiny-datatable-column
  # https://stackoverflow.com/questions/59428159/in-r-shinyproxy-how-do-i-get-the-order-of-columns-from-a-dtdatatable-after-a
  ### Create data table ###
  output$rank_table_top <- DT::renderDT({
    
    
    # Data Processing
    data <- as.data.frame(data_ranking())
    ncol_data <- ncol(data)     # Number of columns (needed do exclude increase/decrease columns)
    ncol_data_2 <- (ncol(data)/2) 
    
    data[is.na(data)] <- as.numeric("-")    # Replace NAs with -

    
    input_choice_2 <- colnames(data[,2:(ncol_data/2)])
    
    # Create Data Table
    data_table <- data %>% DT::datatable(
      selection = 'none', 
      editable = F,
      rownames = T,
      class = 'cell-border stripe',
      extensions = "ColReorder",
      
      
      # caption = 'Table 1: This is a simple caption for the table.',
      
      # # Download Button (disabled on Website)
      # extensions = 'Buttons',
      
      # Customization
      options = list(order = list(1, 'asc'),
                     lengthChange = F, 
                     pageLength = 200, 
                     autoWidth = F,
                     searching = T,
                     paging = F,
                     info = F,
                     colReorder = T,
                     # ordering = TRUE,
                     
                     # # Download Button (disabled on Website <- not working when App is embedded)
                     # dom = 'Bt',
                     # buttons = c('csv', 'excel'),
                     # buttons = list(
                     #   list(extend = c('csv'), text = c('CSV'), filename = paste0("Green Finance Indicators ", input$years),
                     #        exportOptions = list(
                     #          columns = c(1:(ncol_data - length(input_choice)))
                     #        )),
                     #   list(extend = c('excel'), text = c('EXCEL'), filename = paste0("Green Finance Indicators ", input$years),
                     #        exportOptions = list(
                     #          columns = c(1:(ncol_data - length(input_choice)))
                     #        ))
                     #   ),

                     scrollX = "100%",
                     columnDefs = list(list(targets = (ncol_data_2 + 1):ncol_data, visible = FALSE))
                     ),
      callback=JS("table.on( 'order.dt search.dt', function () {
                                table.column(0, {search:'applied', order:'applied'}).nodes().each( function (cell, i) {
                                      cell.innerHTML = i+1;});}).draw();")
      )
    
    
    # # Add background color (red = decrease, green = increase, yellow = constant)
    for (indicator in input_choice_2) {
      data_table <- data_table %>% DT::formatStyle(
        indicator,paste0("score_",indicator), backgroundColor = styleEqual(c(-1,0, 1), c('#FFCCCB','#FFFFE0','#CCFFCC'))
      )
    }
    
    
    # Return data table
    return(data_table)
  }) 

  
  
  # Change indicator dynamically ==========
  
  numb_indic_dd <- 2    # number of tab panels with indicator drop down
  
  
  lapply(seq(numb_indic_dd), function(i) {
    observeEvent(input[[paste0("variable_input", i)]], {    # Observe other Dropdowns and ...
      select_value <- input[[paste0("variable_input", i)]]  # ... save changed value 
      
      # Update other Dropdowns
      lapply(seq(numb_indic_dd)[-i], function(k){           
        updateSelectInput(session, as.character(paste0("variable_input", k)), select = select_value)
      })
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
# rsconnect::configureApp(account="moralmarket", server = "6733121", appName="Green_Finance_Indicators", size="xxxlarge")
# rsconnect::showLogs(appName="Green_Finance_Indicators",streaming=TRUE)
