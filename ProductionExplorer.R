library(shiny)
library(shinythemes)
library(DT)
library(reshape2)
library(rsconnect)
library(lubridate)
library(tidyverse)
library(DT)

#--------------------------------------------------------------------------------------
#-------------------------------- UI functions ------------------------------------
#--------------------------------------------------------------------------------------

# Define UI for data upload app ----
ui <- fluidPage(tags$head
                (tags$style
                  (list(HTML("hr {border-top: 1px solid #808080;}"),
                        ".topimg {margin-left:-10px;margin-right:-10px;margin-top:-10px;vertical-align: middle;}"))),
                theme = shinytheme("yeti"),
         navbarPage(theme = "yeti",
           title = "RAP Production Explorer",   
            # ------------------------------- Upload tab ----------------------------------------------------    
              tabPanel("Upload",
                         verbatimTextOutput("upload"),
                
                # Sidebar layout with input and output definitions ----
                sidebarLayout(
                  
                  # Sidebar panel for inputs ----
                  sidebarPanel(

                    # Upload tab UI elements
                    img(src='https://i.ibb.co/6mTbkZ7/RAPproduction.png', align = "center", width = '100%'),
                    tags$h3('Purpose'),
                    'This application analyzes Rangeland Analysis Platform 16-day production data. It is designed to assist with the assessment of current and historic departures from the long-term average.',
                    br(),
                    tags$h3('Disclaimer'),
                    'This application is preliminary and is subject to change.',
                    br(),
                    tags$h3('Instructions'),
                    '1) Download a 16-day biomass CSV from RAP ', a('(help).',href = 'https://support.rangelands.app/article/70-production-explorer'),
                    br(),
                    '2) Define a start and end year for calculating the long-term average.',
                    br(),
                    '3) Upload the RAP 16-day biomass CSV file using the "browse" button below.',
                    br(),
                    '4) Click on "Current" and "Historic" tabs to explore the data.',
                    br(),
                    br(),
                    a('For more information visit support.rangelands.app',href = 'https://support.rangelands.app/article/70-production-explorer'),
                    br(),
                    br(),
                    textInput("txt1", "Start year", value = 2001),
                    textInput("txt2", "End year", value = 2020),
                    
                    # Input: Select a file (CSV) ----
                    fileInput("file1", "Choose CSV File",
                              multiple = FALSE,
                              accept = c("text/csv",
                                         "text/comma-separated-values,text/plain",
                                         ".csv")) ),
                  
                  # Main panel for displaying outputs ----
                  mainPanel("Table will display below if it has been loaded properly.",
                    
                    # Output: Data file ----
                    tableOutput("contents") ) ) ),
           
           # ---------------------------- Current tab ----------------------------------------------------
           
           # Current tab UI elements   
           tabPanel('Current',
                    hr(),
                    tags$h2('Site summary'),
                    tags$h4('Current year'),
                    textOutput('currentText1'),
                    tags$h4(textOutput('currentSite')),
                    textOutput('currentText2'),
                    textOutput('currentText3'),
                    textOutput('currentText4'),
                    br(),
                    hr(),
                    tags$h2('Current year production plot'),
                    plotOutput('currentPlot')),
              
           # ------------------------------ Historical tab -----------------------------------------------      
           
           # Historic tab UI elements
           tabPanel('Historic',
                    hr(),
                    tags$h2('Site summary'),
                    tags$h4(textOutput('historicSite')),
                    textOutput('historicText1'),
                    textOutput('historicText2'),
                    textOutput('historicText3'),
                    br(),
                    hr(),
                    tags$h2('Time-series plot'),
                    plotOutput("historicPlot"),
                    br(),
                    hr(),
                    tags$h2('Time-series table'),
                    textOutput('historicTSText'),
                    DT::dataTableOutput('historicTable'))           ))

#--------------------------------------------------------------------------------------
#-------------------------------- Server functions ------------------------------------
#--------------------------------------------------------------------------------------

# Define server logic to read selected file ----
server <- function(input, output) {

#--------------------------------- Main tab--------------------------------------------
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = TRUE,
                       sep = ",") %>%
          select(-c(doy,year)) %>%
          mutate(afgAGB = replace(afgAGB, afgAGB == -9999 ,NA),
                 pfgAGB = replace(pfgAGB, pfgAGB == -9999 ,NA),
                 herbaceousAGB = replace(herbaceousAGB, herbaceousAGB == -9999 ,NA))
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
  })
  
#--------------------------------- Current tab--------------------------------------------  

  # Current year object to be used in filtering later
  curr_year <- reactive({ 
    year(today())
  })
  
  # Dataframe to produce data elements for this tab
  curr_lt_df <- reactive({
    
    req(input$file1)
    
    # Pre-process data
    df <- read.csv(input$file1$datapath,
                   header = TRUE,
                   sep = ",") %>%
      mutate(afgAGB = replace(afgAGB, afgAGB == -9999 ,NA),
             pfgAGB = replace(pfgAGB, pfgAGB == -9999 ,NA),
             herbaceousAGB = round(replace(herbaceousAGB, herbaceousAGB == -9999 ,NA)))
    
    # Produce columns for historic averages and percentages
    lt_df <- df %>%
      filter(year >= as.numeric(input$txt1)) %>%
      filter(year <= as.numeric(input$txt2)) %>%
      drop_na() %>%
      group_by(doy) %>%
      mutate(avg_per = mean(pfgAGB),
             avg_ann = mean(afgAGB),
             `Long-term average` = mean(herbaceousAGB),
             `50% of long-term average` = `Long-term average` * .5,
             `75% of long-term average` = `Long-term average` * .75)%>%
      select(c( doy,`Long-term average`, `75% of long-term average`, `50% of long-term average`)) %>%
      unique()
    
    # Produce column for current year
    cu_df <- df %>%
      filter(year == curr_year()) %>%
      drop_na() %>%
      select(c(doy,`Current year` = herbaceousAGB))
    
    # Join current year to historical data
    lt_df %>%
      left_join(cu_df, by = 'doy') %>%
      select(c( doy,`Current year`, `Long-term average`, `75% of long-term average`, `50% of long-term average`)) %>%
      melt(id = c('doy')) })
  
  
  # Plot for this tab
  output$currentPlot <- renderPlot({
    ggplot(curr_lt_df())+
      geom_line(mapping = aes(x = as.Date(doy, origin = paste(as.character(curr_year()),'-01-01',sep = '')), y = value, color = variable, linetype = variable, size = variable)) +
      labs(y = 'Production (lbs/acre)') +
      scale_x_date(date_labels = '%b %d')+
      scale_color_manual(
        values = c(
          `Long-term average`="grey40",
          `75% of long-term average`="orange",
          `50% of long-term average`="red",
          `Current year`="grey20"))+
      scale_linetype_manual(
        values = c(
          `Long-term average`="dashed",
          `75% of long-term average`="dashed",
          `50% of long-term average`="dashed",
          `Current year`="solid"))+
      scale_size_manual(
        values = c(
          `Long-term average`=1,
          `75% of long-term average`=1,
          `50% of long-term average`=1,
          `Current year`=2))+
      theme_minimal()+
      theme(axis.title = element_text(size = 20, color = 'grey30'),
            axis.title.x = element_blank(),
            axis.text.y = element_text(size = 18, color = 'grey30'),
            axis.text.x = element_text(size = 18, color = 'grey30'),
            legend.title = element_blank(),
            legend.text = element_text(size = 18, color = 'grey30'),
            legend.position="top")
  })
  
  # Current date object for printing in dynamic text strings
  curr_date <- reactive({ 
    curr_prod_df = 
      curr_lt_df()%>%
      filter(variable == 'Current year')%>%
      drop_na()
    as.Date(max(curr_prod_df$doy), origin = '2021-01-01')
  })
  
  # Text string for Site Summary header
  output$currentSite <- renderText({
    expr <- paste('Long-term average (', as.character(input$txt1), '-', as.character(input$txt2), ')', sep = '')
    print(expr)         
  })
  
  # Text string for current year production
  output$currentText1 <- renderText({ 
    curr_prod_df = 
    curr_lt_df()%>%
      filter(variable == 'Current year')%>%
      drop_na()
    curr_prod = round(sum(curr_prod_df$value),0)
    curr_date = format(curr_date(), "%B %d")
    
    expr <- paste('Production through ', as.character(curr_date), ' of this year: ', as.character(curr_prod), ' lbs/acre.', sep = '')
    print(expr)
  })
  
  # Text string for historic average of production
  output$currentText2 <- renderText({ 
    hist_prod_df = curr_lt_df()%>%
      filter(variable == 'Long-term average') %>%
      filter(doy <= lubridate::yday(curr_date()))
    hist_prod = round(sum(hist_prod_df$value),0)
    curr_date = format(curr_date(), "%B %d")
    
    expr <- paste('Average production through ', as.character(curr_date), ': ', as.character(hist_prod), ' lbs/acre.', sep = '')
    print(expr)
  })
  
  # Text string for 75% of historic average of production
  output$currentText3 <- renderText({ 
    hist_prod_df = curr_lt_df()%>%
      filter(variable == '75% of long-term average') %>%
      filter(doy <= lubridate::yday(curr_date()))
    hist_prod = round(sum(hist_prod_df$value),0)
    curr_date = format(curr_date(), "%b %d")
    
    expr <- paste('75% of average production through ', as.character(curr_date), ': ', as.character(hist_prod), ' lbs/acre.', sep = '')
    print(expr)
  })
  
  # Text string for 50% of historic average of production
  output$currentText4 <- renderText({ 
    hist_prod_df = curr_lt_df()%>%
      filter(variable == '50% of long-term average') %>%
      filter(doy <= lubridate::yday(curr_date()))
    hist_prod = round(sum(hist_prod_df$value),0)
    curr_date = format(curr_date(), "%b %d")
    
    expr <- paste('50% of average production through ', as.character(curr_date), ': ', as.character(hist_prod), ' lbs/acre.', sep = '')
    print(expr)
  })
    
#--------------------------------- Historic tab--------------------------------------------
  
  # Dataframe to produce data elements for this tab
  hist_lt_df <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath,
                     header = TRUE,
                     sep = ",") %>%
      mutate(afgAGB = replace(afgAGB, afgAGB == -9999 ,NA),
             pfgAGB = replace(pfgAGB, pfgAGB == -9999 ,NA),
             herbaceousAGB = replace(herbaceousAGB, herbaceousAGB == -9999 ,NA)) %>%
      filter(year >= as.numeric(input$txt1)) %>%
      filter(year <= as.numeric(input$txt2)) %>%
      drop_na() %>%
      group_by(year) %>%
      mutate(`Annual production` = round(sum(herbaceousAGB)),1) %>%
      select(c(Year = year, `Annual production`)) %>%
      unique()
  })

  # Data table for this tab
  output$historicTable <- DT::renderDataTable({

    mean_prod = mean(hist_lt_df()$`Annual production`)
    
    lt_df <- hist_lt_df() %>%
      mutate(`Percent of average` = (`Annual production` / mean_prod))

    DT::datatable(lt_df, rownames = FALSE, 
                  options = list(pageLength = nrow(lt_df), 
                                        lengthChange = FALSE,  
                                        searching = FALSE,
                                        initComplete = JS("function(settings, json) {$(this.api().table().header()).css({'font-size' : '16px'});}"),
                                        "function(settings, json) {",
                                        "$(this.api().table().header()).css({'font-size': '5px', 'background-color': '#c2d1f0', 'color': '#fff'});",
                                        "}")) %>%  
      formatStyle(columns = colnames(.$x$data), `font-size` = '16px') %>% 
      formatPercentage("Percent of average", 1)
  })

  # Plot for this tab
  output$historicPlot <- renderPlot({
    
    prod_mean = mean(hist_lt_df()$`Annual production`)
    prod_75 = prod_mean *.75
    prod_50 = prod_mean *.50
    
    plot_df <-     hist_lt_df() %>%
      mutate(`Long-term average`= prod_mean,
             `75% of long-term average`= prod_75,
             `50% of long-term average`= prod_50) %>%
      melt(id = 'Year')
    
    ggplot() +
      geom_line(plot_df, mapping = aes(x = Year, y = value, color = variable, size = variable, linetype = variable))+
      scale_color_manual(values = c(`Annual production` = "grey0",
                                               `Long-term average`= "grey40",
                                               `75% of long-term average` = "orange",
                                               `50% of long-term average` = "red"))+
      scale_size_manual(values = c(`Annual production` = 2,
                                               `Long-term average`= 1,
                                               `75% of long-term average` = 1,
                                               `50% of long-term average` = 1))+
      scale_linetype_manual(values = c(`Annual production` = 'solid',
                                              `Long-term average`= 'dashed',
                                              `75% of long-term average` = 'dashed',
                                              `50% of long-term average` = 'dashed'))+
      labs(y = 'Production (lbs/acre')+
      ylim(0, max(hist_lt_df()$`Annual production` +(max(hist_lt_df()$`Annual production`) * .1)))+
      theme_minimal()+
      theme(axis.title.x=element_blank(),
            axis.title = element_text(size = 20, color = 'grey30'),
            axis.text.y = element_text(size = 18, color = 'grey30'),
            axis.text.x = element_text(size = 18, color = 'grey30'),
            legend.title = element_blank(),
            legend.text = element_text(size = 18, color = 'grey30'),
            legend.position="top")
  })
  
  # Text string for average annual production
  output$historicText1 <- renderText({
     mean_prod = mean(hist_lt_df()$`Annual production`)
     
     expr = paste('Average annual production: ', as.character(round(mean_prod),0), ' lbs/acre.', sep='')
     print(expr)
   })
  
  # Text string for 75% of average annual production
  output$historicText2 <- renderText({
     mean_prod = mean(hist_lt_df()$`Annual production`)
     threshold = mean_prod * .75
     
     expr = paste('75% of average annual production: ', as.character(round(threshold,0)), ' lbs/acre.', sep='')
     print(expr)
   })
  
  # Text string for 50% of average annual production
  output$historicText3 <- renderText({
     mean_prod = mean(hist_lt_df()$`Annual production`)
     threshold = mean_prod * .5
     
     expr = paste('50% of average annual production: ', as.character(round(threshold,0)), ' lbs/acre.', sep='')
     print(expr)
   })
  
  # Text string for Site Summary header
  output$historicSite <- renderText({
     expr <- paste('Long-term average (', as.character(input$txt1), '-', as.character(input$txt2), ')', sep = '')
     print(expr)         
   })
  
  # Text String for table header
  output$historicTSText <- renderText({
     mean_prod = mean(hist_lt_df()$`Annual production`)
     
     expr <- paste('Annual production relative to long-term (', as.character(input$txt1), '-', as.character(input$txt2), ') average of ', as.character(round(mean_prod),1), ' lbs/acre', sep = '')
   })
}

# Create Shiny app ----
shinyApp(ui, server)