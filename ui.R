require(shinydashboard)
library(shinyjs)
library(htmlwidgets)
library(DT)

#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "2020 Live Election Forecast", titleWidth = 400)  

sidebar <- dashboardSidebar(
  tags$script(JS("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';")),
  tags$style(
    type = 'text/css',
    "label {font-size: 12px}
                  .selectize-input { margin-bottom: -15px; margin-top: -10px; height: 10px; font-size: 12px; padding-top: 5px;}
                  .selectize-dropdown { margin-bottom: -15px;  margin-top: -10px; height: 10px; font-size: 12px; padding-top: 5px;}"
  ),
  
  width = 400,
  fluidRow(
      ### COLUMN 1      
      column(
        width = 4,
        selectizeInput(
          df[1, 3],
          label = df[1, 1],
          choices = c("Biden", "Trump"),
          options = list(
            placeholder = '',
            onInitialize = I('function() { this.setValue(""); }')
          ),
          width = 115
        ),
        selectizeInput(
          df[2, 3],
          label = df[2, 1],
          choices = c("Biden", "Trump"),
          options = list(
            placeholder = '',
            onInitialize = I('function() { this.setValue(""); }')
          ),
          width = 115
        ),
        selectizeInput(
          df[3, 3],
          label = df[3, 1],
          choices = c("Biden", "Trump"),
          options = list(
            placeholder = '',
            onInitialize = I('function() { this.setValue(""); }')
          ),
          width = 115
        ),
        selectizeInput(
          df[4, 3],
          label = df[4, 1],
          choices = c("Biden", "Trump"),
          options = list(
            placeholder = '',
            onInitialize = I('function() { this.setValue(""); }')
          ),
          width = 115
        ),
        selectizeInput(
          df[5, 3],
          label = df[5, 1],
          choices = c("Biden", "Trump"),
          options = list(
            placeholder = '',
            onInitialize = I('function() { this.setValue(""); }')
          ),
          width = 115
        ),
        selectizeInput(
          df[6, 3],
          label = df[6, 1],
          choices = c("Biden", "Trump"),
          options = list(
            placeholder = '',
            onInitialize = I('function() { this.setValue(""); }')
          ),
          width = 115
        ),
        selectizeInput(
          df[7, 3],
          label = df[7, 1],
          choices = c("Biden", "Trump"),
          options = list(
            placeholder = '',
            onInitialize = I('function() { this.setValue(""); }')
          ),
          width = 115
        ),
        selectizeInput(
          df[8, 3],
          label = df[8, 1],
          choices = c("Biden", "Trump"),
          options = list(
            placeholder = '',
            onInitialize = I('function() { this.setValue(""); }')
          ),
          width = 115
        ),
        selectizeInput(
          df[9, 3],
          label = df[9, 2],
          choices = c("Biden", "Trump"),
          options = list(
            placeholder = '',
            onInitialize = I('function() { this.setValue(""); }')
          ),
          width = 115
        ),
        selectizeInput(
          df[10, 3],
          label = df[10, 1],
          choices = c("Biden", "Trump"),
          options = list(
            placeholder = '',
            onInitialize = I('function() { this.setValue(""); }')
          ),
          width = 115
        ),
        selectizeInput(
          df[11, 3],
          label = df[11, 1],
          choices = c("Biden", "Trump"),
          options = list(
            placeholder = '',
            onInitialize = I('function() { this.setValue(""); }')
          ),
          width = 115
        ),
        selectizeInput(
          df[12, 3],
          label = df[12, 1],
          choices = c("Biden", "Trump"),
          options = list(
            placeholder = '',
            onInitialize = I('function() { this.setValue(""); }')
          ),
          width = 115
        ),
        selectizeInput(
          df[13, 3],
          label = df[13, 1],
          choices = c("Biden", "Trump"),
          options = list(
            placeholder = '',
            onInitialize = I('function() { this.setValue(""); }')
          ),
          width = 115
        ),
        selectizeInput(
          df[14, 3],
          label = df[14, 1],
          choices = c("Biden", "Trump"),
          options = list(
            placeholder = '',
            onInitialize = I('function() { this.setValue(""); }')
          ),
          width = 115
        ),
        selectizeInput(
          df[15, 3],
          label = df[15, 1],
          choices = c("Biden", "Trump"),
          options = list(
            placeholder = '',
            onInitialize = I('function() { this.setValue(""); }')
          ),
          width = 115
        ),
        selectizeInput(
          df[16, 3],
          label = df[16, 1],
          choices = c("Biden", "Trump"),
          options = list(
            placeholder = '',
            onInitialize = I('function() { this.setValue(""); }')
          ),
          width = 115
        ),
        selectizeInput(
          df[17, 3],
          label = df[17, 1],
          choices = c("Biden", "Trump"),
          options = list(
            placeholder = '',
            onInitialize = I('function() { this.setValue(""); }')
          ),
          width = 115
        ),
      ),
      ### COLUMN 2
      column(
        4, style='padding:5px;',
        selectizeInput(
          df[18, 3],
          label = df[18, 1],
          choices = c("Biden", "Trump"),
          options = list(
            placeholder = '',
            onInitialize = I('function() { this.setValue(""); }')
          ),
          width = 115
        ),
        selectizeInput(
          df[19, 3],
          label = df[19, 1],
          choices = c("Biden", "Trump"),
          options = list(
            placeholder = '',
            onInitialize = I('function() { this.setValue(""); }')
          ),
          width = 115
        ),
        selectizeInput(
          df[20, 3],
          label = df[20, 1],
          choices = c("Biden", "Trump"),
          options = list(
            placeholder = '',
            onInitialize = I('function() { this.setValue(""); }')
          ),
          width = 115
        ),
        selectizeInput(
          df[21, 3],
          label = df[21, 1],
          choices = c("Biden", "Trump"),
          options = list(
            placeholder = '',
            onInitialize = I('function() { this.setValue(""); }')
          ),
          width = 115
        ),
        selectizeInput(
          df[22, 3],
          label = df[22, 1],
          choices = c("Biden", "Trump"),
          options = list(
            placeholder = '',
            onInitialize = I('function() { this.setValue(""); }')
          ),
          width = 115
        ),
        selectizeInput(
          df[23, 3],
          label = df[23, 1],
          choices = c("Biden", "Trump"),
          options = list(
            placeholder = '',
            onInitialize = I('function() { this.setValue(""); }')
          ),
          width = 115
        ),
        selectizeInput(
          df[24, 3],
          label = df[24, 1],
          choices = c("Biden", "Trump"),
          options = list(
            placeholder = '',
            onInitialize = I('function() { this.setValue(""); }')
          ),
          width = 115
        ),
        selectizeInput(
          df[25, 3],
          label = df[25, 1],
          choices = c("Biden", "Trump"),
          options = list(
            placeholder = '',
            onInitialize = I('function() { this.setValue(""); }')
          ),
          width = 115
        ),
        selectizeInput(
          df[26, 3],
          label = df[26, 1],
          choices = c("Biden", "Trump"),
          options = list(
            placeholder = '',
            onInitialize = I('function() { this.setValue(""); }')
          ),
          width = 115
        ),
        selectizeInput(
          df[27, 3],
          label = df[27, 1],
          choices = c("Biden", "Trump"),
          options = list(
            placeholder = '',
            onInitialize = I('function() { this.setValue(""); }')
          ),
          width = 115
        ),
        selectizeInput(
          df[28, 3],
          label = df[28, 1],
          choices = c("Biden", "Trump"),
          options = list(
            placeholder = '',
            onInitialize = I('function() { this.setValue(""); }')
          ),
          width = 115
        ),
        selectizeInput(
          df[29, 3],
          label = df[29, 1],
          choices = c("Biden", "Trump"),
          options = list(
            placeholder = '',
            onInitialize = I('function() { this.setValue(""); }')
          ),
          width = 115
        ),
        selectizeInput(
          df[30, 3],
          label = df[30, 1],
          choices = c("Biden", "Trump"),
          options = list(
            placeholder = '',
            onInitialize = I('function() { this.setValue(""); }')
          ),
          width = 115
        ),
        selectizeInput(
          df[31, 3],
          label = df[31, 1],
          choices = c("Biden", "Trump"),
          options = list(
            placeholder = '',
            onInitialize = I('function() { this.setValue(""); }')
          ),
          width = 115
        ),
        selectizeInput(
          df[32, 3],
          label = df[32, 1],
          choices = c("Biden", "Trump"),
          options = list(
            placeholder = '',
            onInitialize = I('function() { this.setValue(""); }')
          ),
          width = 115
        ),
        selectizeInput(
          df[33, 3],
          label = df[33, 1],
          choices = c("Biden", "Trump"),
          options = list(
            placeholder = '',
            onInitialize = I('function() { this.setValue(""); }')
          ),
          width = 115
        ),
        selectizeInput(
          df[34, 3],
          label = df[34, 1],
          choices = c("Biden", "Trump"),
          options = list(
            placeholder = '',
            onInitialize = I('function() { this.setValue(""); }')
          ),
          width = 115
        )
      ),
      ### COLUMN 3
      column(
        4, style='padding:5px;',
        selectizeInput(
          df[35, 3],
          label = df[35, 1],
          choices = c("Biden", "Trump"),
          options = list(
            placeholder = '',
            onInitialize = I('function() { this.setValue(""); }')
          ),
          width = 115
        ),
        
        selectizeInput(
          df[36, 3],
          label = df[36, 1],
          choices = c("Biden", "Trump"),
          options = list(
            placeholder = '',
            onInitialize = I('function() { this.setValue(""); }')
          ),
          width = 115
        ),
        selectizeInput(
          df[37, 3],
          label = df[37, 1],
          choices = c("Biden", "Trump"),
          options = list(
            placeholder = '',
            onInitialize = I('function() { this.setValue(""); }')
          ),
          width = 115
        ),
        selectizeInput(
          df[38, 3],
          label = df[38, 1],
          choices = c("Biden", "Trump"),
          options = list(
            placeholder = '',
            onInitialize = I('function() { this.setValue(""); }')
          ),
          width = 115
        ),
        selectizeInput(
          df[39, 3],
          label = df[39, 1],
          choices = c("Biden", "Trump"),
          options = list(
            placeholder = '',
            onInitialize = I('function() { this.setValue(""); }')
          ),
          width = 115
        ),
        selectizeInput(
          df[40, 3],
          label = df[40, 1],
          choices = c("Biden", "Trump"),
          options = list(
            placeholder = '',
            onInitialize = I('function() { this.setValue(""); }')
          ),
          width = 115
        ),
        selectizeInput(
          df[41, 3],
          label = df[41, 1],
          choices = c("Biden", "Trump"),
          options = list(
            placeholder = '',
            onInitialize = I('function() { this.setValue(""); }')
          ),
          width = 115
        ),
        selectizeInput(
          df[42, 3],
          label = df[42, 1],
          choices = c("Biden", "Trump"),
          options = list(
            placeholder = '',
            onInitialize = I('function() { this.setValue(""); }')
          ),
          width = 115
        ),
        selectizeInput(
          df[43, 3],
          label = df[43, 1],
          choices = c("Biden", "Trump"),
          options = list(
            placeholder = '',
            onInitialize = I('function() { this.setValue(""); }')
          ),
          width = 115
        ),
        selectizeInput(
          df[44, 3],
          label = df[44, 1],
          choices = c("Biden", "Trump"),
          options = list(
            placeholder = '',
            onInitialize = I('function() { this.setValue(""); }')
          ),
          width = 115
        ),
        selectizeInput(
          df[45, 3],
          label = df[45, 2],
          choices = c("Biden", "Trump"),
          options = list(
            placeholder = '',
            onInitialize = I('function() { this.setValue(""); }')
          ),
          width = 115
        ),
        selectizeInput(
          df[46, 3],
          label = df[46, 1],
          choices = c("Biden", "Trump"),
          options = list(
            placeholder = '',
            onInitialize = I('function() { this.setValue(""); }')
          ),
          width = 115
        ),
        selectizeInput(
          df[47, 3],
          label = df[47, 1],
          choices = c("Biden", "Trump"),
          options = list(
            placeholder = '',
            onInitialize = I('function() { this.setValue(""); }')
          ),
          width = 115
        ),
        selectizeInput(
          df[48, 3],
          label = df[48, 1],
          choices = c("Biden", "Trump"),
          options = list(
            placeholder = '',
            onInitialize = I('function() { this.setValue(""); }')
          ),
          width = 115
        ),
        selectizeInput(
          df[49, 3],
          label = df[49, 1],
          choices = c("Biden", "Trump"),
          options = list(
            placeholder = '',
            onInitialize = I('function() { this.setValue(""); }')
          ),
          width = 115
        ),
        selectizeInput(
          df[50, 3],
          label = df[50, 1],
          choices = c("Biden", "Trump"),
          options = list(
            placeholder = '',
            onInitialize = I('function() { this.setValue(""); }')
          ),
          width = 115
        ),
        selectizeInput(
          df[51, 3],
          label = df[51, 1],
          choices = c("Biden", "Trump"),
          options = list(
            placeholder = '',
            onInitialize = I('function() { this.setValue(""); }')
          ),
          width = 115
        )
      )
    )
)

body <- dashboardBody(
  fluidPage(
  # Show a plot of the generated map
  fluidRow(
    column(3,valueBoxOutput("biden_win", width=NULL)), 
    column(3,valueBoxOutput("biden_ec", width=NULL)),
    column(3,valueBoxOutput("trump_win", width=NULL)),
    column(3,valueBoxOutput("trump_ec", width=NULL))
  ),
  fluidRow(column(12,plotOutput("mapPlot"))),
  br(),
  br(),
  fluidRow(column(12,dataTableOutput("data_table")))
  )
)
  
    
#completing the ui part with dashboardPage
ui <- dashboardPage(title = '2020 Live Election Forecast', header, sidebar, body, skin='blue')
