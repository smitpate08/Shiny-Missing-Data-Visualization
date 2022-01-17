#Importing Libraries
library(shiny)
library(shinydashboard)
library(dplyr)
library(visdat)
library(ggplot2)
library(shinyBS)
library(DT)
#=======UI======================================
ui<-dashboardPage(
  dashboardHeader(title="Missing Data Visualization",titleWidth = 230),
  dashboardSidebar(
    fileInput("file1", "Upload CSV File below",
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv")
    )),
  dashboardBody(
    fluidRow(
      div(id="popme1",box(plotOutput("Plot1"),title="Columns with Missing values in %",solidHeader = TRUE,status = "primary")),
      bsModal("modalExample1", "Columns with Missing values in %", "popme1", size = "large",plotOutput("Plot11")),
      div(id="popme2",box(plotOutput("Plot2"),title="Columns Data Type",solidHeader = TRUE,status = "primary")),
      bsModal("modalExample2", "Columns Data Type", "popme2", size = "large", plotOutput("Plot22")),
      div(id="popme3",fluidRow(column(width=12,box( dataTableOutput("Missing_datatable"),width = NULL)))),
      bsModal("modalExample3", "Rows with Missing values", "popme3", size = "large", DTOutput("Missing_datatable2"))
      )
  )
)

#=======Server============================================================
server<-function(input, output,session) {
  #Creating reactivity so file is read once
   data <- reactive({
    validate(need(input$file1!= "","Please use Browse button on left side to upload csv file"))
    req(input$file1)
    read.csv(input$file1$datapath, header = TRUE,
             sep = ",")
  })
  
   #Rendering Plot using Visdat's vis_miss module (Top Left Corner plot)
  output$Plot1 <-renderPlot({
    vis_miss(req(data()),warn_large_data = FALSE,sort_miss = TRUE) + 
      theme(text = element_text(size=15),axis.text.x = element_text(angle=90,hjust=0))
  })
  
  #Modaldialog code for Plot 1 (Top Left Corner plot)
  output$Plot11 <-renderPlot({
    vis_miss(req(data()),warn_large_data = FALSE,sort_miss = TRUE) + 
      theme(text = element_text(size=15),axis.text.x = element_text(angle=90,hjust=0))
  })
  
  #Rendering Plot using Visdat's module (Data Type Plot (Top right corner plot))
  output$Plot2 <- renderPlot({
    vis_dat(req(data()),warn_large_data = FALSE) + 
      theme(text = element_text(size=15),axis.text.x = element_text(angle=90,hjust=0))
  })
  
  #Modaldialog code for Plot 2 (Top right Corner plot)
  output$Plot22 <- renderPlot({
    vis_dat(req(data()),warn_large_data = FALSE) + 
      theme(text = element_text(size=15),axis.text.x = element_text(angle=90,hjust=0))
  })
  
  #Rendering Data Table using DT package, DPLYR is use to filter rows with missing values
  output$Missing_datatable <-DT::renderDT({
    req(data()) %>% 
      filter_all(any_vars(is.na(.))) 
  },options= list(scrollX = TRUE,columnDefs =list(list(className = 'dt-center', 
                                                             targets = "_all"))))
    #Modaldialog for DataTable
   output$Missing_datatable2 <-DT::renderDT({
    req(data()) %>% 
      filter_all(any_vars(is.na(.))) 
  },options= list(scrollX = TRUE,columnDefs =list(list(className = 'dt-center', 
                                                       targets = "_all"))))
  
  }

# Run the application 
shinyApp(ui = ui, server = server)

