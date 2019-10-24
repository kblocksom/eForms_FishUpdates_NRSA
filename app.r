library(shiny)
library(shinyjs)
## shinysky is to customize buttons
#library(shinysky) # not available for this version of R
library(DT)
library(data.table)
library(lubridate)
library(shinyalert)


# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

useShinyalert()

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("DT Editor Minimal Example"),
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),
  actionButton("refresh", "Reset",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
  
  helpText("Note: Remember to save any updates!"),
  br(),
  ### tags$head() is to customize the download button
  tags$head(tags$style(".butt{background-color:#230682;} .butt{color: #e6ebef;}")),
  downloadButton("Trich_csv", "Download in CSV", class="butt"),
  useShinyalert(), # Set up shinyalert
  uiOutput("MainBody_trich"),actionButton(inputId = "Updated_trich",label = "Save")
))

shinyServer(function(input, output, session){
  
  ### interactive dataset 
  vals_trich<-reactiveValues()
  vals_trich$Data<-readRDS("note.rds")
  
  observeEvent(input$refresh, {
    shinyjs::js$refresh()
  })
  #### MainBody_trich is the id of DT table
  output$MainBody_trich<-renderUI({
    fluidPage(
      hr(),
      column(12,dataTableOutput("Main_table_trich"))
    ) 
  })
  
  #### render DataTable part ####
  output$Main_table_trich<-renderDataTable({
    DT=vals_trich$Data
    datatable(DT,editable = TRUE, selection = "none") 
  }, server = T )
  
  proxy = dataTableProxy('Main_table_trich')
  
  observeEvent(input$Main_table_trich_cell_edit, {
    
    info = input$Main_table_trich_cell_edit
    
    str(info) 
    i = info$row 
    j = info$col 
    v = info$value
    
    vals_trich$Data[i, j] <<- DT::coerceValue(v, vals_trich$Data[i, j]) 
    replaceData(proxy, vals_trich$Data, resetPaging = FALSE) # important
    
    
  })
  
  
  ### save to RDS part 
  observeEvent(input$Updated_trich,{
    saveRDS(vals_trich$Data, "note.rds")
    shinyalert(title = "Saved!", type = "success")
  })
  
  
  
  
  
  ### This is nothing related to DT Editor but I think it is nice to have a download function in the Shiny so user 
  ### can download the table in csv
  output$Trich_csv<- downloadHandler(
    filename = function() {
      paste("Trich Project-Progress", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data.frame(vals_trich$Data), file, row.names = F)
    }
  )
  
})