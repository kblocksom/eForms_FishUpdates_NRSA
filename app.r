source("global.r")

# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#

useShinyalert()

# Define UI for application that draws a histogram
ui = fluidPage(
  
  # Application title
  titlePanel("NRSA Fish Collection Data Update Tool (v. 1.0)"),
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),
  actionButton("refresh", "Reset",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
  
  helpText('The NRSA Fish Collection Data Update Tool allows crews to upload the .JSON file containing fish 
           collection data as submitted to NARS IM. They can then update the data (names and counts, including 
           adding or deleting rows) and download an updated file in JSON format that can be submitted to 
           NARS IM. This process replaces use of fillable PDFs for fish collection data updates for 
           those crews. Please direct all questions related to tool troubleshooting and feature requests to
           Karen Blocksom (Blocksom.Karen@epa.gov).'),
  br(),
  
  
  ### tags$head() is to customize the download button
  tags$head(tags$style(".butt{background-color:#230682;} .butt{color: #e6ebef;}")),
  fileInput(inputId='filenm', buttonLabel='Browse...', label='Please select a _FISH.JSON file',
            multiple=FALSE, accept=c('json','JSON')), 
  
  downloadButton("downloadJSON", "Save Updated Results as .JSON", class="butt"),
  useShinyalert(), # Set up shinyalert
  uiOutput("MainBody_fish")
  #,actionButton(inputId = "Updated_fish",label = "Save")
)

server = function(input, output, session){
  
  ### interactive dataset 
  vals_fish<-reactiveValues()
  
  observeEvent(input$filenm, {
    vals_fish$Data<-organizationShiny(input$filenm$datapath,
                                      input$filenm$name)
  })
  
  
  observeEvent(input$refresh, {
    shinyjs::js$refresh()
  })
  #### MainBody_trich is the id of DT table
  output$MainBody_fish<-renderUI({
    fluidPage(
      hr(),
      column(12,dataTableOutput("Main_table_fish"))
    ) 
  })
  
  
  #### render DataTable part ####
  output$Main_table_fish<-renderDataTable({
    DT=vals_fish$Data
    datatable(DT,editable = list(target="cell",disable = list(columns = c(1:9))),
              rownames=F,
     selection = "none") 
  }, server=TRUE)
  
  proxy = dataTableProxy('Main_table_fish')
  
  observeEvent(input$Main_table_fish_cell_edit, {
    
    info = input$Main_table_fish_cell_edit
    
    str(info) 
    i = info$row 
    j = info$col + 1L
    v = info$value
    
    vals_fish$Data[i, j] <<- DT::coerceValue(v, vals_fish$Data[i, j]) 
    replaceData(proxy, vals_fish$Data, resetPaging = FALSE) # important
    
    
  })
  
  
  # ### save to RDS part 
  # observeEvent(input$Updated_fish,{
  #   saveRDS(vals_fish$Data, "fish.rds")
  #   shinyalert(title = "Saved!", type = "success")
  # })
  # 
  # 
  
  
  
  ### This is nothing related to DT Editor but I think it is nice to have a download function in the Shiny so user 
  ### can download the table in csv
  # output$Trich_csv<- downloadHandler(
  #   filename = function() {
  #     paste("Trich Project-Progress", Sys.Date(), ".csv", sep="")
  #   },
  #   content = function(file) {
  #     write.csv(data.frame(vals_trich$Data), file, row.names = F)
  #   }
  # )
  
  output$downloadJSON <- downloadHandler(filename = function(){paste(str_extract(input$filenm$name,"[:alnum:]+\\_[:alpha:]+\\_[:alnum:]+\\_[:alnum:]\\_FISH"), ".json", sep="")},
                                         content = function(file) {
                                           updData.wide <- subset(vals_fish$Data, select=-PAGE) %>%
                                             subset(NAME_COM!='' & !is.na(NAME_COM)) %>%
                                             melt(id.vars=c('UID','SITE_ID','VISIT_NO','YEAR','STUDYNAME','APP_PLATFORM','APP_VERSION','SAMPLE_TYPE','LINE'),na.rm=TRUE) %>%
                                             subset(value!='') %>%
                                             mutate(variable=paste(LINE,variable,sep='_')) %>%
                                             subset(select=-LINE) %>%
                                             dcast(UID+SITE_ID+VISIT_NO+YEAR+STUDYNAME+APP_PLATFORM+APP_VERSION+SAMPLE_TYPE~variable, value.var='value') %>%
                                             subset(select=-SAMPLE_TYPE)
                                           
                                           formatData <- list(UID=unique(updData.wide$UID),SITE_ID=unique(updData.wide$SITE_ID),
                                                              VISIT_NO=unique(updData.wide$VISIT_NO),YEAR=unique(updData.wide$YEAR),
                                                              STUDYNAME=unique(updData.wide$STUDYNAME),APP_PLATFORM=unique(updData.wide$APP_PLATFORM),
                                                              APP_VERSION=unique(updData.wide$APP_VERSION),FISH=unbox(updData.wide[,8:length(updData.wide)]))
                                           
                                           jsonData <- jsonlite::write_json(formatData,auto_unbox=TRUE,prettify=TRUE,file) 
                                           
                                           
                                         }
  )

  session$onSessionEnded(function() {
    stopApp()
  }) 

}
# Run the application 
shinyApp(ui = ui, server = server)