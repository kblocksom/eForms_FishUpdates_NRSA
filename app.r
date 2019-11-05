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

  p('The NRSA Fish Collection Data Update Tool allows crews to upload the .JSON file containing fish 
           collection data as submitted to NARS IM. They can then update the data (names and counts, including 
           adding or deleting rows) and download an updated file (of the same name) in JSON format that can be submitted to 
           NARS IM. This process replaces use of fillable PDFs for fish collection data updates for 
           those crews. Please direct all questions related to tool troubleshooting and feature requests to
           Karen Blocksom (Blocksom.Karen@epa.gov).'),
  br(),
  
  ### tags$head() is to customize the download button
  tags$head(tags$style(".butt{background-color:#230682;} .butt{color: #e6ebef;}")),
  fileInput(inputId='filenm', buttonLabel='Browse...', label='Please select a _FISH.JSON file to edit',
            multiple=FALSE, accept=c('json','JSON')), 
  
  helpText('Note: To discard changes and start over, reload file before saving to JSON file.'),
  downloadButton("downloadJSON", "Save Updated Results to .JSON", class="butt"),
  useShinyalert(), # Set up shinyalert
  uiOutput("MainBody_fish")
)

server = function(input, output, session){
  
  ### interactive dataset 
  vals_fish<-reactiveValues()
  
  observeEvent(input$filenm, {
    vals_fish$Data<-organizationShiny(input$filenm$datapath,
                                      input$filenm$name)
  })
  
  
  #### MainBody_trich is the id of DT table
  output$MainBody_fish<-renderUI({
    fluidPage(
      hr(),
      column(6,offset = 6,
             HTML('<div class="btn-group" role="group" aria-label="Fish Updates" style = "padding:10px">'),
             ### tags$head() This is to change the color of "Add a new row" button
             tags$head(tags$style(".butt2{background-color:#231651;} .butt2{color: #e6ebef;}")),
             div(style="display:inline-block;width:30%;text-align: center;",actionButton(inputId = "Add_row_head",label = "Add", class="butt2") ),
             tags$head(tags$style(".butt4{background-color:#4d1566;} .butt4{color: #e6ebef;}")),
             div(style="display:inline-block;width:30%;text-align: center;",actionButton(inputId = "mod_row_head",label = "Edit", class="butt4") ),
             tags$head(tags$style(".butt3{background-color:#590b25;} .butt3{color: #e6ebef;}")),
             div(style="display:inline-block;width:30%;text-align: center;",actionButton(inputId = "Del_row_head",label = "Delete", class="butt3") ),
             HTML('</div>') ),
      
      column(12,dataTableOutput("Main_table_fish")),
      tags$script("$(document).on('click', '#Main_table_fish button', function () {
                  Shiny.onInputChange('lastClickId',this.id);
                  Shiny.onInputChange('lastClick', Math.random()) });")
      
    ) 
  })
  
  
  #### render DataTable part ####
  output$Main_table_fish<-renderDataTable({
    DT=vals_fish$Data
    datatable(DT,
              rownames=F, 
              selection = "single",escape=F,
              options = list(
                columnDefs = list(list(className = 'dt-center', targets = 2:(length(vals_fish$Data)-1))))
    )
  })
  
  observeEvent(input$Add_row_head, {
    ### This is the pop up board for input a new row
    showModal(modalDialog(title = "Add a new row",
                          textInput(paste0("Name_add", input$Add_row_head), "Fish Species"),
                          textInput(paste0("Count6_add", input$Add_row_head), "Count < 6in length"),
                          textInput(paste0("Count12_add", input$Add_row_head), "Count 6-12in length"),
                          textInput(paste0("Count18_add", input$Add_row_head), "Count 12-18in length"),  
                          textInput(paste0("Count19_add", input$Add_row_head), "Count > 18in length"),
                          modalButton("Cancel"),
                          actionButton("go", "Add row"),
                          easyClose = TRUE, footer = NULL ))
    
  })
  ### Add a new row to DT  - REORDER COLUMNS IF A PARTICULAR COUNT COLUMN NOT IN CURRENT DATA?
  observeEvent(input$go, {
    newLINE <- max(as.numeric(vals_fish$Data$LINE)) + 1
    numCol <- length(vals_fish$Data) - 17
    newEmptyCol <- setNames(data.frame(matrix(data='', ncol = numCol, nrow = 1)), names(vals_fish$Data)[18:length(names(vals_fish$Data))])

    new_row=data.table(unique(data.frame(vals_fish$Data[,1:9])),
                       LINE=as.character(newLINE),
      NAME_COM=toupper(as.character(input[[paste0("Name_add", input$Add_row_head)]])),
      COUNT_6=as.character(input[[paste0("Count6_add", input$Add_row_head)]]),
      COUNT_12=as.character(input[[paste0("Count12_add", input$Add_row_head)]]),
      COUNT_18=as.character(input[[paste0("Count18_add", input$Add_row_head)]]),
      COUNT_19=as.character(input[[paste0("Count19_add", input$Add_row_head)]]),
      newEmptyCol, 
      stringsAsFactors=F)
    vals_fish$Data<-rbind(vals_fish$Data,new_row,fill=TRUE) 
    removeModal()
  })
  
  ### delete selected rows part
  ### this is warning messge for deleting
  observeEvent(input$Del_row_head,{
    showModal(
      if(length(input$Main_table_fish_rows_selected)>=1 ){
        modalDialog(
          title = "Warning",
          paste("Are you sure you want to delete",length(input$Main_table_fish_rows_selected),"rows? Note that row will still exist but be blank for updating purposes." ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("ok", "Yes")
          ), easyClose = TRUE)
      }else{
        modalDialog(
          title = "Warning",
          paste("Please select row(s) that you want to delete!" ),easyClose = TRUE
        )
      }
      
    )
  })
  
  ### If user say OK, then delete the selected rows
  observeEvent(input$ok, {
    vals_fish$Data[input$Main_table_fish_rows_selected,11:ncol(vals_fish$Data)] <- lapply(vals_fish$Data[input$Main_table_fish_rows_selected,11:ncol(vals_fish$Data)],
                                                                                          function(x){x=' '}) 
    #vals_fish$Data=vals_fish$Data[-input$Main_table_fish_rows_selected,]
    removeModal()
  })
  
  
  ### edit button
  observeEvent(input$mod_row_head,{
    showModal(
      if(length(input$Main_table_fish_rows_selected)>=1 ){
        modalDialog(
          fluidPage(
            h3(strong("Modification"),align="center"),
            hr(),
            dataTableOutput('row_modif'),
            actionButton("save_changes","Save changes"),
            tags$script(HTML("$(document).on('click', '#save_changes', function () {
                             var list_value=[]
                             for (i = 0; i < $( '.new_input' ).length; i++)
                             {
                             list_value.push($( '.new_input' )[i].value)
                             }
                             Shiny.onInputChange('newValue', list_value) });")) ), size="l" )
  }else{
    modalDialog(
      title = "Warning",
      paste("Please select the row that you want to edit!" ),easyClose = TRUE
    )
  }
  
          )
    
    })
  
  
  
  
  #### modify part
  output$row_modif<-renderDataTable({
    selected_row=input$Main_table_fish_rows_selected
    old_row <- vals_fish$Data[selected_row,11:(length(vals_fish$Data))]
    row_change <- list()
    for(i in colnames(old_row))
    {
      if (is.numeric(vals_fish$Data[[i]]))
      {
        row_change[[i]]<-paste0('<input class="new_input" value= ','"',old_row[[i]],'"','  type="number" id=new_',i,' ><br>')
      } 
      else 
        row_change[[i]]<-paste0('<input class="new_input" value= ','"',old_row[[i]],'"',' type="textarea"  id=new_',i,'><br>')
    }
    
    row_change <- as.data.table(row_change)
    setnames(row_change,colnames(old_row))
    DT <- cbind(as.data.table(vals_fish$Data[selected_row,1:10]),row_change,stringsAsFactors=F)
    DT
  },escape=F,options=list(dom='t',ordering=F,scrollX = TRUE),selection="none")

  
  
  ### This is to replace the modified row to existing row
  observeEvent(input$newValue,
               {
                 newValue <- lapply(input$newValue,function(col){ ifelse(col=='', ' ',ifelse(col=='NA','',col))})
                 DF=as.data.table(data.frame(lapply(newValue, function(x) t(data.frame(x)))))
                 DF=data.table(vals_fish$Data[input$Main_table_fish_rows_selected,1:10],DF,stringsAsFactors=F)
                 colnames(DF)=colnames(vals_fish$Data)
                 vals_fish$Data[input$Main_table_fish_rows_selected]<-DF
                 
                 removeModal()
               }
  )
  
  
 
  
  output$downloadJSON <- downloadHandler(filename = function(){paste(str_extract(input$filenm$name,"[:alnum:]+\\_[:alpha:]+\\_[:alnum:]+\\_[:alnum:]\\_FISH"), ".json", sep="")},
                                         content = function(file) {
                                           updData.wide <- subset(vals_fish$Data, select=-PAGE) %>%
                                             # subset(NAME_COM!='' & !is.na(NAME_COM)) %>% # Do not need to explicitly remove these because we cannot completely remove data
                                             melt(id.vars=c('UID','SITE_ID','VISIT_NO','YEAR','STUDYNAME','APP_PLATFORM','APP_VERSION','SAMPLE_TYPE','LINE'),na.rm=TRUE) %>%
                                             subset(value!='' & value!='NA') %>%
                                             mutate(variable=paste(LINE,variable,sep='_'),value=ifelse(value %in% c(' '),'',value)) %>%
                                             mutate(value=ifelse(str_detect(variable,'NAME_COM|INTRODUCED|HYBRID'),toupper(value),value)) %>%
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