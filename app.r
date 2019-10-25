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
  # shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),
  # actionButton("refresh", "Reset",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
  
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
  #,actionButton(inputId = "Updated_fish",label = "Save")
)

server = function(input, output, session){
  
  ### interactive dataset 
  vals_fish<-reactiveValues()
  
  observeEvent(input$filenm, {
    vals_fish$Data<-organizationShiny(input$filenm$datapath,
                                      input$filenm$name)
  })
  
  
  # observeEvent(input$refresh, {
  #   shinyjs::js$refresh()
  # })
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
             ### Optional: a html button 
             # HTML('<input type="submit" name="Add_row_head" value="Add">'),
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
              #editable = list(target="cell",disable = list(columns = c(1:9))),
              rownames=F, 
              selection = "single",escape=F) 
  })
  
  observeEvent(input$Add_row_head, {
    ### This is the pop up board for input a new row
    showModal(modalDialog(title = "Add a new row",
                          textInput(paste0("Name_add", input$Add_row_head), "Fish Species"),
                          textInput(paste0("Count6_add", input$Add_row_head), "Count < 6in length"),
                          textInput(paste0("Count12_add", input$Add_row_head), "Count 6-12in length"),
                          textInput(paste0("Count18_add", input$Add_row_head), "Count 12-18in length"),  
                          textInput(paste0("Count19_add", input$Add_row_head), "Count > 18in length"),
                          
                          actionButton("go", "Add row"),
                          easyClose = TRUE, footer = NULL ))
    
  })
  ### Add a new row to DT  
  observeEvent(input$go, {
    newLINE <- max(as.numeric(vals_fish$Data$LINE)) + 1
    new_row=data.table(unique(data.frame(vals_fish$Data[,1:9])),
                       LINE=as.character(newLINE),
      NAME_COM=toupper(as.character(input[[paste0("Name_add", input$Add_row_head)]])),
      COUNT_6=input[[paste0("Count6_add", input$Add_row_head)]],
      COUNT_12=input[[paste0("Count12_add", input$Add_row_head)]],
      COUNT_18=input[[paste0("Count18_add", input$Add_row_head)]],
      COUNT_19=input[[paste0("Count19_add", input$Add_row_head)]]
      ,stringsAsFactors=F)
    vals_fish$Data<-gtools::smartbind(vals_fish$Data,new_row)
    removeModal()
  })
  
  ### delete selected rows part
  ### this is warning messge for deleting
  observeEvent(input$Del_row_head,{
    showModal(
      if(length(input$Main_table_fish_rows_selected)>=1 ){
        modalDialog(
          title = "Warning",
          paste("Are you sure you want to delete",length(input$Main_table_fish_rows_selected),"rows?" ),
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
    vals_fish$Data=vals_fish$Data[-input$Main_table_fish_rows_selected,]
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
    old_row <- vals_fish$Data[selected_row]
    row_change <- list()
   # varList <- c('NAME_COM','INTRODUCED','HYBRID','COUNT_6','COUNT_12','COUNT_18','COUNT_19')
   # for (i in varList)
    for(i in colnames(old_row))
    {
      if (is.numeric(vals_fish$Data[[i]]))
      {
        row_change[[i]]<-paste0('<input class="new_input" value= ','"',old_row[[i]],'"','  type="number" id=new_',i,' ><br>')
      } 
      # else if( is.Date(vals_fish$Data[[i]])){
      #   row_change[[i]]<-paste0('<input class="new_input" value= ','"',old_row[[i]],'"',' type="date" id=new_  ',i,'  ><br>') 
      # }
      else 
        row_change[[i]]<-paste0('<input class="new_input" value= ','"',old_row[[i]],'"',' type="textarea"  id=new_',i,'><br>')
    }
   # row_change.full <- data.frame(old_row[,names(old_row) %nin% varList],row_change,stringsAsFactors=F)
    row_change <- as.data.table(row_change)
    setnames(row_change,colnames(old_row))
    # setnames(row_change,varList)
    # colUpd <- match(varList,names(old_row))
    # print(colUpd)
    # 
    # newVal <- cbind(as.data.table(old_row[,1:(min(colUpd)-1)]),row_change,as.data.table(old_row[,(max(colUpd)+1):ncol(old_row)]))
    # print(newVal)
    # 
    # DT <- newVal
    DT <- row_change
    DT 
  },escape=F,options=list(dom='t',ordering=F,scrollX = TRUE),selection="none")
#, editable=list(disable = list(columns = c(1:10))))
  
  
  
  ### This is to replace the modified row to existing row
  observeEvent(input$newValue,
               {
                 newValue <- input$newValue
                 DF=as.data.table(data.frame(lapply(newValue, function(x) t(data.frame(x)))))
                 colnames(DF)=colnames(vals_fish$Data)
                 vals_fish$Data[input$Main_table_fish_rows_selected]<-DF
                 
                 removeModal()
               }
  )
  
  
  # proxy = dataTableProxy('Main_table_fish')
  # 
  # observeEvent(input$Main_table_fish_cell_edit, {
  #   
  #   info = input$Main_table_fish_cell_edit
  #   
  #   str(info) 
  #   i = info$row 
  #   j = info$col + 1L
  #   v = info$value
  #   
  #   vals_fish$Data[i, j] <<- DT::coerceValue(v, vals_fish$Data[i, j]) 
  #   replaceData(proxy, vals_fish$Data, resetPaging = FALSE) # important
  #   
  #   
  # })
  
  
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