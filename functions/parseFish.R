


organizationShiny <- function(filePath,fileName){
  
  print(fileName)

  fishIn.1 <- eFormsParseJSON(filePath) 

  fishIn.2 <- eFormsOrganize(fishIn.1) 
  
    return(fishIn.2)
  }
    

# Need to convert back to JSON file after editing is done
# deparseData <- function(updData){
#   # First need to melt updData back to original format
#   updData <- subset(updData, select = -PAGE)
#   
#   varLong <- names(updData)[names(updData) %nin% c('UID','SITE_ID','VISIT_NO','YEAR','STUDYNAME','APP_PLATFORM','APP_VERSION','SAMPLE_TYPE','LINE')]
#   updData.long <- reshape(updData, idvar = c('UID','SITE_ID','VISIT_NO','YEAR','STUDYNAME','APP_PLATFORM','APP_VERSION','SAMPLE_TYPE','LINE'),
#                           direction = 'long', varying = varLong, times = varLong, v.names = 'value', timevar = 'variable')
#   updData.long <- subset(updData.long, value != '' & !is.na(value))
#   updData.long$variable <- with(updData.long, paste(LINE, variable, sep = '_'))
#   updData.long <- subset(updData.long, select = -LINE)
#   
#   updData.wide <- reshape(updData.long, idvar = c('UID','SITE_ID','VISIT_NO','YEAR','STUDYNAME','APP_PLATFORM','APP_VERSION','SAMPLE_TYPE'),
#                           direction = 'wide', timevar = 'variable', v.names = 'value')
#   names(updData.wide) <- gsub('value\\.', '', names(updData.wide))
#   updData.wide <- subset(updData.wide, select = -SAMPLE_TYPE)
#   
#   
#   # updData.wide <- subset(updData, select=-PAGE) %>%
#   #   melt(id.vars=c('UID','SITE_ID','VISIT_NO','YEAR','STUDYNAME','APP_PLATFORM','APP_VERSION','SAMPLE_TYPE','LINE'),na.rm=TRUE) %>%
#   #   subset(value!='') %>%
#   #   mutate(variable=paste(LINE,variable,sep='_')) %>%
#   #   subset(select=-LINE) %>%
#   #   dcast(UID+SITE_ID+VISIT_NO+YEAR+STUDYNAME+APP_PLATFORM+APP_VERSION+SAMPLE_TYPE~variable, value.var='value') %>%
#   #   subset(select=-SAMPLE_TYPE)
#   
#   formatData <- list(UID=unique(updData.wide$UID),SITE_ID=unique(updData.wide$SITE_ID),
#                      VISIT_NO=unique(updData.wide$VISIT_NO),YEAR=unique(updData.wide$YEAR),
#                      STUDYNAME=unique(updData.wide$STUDYNAME),APP_PLATFORM=unique(updData.wide$APP_PLATFORM),
#                      APP_VERSION=unique(updData.wide$APP_VERSION),FISH=unbox(updData.wide[,8:length(updData.wide)]))
#   
#   jsonData <- jsonlite::toJSON(formatData,auto_unbox=TRUE,prettify=TRUE) 
#     
#   
# }

# Need this at all??
karenWriteShiny <- function(filelist, finalList){
  # Create the first part of the filename for writing to a .csv file, based on visit info and sample type
#  subName.out <- str_extract(paste(path,filelist[1],sep='/'),"[:alnum:]+\\_[:alpha:]+\\_[:alnum:]+\\_[:alnum:]\\_")
  subName.out <- str_extract(filelist[1],"[:alnum:]+\\_[:alpha:]+\\_[:alnum:]+\\_[:alnum:]\\_")
  print(subName.out)
  #if( fileFormat == '.xlsx'){
    fishOut <- finalList[!(names(finalList) %in% specialCases)]
      
    return(c(map(others,1)))

}


