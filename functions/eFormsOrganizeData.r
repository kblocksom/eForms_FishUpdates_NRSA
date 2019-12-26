eFormsOrganize <- function(rawData){
  # Extract visit info
  visitinfo <- as.data.frame(rawData[1:7],stringsAsFactors=F)
  # Extract sample type from 8th element in each file
  sampletype <- names(rawData)[8]
  # PHAB sample types are special
  sampletype <- substring(sampletype,1,5)
   
  # Create data frame of parsed data to start with, making them all character variables 
  parsedData <- as.data.frame(rawData[8])
  parsedData[,names(parsedData)] <- lapply(parsedData[,names(parsedData)], as.character)
  
  # run parsed data through organizing function, based on sample type 
  rr <- organizeFish(parsedData)
  
  
  # bind with visit info and create list 
  # in process for later use
  ss <- as.data.table(cbind(visitinfo, rr))
  
  return(ss)
}

#############################################################################################################
# This begins the section which organizes the parsed data by sample type
organizeFish <- function(parsedIn){
 # pulls out and formats species by line number and sample type
  aa <- subset(parsedIn, select=str_detect(names(parsedIn), 'FISH\\.[:digit:]'))
  aa$PAGE <- '1'
  
  varLong <- names(aa)[names(aa) %nin% c('PAGE')]
  bb <- reshape(aa, idvar = c('PAGE'), direction = 'long', varying = varLong,
                times = varLong, v.names = 'RESULT', timevar = 'variable')
  bb$variable <- with(bb, gsub('FISH\\.','',variable))
  bb$LINE <- with(bb, str_extract(variable, '[:digit:]+'))
  bb$SAMPLE_TYPE <- with(bb, ifelse(str_detect(variable,'FTIS'),'FTIS',
                                    ifelse(str_detect(variable,'FPLG'),'FPLG','FISH')))
  bb$PARAMETER <- with(bb, ifelse(SAMPLE_TYPE %in% c('FTIS','FPLG'),str_replace(variable,'[:digit:]+\\_FTIS\\_|[:digit:]+\\_FPLG\\_', ''),
                                  str_replace(variable, '[:digit:]+\\_', '')))
  
  cc <- subset(bb, SAMPLE_TYPE == 'FISH', select = c('SAMPLE_TYPE','PAGE','LINE','PARAMETER','RESULT'))

  # cast into wide format for review
  cc.wide <- reshape(cc, idvar = c('SAMPLE_TYPE','PAGE','LINE'), direction = 'wide',
                     v.names = 'RESULT', timevar = 'PARAMETER')
  names(cc.wide) <- gsub('RESULT\\.', '', names(cc.wide))
  
  cc.wide <- cc.wide[order(cc.wide$PAGE, as.numeric(cc.wide$LINE)),]
  
  nameList <- data.frame(varName=c('SAMPLE_TYPE','PAGE','LINE','NAME_COM','INTRODUCED','HYBRID','COUNT_6','COUNT_12','COUNT_18','COUNT_19','ANOM_COUNT','MORT_CT','VOUCH_UNK','VOUCH_QA','VOUCH_PHOTO','VOUCH_NUM','SEQUENCE','TAG','FISH_COMMENT','PHOTO_COMMENT'), varOrder=seq(1,20),stringsAsFactors = F)
  
  subList <- subset(nameList, varName %in% names(cc.wide))
  subList <- subList[order(subList$varOrder),]

  cc.wide <- cc.wide[, subList$varName] 
  
  return(cc.wide)
}

