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
  # Start by separating data that describe samples and those that describe species
  # aa pulls out sample information by SAMPLE_TYPE and sets LINE=0
  # aa <- subset(parsedIn, select=str_detect(names(parsedIn), 'FISH\\.[:alpha:]')) %>%
  #   mutate(PAGE='1', LINE='0') %>%
  #   melt(id.vars=c('PAGE','LINE'), variable.name='PARAMETER', value.name='RESULT') %>%
  #   mutate(SAMPLE_TYPE=substring(PARAMETER,6,9), 
  #          PARAMETER=gsub('FISH\\.VERT\\_|FISH\\.FTIS\\_|FISH\\.FPLG\\_|FISH\\.FISH_', '', PARAMETER)) %>%
  #   select(SAMPLE_TYPE, PAGE, LINE, PARAMETER, RESULT) %>%
  #   filter(SAMPLE_TYPE=='FISH')
  # bb pulls out and formats species by line number and sample type
  bb <- subset(parsedIn, select=str_detect(names(parsedIn), 'FISH\\.[:digit:]')) %>%
    mutate(PAGE='1') %>%
    melt(id.vars='PAGE',value.name='RESULT') %>%
    mutate(variable = gsub('FISH\\.','',variable),
           LINE=str_extract(variable, '[:digit:]+'),
           SAMPLE_TYPE=ifelse(str_detect(variable,'FTIS'),'FTIS',
                              ifelse(str_detect(variable,'FPLG'),'FPLG','FISH'))) %>%
    mutate(PARAMETER=ifelse(SAMPLE_TYPE %in% c('FTIS','FPLG'),str_replace(variable,'[:digit:]+\\_FTIS\\_|[:digit:]+\\_FPLG\\_', ''),
                            str_replace(variable, '[:digit:]+\\_', ''))) %>%
    select(SAMPLE_TYPE, PAGE, LINE, PARAMETER, RESULT) %>%
    filter(SAMPLE_TYPE=='FISH')
  # stack aa and bb on top of one another
  cc <- bb
  # cast into wide format for review
  cc.wide <- dcast(cc,SAMPLE_TYPE+PAGE+LINE~PARAMETER,value.var='RESULT') %>%
    arrange(PAGE,as.numeric(LINE))
  
  nameList <- data.frame(varName=c('SAMPLE_TYPE','PAGE','LINE','NAME_COM','INTRODUCED','HYBRID','COUNT_6','COUNT_12','COUNT_18','COUNT_19','ANOM_COUNT','MORT_CT','VOUCH_UNK','VOUCH_QA','VOUCH_PHOTO','VOUCH_NUM','SEQUENCE','TAG','FISH_COMMENT','PHOTO_COMMENT'), varOrder=seq(1,20),stringsAsFactors = F)
  
  subList <- filter(nameList,varName %in% names(cc.wide)) %>%
    arrange(varOrder)
  
  cc.wide <- cc.wide[,subList$varName] 
  
  return(cc.wide)
}

