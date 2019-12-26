list.of.packages <- c("shiny","Hmisc","dplyr","shinyjs","shinythemes","shinyBS","RJSONIO","DT","stringr","jsonlite","shinyalert","gtools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages,function(x){library(x,character.only=TRUE)}) 

source('functions/eFormsParseJSON_basic.r')
source('functions/eFormsParseJSONtext.r') 
source('functions/eFormsOrganizeData.r')
source('functions/parseFish.R')
fishTaxa <- readRDS("data/fishtaxa.rds") %>%
  mutate(FINAL_NAME=as.factor(FINAL_NAME))
