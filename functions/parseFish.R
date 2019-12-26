


organizationShiny <- function(filePath,fileName){
  
  print(fileName)

  fishIn.1 <- eFormsParseJSON(filePath) 

  fishIn.2 <- eFormsOrganize(fishIn.1) 
  
    return(fishIn.2)
  }
    



# karenWriteShiny <- function(filelist, finalList){
#   # Create the first part of the filename for writing to a .csv file, based on visit info and sample type
# #  subName.out <- str_extract(paste(path,filelist[1],sep='/'),"[:alnum:]+\\_[:alpha:]+\\_[:alnum:]+\\_[:alnum:]\\_")
#   subName.out <- str_extract(filelist[1],"[:alnum:]+\\_[:alpha:]+\\_[:alnum:]+\\_[:alnum:]\\_")
#   print(subName.out)
#   #if( fileFormat == '.xlsx'){
#     fishOut <- finalList[!(names(finalList) %in% specialCases)]
#       
#     return(c(map(others,1)))
# 
# }



