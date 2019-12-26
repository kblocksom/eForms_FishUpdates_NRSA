


organizationShiny <- function(filePath,fileName){
  
  print(fileName)

  fishIn.1 <- eFormsParseJSON(filePath) 

  fishIn.2 <- eFormsOrganize(fishIn.1) 
  
    return(fishIn.2)
  }
  