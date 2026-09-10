comapre_DT <- function(DT_A,DT_B) {
  if(!is.data.table(DT_A)){

  }
    stop(paste(DT_A," is not in data table format"))
  if(!is.data.table(DT_B))
    stop(paste(DT_B," is not in data table format"))

  if(!setequal(names(DT_A),names(DT_B)))
    return(FALSE)
  for (c in names) {

  }
}

matching_data <- function(DT_OR_PATH,LIST_DT_OR_PATH){
  #Matching HEI to FAC data----
  if(is.character(DT_OR_PATH))
    DT_OR_PATH <- fread(h,na.strings = "")
  year <- as.numeric(DT_OR_PATH[1,1])
  output <- "NO_MATCH"
  for (f in LIST_DT_OR_PATH) {
    if(is.character(LIST_DT_OR_PATH)){
      #reading just the first row to check if it's the correct file
      if(as.numeric(substr(f,nchar(f)-7,nchar(f)-4))==year){
         return( fread(f,na.strings = ""))
    }}
    else if(f[1,1]==year)
      return(f)
  }
  return(output)
}

write_table <- function(CLEAN_TABLE, FOLDER,OPERATION = "Clean",LEVELS = c("HEI","PRO","FAC","STD"),YEAR, LANGUAGE){
  if(LANGUAGE=="POR"){
    LEVELS <- convert_with_array(LEVELS,c("HEI","PRO","FAC","STD"),c("IES","PRO","DOC","ALN"),LANGUAGE)
    fwrite(CLEAN_TABLE,paste0(FOLDER,OPERATION,"_",paste(LEVELS,collapse = "_"),"_",YEAR,".csv"),
           dec = ",",sep = ";")
  }
  else
    fwrite(CLEAN_TABLE,paste0(FOLDER,OPERATION,"_",paste(LEVELS,collapse = "_"),"_",YEAR,".csv"),
              encoding = "UTF-8", dec = ".",sep = ",")

  print(paste("File ",paste0(OPERATION,"_",paste(LEVELS,collapse = "_"),"_",YEAR,"created in ",FOLDER)))
}


check_NA_array <- function(ARRAY){
  ARRAY <- unique(is.na(ARRAY)==TRUE)
  if(length(ARRAY)==1)
    if(ARRAY==TRUE)
      return(TRUE)
  return(FALSE)
}
