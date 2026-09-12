#' @include global.R
#' @include checks.R
#' @include standartization.R

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
#
matching_data <- function(DT_OR_PATH,LIST_DT_OR_PATH,RETURN_DT = TRUE){
  #Matching HEI to FAC data----
  if(is.character(DT_OR_PATH))
    year <- as.numeric(substr(DT_OR_PATH,nchar(DT_OR_PATH)-7,nchar(DT_OR_PATH)-4))
  else if(is.data.table(DT_OR_PATH))
    year <- as.numeric(DT_OR_PATH[1,1])
  else stop(paste("matching data received an input that was neither a data table of a path: ",DT_OR_PATH))
  output <- "NO_MATCH"
  for (f in LIST_DT_OR_PATH) {
    if(is.character(f)){
      if(as.numeric(substr(f,nchar(f)-7,nchar(f)-4))==year){
        output <- f
    }}
    else if(f[1,1]==year)
      output <- f
  }
  if(RETURN_DT == FALSE){
    if(is.character(output))
      return(output)
    stop("matching_data received DT_OR_PATH that's not of type character, but RETURN_DT is FALSE")
  }
  else{
    if(is.data.table(output))
      return(output)
    else return(fread(output,na.strings = ""))
  }
}

write_table <- function(CLEAN_TABLE, FOLDER,OPERATION = "Clean",LEVELS = c("HEI","PRO","FAC","STD"),YEAR, LANGUAGE){
  if(LANGUAGE=="POR"){
    LEVELS <- convert_with_array(LEVELS,c("HEI","PRO","FAC","STD"),c("IES","PRO","DOC","ALN"),LANGUAGE)
    fwrite(CLEAN_TABLE,paste0(FOLDER,OPERATION,"_",paste(LEVELS,collapse = "_"),"_",YEAR,".csv"),
           dec = ",",sep = ";")
  }
  else
    fwrite(CLEAN_TABLE,paste0(FOLDER,"/",OPERATION,"_",paste(LEVELS,collapse = "_"),"_",YEAR,".csv"),
              encoding = "UTF-8", dec = ".",sep = ",")

  print(paste("File ",paste0(OPERATION,"_",paste(LEVELS,collapse = "_"),"_",YEAR," created in ",FOLDER)))
}

extract_year <- function(FILE_PATH){
  return(as.numeric(substr(FILE_PATH,nchar(FILE_PATH)-7,nchar(FILE_PATH)-4)))
}

