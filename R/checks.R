check_file <- function(PATH,ORIGIN){
  if(!is.character(PATH))
    stop(paste(ORIGIN," received a path that's not a character string: ",PATH))
  if(!file.exists(PATH))
    stop(paste(ORIGIN," received a path to a inexisting file: ",PATH))
  if(!grepl('\\.csv',PATH,ignore.case = T))
    stop(paste(ORIGIN,"received a file that is not in csv format: ",PATH))
  if(is.na(as.numeric(substr(PATH,nchar(PATH)-7,nchar(PATH)-4))))
    stop(paste(ORIGIN,"received a file with a name that doesnt end in a year (e.g. 2009): ",PATH))

  return(TRUE)

}

check_supported_year <- function(YEAR,ORIGIN,YEAR_GAP = 2009:2023){
  if(YEAR %notin% YEAR_GAP)
    stop(paste(ORIGIN, " received a year that is not supported: ",YEAR))
}

check_supported_LANGUAGE <- function(LANGUAGE,ORIGIN){
  if(toupper(LANGUAGE) %notin% c("ENG","POR"))
    stop(paste("clean_HEI received a language that is not supported: ",LANGUAGE,". Currently supported are ENG and POR"))
}

check_read_from <- function(READ_FROM,ORIGIN){
  if(length(READ_FROM)==1){
    if(dir.exists(READ_FROM)){
      path_files <- list.files(READ_FROM,full.names = T)
      for (f in path_files) {check_file(f,ORIGIN)}
      return(path_files)
    }
    else if(check_file(READ_FROM,ORIGIN))
      return(READ_FROM)
  }
  else if(length(READ_FROM)<=1){
    for (f in READ_FROM) {check_file(f,ORIGIN)}
    return(READ_FROM)
  }
  else stop(paste(ORIGIN," received data to read from that nis neither a directory path, file path or vector of file paths: ",READ_FROM))
}

check_clean<- function(DATA, ORIGIN, LANGUAGE = "ENG" ){
  if(!is.data.table(DATA))
    stop(paste(ORIGIN, " received data that ins't in the data table format"))

  key <- paste0(names(DATA),collapse = "")

  #print(key)

  if(key %notin% references$clean_keys$KEY)
    stop(paste(
      ORIGIN, " received data whose headers don't match the clean pattern of this package: "),
      paste(names(DATA)))

  return(references$clean_keys[KEY == key,VALUE] |> as.character())

}
