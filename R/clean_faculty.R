
#' Read a csv file from CENSUP and create a standardized file for faculty level data
#'
#' @param READ_FROM the path to a csv file or a folder containing csv files
#' @param OUTPUT_TO the path to a folder where the standardized file is to be written to.
#'  Defaults to "return", whereby the function simply returns and doesn't save the table
#' @param LANGUAGE the language in which the data should be standardized to. defaults to English
#' @param CITY_NAMES convert city codes to names, TRUE by default, but slower
#'
#' @returns if OUTPUT_TO = "RETURN", returns a data table or a list of data tables
#' @export
#'
#' @examples
#' \dontrun{clean_FAC()}
clean_FAC <- function(READ_FROM,OUTPUT_TO = "RETURN",LANGUAGE = "ENG",CITY_NAMES = T){

  # checking inputs
  path_files <-  check_read_from(READ_FROM,"clean_FAC")

  for (f in READ_FROM) { check_file(f,"clean_FAC")}

  check_supported_LANGUAGE(LANGUAGE)

  if(OUTPUT_TO != "RETURN"){
    if(!dir.exists(OUTPUT_TO))
      stop(paste("clean_FAC received a path that does not point to an existing directory: ",OUTPUT_TO))
  }


  # iterating through the files----

  output <- list()
  for (f in path_files) {


    #check file contents

    raw_year <- as.numeric(substr(f,nchar(f)-7,nchar(f)-4))

    check_supported_year(raw_year,"clean_FAC")

    print(paste("reading file ",f))

    raw <- fread(f, encoding = "Latin-1",na.strings = "")

    # Standardize headers----
    clean_table <- convert_headers(raw,references$FAC$names,raw_year)
    rm(raw)
    gc()

    # standardizing variables----
    if(raw_year>=2010){
      if(CITY_NAMES==T)
        clean_table[,Birth_city := convert_city(Birth_city)]
      clean_table[,Birth_state := convert_state(Birth_state)]
    }


    clean_table[,Census_year := as.numeric(Census_year)]
    clean_table[,Census_year := raw_year]

    clean_table[,HEI_Academic_level := convert_academic_level(HEI_Academic_level,LANGUAGE) ]
    clean_table[,HEI_Administrative_type := convert_administrative_type(
      HEI_Administrative_type,raw_year,LANGUAGE)]

    clean_table[,Nationality := Convert_nationality(Nationality,LANGUAGE)]


    clean_table[,Education:=convert_faculty_education(Education,LANGUAGE)]
    clean_table[,Race:=convert_race(Race,raw_year, LANGUAGE)]
    clean_table[,Sex:=convert_sex(Sex,LANGUAGE)]
    clean_table[,Work_contract:=convert_faculty_work_contract(Work_contract,LANGUAGE)]
    clean_table[,Status := convert_faculty_status(Status,raw_year,LANGUAGE)]


    # Convert vars to booleans
    for (c in names(clean_table)[17:36]) {
      if(raw_year<= 2009 & c %in% c("Work_research","Visiting"))
        next

      clean_table[,paste0(c) := .SD == 1,.SDcols = c]
      clean_table[,paste0(c) := replace(.SD,is.na(.SD),FALSE) ,.SDcols = c]
      clean_table[,paste0(c) := lapply(.SD,as.numeric) ,.SDcols = c]
    }


    setkey(clean_table,"Code_contract")


    clean_table <- convert_headers(clean_table,references$FAC$names,
                                   "Standard",LANGUAGE)


    if(OUTPUT_TO=="RETURN")
      output[[length(output)+1]] <- clean_table
    else write_table(clean_table,OUTPUT_TO, "Clean","FAC",clean_table[1,1],LANGUAGE)
  }
  #output----
  if(OUTPUT_TO=="RETURN"){
    if(length(output)==1)
      return(output[[1]])
    else
      return(output)
  }
}

