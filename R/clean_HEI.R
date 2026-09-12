#' @include global.R
#' @include checks.R
#' @include standartization.R
#' @include utils.R
#' @include agregation.R

#' Read csv files from CENSUP and create a standardized file for HEI level data
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
#' \dontrun{clean_HEI()}
clean_HEI <- function(READ_FROM,OUTPUT_TO = "RETURN",LANGUAGE = "ENG", CITY_NAMES = TRUE){


  # checking inputs----
  path_files <- check_read_from(READ_FROM,"clean_HEI")

  check_supported_LANGUAGE(LANGUAGE)

  if(OUTPUT_TO != "RETURN"){
    if(!dir.exists(OUTPUT_TO))
      stop(paste("clean_HEI received a path that does not point to an existing directory: ",OUTPUT_TO))
  }



  #iterating through inputs----

  output <- list()
  for (f in path_files) {

    #Check file contents

    raw_year <- as.numeric(substr(f,nchar(f)-7,nchar(f)-4))
    check_supported_year(raw_year,"Clean_HEI")
    print(paste("reading file ",f))
    raw <- fread(f, encoding = "Latin-1",na.strings = "")

    # Standardize headers----
    clean_table <- convert_headers(raw,references$HEI$names,raw_year)
    rm(raw)
    gc()

    # standardizing variables----
    if(CITY_NAMES==T) clean_table[,City := convert_city(City)]

    if(raw_year<=2009){
      clean_table[,Confessional := FALSE]
      clean_table[,Communitary := FALSE]

      clean_table[Administrative_type=="PARTICULAR CONFESSIONAL" | Administrative_type==5,Confessional:= TRUE]
      clean_table[Administrative_type=="PARTICULAR COMUNITARIA" | Administrative_type==6,Communitary := TRUE]
      clean_table[,Confessional := as.numeric(Confessional)]
      clean_table[,Communitary := as.numeric(Communitary)]
    }

    clean_table[,Census_year := as.numeric(Census_year)]
    clean_table[,Census_year := raw_year]

    clean_table[,Academic_level := convert_academic_level(Academic_level, LANGUAGE) ]
    clean_table[,Administrative_type := convert_administrative_type(
      Administrative_type,raw_year, LANGUAGE)]

    clean_table[,Region := convert_region(Region,LANGUAGE)]
    clean_table[,State := convert_state(State)]


    setkey(clean_table,"Code")

    clean_table <- convert_headers(clean_table,references$HEI$names,
                                   "Standard",LANGUAGE)


    if(OUTPUT_TO=="RETURN")
      output[[length(output)+1]] <- clean_table
    else
      write_table(clean_table,OUTPUT_TO, "Clean","HEI",clean_table[1,1],LANGUAGE)
  }
  # if necessary, return values
  if(OUTPUT_TO=="RETURN"){
    if(length(output)==1)
      return(output[[1]])
    else
      return(output)
  }
}

