#' Title
#' @param READ_FROM the path to a csv file or a folder containing csv files for HEI data
#' @param OUTPUT_TO the path to a folder where the standardized file is to be written to.
#'  Defaults to "return", whereby the function simply returns and doesn't save the table
#' @param READ_FROM_FAC the path to a csv file or a folder containing csv files for faculty data
#' @param LANGUAGE the language in which the data should be standardized to. defaults to English
#' @param CITY_NAMES convert city codes to names, TRUE by default, but slower
#'
#' @returns if OUTPUT_TO = "RETURN", returns a data table or a list of data tables
#' @export
#'
#' @examples
#' \dontrun{clean_HEI_FAC()}
clean_HEI_FAC <- function(READ_FROM_HEI,OUTPUT_TO = "RETURN",
                          READ_FROM_FAC = "NONE", LANGUAGE = "ENG", CITY_NAMES = TRUE){

  # Check inputs----
  HEI_files <- check_read_from(READ_FROM_HEI,"READ_FROM_HEI")
  if(READ_FROM_FAC!="NONE")
    FAC_files <- check_read_from(READ_FROM_FAC,"READ_FROM_HEI")
  else FAC_files <- "NONE"

  check_supported_LANGUAGE(LANGUAGE)

  if(OUTPUT_TO != "RETURN"){
    if(!dir.exists(OUTPUT_TO))
      stop(paste("clean_HEI received a path that does not point to an existing directory: ",OUTPUT_TO))
  }

  #iterating through inputs----

  output <- list()
  for (h in HEI_files) {
    #Check file contents

    raw_year <- as.numeric(substr(h,nchar(h)-7,nchar(h)-4))
    check_supported_year(raw_year,"Clean_HEI_FAC")

    if(raw_year %in% 2009:2019 & FAC_files == "NONE")
      warning(paste("clean_HEI_FAC received data for census year ",raw_year,
                    ", but not data to READ_FROM_FAC, this will results in NAs
                    to faculty variables"))

    # Standardize headers----
    clean_h <- clean_HEI(h,LANGUAGE=LANGUAGE,CITY_NAMES=CITY_NAMES)
    clean_table <- 0
    gc()


    if(FAC_files!="NONE"){
      f <- matching_data(h,FAC_files)

      if(f != "NO_MATCH"){
        clean_table <- combine_HEI_FAC(
          CLEAN_HEI = , clean_h,
          CLEAN_FAC = clean_FAC(
            READ_FROM = f,
            LANGUAGE= LANGUAGE,
            CITY_NAMES=CITY_NAMES),

          LANGUAGE= LANGUAGE,
          CITY_NAMES=CITY_NAMES)
      }
      else{
        clean_table <- cbind(
          clean_h,
          #convert the remaining cols from HEI_FAC, keeping the original data, if any
          convert_headers(
            TABLE_RAW = fread(h, encoding = "Latin-1",na.strings = ""),
            TABLE_NAMES = references$HEI_FAC$names[
              ENG %notin% references$HEI$names$ENG],
            FROM = raw_year)
        )
      }
    }
    else{
      clean_table <- cbind(
        clean_h,
        #convert the remaining cols from HEI_FAC, keeping the original data, if any
        convert_headers(
          TABLE_RAW = fread(h, encoding = "Latin-1",na.strings = ""),
          TABLE_NAMES = references$HEI_FAC$names[
            ENG %notin% references$HEI$names$ENG],
          FROM = raw_year)
      )
    }
    #standardize variables----

    #no standardization necessary for values

    setkey(clean_table,"Code")

    clean_table <- convert_headers(clean_table,references$HEI_FAC$names,
                                   "Standard",LANGUAGE)


    if(OUTPUT_TO=="RETURN")
      output[[length(output)+1]] <- clean_table
    else
      write_table(clean_table, OUTPUT_TO, "Clean",c("HEI","FAC"),raw_year,LANGUAGE)
  }

  # if necessary, return values
  if(OUTPUT_TO=="RETURN"){
    if(length(output)==1)
      return(output[[1]])
    else
      return(output)
  }


}
