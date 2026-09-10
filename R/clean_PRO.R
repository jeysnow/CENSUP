
#' Read a csv file from CENSUP and create a standardized file for program level data
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
#' \dontrun{clean_PRO()}
clean_PRO <- function(READ_FROM,OUTPUT_TO = "RETURN",LANGUAGE = "ENG",CITY_NAMES = T){

  # checking inputs----
  path_files <-  check_read_from(READ_FROM,"clean_PRO")

  for (f in READ_FROM) { check_file(f,"clean_PRO")}

  check_supported_LANGUAGE(LANGUAGE)

  if(OUTPUT_TO != "RETURN"){
    if(!dir.exists(OUTPUT_TO))
      stop(paste("clean_PRO received a path that does not point to an existing directory: ",OUTPUT_TO))
  }


  # iterating through the files----

  output <- list()
  for (f in path_files) {


    #check file contents----

    raw_year <- as.numeric(substr(f,nchar(f)-7,nchar(f)-4))

    check_supported_year(raw_year,"clean_PRO")

    print(paste("reading file ",f))

    raw <- fread(f, encoding = "Latin-1",na.strings = "")

    # Standardize headers----
    clean_table <- convert_headers(raw,references$PRO$names,raw_year)
    rm(raw)
    gc()

    # standardizing variables----
    if(raw_year>=2010){
      if(CITY_NAMES==T)
        clean_table[,City := convert_city(City)]
      clean_table[,State := convert_state(State)]
    }

    clean_table[,Census_year := as.numeric(Census_year)]
    clean_table[,Census_year := raw_year]


    clean_table[,HEI_Academic_level := convert_academic_level(HEI_Academic_level,LANGUAGE) ]
    clean_table[,HEI_Administrative_type := convert_administrative_type(
      HEI_Administrative_type,raw_year,LANGUAGE)]


    clean_table[,Degree_type := convert_degree_type(Degree_type,LANGUAGE) ]
    clean_table[,Educational_mode := convert_ed_mode(Educational_mode,LANGUAGE) ]
    clean_table[,Workload_distance_perc := convert_workload_dist(Workload_distance_perc) ]
    clean_table[,Sequential := as.numeric(Sequential==2) ]
    clean_table[,Code_CINE := gsub("\"","",Code_CINE) ]


    #aggregate Plaaces and Applicants----
    if(raw_year <=2019){

      clean_table[,Places_day := aggregating_PRO_places_day(clean_table,RETURN_KEYS = F)]
      clean_table[,Places_new := aggregating_PRO_places_new(clean_table,RETURN_KEYS = F)]
      clean_table[,Places_selec_process := aggregating_PRO_places_selec(clean_table,RETURN_KEYS = F)]
      clean_table[,Places_remaining := aggregating_PRO_places_remain(clean_table,RETURN_KEYS = F)]
      clean_table[,Places_special := aggregating_PRO_places_special(clean_table,RETURN_KEYS = F)]
      clean_table[,Applicants_day := aggregating_PRO_applicants_day(clean_table,RETURN_KEYS = F)]
      clean_table[,Applicants_new := aggregating_PRO_applicants_new(clean_table,RETURN_KEYS = F)]
      clean_table[,Applicants_selec_process := aggregating_PRO_applicants_selec(clean_table,RETURN_KEYS = F)]
      clean_table[,Applicants_remaining := aggregating_PRO_applicants_remain(clean_table,RETURN_KEYS = F)]
      clean_table[,Applicants_special := aggregating_PRO_applicants_special(clean_table,RETURN_KEYS = F)]

      #although years 2017 ~2019 supposedelly have yearly totals, they are all empty
      clean_table[,Places_evening := aggregating_PRO_places_eve(clean_table,RETURN_KEYS = F)]
      clean_table[,Places_distance := aggregating_PRO_places_dist(clean_table,RETURN_KEYS = F)]
      clean_table[,Applicants_evening := aggregating_PRO_applicants_eve(clean_table,RETURN_KEYS = F)]
      clean_table[,Applicants_distance := aggregating_PRO_applicants_dist(clean_table,RETURN_KEYS = F)]

      clean_table[,Applicants_total := rowSums(clean_table[,c("Applicants_evening","Applicants_distance","Applicants_day")],na.rm = T)]
    }


    setkey(clean_table,"Code")

    clean_table <- convert_headers(clean_table,references$PRO$names,
                                   "Standard",LANGUAGE)



    if(OUTPUT_TO=="RETURN")
      output[[length(output)+1]] <- clean_table
    else write_table(clean_table,OUTPUT_TO, "Clean","PRO",clean_table[1,1],LANGUAGE)
  }
  #output----
  if(OUTPUT_TO=="RETURN"){
    if(length(output)==1)
      return(output[[1]])
    else
      return(output)
  }
}

