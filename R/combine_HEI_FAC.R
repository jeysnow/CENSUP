#' Combines faculty data at the HEI level and merges with HEI data
#'
#' @param CLEAN_HEI A data.table with clean HEI data, or a list of such,
#' or a path to a file containing clean HEI data, or vector of such,
#' or a path to a folder containing only files with clean HEI data
#' @param CLEAN_FAC A data.table with clean FAC data, or a list of such,
#' or a path to a file containing clean FAC data, or vector of such,
#' or a path to a folder containing only files with clean FAC data
#' @param OUTPUT_TO the path to a folder where the standardized file is to be written to.
#'  Defaults to "return", whereby the function simply returns and doesn't save the table
#' @param LANGUAGE the language in which the data should be standardized to. defaults to English
#' @param CITY_NAMES convert city codes to names, TRUE by default, but slower
#'
#' @returns if OUTPUT_TO = "RETURN", returns a data table or a list of data tables
#' @export
#'
#' @examples
#' \dontrun{combine_HEI_FAC()}
combine_HEI_FAC <- function(CLEAN_HEI,CLEAN_FAC,OUTPUT_TO = "RETURN",
                            LANGUAGE = "ENG", CITY_NAMES = TRUE) {
  #Check inputs----
  if(is.character(CLEAN_HEI)){
    clean_h <- check_read_from(CLEAN_HEI,"combine_HEI_FAC")
  }

  else if(!is.data.table(CLEAN_HEI)){ #if it's not a data table, it's potentially a list thereof
    for (dt in CLEAN_HEI) {
      if(check_clean(dt) %notin% c("HEI","IES"))
        stop(paste("combine_HEI_FAC received a list where not all elements are Clean_HEIs"))
    }
    clean_h <- CLEAN_HEI
  }
  else clean_h <- list(CLEAN_HEI)

  if(is.character(CLEAN_FAC))
    clean_f <- check_read_from(CLEAN_FAC,"combine_HEI_FAC")
  else if(!is.data.table(CLEAN_FAC)){#if it's not a data table, it's potentially a list thereof
    for (dt in CLEAN_FAC) {
      if(check_clean(dt) %notin% c("FAC","DOC"))
        stop(paste("combine_HEI_FAC received a list where not all elements are Clean_FACs"))
    }
    clean_f <- CLEAN_FAC
  }
  else clean_f <- list(CLEAN_FAC)

  check_supported_LANGUAGE(LANGUAGE)

  if(OUTPUT_TO != "RETURN"){
    if(!dir.exists(OUTPUT_TO))
      stop(paste("combie_HEI_FAC received a path that does not point to an existing directory: ",OUTPUT_TO))
  }



  #iterate thorugh itens----
  output <- list()

  for (h in clean_h) {

    if(is.character(h))
      h <- fread(h,encoding = "Latin-1",na.strings = "")

    #Matching HEI to FAC data----
    f <- matching_data(h,clean_f)

    aggregated <- f[,.(
      Fac_active = sum(Status=="Active"),
      Fac_fem = sum(Sex=="Female"),
      Fac_male = sum(Sex=="Male")
      ),HEI_Code]

    aggregated <- cbind(aggregated,disaggregate_FAC_education(
      f$Education, f$HEI_Code,RETURN_KEYS = F))

    aggregated[,Fac_full_time := f[,sum( Work_contract %in% c(
      "Full time - exclusive","Full time - not exclusive")),
      HEI_Code][,2]]
    aggregated <- cbind(aggregated,disaggregate_FAC_work(
      f$Work_contract, f$HEI_Code,RETURN_KEYS = F))

    aggregated <- cbind(aggregated,disaggregate_age(
      f$Age, f$HEI_Code,RETURN_KEYS = F))

    aggregated <- cbind(aggregated,disaggregate_race(
      f$Race, f$HEI_Code,RETURN_KEYS = F))

    aggregated[,Fac_Brazilian := f[,sum(Nationality=="Brazilian"),
      HEI_Code][,2]]

    aggregated[,Fac_disabilities := f[,sum(Disability),HEI_Code][,2]]


    # combine aggregated data to HEI data

    clean_table <- merge(h,aggregated,by.x= "Code",by.y = "HEI_Code",all.x = T)

    setcolorder(clean_table,neworder = references$HEI_FAC$names$ENG)

    setkey(clean_table,"Code")

    if(toupper(LANGUAGE)=="POR")
      setnames(clean_table,new = references$FAC$names$POR)


    if(OUTPUT_TO=="RETURN")
      output[[length(output)+1]] <- clean_table
    else
      write_table(clean_table, OUTPUT_TO, "Clean",c("HEI","FAC"),clean_table[1,1],LANGUAGE)
    }
  #output----
  if(OUTPUT_TO=="RETURN"){
    if(length(output)==1)
      return(output[[1]])
    else
      return(output)
  }
}


