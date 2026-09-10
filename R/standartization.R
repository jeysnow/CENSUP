#HEI----
convert_academic_level <- function(ACADEMIC_LEVEL, LANGUAGE = "ENG"){
  return(convert_with_array(
    ACADEMIC_LEVEL,
    REFERENCE_ENG = c(
      "University","University center","College",
      "Federal institute","Federal center"),

    REFERENCE_POR = c(
      "Universidade","Centro universitário","Faculdade",
      "IFET","CEFET"),
    LANGUAGE = LANGUAGE
    ))
}

convert_administrative_type <- function(ADM_TYPE,YEAR, LANGUAGE = "ENG"){
  return(convert_with_table(
    ADM_TYPE,
    COL = YEAR,
    TABLE = references$HEI$adm_type,
    LANGUAGE = LANGUAGE
  ))
}
#PRO----
convert_degree_type <- function(DEGREE_TYPE, LANGUAGE = "ENG"){
  return(convert_with_array(
    DEGREE_TYPE,
    REFERENCE_ENG = c(
      "Bachelor","License","Technology",
      "Bachelor and License"),

    REFERENCE_POR = c(
      "Bacharelado","Licenciatura","Tecnológico",
      "Bacharelado e Licenciatura"),
    LANGUAGE = LANGUAGE
  ))
}

convert_ed_mode <- function(EDUCATIONAL_MODE,LANGUAGE="ENG"){
  if(check_NA_array(EDUCATIONAL_MODE)==T)
    return(EDUCATIONAL_MODE)

  if(LANGUAGE=="POR")
    return(convert_binary(EDUCATIONAL_MODE,"Presencial","EaD"))
  else return(convert_binary(EDUCATIONAL_MODE,"Classroom","Distance"))
}

convert_workload_dist <- function(WORKLOAD_DIST){
  if(check_NA_array(WORKLOAD_DIST)==T)
    return(WORKLOAD_DIST)

  for (i in 1:length(WORKLOAD_DIST)) {
    if(!is.na(WORKLOAD_DIST[i]))
      if(WORKLOAD_DIST[i]<1)
        WORKLOAD_DIST[i] <- WORKLOAD_DIST[i]*100
  }
  return(WORKLOAD_DIST)
}


#geographic----
convert_region<- function(REGION, LANGUAGE = "ENG"){
  if(check_NA_array(REGION)==T)
    return(REGION)

  if(is.numeric(REGION)){
    return(convert_with_array(
      REGION,
      REFERENCE_POR = c("Norte", "Nordeste", "Sudeste","Sul", "Centro-oeste"),
      REFERENCE_ENG = c("North","North-East",  "South-East","South", "Center-West"),
      LANGUAGE = LANGUAGE
    ))
  }
  else
    return(convert_with_dictionary(
      toupper(REGION),
      KEYS = c("NORTE","NORDESTE","SUDESTE","SUL","CENTRO-OESTE","CENTRO_OESTE"),
      REFERENCE_POR = c("Norte", "Nordeste", "Sudeste","Sul", "Centro-oeste","Centro-oeste"),
      REFERENCE_ENG = c("North","North-East",  "South-East","South", "Center-West","Center-West"),
      LANGUAGE = LANGUAGE
    ))
}

convert_city <- function(CITY_CODES){
  return(convert_with_dictionary(
    CITY_CODES,
    KEYS =  references$geographic$city$City_code,
    REFERENCE_POR =  references$geographic$city$City_name,
    REFERENCE_ENG =  references$geographic$city$City_name
  ))
}

convert_state <- function(STATE_CODES) {
  if(check_NA_array(STATE_CODES)==T)
    return(STATE_CODES)
  if(is.numeric(STATE_CODES))
    return(convert_with_dictionary(
      STATE_CODES,
      KEYS =  references$geographic$state$Code,
      REFERENCE_POR =  references$geographic$state$State,
      REFERENCE_ENG =  references$geographic$state$State
    ))
  else if(nchar(STATE_CODES[1])==2){
    return(convert_with_dictionary(
      STATE_CODES,
      KEYS =  references$geographic$state$Acronym,
      REFERENCE_POR =  references$geographic$state$State,
      REFERENCE_ENG =  references$geographic$state$State
    ))
  }
  else stop(paste("convert_state received unrecognized input: ",paste(STATE_CODES)))
}


#individual----

convert_race <- function(RACE,YEAR,LANGUAGE="ENG"){
  return(convert_with_table(
    RACE,
    COL = YEAR,
    TABLE = references$individual$race,
    LANGUAGE = LANGUAGE
  ))
}

convert_sex <- function(SEX,LANGUAGE="ENG"){
  if(check_NA_array(SEX)==T)
    return(SEX)
  if(is.character(SEX)==TRUE)
    SEX <- SEX == "FEMININO"

  if(LANGUAGE=="POR")
    return(convert_binary(SEX,"Feminino","Masculino"))
  else return(convert_binary(SEX,"Female","Male"))
}

Convert_nationality  <- function(NATIONALITY,LANGUAGE="ENG"){
  return(convert_with_array(
    NATIONALITY,
    REFERENCE_POR = c("Brasileira","Brasileira - nascido no exterior", "Estrangeira"),
    REFERENCE_ENG = c("Brazilian","Brazilian - born abroad", "Foreign"),
    LANGUAGE = LANGUAGE
  ))

}
#FAC----
convert_faculty_status <- function(FACULTY_STATUS,YEAR,LANGUAGE="ENG"){
  return(convert_with_table(
    FACULTY_STATUS,
    COL = YEAR,
    TABLE = references$FAC$status,
    LANGUAGE = LANGUAGE
  ))
}

convert_faculty_education <- function(FACULTY_EDUCATION,LANGUAGE="ENG"){
  return(convert_with_array(
    FACULTY_EDUCATION,
    REFERENCE_POR = c("Médio", "Graduação", "Especialização","Mestrado", "Doutorado"),
    REFERENCE_ENG = c("Highschool", "Bachelor", "Specialization","Master", "PhD"),
    LANGUAGE = LANGUAGE
  ))
}

convert_faculty_work_contract <- function(WORK_CONTRACT,LANGUAGE="ENG"){
  return(convert_with_array(
    WORK_CONTRACT,
    REFERENCE_POR = c("Integral - exclusivo","Integral - não exclusivo","Parcial","Horista"),
    REFERENCE_ENG = c("Full time - exclusive", "Full time - not exclusive",
                      "Part-time","Hourly"),
    LANGUAGE = LANGUAGE
  ))
}

#generic convertors----

convert_binary <- function(ARRAY,VALUE_TRUE,VALUE_FALSE,KEY_TRUE = 1){
  if(check_NA_array(ARRAY)==T)
    return(ARRAY)


  ARRAY <- as.numeric(ARRAY)

  ARRAY <- ARRAY==KEY_TRUE
  for (i in 1:length(ARRAY)) {
    if(ARRAY[i]==TRUE) ARRAY[i] <- VALUE_TRUE
    else ARRAY[i] <- VALUE_FALSE
  }
  return(ARRAY)
}

convert_with_array <- function(ARRAY,REFERENCE_ENG,REFERENCE_POR, LANGUAGE="ENG"){
  if(check_NA_array(ARRAY)==T)
    return(ARRAY)

  if(!is.numeric(ARRAY))
    stop("convert_with_array  received data that was not numeric")

  if(toupper(LANGUAGE)=="POR")
    labs <- REFERENCE_POR
  else labs <- REFERENCE_ENG

  for (i in 1:length(ARRAY)) {
    ARRAY[i] <- labs[as.numeric(ARRAY[i])]

  }
  return(ARRAY)
}

convert_with_dictionary <- function(ARRAY,KEYS, REFERENCE_ENG,REFERENCE_POR, LANGUAGE="ENG"){
  if(check_NA_array(ARRAY)==T)
    return(ARRAY)

  if(typeof(ARRAY)!=typeof(KEYS))
    stop("convert_with_dictionary received data that didn't match the type of keys")

  if(toupper(LANGUAGE)=="POR")
    labs <- REFERENCE_POR
  else labs <- REFERENCE_ENG

  for (i in 1:length(ARRAY)) {
    ARRAY[i] <- labs[match(ARRAY[i],KEYS)]

  }
  return(ARRAY)
}

convert_with_table <- function(ARRAY,TABLE,COL, LANGUAGE="ENG"){
  if(check_NA_array(ARRAY)==T)
    return(ARRAY)

  if(!is.data.table(TABLE))
    WARNING("convert_with_table received a table that was not in data table format")

  if(LANGUAGE %notin% names(TABLE))
    stop(paste(LANGUAGE," is not an header on the provided table"))

  if(COL %notin% names(TABLE))
    stop(paste(COL," is not an header on the provided table"))

  if(typeof(ARRAY) != typeof(TABLE[[as.character(COL)]]))
    stop("convert_with_table received data that didn't match the type of keys")

  for (i in 1:length(ARRAY)) {
    if(is.na(ARRAY[i]))
      next

    ARRAY[i] <- TABLE[eval(as.name((COL)))==ARRAY[i],
                      eval(as.name(LANGUAGE))]
  }
  return(ARRAY)
}

convert_headers <- function(TABLE_RAW,TABLE_NAMES,FROM,TO = "Standard"){

  raw_names <- TABLE_NAMES[[ as.character(FROM)]]

  clean_names <- TABLE_NAMES[[ as.character(TO)]]



  #makes sure the end table has all the col names from TO
  clean_table <- data.table(matrix(nrow = 0, ncol = length(raw_names)))

  setnames(clean_table,new = raw_names)

  clean_table <- rbind(
    fill = T,
    clean_table,
    TABLE_RAW[,.SD,.SDcols = setdiff(names(clean_table),c(""))]
  )
  setnames(clean_table,new = clean_names)

  #remove blank col names from end table
  clean_table <- clean_table[,.SD,.SDcols = setdiff(names(clean_table),c(""))]


  return(clean_table)
}
