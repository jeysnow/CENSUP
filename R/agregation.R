#individual----
disaggregate_age <- function(
    AGES, GROUP_BY_VALUES, GROUP_BY_NAME = "HEI_Code",
    UPPER_LIMITS = c(29,34,39,44,49,54,59,200),
    COL_NAMES = c("Fac_age_0-29","Fac_age_30-34","Fac_age_35-39","Fac_age_40-44",
                  "Fac_age_45-49","Fac_age_50-54","Fac_age_55-59","Fac_age_60+"),
    RETURN_KEYS = TRUE){
  if(length(UPPER_LIMITS)!=length(COL_NAMES))
    warning(paste("aggregate_age received missmatching upper limits and columns,
                  which will generate NAs. limits: ",paste(UPPER_LIMITS,collapse = ","),
                ", Columns: ",paste(COL_NAMES,collapse = ",")))

  keys <- unique(GROUP_BY_VALUES)
  clean_table <- data.table(matrix(nrow = length(keys), ncol = length(COL_NAMES)+1))
  setnames(clean_table,new = c(GROUP_BY_NAME, COL_NAMES))
  set(clean_table, j = GROUP_BY_NAME,value = as.numeric(clean_table[[GROUP_BY_NAME]]))
  set(clean_table, j = GROUP_BY_NAME,value = keys)

  ref_table <- data.table(ID = GROUP_BY_VALUES,data = AGES)

  # add 0 and start from the second to calculate lower limits
  UPPER_LIMITS <- c(0,UPPER_LIMITS)
  for (col in 1:length(COL_NAMES)) {

    set(clean_table, j = COL_NAMES[col],value = as.numeric(clean_table[[COL_NAMES[col]]]))
    for (row in 1:length(keys)) {
      set(clean_table,i= row, j= COL_NAMES[col],
          value = ref_table[ID == keys[row] ,.(value = sum(
            data <= UPPER_LIMITS[col+1] & data > UPPER_LIMITS[col])),ID][,2])
    }
  }
  if(RETURN_KEYS == FALSE)
    return(clean_table[,-1])
  else return(clean_table)
}

disaggregate_race <- function(
    RACE,GROUP_BY_VALUES,GROUP_BY_NAME = "HEI_Code",
    LEVELS = c("White", "Black", "Brown", "Yellow", "Indigenous", "Undeclared"),
    COL_NAMES = c("Fac_white","Fac_Black","Fac_brown","Fac_yellow",
                  "Fac_indigenous","Fac_ethn_non-disclosed"),
    RETURN_KEYS = TRUE) {

  for (i in 1:length(RACE)) {
    if(RACE[i] == "Not available")
      RACE[i] <- "Undeclared"
  }

  return(disaggregate_count(RACE,GROUP_BY_VALUES,GROUP_BY_NAME,LEVELS,COL_NAMES,RETURN_KEYS))

}

#FAC----
disaggregate_FAC_education <- function(
    EDUCATION ,GROUP_BY_VALUES, GROUP_BY_NAME = "HEI_Code",
    LEVELS = c("Highschool", "Bachelor", "Specialization","Master", "PhD"),
    COL_NAMES = c("Fac_high-school","Fac_bachelor","Fac_specialist",
                  "Fac_master","Fac_phd"),
    RETURN_KEYS = TRUE) {
  return(disaggregate_count(EDUCATION,GROUP_BY_VALUES,GROUP_BY_NAME,LEVELS,COL_NAMES,RETURN_KEYS))

}

disaggregate_FAC_work <- function(
    WORK_CONTRACT ,GROUP_BY_VALUES, GROUP_BY_NAME = "HEI_Code",
    LEVELS = c("Full time - exclusive", "Full time - not exclusive",
               "Part-time","Hourly"),
    COL_NAMES = c("Fac_full_exclusive","Fac_full_non-exclusive",
                  "Fac_part_time","Fac_hourly"),
    RETURN_KEYS = TRUE) {
  return(disaggregate_count(WORK_CONTRACT,GROUP_BY_VALUES,GROUP_BY_NAME,LEVELS,COL_NAMES,RETURN_KEYS))

}


#PRO places----
aggregating_PRO_places_day <- function(
    FULL_TABLE,GROUP_BY_NAME = "Code",
    COL_NAME = "Places_day",RETURN_KEYS = TRUE) {
  cols <- 0

  return(aggregate_sum(
    GROUP_BY_NAME=GROUP_BY_NAME,COL_NAME=COL_NAME,RETURN_KEYS = RETURN_KEYS,
    DATA_TABLE =
      FULL_TABLE[,.SD,.SDcols= c(
        GROUP_BY_NAME,
        "Places_new_full-time",
        "Places_new_morning",
        "Places_new_afternoon",
        "Places_remaining_full-time",
        "Places_remaining_morning",
        "Places_remaining_afternoon",
        "Places_special_full-time",
        "Places_special_morning",
        "Places_special_afternoon",
        "Places_main_full-time",
        "Places_main_morning",
        "Places_main_afternoon",
        "Places_other_full-time",
        "Places_other_morning",
        "Places_other_afternoon",
        "Places_full-time",
        "Places_morning",
        "Places_afternoon"
        )]
  ))

}

aggregating_PRO_places_eve <- function(
    FULL_TABLE,GROUP_BY_NAME = "Code",
    COL_NAME = "Places_evening",RETURN_KEYS = TRUE) {
  cols <- 0

  return(aggregate_sum(
    GROUP_BY_NAME=GROUP_BY_NAME,COL_NAME=COL_NAME,RETURN_KEYS = RETURN_KEYS,
    DATA_TABLE =
      FULL_TABLE[,.SD,.SDcols= c(
        GROUP_BY_NAME,
        "Places_new_evening",
        "Places_remaining_evening",
        "Places_special_evening",
        "Places_main_evening",
        "Places_other_evening",
        "Places_evening"
      )]
  ))

}

aggregating_PRO_places_dist <- function(
    FULL_TABLE,GROUP_BY_NAME = "Code",
    COL_NAME = "Places_distance",RETURN_KEYS = TRUE) {
  cols <- 0

  return(aggregate_sum(
    GROUP_BY_NAME=GROUP_BY_NAME,COL_NAME=COL_NAME,RETURN_KEYS = RETURN_KEYS,
    DATA_TABLE =
      FULL_TABLE[,.SD,.SDcols= c(
        GROUP_BY_NAME,
        "Places_new_distance",
        "Places_remaining_distance",
        "Places_special_distance",
        "Places_main_distance",
        "Places_other_distance",
        "Places_distance"
      )]
  ))

}

aggregating_PRO_places_new <- function(
    FULL_TABLE,GROUP_BY_NAME = "Code",
    COL_NAME = "Places_new",RETURN_KEYS = TRUE) {
  cols <- 0

  return(aggregate_sum(
    GROUP_BY_NAME=GROUP_BY_NAME,COL_NAME=COL_NAME,RETURN_KEYS = RETURN_KEYS,
    DATA_TABLE =
      FULL_TABLE[,.SD,.SDcols= c(
        GROUP_BY_NAME,
        "Places_new_full-time",
        "Places_new_morning",
        "Places_new_afternoon",
        "Places_new_evening",
        "Places_new_distance"
      )]
  ))

}

aggregating_PRO_places_special <- function(
    FULL_TABLE,GROUP_BY_NAME = "Code",
    COL_NAME = "Places_special",RETURN_KEYS = TRUE) {
  cols <- 0

  return(aggregate_sum(
    GROUP_BY_NAME=GROUP_BY_NAME,COL_NAME=COL_NAME,RETURN_KEYS = RETURN_KEYS,
    DATA_TABLE =
      FULL_TABLE[,.SD,.SDcols= c(
        GROUP_BY_NAME,
        "Places_special_full-time",
        "Places_special_morning",
        "Places_special_afternoon",
        "Places_special_evening",
        "Places_special_distance"
      )]
  ))

}

aggregating_PRO_places_remain <- function(
    FULL_TABLE,GROUP_BY_NAME = "Code",
    COL_NAME = "Places_remaining",RETURN_KEYS = TRUE) {
  cols <- 0

  return(aggregate_sum(
    GROUP_BY_NAME=GROUP_BY_NAME,COL_NAME=COL_NAME,RETURN_KEYS = RETURN_KEYS,
    DATA_TABLE =
      FULL_TABLE[,.SD,.SDcols= c(
        GROUP_BY_NAME,
        "Places_remaining_full-time",
        "Places_remaining_morning",
        "Places_remaining_afternoon",
        "Places_remaining_evening",
        "Places_remaining_distance"
      )]
  ))

}

aggregating_PRO_places_selec <- function(
    FULL_TABLE,GROUP_BY_NAME = "Code",
    COL_NAME = "Places_selec_process",RETURN_KEYS = TRUE) {
  cols <- 0

  return(aggregate_sum(
    GROUP_BY_NAME=GROUP_BY_NAME,COL_NAME=COL_NAME,RETURN_KEYS = RETURN_KEYS,
    DATA_TABLE =
      FULL_TABLE[,.SD,.SDcols= c(
        GROUP_BY_NAME,
        "Places_main_full-time",
        "Places_main_morning",
        "Places_main_afternoon",
        "Places_main_evening",
        "Places_main_distance",
        "Places_other_full-time",
        "Places_other_morning",
        "Places_other_afternoon",
        "Places_other_evening",
        "Places_other_distance"
      )]
  ))

}
#PRO applicants----
aggregating_PRO_applicants_day <- function(
    FULL_TABLE,GROUP_BY_NAME = "Code",
    COL_NAME = "Applicants_day",RETURN_KEYS = TRUE) {
  cols <- 0

  return(aggregate_sum(
    GROUP_BY_NAME=GROUP_BY_NAME,COL_NAME=COL_NAME,RETURN_KEYS = RETURN_KEYS,
    DATA_TABLE =
      FULL_TABLE[,.SD,.SDcols= c(
        GROUP_BY_NAME,
        "Applicants_new_full-time",
        "Applicants_new_morning",
        "Applicants_new_afternoon",
        "Applicants_remaining_full-time",
        "Applicants_remaining_morning",
        "Applicants_remaining_afternoon",
        "Applicants_special_full-time",
        "Applicants_special_morning",
        "Applicants_special_afternoon",
        "Applicants_main_full-time",
        "Applicants_main_morning",
        "Applicants_main_afternoon",
        "Applicants_other_full-time",
        "Applicants_other_morning",
        "Applicants_other_afternoon",
        "Applicants_full-time",
        "Applicants_morning",
        "Applicants_afternoon"
      )]
  ))

}

aggregating_PRO_applicants_eve <- function(
    FULL_TABLE,GROUP_BY_NAME = "Code",
    COL_NAME = "Applicants_evening",RETURN_KEYS = TRUE) {
  cols <- 0

  return(aggregate_sum(
    GROUP_BY_NAME=GROUP_BY_NAME,COL_NAME=COL_NAME,RETURN_KEYS = RETURN_KEYS,
    DATA_TABLE =
      FULL_TABLE[,.SD,.SDcols= c(
        GROUP_BY_NAME,
        "Applicants_new_evening",
        "Applicants_remaining_evening",
        "Applicants_special_evening",
        "Applicants_main_evening",
        "Applicants_other_evening",
        "Applicants_evening"
      )]
  ))

}

aggregating_PRO_applicants_dist <- function(
    FULL_TABLE,GROUP_BY_NAME = "Code",
    COL_NAME = "Applicants_distance",RETURN_KEYS = TRUE) {
  cols <- 0

  return(aggregate_sum(
    GROUP_BY_NAME=GROUP_BY_NAME,COL_NAME=COL_NAME,RETURN_KEYS = RETURN_KEYS,
    DATA_TABLE =
      FULL_TABLE[,.SD,.SDcols= c(
        GROUP_BY_NAME,
        "Applicants_new_distance",
        "Applicants_remaining_distance",
        "Applicants_special_distance",
        "Applicants_main_distance",
        "Applicants_other_distance",
        "Applicants_distance"
      )]
  ))

}

aggregating_PRO_applicants_new <- function(
    FULL_TABLE,GROUP_BY_NAME = "Code",
    COL_NAME = "Applicants_new",RETURN_KEYS = TRUE) {
  cols <- 0

  return(aggregate_sum(
    GROUP_BY_NAME=GROUP_BY_NAME,COL_NAME=COL_NAME,RETURN_KEYS = RETURN_KEYS,
    DATA_TABLE =
      FULL_TABLE[,.SD,.SDcols= c(
        GROUP_BY_NAME,
        "Applicants_new_full-time",
        "Applicants_new_morning",
        "Applicants_new_afternoon",
        "Applicants_new_evening",
        "Applicants_new_distance"
      )]
  ))

}

aggregating_PRO_applicants_special <- function(
    FULL_TABLE,GROUP_BY_NAME = "Code",
    COL_NAME = "Applicants_special",RETURN_KEYS = TRUE) {
  cols <- 0

  return(aggregate_sum(
    GROUP_BY_NAME=GROUP_BY_NAME,COL_NAME=COL_NAME,RETURN_KEYS = RETURN_KEYS,
    DATA_TABLE =
      FULL_TABLE[,.SD,.SDcols= c(
        GROUP_BY_NAME,
        "Applicants_special_full-time",
        "Applicants_special_morning",
        "Applicants_special_afternoon",
        "Applicants_special_evening",
        "Applicants_special_distance"
      )]
  ))

}

aggregating_PRO_applicants_remain <- function(
    FULL_TABLE,GROUP_BY_NAME = "Code",
    COL_NAME = "Applicants_remaining",RETURN_KEYS = TRUE) {
  cols <- 0

  return(aggregate_sum(
    GROUP_BY_NAME=GROUP_BY_NAME,COL_NAME=COL_NAME,RETURN_KEYS = RETURN_KEYS,
    DATA_TABLE =
      FULL_TABLE[,.SD,.SDcols= c(
        GROUP_BY_NAME,
        "Applicants_remaining_full-time",
        "Applicants_remaining_morning",
        "Applicants_remaining_afternoon",
        "Applicants_remaining_evening",
        "Applicants_remaining_distance"
      )]
  ))

}

aggregating_PRO_applicants_selec <- function(
    FULL_TABLE,GROUP_BY_NAME = "Code",
    COL_NAME = "Applicants_selec_process",RETURN_KEYS = TRUE) {
  cols <- 0

  return(aggregate_sum(
    GROUP_BY_NAME=GROUP_BY_NAME,COL_NAME=COL_NAME,RETURN_KEYS = RETURN_KEYS,
    DATA_TABLE =
      FULL_TABLE[,.SD,.SDcols= c(
        GROUP_BY_NAME,
        "Applicants_main_full-time",
        "Applicants_main_morning",
        "Applicants_main_afternoon",
        "Applicants_main_evening",
        "Applicants_main_distance",
        "Applicants_other_full-time",
        "Applicants_other_morning",
        "Applicants_other_afternoon",
        "Applicants_other_evening",
        "Applicants_other_distance"
      )]
  ))

}

#generic----
disaggregate_count <-function(ARRAY,GROUP_BY_VALUES,GROUP_BY_NAME,LEVELS, COL_NAMES,RETURN_KEYS = TRUE){
  if(length(LEVELS)!=length(COL_NAMES))
    stop(paste("disaggregate_count received missmatching levels and columns: ",
               paste(LEVELS,collapse = ","),
               ", Columns: ",paste(COL_NAMES,collapse = ",")))

  clean_table <- data.table(ID = unique(GROUP_BY_VALUES))

  #setnames(clean_table,new = COL_NAMES)

  value_table <- dcast(data.table(ID = GROUP_BY_VALUES,
                                  categories = ARRAY),
                       ID~categories, fun.aggregate = length)

  for (i in 1:length(COL_NAMES)) {
    if(LEVELS[i] %in% names(value_table))
      clean_table <- cbind(clean_table,value_table[,.SD,.SDcols = LEVELS[i]])
    else set(clean_table, j = LEVELS[i],value = NA)
  }

  setnames(clean_table,new = c(GROUP_BY_NAME, COL_NAMES))

  if(RETURN_KEYS == FALSE)
    return(clean_table[,-1])
  else return(clean_table)


}

aggregate_sum <-function(DATA_TABLE,GROUP_BY_NAME,COL_NAME,RETURN_KEYS = TRUE){
  if(match(GROUP_BY_NAME,names(DATA_TABLE))!=1)
    stop("aggregate_sum received a data table where the first column is not the ID to be grouped by")


  keys <- unique(DATA_TABLE[[GROUP_BY_NAME]])

  DATA_TABLE <- DATA_TABLE[,-1]

  DATA_TABLE[,ID := keys]



  suppressWarnings(clean_table <- melt(DATA_TABLE,"ID"))

  clean_table <- clean_table[,.(sum(value,na.rm = T)),ID]

  setnames(clean_table,new = c(GROUP_BY_NAME, COL_NAME))

  if(RETURN_KEYS == FALSE)
    return(clean_table[,-1])
  else return(clean_table)

}
