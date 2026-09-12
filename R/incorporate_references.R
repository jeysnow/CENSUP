#year <- 2019

# fread(paste0("/Data/CENSUP/Raw_Data/Program/DM_CURSO_",year,".csv"),
#       encoding = "Latin-1",nrows = 10) |> fwrite(
#         paste0("./inst/extdata/PRO_inputs/PRO_",year,".csv"))




update_references <- function(){
  references <- list()

  references$FAC <- list()

  references$FAC$names <- fread(
    encoding = "Latin-1",header = T,
    "./reference_tables/FAC_names.csv")
  references$FAC$status <- fread(
    encoding = "Latin-1", na.strings = "",header = T,
    "./reference_tables/FAC_status.csv")

  references$individual <- list()

  references$individual$race <- fread(
    encoding = "Latin-1", na.strings = "",header = T,
    "./reference_tables/individual_race.csv")

  references$geographic <- list()

  references$geographic$city <- fread(
    na.strings = "",header = T,
    "./reference_tables/geographic_cities.csv")
  references$geographic$state <- fread(
    encoding = "Latin-1", na.strings = "",header = T,
    "./reference_tables/geographic_states.csv")

  references$HEI <- list()

  references$HEI$names <- fread(
    encoding = "Latin-1", header = T,
    "./reference_tables/HEI_names.csv")

  references$HEI$adm_type <- fread(
    encoding = "Latin-1", na.strings = "",header = T,
    "./reference_tables/HEI_administrative_type.csv")

  references$HEI_FAC <- list()

  references$HEI_FAC$names <- fread(
    encoding = "Latin-1", header = T,
    "./reference_tables/HEI_FAC_names.csv")

  references$PRO <- list()

  references$PRO$names <- fread(
    encoding = "Latin-1", header = T,
    "./reference_tables/PRO_names.csv")



    references$clean_keys <- data.table(
    KEY = c(paste0(references$HEI$names$ENG,collapse = ""),
            paste0(references$HEI$names$POR,collapse = ""),
            paste0(references$FAC$names$ENG,collapse = ""),
            paste0(references$FAC$names$POR,collapse = ""),
            paste0(references$HEI_FAC$names$ENG,collapse = ""),
            paste0(references$HEI_FAC$names$POR,collapse = ""),
            paste0(references$PRO$names$ENG,collapse = ""),
            paste0(references$PRO$names$POR,collapse = "")
    ),
    VALUE = c(
      "HEI", "IES","FAC","DOC","HEI_FAC","IES_DOC","PRO","CUR"
    )
    )

usethis::use_data(references,internal = T,overwrite = T)
}


