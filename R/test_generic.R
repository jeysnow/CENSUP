#' @include global.R
#' @include checks.R
#' @include utils.R

test_CENSUP <- function(FUN,YEARS,WRITE = FALSE){
  if(is.numeric(YEARS)==FALSE)
    stop(paste("test_CENSUP received non numerical years for fun: ",FUN))
  if(is.logical(WRITE)==FALSE)
    stop(paste("test_CENSUP received non logical WRITE for fun: ",FUN))

  ARGS <- list()
  refs <- list()
  output <- list()
  switch (FUN,
          "combine_HEI_FAC" = {
            ARGS <- list(
              CLEAN_HEI = testthat::test_path("HEI_refs",paste0("HEI_",YEARS,".csv")),
              CLEAN_FAC = testthat::test_path("FAC_refs",paste0("FAC_",intersect(YEARS,2009:2019),".csv")),
              CITY_NAMES = F)

            #get the references for test
            files <- list.files(testthat::test_path("HEI_FAC_refs"),full.names = T)
            for (i in 1:length(files)) {
              if(extract_year(files[[i]]) %in% YEARS)
                refs[[length(refs)+1]] <- fread(testthat::test_path(
                  "HEI_FAC_refs",paste0("HEI_FAC_",extract_year(files[[i]]),".csv")),
                  encoding = "Latin-1",na.strings = "")
            }

          },
          "clean_HEI_FAC" = {
            ARGS <- list(
              READ_FROM_HEI = testthat::test_path("HEI_inputs",paste0("HEI_",YEARS,".csv")),
              CITY_NAMES = F)

            if(length(intersect(YEARS,2009:2019))>0)
              ARGS[["READ_FROM_FAC"]] <- testthat::test_path("FAC_inputs",paste0("FAC_",intersect(YEARS,2009:2019),".csv"))

            #get the references for test
            files <- list.files(testthat::test_path("HEI_FAC_refs"),full.names = T)
            for (i in 1:length(files)) {
              if(extract_year(files[[i]]) %in% YEARS)
                refs[[length(refs)+1]] <- fread(testthat::test_path(
                  "HEI_FAC_refs",paste0("HEI_FAC_",extract_year(files[[i]]),".csv")),
                  encoding = "Latin-1",na.strings = "")
            }
          },
          "clean_FAC" = {
            ARGS <- list(
              READ_FROM = testthat::test_path("FAC_inputs",paste0("FAC_",YEARS,".csv")),
              CITY_NAMES = F)

            #get the references for test
            files <- list.files(testthat::test_path("FAC_refs"),full.names = T)
            for (i in 1:length(files)) {
              if(extract_year(files[[i]]) %in% YEARS)
                refs[[length(refs)+1]] <- fread(testthat::test_path(
                  "FAC_refs",paste0("FAC_",extract_year(files[[i]]),".csv")),
                  encoding = "Latin-1",na.strings = "")
            }
          },
          "clean_HEI" = {
            ARGS <- list(
              READ_FROM = testthat::test_path("HEI_inputs",paste0("HEI_",YEARS,".csv")),
              CITY_NAMES = F)

            #get the references for test
            files <- list.files(testthat::test_path("HEI_refs"),full.names = T)
            for (i in 1:length(files)) {
              if(extract_year(files[[i]]) %in% YEARS)
                refs[[length(refs)+1]] <- fread(testthat::test_path(
                  "HEI_refs",paste0("HEI_",extract_year(files[[i]]),".csv")),
                  encoding = "Latin-1",na.strings = "")
            }
          },
          "clean_PRO" = {
            ARGS <- list(
              READ_FROM = testthat::test_path("PRO_inputs",paste0("PRO_",YEARS,".csv")),
              CITY_NAMES = F)

            #get the references for test
            files <- list.files(testthat::test_path("PRO_refs"),full.names = T)
            for (i in 1:length(files)) {
              if(extract_year(files[[i]]) %in% YEARS)
                refs[[length(refs)+1]] <- fread(testthat::test_path(
                  "PRO_refs",paste0("PRO_",extract_year(files[[i]]),".csv")),
                  encoding = "Latin-1",na.strings = "")
            }

          },
          stop(paste("test_CENSUP tried to test an unsupported function: ",FUN))
  )
  if(WRITE==TRUE){
    if(!dir.exists("data_test"))
      dir.create(testthat::test_path("data_test"))

    ARGS["OUTPUT_TO"] <- testthat::test_path("data_test")

    testthat::test_that(paste0("testing ",FUN," writing to data_test for years: ",paste(YEARS,collapse = ",")),{
      testthat::expect_no_error(
        do.call(FUN, ARGS)
      )
    })

    files <- list.files(testthat::test_path("data_test"),full.names = TRUE)
    for (i in 1:length(files)) {
      output[[length(output)+1]] <- fread(files[[i]],na.strings = "")

    }
    #make sure test directory is cleaned for future tests
    unlink(testthat::test_path("data_test"),recursive = T)
    dir.create(testthat::test_path("data_test"))
  }
  else output <- do.call(FUN, ARGS)
  if(is.data.table(output)){
    output <- list(output)
  }
  for (DT in output) {
    r <- matching_data(DT,refs)
    y <- DT[1,1]

    test_that(paste0(FUN," for year ",y,", Column names"), {
      expect_setequal(names(DT) |> as.array(),names(r) |> as.array())
    })

    for (c in names(DT)) {
      test_that(paste0(FUN, " year ",y,", Column ",c), {
        expect_setequal(DT[[as.character(c)]],r[[as.character(c)]])
      })
    }
  }
}

