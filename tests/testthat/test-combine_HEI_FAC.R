for (y in c(2017)) {
  ref <- fread(test_path("HEI_FAC_refs",paste0("HEI_FAC_",y,".csv")),
    encoding = "Latin-1",
    ,na.strings = "")

  test_that(paste0("Clean HEI_FAC conversion as return for year ",y,"writing to data_test"), {
    expect_no_error(
      combine_HEI_FAC(
        CLEAN_HEI = test_path("HEI_refs",paste0("HEI_",y,".csv")),
        CLEAN_FAC = test_path("FAC_refs",paste0("FAC_",y,".csv")),
        OUTPUT_TO = test_path("data_test"),
        CITY_NAMES = F))
  })

  output <- combine_HEI_FAC(
    CLEAN_HEI = test_path("HEI_refs",paste0("HEI_",y,".csv")),
    CLEAN_FAC = test_path("FAC_refs",paste0("FAC_",y,".csv")),
    CITY_NAMES = F)

  test_that(paste0("Combine HEI_FAC for year ",y,", Column names"), {
    expect_setequal(names(output) |> as.array(),names(ref) |> as.array())
  })



  # for (c in names(output)) {
  #   test_that(paste0("Combine HEI_FAC year ",y,", Column ",c), {
  #
  #
  #     expect_setequal(output[[as.character(c)]],ref[[as.character(c)]])
  #   })
  # }

  output <- fread(test_path("data_test",paste0( "Clean_HEI_FAC_",y,".csv",collapse = "")),
                  #encoding = "Latin-1",
                  na.strings = "")

  test_that(paste0("Combine HEI_FAC from file for year ",y,", Column names"), {
    expect_setequal(names(output) |> as.array(),names(ref) |> as.array())
  })

  # for (c in names(output)) {
  #   test_that(paste0("Clean HEI_FAC conversion from file for year ",y,", Column ",c), {
  #
  #
  #     expect_setequal(output[[as.character(c)]],ref[[as.character(c)]])
  #   })
  # }


}
