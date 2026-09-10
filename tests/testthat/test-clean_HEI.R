for (y in c(#2009,2010,2013,
            2017,2023)) {
  ref <- fread(test_path("HEI_refs",paste0("HEI_",y,".csv")),
    encoding = "Latin-1",
    ,na.strings = "")

  test_that(paste0("Clean HEI conversion as return for year ",y,"writing to data_test"), {
    expect_no_error(clean_HEI(OUTPUT_TO = test_path("data_test"),
      test_path("HEI_inputs",paste0("HEI_",y,".csv")),
      CITY_NAMES = F))

  })

  output <- clean_HEI(
    test_path("HEI_inputs",paste0("HEI_",y,".csv")),
    CITY_NAMES = F)

  test_that(paste0("Clean HEI conversion as return for year ",y,", Column names"), {
    expect_setequal(names(output) |> as.array(),names(ref) |> as.array())
  })



  # for (c in names(output)) {
  #   test_that(paste0("Clean HEI conversion for year ",y,", Column ",c), {
  #
  #
  #     expect_setequal(output[[as.character(c)]],ref[[as.character(c)]])
  #   })
  # }

  output <- fread(test_path("data_test",paste0( "Clean_HEI_",y,".csv",collapse = "")),
                  #encoding = "Latin-1",
                  na.strings = "")

  test_that(paste0("Clean HEI conversion from file for year ",y,", Column names"), {
    expect_setequal(names(output) |> as.array(),names(ref) |> as.array())
  })

  # for (c in names(output)) {
  #   test_that(paste0("Clean HEI conversion from file for year ",y,", Column ",c), {
  #
  #
  #     expect_setequal(output[[as.character(c)]],ref[[as.character(c)]])
  #   })
  # }


}
