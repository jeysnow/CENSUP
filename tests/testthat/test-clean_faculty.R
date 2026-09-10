for (y in c(2009,2010,2013,2017)) {
  ref <- fread(test_path("FAC_refs",paste0("FAC_",y,".csv")),
    encoding = "Latin-1",
    ,na.strings = "")

  test_that(paste0("Clean faculty conversion for year ",y), {
    expect_no_error(clean_FAC(OUTPUT_TO = test_path("data_test"),
      test_path("FAC_inputs",paste0("FAC_",y,".csv")),
      CITY_NAMES = F))

  })
  output <- clean_FAC(
      test_path("FAC_inputs",paste0("FAC_",y,".csv")),
      CITY_NAMES = F)

  test_that(paste0("Clean faculty conversion for year ",y,", Column names"), {
    expect_setequal(names(output) |> as.array(),names(ref) |> as.array())
  })


  # for (c in names(output)) {
  #   test_that(paste0("Clean faculty conversion for year ",y,", Column ",c), {
  #
  #
  #     expect_setequal(output[[as.character(c)]],ref[[as.character(c)]])
  #   })
  # }

  output <- fread(test_path("data_test",paste0( "Clean_FAC_",y,".csv",collapse = "")),
                  #encoding = "Latin-1",
                  na.strings = "")

  test_that(paste0("Clean FAC conversion from file for year ",y,", Column names"), {
    expect_setequal(names(output) |> as.array(),names(ref) |> as.array())
  })

  # for (c in names(output)) {
  #   test_that(paste0("Clean FAC conversion from file for year ",y,", Column ",c), {
  #
  #
  #     expect_setequal(output[[as.character(c)]],ref[[as.character(c)]])
  #   })
  # }
}
