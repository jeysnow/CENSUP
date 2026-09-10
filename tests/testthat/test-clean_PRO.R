for (y in c(2017,2023)) {
  ref <- fread(test_path("PRO_refs",paste0("PRO_",y,".csv")),
    encoding = "Latin-1",
    ,na.strings = "")
  print(test_path("PRO_inputs",paste0("PRO_",y,".csv")))
  test_that(paste0("Clean PRO conversion as return for year ",y," writing to data_test"), {
    expect_no_error(clean_PRO(OUTPUT_TO = test_path("data_test"),
                              test_path("PRO_inputs",paste0("PRO_",y,".csv")),
                              CITY_NAMES = F))

  })
  output <- clean_PRO(
    test_path("PRO_inputs",paste0("PRO_",y,".csv")),
    CITY_NAMES = F)

  test_that(paste0("Clean PRO conversion as return for year ",y,", Column names"), {
    expect_setequal(names(output) |> as.array(),names(ref) |> as.array())
  })



  # for (c in names(output)) {
  #   test_that(paste0("Clean PRO conversion for year ",y,", Column ",c), {
  #
  #
  #     expect_setequal(output[[as.character(c)]],ref[[as.character(c)]])
  #   })
  # }

  output <- fread(test_path("data_test",paste0("Clean_PRO_",y,".csv")),
                  #encoding = "Latin-1",
                  na.strings = "")

  test_that(paste0("Clean PRO conversion from file for year ",y,", Column names"), {
    expect_setequal(names(output) |> as.array(),names(ref) |> as.array())
  })

  # for (c in names(output)) {
  #   test_that(paste0("Clean PRO conversion from file for year ",y,", Column ",c), {
  #
  #
  #     expect_setequal(output[[as.character(c)]],ref[[as.character(c)]])
  #   })
  # }


}
