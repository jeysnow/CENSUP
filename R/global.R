#'@import data.table
#'@importFrom utils data
#'
#'
#'@export
#'@include global.R

# fix to add the references to the global environment despite it alrady being in sysdata.rda
# https://stackoverflow.com/questions/32964741/accessing-sysdata-rda-within-package-functions

references <- get0("references", envir = asNamespace("CENSUP"))


# this exists solely to remove spurious notes from CDM check, due
# to the use of data table. For details, see here:
# doesn't seem to work though
# https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/
globalVariables(c(
  references$HEI$names$Standard,
  references$FAC$names$Standard,
  references$HEI_FAC$names$Standard,
  references$PRO$names$Standard
),package = "CENSUP")



