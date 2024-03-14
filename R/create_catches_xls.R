#' Function to create a xls file with the catches sampling document from SIRENO's database
#' @param catches: data frame returned by the importRIMCatches() or
#' importRIMFiles() functions.
#' @param lens: lengths data frame returned by the importRIMLengths() or
#' importRIMFiles() functions.
#' @return a new table in xls format to see the data in a diferent format
#' where de values of the columns turns to the rows and vice versa
#' @export
createCatchesXls <- function(lens, catches){

  sampled_species <- lens[,c("COD_ID", "PUERTO", "FECHA_MUE", "BARCO", "ESP_MUE",
                             "CATEGORIA", "ESP_CAT", "SEXO")]

  check_catches <- merge(catches, sampled_species, all.x = TRUE)
  check_catches <- check_catches[, c("COD_ID", "PUERTO", "FECHA_MUE", "BARCO", "ESP_MUE",
                                     "CATEGORIA", "ESP_CAT", "SEXO", "P_DESEM")]

  # generate pivot table
  pt <- PivotTable$new()
  pt$addData(check_catches)
  pt$addRowDataGroups("PUERTO", addTotal=FALSE)
  pt$addRowDataGroups("FECHA_MUE", addTotal=FALSE)
  pt$addRowDataGroups("BARCO", addTotal=FALSE)
  pt$addRowDataGroups("ESP_MUE", addTotal=FALSE)
  pt$addRowDataGroups("CATEGORIA", addTotal=FALSE)
  pt$addRowDataGroups("ESP_CAT", addTotal=FALSE)
  pt$addRowDataGroups("SEXO", addTotal=FALSE)
  pt$defineCalculation(calculationName="P_DESEM", summariseExpression="sum(P_DESEM)")
  pt$renderPivot()

  pt_dataframe <- pt$asDataFrame()

  wb <- createWorkbook(creator = Sys.getenv("USERNAME"))
  name_worksheet <- paste("check_catches", YEAR, MONTH, sep = "_")
  addWorksheet(wb, name_worksheet)
  setRowHeights(wb, name_worksheet, rows = nrow(pt_dataframe), heights = 15)
  setColWidths(wb, name_worksheet, cols = c(1:8), widths = c(12, 12, 15, 30, 30, 30, 3, 10))

  # I don't know why text rotation does not work:
  port_style <- createStyle(textRotation = 255)

  addStyle(wb, name_worksheet, port_style, rows = nrow(pt_dataframe), cols=1)

  pt$writeToExcelWorksheet(wb=wb, wsName=name_worksheet,
                           topRowNumber=1, leftMostColumnNumber=1,
                           applyStyles=TRUE, mapStylesFromCSS=TRUE)

  filename <- file.path(DATA_PATH, paste0(name_worksheet, ".xlsx"))
  exportXlsFile(wb, filename)

}
