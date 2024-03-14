#' Create header file to facilitate the manual sampling check.
#' @param lens Lengths data frame of Sireno report.
#' @return Export xlsx file with headers resume.
#' @export
createHeadersXls <- function (lens) {
  check_headers<-aggregate(lens[,c("COD_ID"), ],
                           by=list(PUERTO=lens$PUERTO, FECHA_MUE=lens$FECHA_MUE,
                                   FECHA_DESEM=lens$FECHA_DESEM, CALADERO_DCF=lens$CALADERO_DCF,
                                   ARTE=lens$ARTE, ESTRATO_RIM=lens$ESTRATO_RIM, BARCO=lens$BARCO,
                                   TIPO_MUE=lens$COD_TIPO_MUE,N_RECHAZOS=lens$N_RECHAZOS), FUN=length)
  names(check_headers)<-c("PUERTO","FECHA_MUE","FECHA_DESEM","CALADERO_DCF","ARTE",
                          "ESTRATO_RIM","BARCO","TIPO_MUESTREO","N_RECHAZOS","N.mue.tallas")

  check_headers <- check_headers [
    with(check_headers, order(PUERTO, FECHA_DESEM, FECHA_MUE)),
  ]

  wb <- openxlsx::createWorkbook()

  name_worksheet <- paste("check_headers", YEAR, MONTH, sep = "_")

  openxlsx::addWorksheet(wb, name_worksheet)
  openxlsx::writeData(wb, name_worksheet, check_headers)

  filename <- file.path(DATA_PATH, paste0(name_worksheet, ".xlsx"))

  exportXlsFile(wb, filename)

}
