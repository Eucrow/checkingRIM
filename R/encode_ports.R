#' This function gives you the codes of the ports what you are working.
#' You have two ways: using a vector with the name of the ports or insert
#' them by an emergent window
#' @param work_ports vector with the names of our working ports
#' @param dialog_box boolean paramater to choose if you work by the vector or the
#' emergent window way. By default is defined with FALSE value, because the main way of
#' work is through a vector. Then, if you want to work choosing the ports you only
#' need to declare dialog_box with TRUE
#' @return the code of the selected working ports
#' @export


encode_ports <- function(work_ports, dialog_box = FALSE) {
  # Charging the sapmuebase's puerto dataframe
  master_data_ports <- sapmuebase::puerto
  if (dialog_box) {
    code_ports <- manage_dialog_box(master_data_ports)
  } else {
    master_data_ports$PUERTO <- toupper(master_data_ports$PUERTO)
    master_data_ports$PUERTO <- gsub(" ", "", master_data_ports$PUERTO)
    df_entry_ports <- data.frame(PUERTO = work_ports)
    df_entry_ports$PUERTO <- toupper(df_entry_ports$PUERTO)
    df_entry_ports$PUERTO <- gsub(" ", "", df_entry_ports$PUERTO)
    df_check_ports <- merge(df_entry_ports, master_data_ports, all.x = TRUE)
    df_check_ports <- df_check_ports[is.na(df_check_ports$COD_PUERTO), ]

    if (nrow(df_check_ports) != 0) {
      wrong_ports <- df_check_ports$PUERTO
      wrong_ports <- paste(wrong_ports, collapse = ", ")
      warningMessage <- paste0("Ha insertado mal/no se encuentran los siguientes puertos: ", wrong_ports, " Por favor, revise los puertos utilizados y ejecute nuevamente la función.")

      print(warningMessage)
    } else {
      code_ports <- as.vector(master_data_ports[master_data_ports$PUERTO %in% df_entry_ports$PUERTO, "COD_PUERTO"])
    }
  }
}
