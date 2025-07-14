#'
#'
#' We build out the header by event
#' @import data.table

build_header <- function(.data, dict = header_columns_dict){
  
  #Coerce to data.table
  .data <- data.table::as.data.table(.data)
  
  #Get the columns used for header building
  .data_out <- .data[,colnames(.data) %in% dict[["col_var"]], with = FALSE]
  
  #Coerce the factors to characters and chop any leading numbers
  .data_out <- lapply(seq_along(dict[["col_var"]]), function(.idx){
    .col <- dict[["col_var"]][.idx]
    if(dict[["numeric_var"]][.idx] == TRUE){
      as.numeric(as.character(.data_out[[.col]]))
    } else{
      gsub("^\\d*?\\s+?", "", as.character(.data_out[[.col]]))
    }
  })
  .data_out <- do.call(cbind.data.frame, .data_out)
  colnames(.data_out) <- dict[["col_var"]]
  
  #Finally, we step through and build out the string and index according to the event type
  .string_out <- lapply(seq_len(nrow(.data_out)), function(.row){
    
    #Extract the variables
    #.event <- toupper(gsub("_arm_1", "", .data_out[[event_var]][.row]))
    .event <- paste0("Visit ", gsub("_arm_1", "", .data_out[[event_var]][.row]))
    .id <- .data_out[[id_var]][.row]
    .sex <- substr(.data_out[["sex"]][.row], 1, 1)
    #.race <- dplyr::recode(.data_out[["race"]][.row], !!!race_recode)
    .race <- race_recode[.data_out[["race"]][.row]]
    .race[is.na(.race)] <- "Oth Race"
    
    #Make the header string
    paste0(.id, "; A1 Age: ", .data_out[["Age"]][.row], "; ", .race, " ", .sex, "; ", .data_out[["educ"]][.row], "Y Edu (", .event, ")")
  })
  names(.string_out) <- paste0("Visit ", (gsub("_arm_1", "", .data_out[[event_var]])))
  
  return(.string_out)
  
}

