#'
#'
#' Comparison dictionaries and functions to build dictionaries
#'

#Static variables
id_var <- c("adc_sub_id")
event_var <- c("redcap_repeat_instance")
demog_cols <- c("Age", "race", "sex", "educ")
reviewer_cols <- c("ccc_form_dt_rev1", "ccc_form_dt_rev2", "ccc_ex_ini_rev1", "ccc_ex_ini_rev2")


#Dictionaries for build_header
header_columns_dict <- list(col_var = c(id_var, event_var, "Age", "race", "sex", "educ"),
                            numeric_var = c(rep(FALSE, 5), TRUE))

race_recode <- c("Black or African American" = "B/AA", "White" = "W")




#D1 colnames from data dictionary
colnames_ccc <- paste0("ccc_", c("cogstat_c2", naccDataDict::redcap_nacc_data[["IVP"]][["D1"]], 
                                 "clin_notes_supp", "clin_notes_anti", "syndrm_stg", "numeric_stg"), "_rev\\d*")
colnames_ccc_text_str <- "clin_notes|((oth|ftld).*?x)"

#The string used to get the columns used in table building
col_pull_grep <- paste("^(", paste(c(id_var, event_var, demog_cols, reviewer_cols, colnames_ccc), collapse = "|"), ")$", sep ="")


#Functions to pull dictionary columns for comparisons
get_ccc_cols <- function(.dat){
  cols_curr <- setdiff(colnames(.dat), c(id_var, event_var, demog_cols))
  return(cols_curr)
}


#Function to get REDCap labels
get_redcap_labels <- function(.dat, redcap_cols,
                              subset_string = "_rev1$",
                              rename_string_in = "ccc_(.*?)_rev1$", 
                              rename_string_out = "\\1"){

  #Extract labels
  labels_curr <- do.call(c, lapply(.dat[,colnames(.dat) %in% redcap_cols], function(.var){attributes(.var)$label}))
  
  #Drop HTML
  labels_curr <- gsub("<.*?>", "", labels_curr)
  
  #Subset based on reviewer 1, name according to redcap variable names using rename_string arguments
  labels_curr <- labels_curr[grep(subset_string, names(labels_curr))]
  names(labels_curr) <- gsub(rename_string_in, rename_string_out, names(labels_curr))
  return(labels_curr)
}






#Function to process the ID
process_id <- function(.id, .head = "ADC"){
  
  #Process header if needed
  if(length(grep(.head, .id, ignore.case = TRUE)) > 0) .id <- as.numeric(gsub(.head, "", .id, ignore.case = TRUE))
  
  #Return NULL if not a valid number
  if(is.na(as.numeric(.id)) | as.numeric(.id) < 0) return(NULL)
  .id <- as.numeric(.id)
  
  #Add leading 0's
  if(.id < 10){ .id <- paste0("00", .id)
  } else if(.id < 100) .id <- paste0("0", .id)
  
  return(paste0(.head, .id))
  
}


