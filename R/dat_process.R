#'
#' Like other reports, we begin with some modified calls to ADRCDash for processing purposes
#' Some functions for data processing
#' 
#' @import REDCapR
#'
#'


redcap_process <- function(){
  
  
  #Read in data using modified redcap_readin function from ADRCDash
  #Unfortunately the export_forms_secondary argument isn't really working, causing issues with the downstream merge so we just pull everything from the NACC REDCap
  #.data <- ADRCDash:::redcap_read_in(simple = TRUE, synth = FALSE, use_spinner = FALSE, use_redcap_factors = TRUE)#,
                                     #export_forms_secondary = c("subject_info", "clinical_consensus_reviewer_1", "clinical_consensus_reviewer_2")
  .data <- ADRCDash:::visit_read_in(token = "REDCAP_NACC_API_NEW", subtable_dict = NULL, .type = "nacc", synth=FALSE, use_redcap_factors = TRUE)[["visits"]]
  
  #Filter out any rows missing all reviewer dates
  .data <- drop_missing_rows(.data)
  
  #Make sure the dataframe is properly sorted according to date
  #.data <- ADRCDash:::redcap_order_rows(.data)
  
  #Build age variable based on DoB
  .data$birthmo <- as.numeric(as.character(.data$birthmo))
  .data[,Age := make_age(.SD, .today=FALSE),by=id_var]
  
  #Subset columns based on our restricted set
  .data <- .data[,grep(col_pull_grep, colnames(.data)), with=FALSE]
  
  #Do a fill down on whatever is left, currently only education and race from A1 based on the new processing done by visit_read_in() in ADRCDash:
  .data <- ADRCDash:::fill_down_rows(.data, dict = c("educ", "race"))
  
  return(as.data.frame(.data))
}





#This is the updated version used by ADRCDash, this is just a placeholder until we update the package

make_age <- function(df, yr_var = "birthyr", mo_var = "birthmo", .today = TRUE, alt_date_field = "a1_form_dt"){
  
  if(.today == TRUE){
    yr_curr <- lubridate::year(lubridate::today())
    mo_curr <- lubridate::month(lubridate::today())
  } else{
    yr_curr <- lubridate::year(df[[alt_date_field]])
    mo_curr <- lubridate::month(df[[alt_date_field]])
  }
  
  mo_adjust <- as.numeric(mo_curr < df[[mo_var]]) * (-1)
  
  age_curr <- yr_curr - df[[yr_var]] + mo_adjust
  
  return(age_curr)
}




drop_missing_rows <- function(.dat, dict = missing_row_dict){
  
  #Step through the dictionary, identify all rows that are missing all variables in any of the sets (e.g. both reviewer dates or initials are missing)
  #We use negate on the complete.cases to identify any row with an NA in the variable set of interest
  idx_drop <- lapply(missing_row_dict, function(.set){
    which(!complete.cases(.dat[,colnames(.dat) %in% .set]))
  })
  
  #Alternative function that uses apply, this is a slower process though so better to use complete.cases instead of any(is.na())
  # idx_drop <- lapply(missing_row_dict, function(.set){
  #   which(apply(.dat[,colnames(.dat) %in% .set], 1, function(xx){any(is.na(xx))==TRUE}))
  # })
  
  #Use Reduce to get the intersection across all list returns
  idx_drop <- Reduce(intersect, idx_drop)
  
  #Drop the missing rows
  if(length(idx_drop) > 0){
    .dat <- .dat[-idx_drop,]
  }
  
  return(.dat)
  
  
}

#Dictionary for missing rows, we just check to make sure at least one of these pairings exists

missing_row_dict = list(c("ccc_form_dt_rev1", "ccc_form_dt_rev2"),
                        c("ccc_ex_ini_rev1", "ccc_ex_ini_rev2"))



pull_id <- function(.dat, .id, .id_var = id_var){
  if(!is.null(.id)){
    .out <- .dat[.dat[[.id_var]] %in% .id & !is.na(.dat[[.id_var]]),]
    if(nrow(.out) == 0){ return(NULL)
    } else return(.out)
  } else{
    return(NULL)
  }
}





#Defaults for out of bounds IDs

text_default <- paste("ID not recognized\n",
                      "Please reset and try again")

text_missing <- paste("No Numeric/Syndromal diagnoses listed for ID<br/>",
                      "Reviews assumed blank, please check data")

default_table <- data.frame(V1 = text_default); colnames(default_table) <- ""
missing_table <- data.frame(V1 = " ", V2 = text_missing, V3 = " "); colnames(missing_table) <- c(" ", "  ", "   ")

text_ready <- paste("Tables Ready - Please enter an ADC ID (ADCXXX or number)")
ready_table <- data.frame(V1 = text_ready); colnames(ready_table) <- ""

text_wait <- paste("Please wait, processing ADRC data")
wait_table <- data.frame(V1 = text_wait); colnames(wait_table) <- ""