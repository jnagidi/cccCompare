#'
#' Main function that takes an ID and builds out the comparison table
#' @import data.table
#'




make_comparison_set <- function(.data, .cols_comp, .labels){
  
  #We start by removing all REDCap labels and making sure all columns are characters
  .data <- 
    do.call(cbind.data.frame, lapply(.data, function(xx){
      attributes(xx)$label <- NULL
      as.character(xx)
  }))
  
  
  #Next subset to only use the relevant comparison columns
  .data_proc <- .data[,colnames(.data) %in% c(event_var, .cols_comp)]
  
  # #Some hard coding to place iv_pre at the end
  # if("iv_pre_arm_1" %in% .data_proc[[event_var]]){
  #   .data_pre <- .data_proc[.data_proc[[event_var]] == "iv_pre_arm_1",]
  #   .data_proc <- .data_proc[-which(.data_proc[[event_var]] == "iv_pre_arm_1"),]
  #   .data_proc <- rbind.data.frame(.data_proc, .data_pre, stringsAsFactors = FALSE)
  # }
  
  #Coerce this object to a data.table before applying the double pivot function for each event
  .data_proc <- data.table::as.data.table(.data_proc)
  .data_proc <- .data_proc[, double_pivot(.SD, .cols_comp) ,by = event_var]
  
  #Next we step through and coerce each event into a comparison list
  .out <- .data_proc[, list(comparison_maker(.SD, .labs = .labels)), by = event_var]
  
  #While this does the desired .BY processing, we ultimately want to get a list subset by event
  .out <- apply(.out, 1, function(.row){
    .list <- .row$V1
    .name <- paste0("Visit ", toupper(gsub("_arm_1", "", .row[[event_var]])))
    return(list(tab = .list, event = .name))
  })
  
  #Since .out is a list, we extract the entries and name them
  .out_list <- lapply(.out, function(.li){.li[["tab"]]})
  .out_names <- do.call(c, lapply(.out, function(.li){.li[["event"]]}))
  names(.out_list) <- .out_names
  
  #We're now all set to return the comparison table
  return(.out_list)
  
}




#The function to handle the pivot process to get each reviewer on their own row

double_pivot <- function(.dat, .cols, pattern_string = "(.*?)_(rev\\d*)"){
  
  #Pivot longer on everything to get reviewer as a column
  .dat <- tidyr::pivot_longer(.dat, cols = dplyr::all_of(.cols),
                              names_to = c("Var", "Rev"), names_pattern = pattern_string)
  
  #Pivot wider to get the double row design
  .dat <- tidyr::pivot_wider(.dat, names_from = "Var", values_from = "value")
  
  return(.dat)
  
}






comparison_maker <- function(.dat, .labs,
                             always_return = c("syndrm_stg", "numeric_stg"),
                             clin_notes = c("Notes or observations IN SUPPORT of a diagnosis",
                                            "Notes or observations AGAINST a diagnosis")){
  
  #Return the missing_table if no diagnoses are found
  if(all(is.na(.dat[,grep(paste(always_return, collapse="|"), colnames(.dat)),with=FALSE]))) return(list(missing_table))
  
  #Start by building the headers on reviewer and date
  .dat$header <- paste(.dat[["initialsd1a"]], " - ", .dat[["frmdated1a"]])
  
  #So for each column...
  comparison_set <- lapply(colnames(.dat)[colnames(.dat) %not_in% c("Rev", "initialsd1a", "frmdated1a", "header")], function(.col){
    
    #For a first pass return NULL if everything is NA regardless of column type
    if(all(is.na(.dat[[.col]]))) return(NULL)
  
    #Otherwise, first process non-free text fields
    if(length(grep(colnames_ccc_text_str, .col)) == 0){
      
      #As long as there's at least one response, build it out a bit
      .dat_out <- as.character(.dat[[.col]])
      .dat_out <- gsub("<b>\\(.*?\\)</b>", "", .dat_out)
      .dat_out[is.na(.dat_out)] <- ""
      
      #Coerce to data frame
      .dat_out <- as.data.frame(t(.dat_out), stringsAsFactors = FALSE)
      colnames(.dat_out) <- paste("Reviewer", 1:ncol(.dat_out), sep = "")
      
      #Add the redcap label as the question and return
      .dat_out <- data.frame(Question = .labs[[.col]], .dat_out, stringsAsFactors=FALSE)
      
      #Finally, add the drop toggle in case consensus isn't needed
      if(length(unique(.dat[[.col]])) == 1 & .col %not_in% always_return){
        .dat_out$consensus_drop <- TRUE
      } else .dat_out$consensus_drop <- FALSE
      
      return(.dat_out)
    
    #If it is a free text field we basically follow the same process but we keep it separate just in case    
    } else {
      
      #Replace empty strings
      .dat_out <- as.character(.dat[[.col]])
      .dat_out[is.na(.dat_out)] <- "No entry"
      
      #Coerce to data frame
      .dat_out <- data.frame(t(.dat_out), stringsAsFactors = FALSE)
      colnames(.dat_out) <- paste("Reviewer", 1:ncol(.dat_out), sep = "")
      
      #Add the redcap label as the question and return
      .dat_out <- data.frame(Question = as.character(.labs[[.col]]), .dat_out, stringsAsFactors = FALSE)
      
      #Same catch variable for free text fields
      if(length(unique(.dat[[.col]])) == 1 & .col %not_in% always_return){
        .dat_out$consensus_drop <- TRUE
      } else .dat_out$consensus_drop <- FALSE
      
      return(.dat_out)
      
    }

  })
  
  #Collapse the comparison list
  comparison_set <- purrr::compact(comparison_set)
  
  #If no differences found, including clinical notes, return NULL
  if(length(comparison_set) == 0) return(NULL)
  
  #Coerce to data frame
  comparison_set <- do.call(rbind.data.frame, comparison_set)
  
  
  #Final processing and the conclusion row
  
  #Temporary data frame dropping clinical notes (these are expected to differ and are always returned)
  #final_check <- comparison_set[comparison_set$Question %not_in% clin_notes,-c(1,ncol(comparison_set))]
  final_check <- comparison_set[comparison_set$Question %not_in% clin_notes,grep("Reviewer", colnames(comparison_set))]
  
  #Check if a review is blank by checking if all entries in a Reviewer's column were processed as ""
  empty_check <- do.call(c, 
                         #lapply(final_check[grep("(Syndromal|Numeric) Staging", comparison_set$Question),],    #This version only check staging with a grep
                         lapply(final_check,
                                function(xx){length(xx[xx==""]) == length(xx)}))
  #For any reviews that are all blank, increment to the first time "Reviewer" is found in the comparison_set datatable
  #Uses the index returned by grep then subtracts 1
  empty_check <- which(empty_check == TRUE)
  if(length(empty_check) > 0) empty_check <- empty_check + grep("Reviewer", colnames(comparison_set))[1] - 1
  
  #If no discrepancies, check if all non-clinical note entries are equal 
  if(sum(apply(final_check, 1, function(.row){length(unique(.row)) == 1})) == nrow(final_check)){
    conclusion <- c("NO DISCREPANCIES - CONFERENCE NOT NEEDED", rep("", ncol(comparison_set)-2))
    comparison_set <- comparison_set[-which(comparison_set$consensus_drop == TRUE),]
  
  #If idx_empty exists, an empty review probably exists
  } else if(length(empty_check) > 0){
    comparison_set[,empty_check] <- NA
    comparison_set[grep("Syndromal", comparison_set$Question),empty_check] <- "Review Appears Empty"
    conclusion <- c("A REVIEW APPEARS TO BE MISSING - PLEASE CHECK", rep("", ncol(comparison_set)-2))
  
  #Otherwise just indicate a discrepancy  
  } else {
    conclusion <- c("DISCREPANCY NOTED - PLEASE REVIEW", rep("", ncol(comparison_set)-2))
  }
  comparison_set <- comparison_set[,-which(colnames(comparison_set) == "consensus_drop")]
  names(conclusion) <- colnames(comparison_set)
  
  
  
  #Add the header and conclusion and return
  # comparison_header <- c("Reviewer Info", t(.dat$header))
  # names(comparison_header) <- colnames(comparison_set)
  # comparison_set <- rbind.data.frame(comparison_header, comparison_set, conclusion)
  comparison_set <- rbind.data.frame(comparison_set, conclusion)
  colnames(comparison_set)[-1] <- paste0(colnames(comparison_set)[-1], " ", t(gsub("NA  -  NA", "<br/>No Reviewer Details", .dat$header)))
  return(list(comparison_set))
}
  
  
  








#The first version which drops matching columns
comparison_make_origr <- function(.dat, .labs,
                             always_return = c("syndrm_stg", "numeric_stg"),
                             clin_notes = c("Notes or observations IN SUPPORT of a diagnosis",
                                            "Notes or observations AGAINST a diagnosis")){
  
  #Start by building the headers on reviewer and date
  .dat$header <- paste(.dat[["ex_ini"]], " - ", .dat[["form_dt"]])
  
  #So for each column...
  comparison_set <- lapply(colnames(.dat)[colnames(.dat) %not_in% c("Rev", "ex_ini", "form_dt", "header")], function(.col){
    
    #We include some 
    
    #For a first pass return NULL if everything is NA regardless of column type
    if(all(is.na(.dat[[.col]])) & .col %not_in% always_return) return(NULL)
    
    #Otherwise, first process non-free text fields
    if(length(grep(colnames_ccc_text_str, .col)) == 0){
      
      #Check if all values are the same, this will catch NA's as well
      #Return NULL if all elements agree
      if(length(unique(.dat[[.col]])) == 1 & .col %not_in% always_return){ return(NULL)
        
        #Otherwsie, build things out a bit
      } else{
        #Replace NA's with blanks
        .dat_out <- as.character(.dat[[.col]])
        .dat_out[is.na(.dat_out)] <- ""
        
        #Coerce to data frame
        .dat_out <- as.data.frame(t(.dat_out), stringsAsFactors = FALSE)
        colnames(.dat_out) <- paste("Reviewer", 1:ncol(.dat_out), sep = "")
        
        #Add the redcap label as the question and return
        .dat_out <- data.frame(Question = .labs[[.col]], .dat_out, stringsAsFactors=FALSE)
        return(.dat_out)
      }
      
      #If it is a free text field we basically follow the same process but we keep it separate just in case
    } else{
      #Replace empty strings
      .dat_out <- as.character(.dat[[.col]])
      .dat_out[is.na(.dat_out)] <- "No entry"
      
      #Coerce to data frame
      .dat_out <- data.frame(t(.dat_out), stringsAsFactors = FALSE)
      colnames(.dat_out) <- paste("Reviewer", 1:ncol(.dat_out), sep = "")
      
      #Add the redcap label as the question and return
      .dat_out <- data.frame(Question = as.character(.labs[[.col]]), .dat_out, stringsAsFactors = FALSE)
      return(.dat_out)
      
    }
    
  })
  
  #Collapse the comparison list
  comparison_set <- purrr::compact(comparison_set)
  
  #If no differences found, including clinical notes, return NULL
  if(length(comparison_set) == 0) return(NULL)
  
  #Coerce to data frame
  comparison_set <- do.call(rbind.data.frame, comparison_set)
  
  
  #Add a conclusion row, drop clinical notes and check if all remaining entries (which are always returned) are equal
  final_check <- comparison_set[comparison_set$Question %not_in% clin_notes,-1]
  if(sum(apply(final_check, 1, function(.row){length(unique(.row)) == 1})) == nrow(final_check)){
    conclusion <- c("NO DISCREPANCIES - CONFERENCE NOT NEEDED", rep("", ncol(comparison_set)-1))
  } else{
    conclusion <- c("DISCREPANCY NOTED - PLEASE REVIEW", rep("", ncol(comparison_set)-1))
  }
  names(conclusion) <- colnames(comparison_set)
  
  
  #Add the header and conclusion and return
  # comparison_header <- c("Reviewer Info", t(.dat$header))
  # names(comparison_header) <- colnames(comparison_set)
  # comparison_set <- rbind.data.frame(comparison_header, comparison_set, conclusion)
  comparison_set <- rbind.data.frame(comparison_set, conclusion)
  colnames(comparison_set)[-1] <- paste0(colnames(comparison_set)[-1], " ", t(.dat$header))
  return(list(comparison_set))
}



                          
