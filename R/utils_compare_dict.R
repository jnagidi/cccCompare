#'
#'
#' Comparison dictionaries and functions to build dictionaries
#'

#Static variables
id_var <- c("adc_sub_id")
event_var <- c("redcap_repeat_instance")
demog_cols <- c("Age", "race", "birthsex", "educ")
reviewer_cols <- c("frmdated1a_rev1", "frmdated1a_rev2", "initialsd1a_rev1", "initialsd1a_rev2")


#Dictionaries for build_header
header_columns_dict <- list(col_var = c(id_var, event_var, "Age", "race", "birthsex", "educ"),
                            numeric_var = c(rep(FALSE, 5), TRUE))

race_recode <- c("Black or African American" = "B/AA", "White" = "W")




#D1 colnames from data dictionary
#Variant using naccDataDict
# colnames_ccc <- paste0("ccc_", c("cogstat_c2", naccDataDict::redcap_nacc_data[["IVP"]][["D1"]], 
#                                  "clin_notes_supp", "clin_notes_anti", "syndrm_stg", "numeric_stg"), "_rev\\d*")
# colnames_ccc_text_str <- "clin_notes|((oth|ftld).*?x)"

#Since naccDataDict is only called here, we're just calling the D1 names here to avoid dispendencies
D1_redcap_names=
  c("dxmethod", "normcog", "scd", "scddxconf", "demented", "mcicritcln", "mcicritimp", "mcicritfun", "mci ", 
    "impnomcifu", "impnomcicg", "impnomclcd", "impnomcio", "impnomciox", "impnomci", 
    "cdommem ", "cdomlang", "cdomattn ", "cdomexec", "cdomvisu", "cdombeh", "cdomaprax", 
    "mbi", "bdommot", "bdomafreg", "bdomimp", "bdomsocial", "bdomthts", "predomsyn", 
    "amndem", "dyexecsyn", "pca", "ppasyn", "ppasynt", "ftdsyn", "lbdsyn", "lbdsynt", 
    "namndem", "pspsyn", "pspsynt", "ctesyn", "cbssyn", "msasyn", "msasynt", "othsyn", "othsynx", 
    "syninfclin", "syninfctst", "syninfbiom", "majdepdx", "majdepdif", "othdepdx", "othdepdif", 
    "bipoldx", "bipoldif", "schizop", "schizoif", "anxiet", "anxietif", "genanx", "panicdisdx", "ocddx", "othanxd", "othanxdx", 
    "ptsddx", "ptsddxif", "ndevdis", "ndevdisif", "delir", "delirif", "othpsy", "othpsyif", "othpsyx", "tbidx", "tbidxif", 
    "epilep", "epilepif", "hyceph", "hycephif", "neop", "neopif", "neopstat", "hiv", "hivif", "postc19", "postc19if", "apneadx", "apneadxif", 
    "othcogill", "othcillif ", "othcogillx", "alcdem", "alcdemif", "impsub", "impsubif", "meds", "medsif", "cogoth", "cogothif", 
    "cogothx", "cogoth2", "cogoth2f", "cogoth2x", "cogoth3", "cogoth3f", "cogoth3x", 
    "alzdis", "alzdisif", "lbdis", "lbdif", "ftld", "psp", "pspif", "cort", "cortif", "ftldmo", "ftldmoif", "ftldnos", "ftldnoif", "ftldsubt", "ftldsubx", 
    "cvd", "cvdif", "msa", "msaif", "cte", "cteif", "ctecert", "downs", "downsif", "hunt", "huntif", "prion", "prionif", "caa", "caaif", "late", "lateif", 
    "othcog", "othcogif", "othcogx")
  
  # c("dxmethod", "normcog", "scd", "scddxconf", "demented", "amndem", "pca", "ppasyn", "ppasynt", "ftdsyn", "lbdsyn", "namndem",
  #    "mciamem", "mciaplus", "mciaplan", "mciapatt", "mciapex", "mciapvis", "mcinon1", "mcin1lan", "mcin1att", "mcin1ex", "mcin1vis", "mcinon2", "mcin2lan", "mcin2att", "mcin2ex", "mcin2vis", "impnomci",
  #    "amylpet", "amylcsf", "fdgad", "hippatr", "taupetad", "csftau", "fdgftld", "tpetftld", "mrftld", "datscan", "othbiom", "othbiomx", "imaglinf", "imaglac", "imagmach", "imagmich", "imagmwmh", "imagewmh", "admut", "ftldmut", "othmut", "othmutx",
  #    "alzdis", "alzdisif", "lbdis", "lbdif", "park", "msa", "msaif", "psp", "pspif", "cort", "cortif", "ftldmo", "ftldmoif", "ftldnos", "ftldnoif", "ftldsubt", "ftldsubx",
  #    "cvd", "cvdif", "prevstk", "strokedec", "stkimag", "infnetw", "infwmh", "esstrem", "esstreif", "downs", "downsif", "hunt", "huntif", "prion", "prionif",
  #    "brninj", "brninjif", "brnincte", "hyceph", "hycephif", "epilep", "epilepif", "neop", "neopif", "neopstat", "hiv", "hivif", "othcog", "othcogif", "othcogx",
  #    "dep", "depif", "deptreat", "bipoldx", "bipoldif", "schizop", "schizoif", "anxiet", "anxietif", "delir", "delirif", "ptsddx", "ptsddxif", "othpsy", "othpsyif", "othpsyx",
  #    "alcdem", "alcdemif", "alcabuse", "impsub", "impsubif", "dysill", "dysillif", "meds", "medsif", "cogoth", "cogothif", "cogothx", "cogoth2", "cogoth2f", "cogoth2x", "cogoth3", "cogoth3f", "cogoth3x")

colnames_ccc <- paste0(c("cogstat_c2", D1_redcap_names, 
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
                              rename_string_in = "(.*?)_rev1$", 
                              rename_string_out = "\\1"){
  
  #Extract labels
  if(is.data.frame(.dat)){
    labels_curr <- do.call(c, lapply(.dat[,colnames(.dat) %in% redcap_cols], function(.var){attributes(.var)$label}))
  } else labels_curr <- .dat
  
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
  if(.id < 10){ .id <- paste0("000", .id)
  } else if(.id < 100){ .id <- paste0("00", .id)
  } else if(.id < 1000){ .id <- paste0("0", .id)}
  
  return(paste0(.head, .id))
  
}



#Dictionary of variables to copy from one event to another
dict_copy <- c("adc_sub_id", "frmdatea1", "birthmo", "birthyr", "raceaian", "raceasian", "raceblack", "ethispanic", "racemena", "racenhpi", "racewhite", "birthsex", "educ")
