#Dictionary for race, includes the seven known race variables plus an entry for multiples
#Gives long and short strings
race_dict <-
  list(raceaaian = 
         c(var = "raceaian",
           long = "American Indian or Alaskan Native",
           short = "AmrInd"),
       raceasian = 
         c(var = "raceasian",
           long = "Asian",
           short = "Asian"),
       raceblack = 
         c(var = "raceblack",
           long = "Black or African American",
           short = "B/AA"),
       ethispanic = 
         c(var = "ethispanic",
           long = "Hispanic",
           short = "Hispnc"),
       racemena = 
         c(var = "racemena",
           long = "Middle Eastern or North African",
           short = "MENA"),
       racenhpi =
         c(var = "racenhpi",
           long = "Native Hawaiian or Pacific Islander",
           short = "PacIsle"),
       racewhite = 
         c(var = "racewhite",
           long = "White",
           short = "White"),
       raceunkn =
         c(var = "raceunkn",
           long = "Unknown",
           short = "Unk"),
       racemult = 
         c(var = NA,
           long = "More than 1 race",
           short = "More1")
  )


#Function for making race variable
# .vec - expected vector from UDS4
# .type - whether to use long or short entries
# .simplify - Boolean where isTRUE then return the racemult entry
# .dict - race_dict
# .sep - If not simplifying, what to collapse the mulitiple races on
#If we were cleaner we'd also have a check for Unknown but we consider it to not be mutually exclusive
make_race_var <- function(.vec, .type, .simplify = FALSE, .dict = race_dict, .sep=" | "){
  
  #Filter dictionary to be those listed in the dataset
  .dict <- .dict[names(.dict) %in% names(.vec)]
  
  #Type check
  if(!.type %in% c("short", "long")) stop("Pick \"short\" or \"long\" for a string option")
  
  #Get the variable names and build the vector of names for short/long
  .race_names <- na.omit(do.call(c, lapply(.dict, \(xx){xx[["var"]]})))
  .race_labels <- do.call(c, lapply(.dict, \(xx){xx[[.type]]}))
  .race_labels <- .race_labels[!names(.race_labels) %in% "racemult"]
  
  #Limit to those colnames
  if(is.data.table(.vec)){.vec <- .vec[,...race_names]
  } else .vec <- .vec[,.race_names, drop=FALSE]
  
  #Easy simplify return is 
  if(isTRUE(.simplify) && sum(as.numeric(unlist(.vec))) > 1) return(.dict[["racemult"]][[.type]])
  
  #Otherwise, build the labels with a logical vector
  #.out <- .race_labels[as.logical(unlist(.vec))]
  .out <- .race_labels[as.logical(as.numeric(unlist(.vec)))]
  .out <- .out[!is.na(.out) & .out != "NA"]
  
  
  #Apply the collapse and return
  if(length(.out) > 1) .out <- paste(.out, collapse=.sep)
  return(.out)
}