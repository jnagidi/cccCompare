#'
#'
#'
#'
#'


build_DT_options <- function(.tab){
  
  if(max(nchar(.tab[,1])) > 75){
    width_1 = "50%"
    width_2 = "25%"
  } else{
    width_1 = "36%"
    width_2 = "32%"
  }
  
  DT_options = 
    list(
      autoWidth = TRUE,
      dom = "t", 
      columnDefs = list(list(width = width_1, targets = 0, searchable = FALSE),
                        list(className = "dt-center", width = width_2, targets = c(1:(ncol(.tab)-1)), searchable = FALSE))
      
      
      
      
      
    )
  
  return(DT_options)
}


#Note since we're using bootstrap we have to define "dt-center" within the CSS document
#see https://datatables.net/reference/option/dom for details



make_compare_table <- function(.tables, .header){
  
  #We make a tab for each event
  .tab_out <- purrr::map(names(.tables), ~tabPanel(.x, renderUI(HTML(create_header_details(.x, .header))),
                                                   DT::renderDT(
                                                     create_table_details(.x, .tables),
                                                     options = build_DT_options(.tables[[.x]]),
                                                     escape = FALSE
                                                   )
  ))
  
  #Use do.call to create the tabsetPanel
  do.call(tabsetPanel, append(.tab_out, list(type = "tabs")))
  
}




create_table_details <- function(.event, .tab){
  
  #Get the table of the specific event
  .tab_curr <- .tab[[.event]]
  
  return(.tab_curr)
  
  
  
}


create_header_details <- function(.event, .head){
  
  .head_curr <- .head[[.event]]
  .head_curr <- paste0("<span class=\"header_style\">", .head_curr, "</span><br>")
  return(.head_curr)
  
}

