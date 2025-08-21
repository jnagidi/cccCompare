#'
#'
#'
#'
#'


build_DT_options <- function(.tab){
  
  question_col_width <- if(max(nchar(.tab[,1])) > 75) "15%" else "15%"
  question_content_width <- "45%"  # Give more space to question content
  reviewer_col_width <- "20%"      # Equal width for reviewer columns
  
  DT_options = 
    list(
      pageLength = 50,
      autoWidth = FALSE,  # Disable autoWidth to use our custom widths
      dom = "t", 
      columnDefs = list(
        list(width = question_col_width, targets = 0, searchable = FALSE),
        list(width = question_content_width, targets = 1, searchable = FALSE, className = "dt-left"),
        list(width = reviewer_col_width, targets = c(2:(ncol(.tab)-1)), searchable = FALSE, className = "dt-center")
      ),
      scrollX = TRUE,  # Enable horizontal scrolling if needed
      scrollCollapse = TRUE
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

