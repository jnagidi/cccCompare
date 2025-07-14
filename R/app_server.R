#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import data.table
#' @noRd
 
 

app_server <- function( input, output, session ) {
  
  #Starting table
  output$comparison_tables <- renderUI({
    tabsetPanel(
      id = "comparison_tabs",
      tabPanel("Ready", DT::renderDT(ready_table, options = list(dom = "t")))
    )
  })
  
  #Standard call to a modification of the initial_redcap_process of ADRCDash
  data_curr <- redcap_process()
  labels_curr <- data_curr[["labels"]]
  data_curr <- data_curr[["data"]]
  
  #Get the column sets for comparison as well as redcap labels
  ccc_cols <- get_ccc_cols(data_curr)
  #ccc_labels <- get_redcap_labels(data_curr, ccc_cols)
  labels_curr <- labels_curr[colnames(data_curr) %in% ccc_cols]
  labels_curr <- gsub("\\.{3}d+$", "", labels_curr)
  names(labels_curr) <- ccc_cols
  ccc_labels <- get_redcap_labels(labels_curr, ccc_cols)
  
  
  
  #Beging by showing the ready table
  output$comparison_tables <- renderUI(tabPanel(ready_table))
  
  #Everything else is held within the observe event button
  observeEvent(input$compare_button, {
    
    #Process the ID
    id_curr <- reactive({process_id(input$id_entry)})
    
    
    #Get the current ID of interest
    data_proc <- pull_id(data_curr, id_curr())
    
    #Return NULL table by default
    if(is.null(data_proc)){
      output$comparison_tables <- renderUI({
        tabsetPanel(
          id = "comparison_tabs",
          tabPanel("No Data", DT::renderDT(default_table, options = list(dom = "t")))
        )
      })
    } else{
      
      #Build the header for each event
      header_set <- build_header(data_proc)
      
      #Build the comparison table
      comparison_set <- make_comparison_set(data_proc, .cols_comp = ccc_cols, .labels = ccc_labels)
      
      #Build out the individual list entries into a tabsetpanel
      output$comparison_tables <- renderUI({
        tabsetPanel(
          id = "comparison_tabs",
          tabPanel("Comparison", make_compare_table(comparison_set, header_set))
        )
      })
    }
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  #So this kind of works but it only prints the final table - Better to just use the above tabsetPanel process
  
  # #Dynamically building out a variable number of tables requires some manipulation of renderUI
  # output$comparison_tables <- 
  #   renderUI({
  #     
  #     #Begin by using an lapply on an empty list that's as long as comparison_set / header_set
  #     lapply(as.list(seq_along(comparison_set)), function(ii){
  #       
  #       #Build a Dummy ID for reference
  #       tab_id <- paste0("DT", ii)
  #       
  #       #Pass the corresponding tableOutput to our primary output object
  #       tableOutput(tab_id)
  #       })
  #     })
  # 
  # #Then step though and render the datatable appropriately
  # for(ii in seq_along(comparison_set)){
  #   tab_id <- paste0("DT", ii)
  #   print(comparison_set[[ii]])
  #   output[[tab_id]] <- renderTable(comparison_set[[ii]])
  # }
    
  
}
