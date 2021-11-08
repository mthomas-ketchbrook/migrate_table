# Got my inspiration from this SO post:
# https://stackoverflow.com/questions/63944953/how-to-conditionally-format-a-cell-in-a-gt-table-based-on-the-value-of-the-cel


add_conditional_format <- function(migrated_data, matrix_data) {
  
  state_start_name <- colnames(migrated_data)[1]
  state_end_name <- colnames(migrated_data)[2]
  metric_name <- colnames(migrated_data)[3]
  
  # Capture the unique values representing decreased risk movement
  green_values <- migrated_data %>% 
    dplyr::filter(.data[[state_start_name]] > .data[[state_end_name]]) %>% 
    dplyr::pull(.data[[metric_name]]) %>% 
    unique() 
 
  # Create a color palette function that goes scales shades of green for the
  # range of values in 'green_values'
  green_pal <- scales::col_numeric(
    palette = c("#f2ffed", "green"), 
    domain = range(min(green_values), max(green_values))
  )
  
  # Add green color scale fill for risk decrease movement
  for (i in 1:(ncol(matrix_data) - 1)) {
    
    for (j in (i + 1):(nrow(matrix_data))) {
      
      cur_val <- as.data.frame(matrix_data)[j, i]
      
      gt <- gt %>%
        gt::tab_style(
          style = gt::cell_fill(
            color = as.character(green_pal(cur_val)),
          ),
          locations = gt::cells_body(
            columns = names(matrix_data)[i],
            rows = j
          )
        )
      
    }
    
  }
  
  # Capture the unique values representing increased risk movement
  red_values <- migrated_data %>% 
    dplyr::filter(.data[[state_start_name]] < .data[[state_end_name]]) %>% 
    dplyr::pull(.data[[metric_name]]) %>% 
    unique()
  
  # Create a color palette function that goes scales shades of red for the range
  # of values in 'red_values'
  red_pal <- scales::col_numeric(
    palette = c("#ffe7e6", "#ff746b"), 
    domain = range(min(red_values), max(red_values))
  )
  
  # Add red color scale fill for risk increase movement
  for (i in 2:(ncol(matrix_data))) {
    
    for (j in (1:(i - 1))) {
      
      cur_val <- as.data.frame(matrix_data)[j, i]
      
      gt <- gt %>%
        gt::tab_style(
          style = gt::cell_fill(
            color = as.character(red_pal(cur_val)),
          ),
          locations = gt::cells_body(
            columns = names(matrix_data)[i],
            rows = j
          )
        )
      
    }
    
  }
  
  # Format all the zero values as filled white
  for (i in 1:ncol(matrix)) {
    
    gt <- gt %>% 
      gt::tab_style(
        style = gt::cell_fill(color = "white"),
        locations = gt::cells_body(
          columns = names(matrix_data)[i],
          rows = eval(parse(text = paste0(names(matrix_data)[i], " == 0")))
        )
      )
    
  }
  
  return(gt)
  
}
