#' Conditionally Format a State Transition Matrix
#'
#' @description `fmt_migrate()` adds green and red color scale 
#'   formatting (similar to what you might see in Excel) to a `gt` table that 
#'   displays the output of `migrate::build_matrix()`
#'
#' @param gt The `gt` table to which the conditional formatting will be applied 
#' @param migrated_data The data frame output from `migrate::migrate()`
#' @param matrix_data The matrix object output from `migrate::build_matrix()`
#'
#' @details 
#' In risk management (especially in finance) a popular type of tabular analysis
#'   called a *"state transition matrix"* or *"state migration matrix"* is used
#'   to show movement of risk in a portfolio from one time point to another. 
#'   Calculating this risk movement and developing these matrices is the goal of 
#'   the **{migrate}** package.
#'   
#' To beautify the output of `migrate::build_matrix()` and make it presentation-
#'   ready, the **{gt}** package can serve as a great companion. However, 
#'   applying the conditional formatting correctly can be tricky.
#' 
#' The inspiration for this approach (using nested 'for' loops to specify the
#'   exact cells in the table & associated color scale values to apply to each
#'   cell) was from edited answer to this Stack Overflow post:
#'   https://stackoverflow.com/questions/63944953/how-to-conditionally-format-a-cell-in-a-gt-table-based-on-the-value-of-the-cel
#'
#' @return
#' A `gt` table with the conditional formatted (as described in *Description*)
#'   applied
#'
#' @examples
#' \dontrun{
#' 
#'   library(migrate)
#'   data(mock_credit)
#'   
#'   # Calculate the migration
#'   migration <- mock_credit %>% 
#'     migrate::migrate(
#'       id = customer_id, 
#'       time = date, 
#'       state = risk_rating, 
#'       metric = principal_balance, 
#'       verbose = FALSE
#'     )
#'   
#'   matrix <- migration %>% 
#'     # Build the migration matrix
#'     migrate::build_matrix(
#'       state_start = risk_rating_start, 
#'       state_end = risk_rating_end, 
#'       metric = principal_balance
#'     ) %>% 
#'     tibble::as_tibble(rownames = NA) 
#'   
#'   
#'   gt <- matrix %>% 
#'     gt::gt(
#'       rownames_to_stub = TRUE
#'     )
#'     
#'   gt %>% 
#'     fmt_migrate(
#'       migrated_data = migration, 
#'       matrix_data = matrix
#'     )
#'   
#' }
fmt_migrate <- function(gt, migrated_data, matrix_data) {
  
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
