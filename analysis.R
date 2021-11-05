

library(migrate)
library(gt)
library(tibble)
library(gtExtras)

# Load package data
data(mock_credit)

# Calculate the migration
tbl <- mock_credit %>% 
  migrate::migrate(
    id = customer_id, 
    time = date, 
    state = risk_rating, 
    metric = principal_balance, 
    verbose = FALSE
  ) %>% 
  # Build the migration matrix
  migrate::build_matrix(
    state_start = risk_rating_start, 
    state_end = risk_rating_end, 
    metric = principal_balance
  ) %>% 
  tibble::as_tibble(rownames = NA) 


gt <- tbl %>% 
  # tibble::rownames_to_column(var = "Start") %>% 
  # dplyr::mutate(row_label = "STARTING RISK RATING") %>% 
  gt::gt(
    # rowname_col = "Start", 
    rownames_to_stub = TRUE
    # groupname_col = "row_label"
  ) %>% 
  gt::tab_spanner(
    label = "ENDING RISK RATING", 
    columns = -1
  ) %>% 
  gt::tab_stubhead(
    label = "STARTING RISK RATING"
  ) %>% 
  gt::tab_header(
    title = "Risk Rating Migration",  
    subtitle = "2021-06-30 --> 2021-09-30"
  ) %>% 
  gt::tab_style(
    style = list(
      gt::cell_text(align = "center")
    ),
    locations = gt::cells_stub(rows = TRUE)
  )


green_values <- mock_credit %>% 
  migrate::migrate(
    id = customer_id, 
    time = date, 
    state = risk_rating, 
    metric = principal_balance, 
    verbose = FALSE
  ) %>% 
  dplyr::filter(risk_rating_start > risk_rating_end) %>% 
  dplyr::pull(principal_balance) %>% 
  unique() 


green_pal <- scales::col_numeric(
  palette = c("white", "green"), 
  domain = range(0, max(green_values))
)



for (i in 1:(ncol(tbl) - 1)) {
  
  for (j in (i + 1):(nrow(tbl))) {
    
    cur_val <- as.data.frame(tbl)[j, i]
    
    gt <- gt %>%
      gt::tab_style(
        style = gt::cell_fill(
          color = as.character(green_pal(cur_val)),
        ),
        locations = gt::cells_body(
          columns = names(tbl)[i],
          rows = j
        )
      )

  }
  
}


# Pink ("Bad") Formatting -------------------------------------------------

pink_values <- mock_credit %>% 
  migrate::migrate(
    id = customer_id, 
    time = date, 
    state = risk_rating, 
    metric = principal_balance, 
    verbose = FALSE
  ) %>% 
  dplyr::filter(risk_rating_start < risk_rating_end) %>% 
  dplyr::pull(principal_balance) %>% 
  unique() 


pink_pal <- scales::col_numeric(
  palette = c("white", "pink"), 
  domain = range(0, max(pink_values))
)

for (i in 2:(ncol(tbl))) {
  
  for (j in (1:(i - 1))) {
    
    cur_val <- as.data.frame(tbl)[j, i]
    
    gt <- gt %>%
      gt::tab_style(
        style = gt::cell_fill(
          color = as.character(pink_pal(cur_val)),
        ),
        locations = gt::cells_body(
          columns = names(tbl)[i],
          rows = j
        )
      )
    
  }
  
}



# Format as Percentages ---------------------------------------------------

zero_replace <- paste(
  rep("-", 6), 
  collapse = ""
)

gt %>% 
  gt::fmt(
    columns = gt::everything(), 
    fns = function(x) ifelse(x == 0, zero_replace, scales::percent(x, accuracy = 0.01))
  )

