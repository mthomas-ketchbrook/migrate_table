

library(migrate)
library(gt)
library(tibble)
library(gtExtras)
library(dplyr)
library(scales)


library(RSocrata)

query_cols <- paste(
  "town", 
  "rating_date", 
  "bond_rating", 
  sep = ", "
)

df <- RSocrata::read.socrata(
  url = glue::glue(
    "https://data.ct.gov/resource/3w9d-7jbi.csv?", 
    "$where=rating_agency like '%Moody%'&",   # filter for only Moody's Ratings
    "$select={query_cols}"   # select only desired columns
  )
)

moodys_ratings <- c(
  ""
)

clean_df <- df %>% 
  dplyr::mutate(rating_date = as.Date(rating_date)) %>% 
  dplyr::arrange(town, rating_date) %>% 
  dplyr::distinct(town, rating_date, .keep_all = TRUE) %>% 
  dplyr::group_by(town) %>% 
  dplyr::filter(dplyr::n() == 2) %>% 
  dply

clean_df %>% 
  migrate::migrate(
    id = town, 
    time = date, 
    state = bond_rating
  )



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
  gt::gt(
    rownames_to_stub = TRUE
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
  palette = c("#f2ffed", "green"), 
  domain = range(min(green_values), max(green_values))
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


# Red ("Bad") Formatting -------------------------------------------------

red_values <- mock_credit %>% 
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


red_pal <- scales::col_numeric(
  palette = c("#ffe7e6", "#ff746b"), 
  domain = range(min(red_values), max(red_values))
)

for (i in 2:(ncol(tbl))) {
  
  for (j in (1:(i - 1))) {
    
    cur_val <- as.data.frame(tbl)[j, i]
    
    gt <- gt %>%
      gt::tab_style(
        style = gt::cell_fill(
          color = as.character(red_pal(cur_val)),
        ),
        locations = gt::cells_body(
          columns = names(tbl)[i],
          rows = j
        )
      )
    
  }
  
}


for (i in 1:ncol(tbl)) {
  
  gt <- gt %>% 
    gt::tab_style(
      style = gt::cell_fill(color = "white"),
      locations = gt::cells_body(
        columns = names(tbl)[i],
        rows = eval(parse(text = paste0(names(tbl)[i], " == 0")))
      )
    )
  
}

gt

# Format as Percentages ---------------------------------------------------

zero_replace <- paste(
  rep("-", 6), 
  collapse = ""
)

gt <- gt %>% 
  gt::fmt(
    columns = gt::everything(), 
    fns = function(x) ifelse(x == 0, zero_replace, scales::percent(x, accuracy = 0.01))
  )

gt 