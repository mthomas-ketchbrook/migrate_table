

library(migrate)
library(gt)
library(tibble)
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

# https://www.moodys.com/sites/products/productattachments/ap075378_1_1408_ki.pdf
moodys_ratings <- c(
  "Aaa", "Aa1", "Aa2", "Aa3", "A1", "A2", "A3", 
  "Baa1", "Baa2", "Baa3", "Ba1", "Ba2", "Ba3", "B1", "B2", "B3", 
  "Caa1", "Caa2", "Caa3", "Ca", "C"
)

clean_df <- df %>% 
  dplyr::mutate(
    rating_date = as.Date(rating_date), 
    bond_rating = factor(bond_rating, levels = moodys_ratings, ordered = TRUE)
  ) %>% 
  dplyr::arrange(town, rating_date) %>% 
  dplyr::distinct(town, rating_date, .keep_all = TRUE)

clean_df %>% 
  migrate::migrate(
    id = town, 
    time = rating_date, 
    state = bond_rating
  )



# Load package data
# data(mock_credit)

# Calculate the migration
mig <- clean_df %>% 
  migrate::migrate(
    id = town, 
    time = rating_date, 
    state = bond_rating, 
    percent = FALSE
  )

matrix <- mig %>% 
  # Build the migration matrix
  migrate::build_matrix(
    state_start = bond_rating_start, 
    state_end = bond_rating_end, 
    metric = count
  ) %>% 
  tibble::as_tibble(rownames = NA) 


gt <- matrix %>% 
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
  ) %>% 
  gt::fmt_missing(
    columns = gt::everything(), 
    missing_text = 0
  )


green_values <- mig %>% 
  dplyr::filter(bond_rating_start > bond_rating_end) %>% 
  dplyr::pull(count) %>% 
  unique() 


green_pal <- scales::col_numeric(
  palette = c("#f2ffed", "green"), 
  domain = range(min(green_values), max(green_values))
)



for (i in 1:(ncol(matrix) - 1)) {
  
  for (j in (i + 1):(nrow(matrix))) {
    
    cur_val <- as.data.frame(matrix)[j, i]
    
    gt <- gt %>%
      gt::tab_style(
        style = gt::cell_fill(
          color = as.character(green_pal(cur_val)),
        ),
        locations = gt::cells_body(
          columns = names(matrix)[i],
          rows = j
        )
      )
    
  }
  
}


# Red ("Bad") Formatting -------------------------------------------------

red_values <- mig %>% 
  dplyr::filter(bond_rating_start < bond_rating_end) %>% 
  dplyr::pull(count) %>% 
  unique() 


red_pal <- scales::col_numeric(
  palette = c("#ffe7e6", "#ff746b"), 
  domain = range(min(red_values), max(red_values))
)

for (i in 2:(ncol(matrix))) {
  
  for (j in (1:(i - 1))) {
    
    cur_val <- as.data.frame(matrix)[j, i]
    
    gt <- gt %>%
      gt::tab_style(
        style = gt::cell_fill(
          color = as.character(red_pal(cur_val)),
        ),
        locations = gt::cells_body(
          columns = names(matrix)[i],
          rows = j
        )
      )
    
  }
  
}


for (i in 1:ncol(matrix)) {
  
  gt <- gt %>% 
    gt::tab_style(
      style = gt::cell_fill(color = "white"),
      locations = gt::cells_body(
        columns = names(matrix)[i],
        rows = eval(parse(text = paste0(names(matrix)[i], " == 0")))
      )
    )
  
}

gt

# Format as Percentages ---------------------------------------------------

zero_replace <- paste(
  rep("-", 6), 
  collapse = ""
)

gt_num <- gt %>% 
  gt::fmt(
    columns = gt::everything(), 
    fns = function(x) ifelse(x == 0, zero_replace, x)
  )

gt_num

gt_pct <- gt %>% 
  gt::fmt(
    columns = gt::everything(), 
    fns = function(x) ifelse(x == 0, zero_replace, scales::percent(x, accuracy = 0.01))
  )

gt 