

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
  )

source("R/fmt_migrate.R")

gt <- gt %>% 
  fmt_migrate(
    migrated_data = mig, 
    matrix_data = matrix
  )

gt <- gt %>% 
  gt::tab_spanner(
    label = "ENDING BOND RATING", 
    columns = -1
  ) %>% 
  gt::tab_stubhead(
    label = "STARTING BOND RATING"
  ) %>% 
  gt::tab_header(
    title = "Bond Rating Migration",  
    subtitle = "2019-07-01 --> 2021-01-01"
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


zero_replace <- paste(
  rep("-", 3), 
  collapse = ""
)

gt <- gt %>% 
  gt::fmt(
    columns = gt::everything(), 
    fns = function(x) ifelse(x == 0, zero_replace, x)
  )

gt
