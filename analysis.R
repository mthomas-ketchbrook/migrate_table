

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
  ) # %>% 
# gt::data_color(
#   columns = c(AAA, AA), 
#   colors = scales::col_numeric(
#     palette = c("red", "white"),
#     domain = NULL
#   )
# ) %>% 
# gt::tab_style(
#   style = gt::cell_fill(color = "red"), 
#   locations = gt::cells_body(
#     columns = "AAA", 
#     rows = 2:5
#   )
# )



for (i in 1:(ncol(tbl) - 1)) {
  
  # cur_col_name <- names(tbl)[i]
  
  gt <- gt %>% 
    gt::tab_style(
      style = gt::cell_fill(color = "red"),
      locations = gt::cells_body(
        columns = names(tbl)[i],
        rows = (i + 1):7
      )
    ) %>% 
    gt::tab_style(
      style = gt::cell_fill(color = "white"),
      locations = gt::cells_body(
        columns = names(tbl)[i],
        rows = eval(parse(text = paste0(names(tbl)[i], " == 0")))
      )
    )
  
}

gt

for (i in 2:(ncol(tbl))) {
  
  # cur_col_name <- names(tbl)[i]
  
  gt <- gt %>% 
    gt::tab_style(
      style = gt::cell_fill(color = "green"),
      locations = gt::cells_body(
        columns = names(tbl)[i],
        rows = 1:(i - 1)
      )
    ) %>% 
    gt::tab_style(
      style = gt::cell_fill(color = "white"),
      locations = gt::cells_body(
        columns = names(tbl)[i],
        rows = eval(parse(text = paste0(names(tbl)[i], " == 0")))
      )
    )
  
}




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

red_pal <- colorRamp(c("red", "white"))

colorRampPalette(c("blue", "red"))

library(RColorBrewer)

cols <- RColorBrewer::brewer.pal(
  name = "Reds", 
  n = length(red_values)
)

mock_credit$risk_rating[1] > mock_credit$risk_rating[2]


