
library(migrate)
library(gt)
library(tibble)
library(dplyr)
library(scales)


# Load package data
data(mock_credit)

# Calculate the migration
migration <- mock_credit %>% 
  migrate::migrate(
    id = customer_id, 
    time = date, 
    state = risk_rating, 
    metric = principal_balance, 
    verbose = FALSE
  )

matrix <- migration %>% 
  # Build the migration matrix
  migrate::build_matrix(
    state_start = risk_rating_start, 
    state_end = risk_rating_end, 
    metric = principal_balance
  ) %>% 
  tibble::as_tibble(rownames = NA) 


gt <- matrix %>% 
  gt::gt(
    rownames_to_stub = TRUE
  )


green_values <- migration %>% 
  dplyr::filter(risk_rating_start > risk_rating_end) %>% 
  dplyr::pull(principal_balance) %>% 
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

red_values <- migration %>% 
  dplyr::filter(risk_rating_start < risk_rating_end) %>% 
  dplyr::pull(principal_balance) %>% 
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

# Format all the zero values as filled white

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

gt <- gt %>% 
  gt::fmt(
    columns = gt::everything(), 
    fns = function(x) ifelse(x == 0, zero_replace, scales::percent(x, accuracy = 0.01))
  )


migrate_hex_md <- paste0(
  "<a href=https://github.com/mthomas-ketchbrook/migrate#migrate->", 
  "<img src='https://raw.githubusercontent.com/mthomas-ketchbrook/migrate/master/man/figures/logo.png' ", 
  "style='height:60px; float:left; vertical-align:middle;'>", 
  "</a>"
)

gt_hex_md <- paste0(
  "<a href=https://gt.rstudio.com/>", 
  "<img src='https://raw.githubusercontent.com/rstudio/gt/master/man/figures/logo.svg' ", 
  "style='height:60px; float:right; vertical-align:middle;'>", 
  "</a>"
)

out <- gt %>% 
  gt::tab_spanner(
    label = "ENDING RISK RATING", 
    columns = -1
  ) %>% 
  gt::tab_stubhead(
    label = "STARTING RISK RATING"
  ) %>% 
  gt::tab_header(
    title = gt::md(
      paste(
        migrate_hex_md,
        "**Risk Rating Migration**", 
        gt_hex_md, 
        "<br>*2021-06-30* &#10145;&#65039; *2021-09-30*"
      )
    )
  ) %>% 
  gt::tab_style(
    style = list(
      gt::cell_text(align = "center")
    ),
    locations = gt::cells_stub(rows = TRUE)
  ) %>% 
  gt::tab_options(
    heading.background.color = "#627D9F", 
    stub.background.color = "#343635", 
    column_labels.background.color = "#343635"
  )

out

gt::gtsave(
  out, 
  filename = "final_gt.html"
)
