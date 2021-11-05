# Got my inspiration from this SO post:
# https://stackoverflow.com/questions/63944953/how-to-conditionally-format-a-cell-in-a-gt-table-based-on-the-value-of-the-cel


color_fill_fn <- function(gt, data, column) {
  
  names(data)
  
  gt %>% 
    gt::tab_style(
      style = gt::cell_fill(color = "red"), 
      locations = gt::cells_body(
        columns = "AAA", 
        rows = 2:5
      )
    )
  
}