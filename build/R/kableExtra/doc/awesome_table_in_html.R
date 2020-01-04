## ------------------------------------------------------------------------
library(knitr)
library(kableExtra)
dt <- mtcars[1:5, 1:6]

## ------------------------------------------------------------------------
# If you are using kableExtra < 0.9.0, you are recommended to set a global option first.
# options(knitr.table.format = "html") 
## If you don't define format here, you'll need put `format = "html"` in every kable function.

## ------------------------------------------------------------------------
kable(dt)

## ------------------------------------------------------------------------
dt %>%
  kable() %>%
  kable_styling()

## ------------------------------------------------------------------------
kable(dt) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

## ------------------------------------------------------------------------
kable(dt) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

## ------------------------------------------------------------------------
kable(dt) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

## ------------------------------------------------------------------------
kable(dt) %>%
  kable_styling(bootstrap_options = "striped", full_width = F)

## ------------------------------------------------------------------------
kable(dt) %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "left")

## ------------------------------------------------------------------------
kable(dt) %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "float_right")

## ------------------------------------------------------------------------
kable(dt) %>%
  kable_styling(bootstrap_options = "striped", font_size = 7)

## ------------------------------------------------------------------------
kable(mtcars[1:10, 1:5]) %>%
  kable_styling(fixed_thead = T)

## ------------------------------------------------------------------------
text_tbl <- data.frame(
  Items = c("Item 1", "Item 2", "Item 3"),
  Features = c(
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Proin vehicula tempor ex. Morbi malesuada sagittis turpis, at venenatis nisl luctus a. ",
    "In eu urna at magna luctus rhoncus quis in nisl. Fusce in velit varius, posuere risus et, cursus augue. Duis eleifend aliquam ante, a aliquet ex tincidunt in. ", 
    "Vivamus venenatis egestas eros ut tempus. Vivamus id est nisi. Aliquam molestie erat et sollicitudin venenatis. In ac lacus at velit scelerisque mattis. "
  )
)

kable(text_tbl) %>%
  kable_styling(full_width = F) %>%
  column_spec(1, bold = T, border_right = T) %>%
  column_spec(2, width = "30em", background = "yellow")

## ------------------------------------------------------------------------
kable(dt) %>%
  kable_styling("striped", full_width = F) %>%
  column_spec(5:7, bold = T) %>%
  row_spec(3:5, bold = T, color = "white", background = "#D7261E")

## ------------------------------------------------------------------------
kable(dt) %>%
  kable_styling("striped", full_width = F) %>%
  row_spec(0, angle = -45)

## ---- message=FALSE, warning=FALSE---------------------------------------
library(dplyr)
mtcars[1:10, 1:2] %>%
  mutate(
    car = row.names(.),
    mpg = cell_spec(mpg, "html", color = ifelse(mpg > 20, "red", "blue")),
    cyl = cell_spec(cyl, "html", color = "white", align = "c", angle = 45, 
                    background = factor(cyl, c(4, 6, 8), 
                                        c("#666666", "#999999", "#BBBBBB")))
  ) %>%
  select(car, mpg, cyl) %>%
  kable(format = "html", escape = F) %>%
  kable_styling("striped", full_width = F)

## ------------------------------------------------------------------------
iris[1:10, ] %>%
  mutate_if(is.numeric, function(x) {
    cell_spec(x, bold = T, 
              color = spec_color(x, end = 0.9),
              font_size = spec_font_size(x))
  }) %>%
  mutate(Species = cell_spec(
    Species, color = "white", bold = T,
    background = spec_color(1:10, end = 0.9, option = "A", direction = -1)
  )) %>%
  kable(escape = F, align = "c") %>%
  kable_styling(c("striped", "condensed"), full_width = F)

## ------------------------------------------------------------------------
sometext <- strsplit(paste0(
  "You can even try to make some crazy things like this paragraph. ", 
  "It may seem like a useless feature right now but it's so cool ",
  "and nobody can resist. ;)"
), " ")[[1]]
text_formatted <- paste(
  text_spec(sometext, color = spec_color(1:length(sometext), end = 0.9),
            font_size = spec_font_size(1:length(sometext), begin = 5, end = 20)),
  collapse = " ")

# To display the text, type `r text_formatted` outside of the chunk

## ------------------------------------------------------------------------
popover_dt <- data.frame(
  position = c("top", "bottom", "right", "left"),
  stringsAsFactors = FALSE
)
popover_dt$`Hover over these items` <- cell_spec(
  paste("Message on", popover_dt$position), # Cell texts
  popover = spec_popover(
    content = popover_dt$position,
    title = NULL,                           # title will add a Title Panel on top
    position = popover_dt$position
  ))
kable(popover_dt, escape = FALSE) %>%
  kable_styling("striped", full_width = FALSE)

## ---- message = FALSE, warning=FALSE-------------------------------------
library(formattable)
mtcars[1:5, 1:4] %>%
  mutate(
    car = row.names(.),
    mpg = color_tile("white", "orange")(mpg),
    cyl = cell_spec(cyl, angle = (1:5)*60, 
                    background = "red", color = "white", align = "center"),
    disp = ifelse(disp > 200,
                  cell_spec(disp, color = "red", bold = T),
                  cell_spec(disp, color = "green", italic = T)),
    hp = color_bar("lightgreen")(hp)
  ) %>%
  select(car, everything()) %>%
  kable(escape = F) %>%
  kable_styling("hover", full_width = F) %>%
  column_spec(5, width = "3cm") %>%
  add_header_above(c(" ", "Hello" = 2, "World" = 2))

## ------------------------------------------------------------------------
kable(dt) %>%
  kable_styling("striped") %>%
  add_header_above(c(" " = 1, "Group 1" = 2, "Group 2" = 2, "Group 3" = 2))

## ------------------------------------------------------------------------
kable(dt) %>%
  kable_styling(c("striped", "bordered")) %>%
  add_header_above(c(" ", "Group 1" = 2, "Group 2" = 2, "Group 3" = 2)) %>%
  add_header_above(c(" ", "Group 4" = 4, "Group 5" = 2)) %>%
  add_header_above(c(" ", "Group 6" = 6))

## ------------------------------------------------------------------------
kable(mtcars[1:10, 1:6], caption = "Group Rows") %>%
  kable_styling("striped", full_width = F) %>%
  pack_rows("Group 1", 4, 7) %>%
  pack_rows("Group 2", 8, 10)

## ---- eval = F-----------------------------------------------------------
#  # Not evaluated. This example generates the same table as above.
#  kable(mtcars[1:10, 1:6], caption = "Group Rows") %>%
#    kable_styling("striped", full_width = F) %>%
#    pack_rows(index = c(" " = 3, "Group 1" = 4, "Group 2" = 3))

## ------------------------------------------------------------------------
kable(dt) %>%
  kable_styling("striped", full_width = F) %>%
  pack_rows("Group 1", 3, 5, label_row_css = "background-color: #666; color: #fff;")

## ---- eval=F-------------------------------------------------------------
#  # Method 1
#  pack_rows() # instead of group_rows()
#  
#  # Method 2
#  library(dplyr)
#  library(kableExtra)
#  
#  # Method 3
#  conflicted::conflict_prefer("group_rows", "kableExtra", "dplyr")

## ------------------------------------------------------------------------
kable(dt) %>%
  kable_styling("striped", full_width = F) %>%
  add_indent(c(1, 3, 5))

## ------------------------------------------------------------------------
collapse_rows_dt <- data.frame(C1 = c(rep("a", 10), rep("b", 5)),
                 C2 = c(rep("c", 7), rep("d", 3), rep("c", 2), rep("d", 3)),
                 C3 = 1:15,
                 C4 = sample(c(0,1), 15, replace = TRUE))
kable(collapse_rows_dt, align = "c") %>%
  kable_styling(full_width = F) %>%
  column_spec(1, bold = T) %>%
  collapse_rows(columns = 1:2, valign = "top")

## ------------------------------------------------------------------------
kable(dt, align = "c") %>%
  kable_styling(full_width = F) %>%
  footnote(general = "Here is a general comments of the table. ",
           number = c("Footnote 1; ", "Footnote 2; "),
           alphabet = c("Footnote A; ", "Footnote B; "),
           symbol = c("Footnote Symbol 1; ", "Footnote Symbol 2")
           )

## ------------------------------------------------------------------------
kable(dt, align = "c") %>%
  kable_styling(full_width = F) %>%
  footnote(general = "Here is a general comments of the table. ",
           number = c("Footnote 1; ", "Footnote 2; "),
           alphabet = c("Footnote A; ", "Footnote B; "),
           symbol = c("Footnote Symbol 1; ", "Footnote Symbol 2"),
           general_title = "General: ", number_title = "Type I: ",
           alphabet_title = "Type II: ", symbol_title = "Type III: ",
           footnote_as_chunk = T, title_format = c("italic", "underline")
           )

## ------------------------------------------------------------------------
dt_footnote <- dt
names(dt_footnote)[2] <- paste0(names(dt_footnote)[2], 
                                footnote_marker_symbol(1))
row.names(dt_footnote)[4] <- paste0(row.names(dt_footnote)[4], 
                                footnote_marker_alphabet(1))
kable(dt_footnote, align = "c", 
      # Remember this escape = F
      escape = F) %>%
  kable_styling(full_width = F) %>%
  footnote(alphabet = "Footnote A; ",
           symbol = "Footnote Symbol 1; ",
           alphabet_title = "Type II: ", symbol_title = "Type III: ",
           footnote_as_chunk = T)

## ------------------------------------------------------------------------
kable(cbind(mtcars, mtcars)) %>%
  kable_styling() %>%
  scroll_box(width = "500px", height = "200px")

## ------------------------------------------------------------------------
kable(cbind(mtcars, mtcars)) %>%
  add_header_above(c("a" = 5, "b" = 18)) %>%
  kable_styling() %>%
  scroll_box(width = "100%", height = "200px")

## ---- eval=FALSE---------------------------------------------------------
#  kable(mtcars) %>%
#    kable_styling() %>%
#    save_kable(file = "table1.html", self_contained = T)

## ---- eval=F-------------------------------------------------------------
#  # Not evaluating
#  xtable::xtable(mtcars[1:4, 1:4], caption = "Hello xtable") %>%
#    xtable2kable() %>%
#    column_spec(1, color = "red")

