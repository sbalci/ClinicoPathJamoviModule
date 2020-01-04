## ---- echo = F-----------------------------------------------------------
options(kableExtra.latex.load_packages = F)

## ------------------------------------------------------------------------
library(knitr)
library(kableExtra)
dt <- mtcars[1:5, 1:6]

## ------------------------------------------------------------------------
# If you are using kableExtra < 0.9.0, you are recommended to set a global option first.
# options(knitr.table.format = "latex") 
## If you don't define format here, you'll need put `format = "latex"` 
## in every kable function.

## ---- eval = FALSE-------------------------------------------------------
#  # Not evaluated. Ilustration purpose
#  options(kableExtra.latex.load_packages = FALSE)
#  library(kableExtra)

## ------------------------------------------------------------------------
# Again, with kableExtra >= 0.9.0, `format = "latex"` is automatically defined
# when this package gets loaded. Otherwise, you still need to define formats
kable(dt, "latex")
# Same: kable(dt, "latex")

## ------------------------------------------------------------------------
kable(dt, "latex", booktabs = T)

## ------------------------------------------------------------------------
kable(dt, "latex", booktabs = T) %>%
  kable_styling(latex_options = "striped")

## ------------------------------------------------------------------------
kable(mtcars[1:8, 1:4], "latex", booktabs = T, linesep = "") %>%
  kable_styling(latex_options = "striped", stripe_index = c(1,2, 5:6))

## ------------------------------------------------------------------------
kable(dt, "latex", caption = "Demo table", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "hold_position"))

## ------------------------------------------------------------------------
kable(cbind(dt, dt, dt), "latex", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "scale_down"))

## ------------------------------------------------------------------------
kable(cbind(dt), "latex", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "scale_down"))

## ------------------------------------------------------------------------
long_dt <- rbind(mtcars, mtcars) 

kable(long_dt, "latex", longtable = T, booktabs = T, caption = "Longtable") %>%
  add_header_above(c(" ", "Group 1" = 5, "Group 2" = 6)) %>%
  kable_styling(latex_options = c("repeat_header"))

## ------------------------------------------------------------------------
kable(dt, "latex", booktabs = T) %>%
  kable_styling(full_width = T) %>%
  column_spec(1, width = "8cm")

## ------------------------------------------------------------------------
kable(dt, "latex", booktabs = T) %>%
  kable_styling(position = "center")

## ------------------------------------------------------------------------
kable(dt, "latex", booktabs = T) %>%
  kable_styling(position = "float_right")

## ------------------------------------------------------------------------
kable(dt, "latex", booktabs = T) %>%
  kable_styling(font_size = 7)

## ------------------------------------------------------------------------
text_tbl <- data.frame(
  Items = c("Item 1", "Item 2", "Item 3"),
  Features = c(
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Proin vehicula tempor ex. Morbi malesuada sagittis turpis, at venenatis nisl luctus a. ",
    "In eu urna at magna luctus rhoncus quis in nisl. Fusce in velit varius, posuere risus et, cursus augue. Duis eleifend aliquam ante, a aliquet ex tincidunt in. ", 
    "Vivamus venenatis egestas eros ut tempus. Vivamus id est nisi. Aliquam molestie erat et sollicitudin venenatis. In ac lacus at velit scelerisque mattis. "
  )
)

kable(text_tbl, "latex", booktabs = T) %>%
  kable_styling(full_width = F) %>%
  column_spec(1, bold = T, color = "red") %>%
  column_spec(2, width = "30em")

## ------------------------------------------------------------------------
kable(dt, "latex", booktabs = T) %>%
  kable_styling("striped", full_width = F) %>%
  column_spec(7, border_left = T, bold = T) %>%
  row_spec(1, strikeout = T) %>%
  row_spec(3:5, bold = T, color = "white", background = "black")

## ------------------------------------------------------------------------
kable(dt, "latex", booktabs = T, align = "c") %>%
  kable_styling(latex_options = "striped", full_width = F) %>%
  row_spec(0, angle = 45)

## ---- message=FALSE, warning=FALSE---------------------------------------
library(dplyr)
mtcars[1:10, 1:2] %>%
  mutate(
    car = row.names(.),
    # You don't need format = "latex" if you have ever defined options(knitr.table.format)
    mpg = cell_spec(mpg, "latex", color = ifelse(mpg > 20, "red", "blue")),
    cyl = cell_spec(cyl, "latex", color = "white", align = "c", angle = 45, 
                    background = factor(cyl, c(4, 6, 8), 
                                        c("#666666", "#999999", "#BBBBBB")))
  ) %>%
  select(car, mpg, cyl) %>%
  kable("latex", escape = F, booktabs = T, linesep = "")

## ------------------------------------------------------------------------
iris[1:10, ] %>%
  mutate_if(is.numeric, function(x) {
    cell_spec(x, "latex", bold = T, color = spec_color(x, end = 0.9),
              font_size = spec_font_size(x))
  }) %>%
  mutate(Species = cell_spec(
    Species, "latex", color = "white", bold = T,
    background = spec_color(1:10, end = 0.9, option = "A", direction = -1)
  )) %>%
  kable("latex", escape = F, booktabs = T, linesep = "", align = "c")

## ------------------------------------------------------------------------
sometext <- strsplit(paste0(
  "You can even try to make some crazy things like this paragraph. ", 
  "It may seem like a useless feature right now but it's so cool ",
  "and nobody can resist. ;)"
), " ")[[1]]
text_formatted <- paste(
  text_spec(sometext, "latex", color = spec_color(1:length(sometext), end = 0.9),
            font_size = spec_font_size(1:length(sometext), begin = 5, end = 20)),
  collapse = " ")

# To display the text, type `r text_formatted` outside of the chunk

## ------------------------------------------------------------------------
kable(dt, "latex", booktabs = T) %>%
  kable_styling() %>%
  add_header_above(c(" " = 1, "Group 1" = 2, "Group 2" = 2, "Group 3" = 2))

## ------------------------------------------------------------------------
kable(dt, "latex", booktabs = T) %>%
  kable_styling(latex_options = "striped") %>%
  add_header_above(c(" ", "Group 1" = 2, "Group 2" = 2, "Group 3" = 2)) %>%
  add_header_above(c(" ", "Group 4" = 4, "Group 5" = 2)) %>%
  add_header_above(c(" ", "Group 6" = 6), bold = T, italic = T)

## ------------------------------------------------------------------------
kable(mtcars[1:10, 1:6], "latex", caption = "Group Rows", booktabs = T) %>%
  kable_styling() %>%
  pack_rows("Group 1", 4, 7) %>%
  pack_rows("Group 2", 8, 10)

## ------------------------------------------------------------------------
kable(dt, "latex", booktabs = T) %>%
  pack_rows("Group 1", 4, 5, latex_gap_space = "2em")

## ---- eval=FALSE---------------------------------------------------------
#  kable(mtcars[1:10, 1:6], "latex", caption = "Group Rows", booktabs = T) %>%
#    kable_styling() %>%
#    pack_rows(index=c(" " = 3, "Group 1" = 4, "Group 2" = 3))
#  # Not evaluated. The code above should have the same result as the first example in this section.

## ---- eval=F-------------------------------------------------------------
#  kable(mtcars[1:2, 1:2], "latex", align = c("cl"))
#  # \begin{tabular}{l|cl|cl}  # Note the column alignment here
#  # \hline
#  #   & mpg & cyl\\
#  # ...

## ------------------------------------------------------------------------
kable(dt, "latex", booktabs = T) %>%
  add_indent(c(1, 3, 5))

## ------------------------------------------------------------------------
collapse_rows_dt <- data.frame(C1 = c(rep("a", 10), rep("b", 5)),
                 C2 = c(rep("c", 7), rep("d", 3), rep("c", 2), rep("d", 3)),
                 C3 = 1:15,
                 C4 = sample(c(0,1), 15, replace = TRUE))
kable(collapse_rows_dt, "latex", booktabs = T, align = "c") %>%
  column_spec(1, bold=T) %>%
  collapse_rows(columns = 1:2, latex_hline = "major", valign = "middle")

## ------------------------------------------------------------------------
kable(collapse_rows_dt[-1], "latex", align = "c", booktabs = T) %>%
  column_spec(1, bold = T, width = "5em") %>%
  row_spec(c(1:7, 11:12) - 1, extra_latex_after = "\\rowcolor{gray!6}") %>%
  collapse_rows(1, latex_hline = "none")

## ------------------------------------------------------------------------
collapse_rows_dt <- expand.grid(
  Country = sprintf('Country with a long name %s', c('A', 'B')),
  State = sprintf('State %s', c('a', 'b')),
  City = sprintf('City %s', c('1', '2')),
  District = sprintf('District %s', c('1', '2'))
) %>% arrange(Country, State, City) %>%
  mutate_all(as.character) %>%
  mutate(C1 = rnorm(n()),
         C2 = rnorm(n()))

kable(collapse_rows_dt, "latex", 
      booktabs = T, align = "c", linesep = '') %>%
  collapse_rows(1:3, row_group_label_position = 'stack') 

## ------------------------------------------------------------------------
row_group_label_fonts <- list(
  list(bold = T, italic = T), 
  list(bold = F, italic = F)
  )
kable(collapse_rows_dt, "latex", 
                     booktabs = T, align = "c", linesep = '') %>%
  column_spec(1, bold=T) %>%
  collapse_rows(1:3, latex_hline = 'custom', custom_latex_hline = 1:3, 
                row_group_label_position = 'stack', 
                row_group_label_fonts = row_group_label_fonts) 

## ------------------------------------------------------------------------
kable(dt, "latex", align = "c") %>%
  kable_styling(full_width = F) %>%
  footnote(general = "Here is a general comments of the table. ",
           number = c("Footnote 1; ", "Footnote 2; "),
           alphabet = c("Footnote A; ", "Footnote B; "),
           symbol = c("Footnote Symbol 1; ", "Footnote Symbol 2")
           )

## ------------------------------------------------------------------------
kable(dt, "latex", align = "c", booktabs = T) %>%
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
                                # That "latex" can be eliminated if defined in global
                                footnote_marker_symbol(1, "latex"))
row.names(dt_footnote)[4] <- paste0(row.names(dt_footnote)[4], 
                                footnote_marker_alphabet(1))
kable(dt_footnote, "latex", align = "c", booktabs = T,
      # Remember this escape = F
      escape = F) %>%
  footnote(alphabet = "Footnote A; ",
           symbol = "Footnote Symbol 1; ",
           alphabet_title = "Type II: ", symbol_title = "Type III: ",
           footnote_as_chunk = T)

## ------------------------------------------------------------------------
kable(dt, "latex", align = "c", booktabs = T, caption = "s") %>%
  footnote(general = "Here is a very very very very very very very very very very very very very very very very very very very very long footnote", 
           threeparttable = T)

## ------------------------------------------------------------------------
dt_lb <- data.frame(
  Item = c("Hello\nWorld", "This\nis a cat"), 
  Value = c(10, 100)
)

dt_lb %>%
  mutate_all(linebreak) %>%
  kable("latex", booktabs = T, escape = F,
        col.names = linebreak(c("Item\n(Name)", "Value\n(Number)"), align = "c"))

## ------------------------------------------------------------------------
kable(dt, "latex", caption = "Demo Table (Landscape)[note]", booktabs = T) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  add_header_above(c(" ", "Group 1[note]" = 3, "Group 2[note]" = 3)) %>%
  add_footnote(c("This table is from mtcars", 
                 "Group 1 contains mpg, cyl and disp", 
                 "Group 2 contains hp, drat and wt"), 
               notation = "symbol") %>%
  pack_rows("Group 1", 4, 5) %>%
  landscape()

## ---- eval = F-----------------------------------------------------------
#  # Not evaluated.
#  
#  # The code below will automatically include the image in the rmarkdown document
#  kable(dt, "latex", booktabs = T) %>%
#    column_spec(1, bold = T) %>%
#    kable_as_image()
#  
#  # If you want to save the image locally, just provide a name
#  kable(dt, "latex", booktabs = T) %>%
#    column_spec(1, bold = T) %>%
#    kable_as_image("my_latex_table")

## ---- eval=F-------------------------------------------------------------
#  # Not evaluating
#  xtable::xtable(mtcars[1:4, 1:4], caption = "Hello xtable") %>%
#    xtable2kable() %>%
#    column_spec(1, color = "red")

