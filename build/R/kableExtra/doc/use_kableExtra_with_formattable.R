## ---- message=FALSE, warning=FALSE---------------------------------------
library(knitr)
library(kableExtra)
library(formattable)
library(dplyr)

## ------------------------------------------------------------------------
mtcars[1:5, 1:4] %>%
  mutate(
    car = row.names(.),
    mpg = color_tile("white", "orange")(mpg),
    cyl = cell_spec(cyl, "html", angle = (1:5)*60, 
                    background = "red", color = "white", align = "center"),
    disp = ifelse(disp > 200,
                  cell_spec(disp, "html", color = "red", bold = T),
                  cell_spec(disp, "html", color = "green", italic = T)),
    hp = color_bar("lightgreen")(hp)
  ) %>%
  select(car, everything()) %>%
  kable("html", escape = F) %>%
  kable_styling("hover", full_width = F) %>%
  column_spec(5, width = "3cm") %>%
  add_header_above(c(" ", "Hello" = 2, "World" = 2))

## ------------------------------------------------------------------------
iris[1:10, ] %>%
  mutate(
    Species = cell_spec(Species, color = spec_color(1:10, option = "A"), link = "#",
                        tooltip = paste0("Sepal Length: ", Sepal.Length))
  ) %>%
  mutate_if(is.numeric, function(x){
    cell_spec(x, "html", color = spec_color(x), font_size = spec_font_size(x), bold = T)
  }) %>%
  kable("html", escape = F, align = "c") %>%
  kable_styling("condensed", full_width = F)

