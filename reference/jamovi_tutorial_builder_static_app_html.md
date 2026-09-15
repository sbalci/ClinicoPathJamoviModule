# Jamovi tutorial builder static HTML

Returns a responsive HTML fragment (no \<head\>/\<body\>) suitable for
jamovi outputs.

## Usage

``` r
jamovi_tutorial_builder_static_app_html(layout = c("wide", "compact"))
```

## Arguments

- layout:

  Choose between the default two-column layout ("wide") or the narrower
  compact layout ("compact").

## Value

A length-one character vector containing HTML markup.
