# Generate an area in R-Abimo format with default values

All default values can be overridden by entering new key-value pairs.

## Usage

``` r
generate_rabimo_area(code, ..., column_info = read_column_info())
```

## Arguments

- code:

  identifier of area

- ...:

  key = value pairs overriding the default column values

- column_info:

  data frame as returned by
  [`read_column_info`](https://kwb-r.github.io/kwb.rabimo/reference/read_column_info.md)
