# Transform R-Abimo input Data into their natural scenario equivalent

Three scenarios are possible:

1.  undeveloped: all paved or constructed areas are set to 0%. No
    connection to the sewer.

2.  forested: like undeveloped, but the land type is declared to be
    "forested".

3.  horticultural: like undeveloped, but the land type is declared to be
    "horticultural".

## Usage

``` r
data_to_natural(data, type = "undeveloped", veg_class = 50)
```

## Arguments

- data:

  the input data in R-Abimo format

- type:

  a character object containing the name of natural scenario. Defaults
  to "undeveloped"

- veg_class:

  vegetation class to assign to each row in `data`. Default: 50

## Value

a dataframe with R-Abimo input data for the chosen natural scenario
