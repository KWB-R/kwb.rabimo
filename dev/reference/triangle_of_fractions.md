# Plot Triangle of Three Fractions

Plot Triangle of Three Fractions

## Usage

``` r
triangle_of_fractions(
  fractions,
  fractions_2 = NULL,
  cols = c("blue", "red", "darkgreen")
)
```

## Arguments

- fractions:

  numeric vector with three values having a sum of one. The names of the
  vector elements are used as labels

- fractions_2:

  optional. Similar to `fractions`. If given, these fractions are shown
  as dashed lines in the plot and the "deltas" between `fractions` and
  `fractions_2` are shown as horizontally stacked bars below the
  triangle.

- cols:

  vector of length three giving the colour names

## Examples

``` r
# blue, red, green | left, right, bottom
components <- c(runoff = 200, infiltration = 50, evaporation = 100)
fractions <- components / sum(components)
triangle_of_fractions(fractions)

triangle_of_fractions(fractions, fractions_2 = c(0.1, 0.3, 0.6))
```
