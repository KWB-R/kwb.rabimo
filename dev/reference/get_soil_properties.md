# Calculate Soil Properties

Provide variables that are relevant to calculate the actual evaporation
for unsealed areas

## Usage

``` r
get_soil_properties(
  land_type,
  veg_class,
  depth_to_water_table,
  field_capacity_30,
  field_capacity_150,
  dbg = FALSE
)
```

## Arguments

- land_type:

  land_type string, one of "vegetationless", "waterbody",
  "horticultural", "urban", "forested"

- veg_class:

  vegetation class

- depth_to_water_table:

  depth to water table

- field_capacity_30:

  field capacity in 30 cm depth

- field_capacity_150:

  field capacity in 150 cm depth

- dbg:

  logical indicating whether or not to show debug messages
