# wmSpatial

Created by: [David Ellis](https://github.com/David-Ellis) </br>
Population Health Management</br>
Birmingham City Council
 
## Introduction

### Installation

``` r
devtools::install_github("BCC-PHM/wmSpatial")
```

### Basic Usage

```r
# Birmingham Council House
df1 <- data.frame(postcode = "B1 1BB")

# 5 Random West Midlands Postcodes
df2 <- data.frame(
  postcode = c("B36 9ST", "B96 6DD", "B70 7EJ", 
               "DY8 3YD", "B21 8BQ")
  )

# Find the TWO (n=2) postcodes nearest to the council house
nearest_postcode(df1, df2, n = 2)
```

Returns:

```
postcode latitude longitude local_authority nearest_pc_1 nearest_pc_2
1   B1 1BB 52.48043 -1.903448      Birmingham      B21 8BQ      B70 7EJ
```
### License

This repository is dual licensed under the [Open Government v3]([https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/) & MIT. All code can outputs are subject to Crown Copyright.
