comparing-sentiment

## Using {targets}

This project uses {targets} for workflow management. To install {targets}:

```{r}
library(remotes)
install_github("wlandau/targets")
```

To run the project pipeline, enter and run the following function:

`targets::tar_make()`

## Directory structure

- Raw data: `data-raw`
- Data that can be shared: `data`
- Functions: `r/functions.R`

## Study data

Please contact the authors for access to the study data.