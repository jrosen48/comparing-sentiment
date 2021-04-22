comparing-sentiment

## Using {targets}

This project uses {targets} for workflow management. To install {targets}:

```{r}
library(remotes)
install_github("wlandau/targets")
```

To run the project pipeline, enter and run the following function:

## Dependencies

- `tidytext::get_sentiments("bing")`
- `tidytext::get_sentiments("loughran")`
- `tidytext::get_sentiments("nrc")`
- `tidytext::get_sentiments("afinn")`

`targets::tar_make()`

## Directory structure

- Raw data: `data-raw`
- Aggregated data and data from external sentiment software: `data`
  - For more information, refer to `create-study-data.R`
- Functions: `r/functions.R`

## Study data

Please contact the authors for access to the study data.
