# noaaclimate
R routines for fetching NOAA (PSL and CPC) climate data. 

To install:

```
devtools::install_github('llperez/noaaclimate')
```

Examples:

```
library(noaaclimate)
psl_monthly(c('nina34', 'soi'), 'xts')
```