
# phenoclim

The goal of phenoclim is to assist you in optimizing and fitting a variety of phenological models. In particular, this package implements models from (citation). The package also comes with a small generated phenological data set to use for examples.

## Parameters for Modeling Flowering

In order to specify the type of model you would like to run you will need to specify four parameters. First is whether you would like to run a Chill/Heat sum threshold model or a time threshold model. Then based on that you have the following choices. When we refer to variable parameters (variablepars)
 we mean that the day of the year which that event occurs varies from year to year based on the start or end dates of the phenological stage in question. 
 
###Time (Day) Threshold Model (modeltype='DT')
1. Start counting chill/heat at harvest. Count it for X days. Use the amount of chill/heat counted to predict flowering. Parameters:
    * variablepars = c('start', 'threshold')
    * startday = 0
2. Start counting chill/heat at harvest. Then count it until X day of the year. Use the amount of chill/heat to predict flowering. Parameters
    * variablepars = c('start')
    * startday=0
3. Start counting chill/heat X days after harvest. Then count it for Y days. Use the amount of chill/heat accumulated to predict flowering. Parameters:
    *



## Example

This is a basic example which shows you how to solve a common problem:

``` r
## basic example code
```
