
# phenoclim

The goal of phenoclim is to assist you in optimizing and fitting a variety of phenological models. In particular, this package implements models from (citation). The package also comes with a small generated phenological data set to use for examples.

## Important Note
You can only have 1 observation per year/location. 

## Parameters for Modeling Flowering

In order to specify the type of model you would like to run you will need to specify four parameters. First is whether you would like to run a Chill/Heat sum threshold model or a time threshold model. Then based on that you have the following choices. When we refer to variable parameters (variablepars) we mean that the day of the year which that event occurs varies from year to year based on the start or end dates of the phenological stage in question. Note: this is different than estimating a parameter. You can either estimate or not estimate both varying and non-varying parameters. Parameters go in the order (if present) [start, threshold, cardinaltemperatures]
 
###Time (Day) Threshold Model (modeltype='DT')
1. Start counting chill/heat at harvest. Count it for X days. Use the amount of chill/heat counted to predict flowering. Parameters:
    * variablepars = c('start', 'threshold')
    * startday = 0
    * threshold = X
2. Start counting chill/heat at harvest. Then count it until X day of the year. Use the amount of chill/heat to predict flowering. Parameters:
    * variablepars = c('start')
    * startday = 0
    * threshold = X
3. Start counting chill/heat X days after harvest. Then count it for Y days. Use the amount of chill/heat accumulated to predict flowering. Parameters:
    * variablepars = c('start', 'threshold')
    * startday = X
    * threshold = Y
4. Start counting chill/heat X days after harvest. Then count it until the Y day of the year. Use the amount of chill/heat accumulated to predict flowering. Parameters:
    * variablepars = c(start)
    * start = X
    * threshold = Y
5. Start counting chill/heat on X day of the year. Then count until Y day of the year. Use the amount of chill/heat accumulated to predict flowering. Parameters:
    * variablepars = NA
    * start = X
    * threshold = Y

###Chill/Heat Threshold Model (modeltype='TTT')
1. Start counting days at harvest. Count days until you have accumulated X amount of chill/heat. Use the days counted to predict flowering. Parameters:
    * variablepars = c('start')
    * start = 0
    * threshold = X
2. Start counting days X days after harvest. Count days until you have accumulated Y amount of chill/heat. Use the days counted to predict flowering. Parameters:
    * variablepars = c('start')
    * start = X
    * threshold = Y
3. Start counting days on X day of the year. Count days until you have accumulated Y amount of chill/heat. Use the days counted to predict flowering. Parameters:
    * variablepars = NA
    * startday = X
    * threshold = Y


## Parameters for Modeling Stage/Season Length

These types of models are generally used to model the length of fruit development in fruit and nut trees (DT models) and lengths of developmental stages for annuals (TTT).
    
### Time (Day) Threshold Model (modeltype=DT)

1. Start counting thermal time at bloom/leaf-out. Count thermal time for X days. Use the thermal time accumulated to predict season length. Parameters:
    * variablepars = c('start', 'threshold')
    * startday = 0
    * threshold = X
2.  Start counting thermal time at bloom/leaf-out. Count thermal time until X day. Use the thermal time accumulated to predict season length. Parameters:
    * variablepars = c('start')
    * startday = 0
    * threshold = X
3.  Start counting thermal time X days after bloom/leaf-out. Count thermal time for Y day. Use the thermal time accumulated to predict season length. Parameters:
    * variablepars = c('start', 'threshold')
    * startday = X
    * threshold = Y
4.  Start counting thermal time X days after bloom/leaf-out. Count thermal time until Y day of the year. Use the thermal time accumulated to predict season length. Parameters:
    * variablepars = c('start')
    * startday = X
    * threshold = Y
5.  Start counting thermal time X day of the year. Count thermal time until Y day of the year. Use the thermal time accumulated to predict season length. Parameters:
    * variablepars = NA
    * startday = X
    * threshold = Y

### Thermal Time Threshold Model (modeltype=TTT)
Note: If you want the day the model hits the thermal time threshold to be the predicted end of stage/season length, set simplified=TRUE.

1. Start counting days at bloom/leaf-out. Count days until you have accumulated X amount of thermal time. Use the days counted to predict flowering. Parameters:
    * variablepars = c('start')
    * start = 0
    * threshold = X
2. Start counting days X days after bloom/leaf-out. Count days until you have accumulated Y amount of thermal time. Use the days counted to predict flowering. Parameters:
    * variablepars = c('start')
    * start = X
    * threshold = Y
3. Start counting days on X day of the year. Count days until you have accumulated Y amount of thermal time. Use the days counted to predict flowering. Parameters:
    * variablepars = NA
    * startday = X
    * threshold = Y


## Example

This is a basic example which shows you how to solve a common problem:

``` r
## basic example code
```


Source list

files <- c('FlowerPlant.R',
    'parameterfunctions.R',
    'general.R',
    'thermal.R',
    'parameterlistclass.R',
    'parameterlistmethods.R',
    'constructors.R',
    'plantmodelclass.R',
    'plantmodelmethods.R',
    'fithelper.R',
    'thermalsum.R',
    'minrmse.R',
    'objectivefunction.R',
    'modelcheck.R',
    'plantmodel.R',
    'crossval.R',
    'diurnal_temperature.R',
    'extractResults.R',
    'flowermodelclass.R',
    'flowermodelmethods.R',
    'flowermodel.R',
    'phenoclim.R',
    'preprocess.R')

sapply(files, function(fn) {
    source(file.path(funpath, fn))
})



