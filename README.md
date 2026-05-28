# GLKNWaterViz (branch `dev`)

## Description

The `dev` branch is the development branch of the `GLKNWaterViz` repo.

## Shiny App

**Live Demo:** [Click here to run the app]( https://kinziebailey.shinyapps.io/GLKNWaterViz-Dev/) 


## Getting started: Running locally 

1.  Create a new R Studio project File -\> New Project -\> Version Control -\> Git -\> <https://github.com/kinziebailey/GLKNWaterViz>
2.  Switch to the `dev` branch. In your terminal:

```{terminal}
git checkout dev
```
3.  Create a Data and a Data/GLKN. In the Console:

```{terminal}
dir.create("Data")
dir.create("Data/GLKN")
```

4.  Copy the data and metadata files to your `Data` folder from [here](https://doimspp.sharepoint.com/:f:/r/sites/NCRNWater/Shared%20Documents/General/Annual-Data-Packages/2024?csf=1&web=1&e=fN3XjJ):
    - chr_loopup.csv
    - stations.csv
    -thresholds.csv

5. Run the wqp_retrival.R code to retrieve data from the [Water Quality Portal](https://www.waterqualitydata.us/)

6.  Confirm that you can run the shiny app.

-   open `global.R`
-   click the "Run App" button at the top of your code editor

7.  If the app runs for you, continue on. Otherwise, contact Kinzie Bailey.
