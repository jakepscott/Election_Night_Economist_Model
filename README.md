# Election Night Live Economist Model
This repository contains the code for an [application](https://jake-scott.shinyapps.io/Economist_Election_Model) that showed live election odds throughout election night and the subsequent days. It took in data on which states were called for which candidates, fed that into the [Economist election model](https://projects.economist.com/us-2020-forecast/president), and then displayed a set of visualizations to show users how odds had evolved and where they stood at a given time. This was something of a speed-run, as I decided to embark on the project at 9pm on Sunday November 1st, and had it up and running by 7pm on Tuesday November 3rd (despite having work on both Monday and Tuesday)! 

Note: The app is no longer automatically reading in data and updating numbers. It is instead a essentially a "snapshot" of what the app looked like when the race was finally called. The code for this snap shot (the current app) can be found in the Economist_Election_Model directory. The original app from election night, which used automatic updating, can be found in the Original_App directory.

![image](https://user-images.githubusercontent.com/56490913/98498984-4825e500-2216-11eb-9875-20ba05c8237b.png)

## How it worked
#### Data 
The main data needed for the application was which states had been called for which candidates. To obtain this data, I had initially planned to have the app to scrape a website like the [New York Times](https://www.nytimes.com/interactive/2020/11/03/us/elections/results-president.html). Unfortunately, due to time constraints, I was unable to do this. However, it was also impractical to use a static data set, like a CSV. That would have required updating the data and redeploying the app each time a state was called. Given that deploying the app even a single time took somewhere between 10 and 15 minutes, that would not have worked on a fast-paced night like election night. 

The solution came in the form of the `gsheet::gsheet2tbl` function, which allows one to read in data from Google Sheets. Using that function, I settled on a middle-ground solution between manual and automatic updating. I manually updated a Goodle Sheet each time a new state was called and the app automatically ingested that data every 10 seconds. 

#### Model
Each time the data was read in it was fed into the Economist Election Model, built by G. Elliott Morris to forecast the winner of the 2020 election. Morris was kind enough to [make public](https://twitter.com/gelliottmorris/status/1322589664797229057) an R script that lets one see the model's forecast and how it changes as certain states are won by certain candidates. I modified this script only moderately, making it output a list of tidy data sets ready to be inputted into vizualizations.

The constant reading in of the data and re-calculation of win probabilities (every 10 seconds) both allowed overall and by-state win probabilities to change each time a new state was called and also highlighted the uncertainty/randomness inherent to a probabilistic model, given that the numbers bounced around slightly even when no new states had been called.  

#### Needle
The code for the needle was lifted almost entirely from Caio Brighenti's [Draft Night Sentiment App](https://github.com/CaioBrighenti/nfl-draft-sentiment). He in turn lifted it from [this](https://stackoverflow.com/questions/50042214/fill-a-polygon-with-gradient-scale-in-r) StackOverflow thread. 

#### Map
The map itself came from the `albersusa` package, and was made interactive using `plotly`. It is worth noting that the `albersusa` package itself does not work in Shiny, and I had to save the shape data as an RDS and read it in. I then needed to convert it back to an `sf` object and manually reset the projection to LEAE. 

#### Table
The table was made using the excellent `gt` package. 

#### Line Chart
While the chart itself was a simple `geom_line`, getting the data for the figure was actually not trivial. I needed to capture how odds had changed throughout the night, even if the app itself wasn't running at a given time. To do this I wrote a local R Script, called LineChartUpdateScript.R, which I kept running almost continuously on my laptop for the days the app was running live. Every 2 minutes this script would read in the data described above on who won which states, input that data into the model, extract the resulting win probabilities, and add a new row to a Google Sheet with those new probabilities and a time stamp. Writing to a Google Sheet was done using `googlesheets4::sheet_append` function. This data was then read into the app every two minutes using the `gsheet2tbl` function. The line chart would then update and display a new timestamp. 

## Prerequisites
```
library(tidyverse)
library(gt)
library(usmap)
library(qpcR)
library(scales)
library(ggtext)
library(ggimage)
library(readr)
library(sf)
library(mvtnorm)
library(politicaldata)
library(shiny)
library(plotly)
library(gsheet)
library(googlesheets4)
```

## Author
* **Jake Scott** - [Twitter](https://twitter.com/jakepscott2020), [Medium](https://medium.com/@jakepscott16), [LinkedIn](https://www.linkedin.com/in/jacob-scott-689875130/)

## Acknowledgements
* [Caio Brighenti](https://twitter.com/CaioBrighenti) - Provided huge help on the project. Helped motivate me to do it and provided inspiration with his Draft Night Sentiment app, dealt with frantic messages at all hours with random questions, helped host the Twitch stream, and more. Needless to say the app as it was would not have happened without his help
* [G. Eliot Morris](https://twitter.com/gelliottmorris?ref_src=twsrc%5Egoogle%7Ctwcamp%5Eserp%7Ctwgr%5Eauthor) - Made the forecast model itself and was kind enough to make it public.
* [StackOverfow](https://stackoverflow.com/questions/50042214/fill-a-polygon-with-gradient-scale-in-r) - Odds needle came from this thread.
