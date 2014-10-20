# Health and Economic Impacts of Storms in US

## Synopsis

In this report, we aim to analyse the economic and health consequences of Storms in USA. In particular, we address the following two questions:

1. Across the United States, which types of events are most harmful with respect to population health?
2. Across the United States, which types of events have the greatest economic consequences?

## The Raw Data Source

The data for this report come in the form of a comma-separated-value file compressed via the bzip2 algorithm to reduce its size. You can download the file from the URL [Storm Data (47Mb)](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2)

Following documentations give more details on how some of the variables are constructed/defined.

* [National Weather Service Storm Data Documentation]([an example](http://example.com/ "Title") inline link)
* National Climatic Data Center Storm Events [FAQ](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2FNCDC%20Storm%20Events-FAQ%20Page.pdf)

The events in the database start in the year 1950 and end in November 2011. In the earlier years of the database there are generally fewer events recorded, most likely due to a lack of good records. More recent years are considered more complete for this report.

## Data Processing
We load the required libararies that we use throughout our analysis

```r
library(dplyr)
library(xtable)
```
We dowload the data and read the zip file into a variable *storm*

```r
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",destfile="Storms.csv.bz2", method="curl", mode="wb")
storm <- read.csv(bzfile("Storms.csv.bz2"))
```

We then process the names of the data frame to lowercase and clean it up a bit

```r
names(storm)
```

```
##  [1] "STATE__"    "BGN_DATE"   "BGN_TIME"   "TIME_ZONE"  "COUNTY"    
##  [6] "COUNTYNAME" "STATE"      "EVTYPE"     "BGN_RANGE"  "BGN_AZI"   
## [11] "BGN_LOCATI" "END_DATE"   "END_TIME"   "COUNTY_END" "COUNTYENDN"
## [16] "END_RANGE"  "END_AZI"    "END_LOCATI" "LENGTH"     "WIDTH"     
## [21] "F"          "MAG"        "FATALITIES" "INJURIES"   "PROPDMG"   
## [26] "PROPDMGEXP" "CROPDMG"    "CROPDMGEXP" "WFO"        "STATEOFFIC"
## [31] "ZONENAMES"  "LATITUDE"   "LONGITUDE"  "LATITUDE_E" "LONGITUDE_"
## [36] "REMARKS"    "REFNUM"
```

```r
lowerCaseNames <- gsub("(.*)", "\\L\\1", names(storm), perl=TRUE)
finalNames = gsub( "__",'', lowerCaseNames)
finalNames = gsub( "_$",'', finalNames)
names(storm) = finalNames
names(storm)
```

```
##  [1] "state"      "bgn_date"   "bgn_time"   "time_zone"  "county"    
##  [6] "countyname" "state"      "evtype"     "bgn_range"  "bgn_azi"   
## [11] "bgn_locati" "end_date"   "end_time"   "county_end" "countyendn"
## [16] "end_range"  "end_azi"    "end_locati" "length"     "width"     
## [21] "f"          "mag"        "fatalities" "injuries"   "propdmg"   
## [26] "propdmgexp" "cropdmg"    "cropdmgexp" "wfo"        "stateoffic"
## [31] "zonenames"  "latitude"   "longitude"  "latitude_e" "longitude" 
## [36] "remarks"    "refnum"
```

To analyaze the health impacts of the storms based on event type, we assume that the main health impacts are represented by the columns *fatalities* and *injuries*. So we do sum up fatalities and injuries and sort them in descending order of number of fatalities and injuries

```r
storm_health_impact <- storm %>% group_by(evtype) %>% 
    summarize(total_fatalities = sum(fatalities),
              total_injuries=sum(injuries)
              )%>% 
    arrange(desc(total_fatalities), 
            desc(total_injuries) )
```

Assuming that injuries and fatalities represent the main impact on health, we can see that the event **TORNADO** has the biggest impact on the USA by far.
## Results


```r
top_10 = head(storm_health_impact,10)
names(top_10) <- c("Event Type", "Fatalities", "Injuries")
#kable(top_10)
print(xtable(top_10), type="html")
```

<!-- html table generated in R 3.1.0 by xtable 1.7-4 package -->
<!-- Sun Oct 19 19:55:28 2014 -->
<table border=1>
<tr> <th>  </th> <th> Event Type </th> <th> Fatalities </th> <th> Injuries </th>  </tr>
  <tr> <td align="right"> 1 </td> <td> TORNADO </td> <td align="right"> 5633.00 </td> <td align="right"> 91346.00 </td> </tr>
  <tr> <td align="right"> 2 </td> <td> EXCESSIVE HEAT </td> <td align="right"> 1903.00 </td> <td align="right"> 6525.00 </td> </tr>
  <tr> <td align="right"> 3 </td> <td> FLASH FLOOD </td> <td align="right"> 978.00 </td> <td align="right"> 1777.00 </td> </tr>
  <tr> <td align="right"> 4 </td> <td> HEAT </td> <td align="right"> 937.00 </td> <td align="right"> 2100.00 </td> </tr>
  <tr> <td align="right"> 5 </td> <td> LIGHTNING </td> <td align="right"> 816.00 </td> <td align="right"> 5230.00 </td> </tr>
  <tr> <td align="right"> 6 </td> <td> TSTM WIND </td> <td align="right"> 504.00 </td> <td align="right"> 6957.00 </td> </tr>
  <tr> <td align="right"> 7 </td> <td> FLOOD </td> <td align="right"> 470.00 </td> <td align="right"> 6789.00 </td> </tr>
  <tr> <td align="right"> 8 </td> <td> RIP CURRENT </td> <td align="right"> 368.00 </td> <td align="right"> 232.00 </td> </tr>
  <tr> <td align="right"> 9 </td> <td> HIGH WIND </td> <td align="right"> 248.00 </td> <td align="right"> 1137.00 </td> </tr>
  <tr> <td align="right"> 10 </td> <td> AVALANCHE </td> <td align="right"> 224.00 </td> <td align="right"> 170.00 </td> </tr>
   </table>
#### Table 1: Top 10 Event Types causing fatalities and Injuries
