---
  title: "Stable Isotope Freezer Inventory"
output: html_notebook
---
  
### Project: Inventory of frozen biological samples collected by the Southwest Fisheries Science Center, Santa Cruz
  
#### Note about inventory database:
#The database is contained in IlyIglesias's NOAA server (rockfish/projects/stableisotopes) 
#--These data were updated so that spp codes match those from cruise data as much as possible.
#-- This inventory does not include what the Hollings scholar (Chris) has continued to inventory.
#-- The shiny app created from this code only displays locations for those samples that had geographic informaion (such as station info) and as such there are some samples in the freezer not included in the app.
#-- where possible, i went through and added dates for missing date info to the best of my ability (total of 30)
#-- as compared to the catch maps data, this project included additionally: hauls from the NWFSC and hauls from 2018

# load libraries
library(tidyverse)
library(lubridate)
library(sf)
library(leaflet)
```

### Load inventory database
#And clean up the existing dataframe

# load inventory database
inventory <- read.csv("data/stable_isotope_inventory.csv", na.strings= " ", header = TRUE, stringsAsFactors = FALSE) # stringsAsFactors-- imports values as character instead of factor

# parse date and create new year column
inventory <-  inventory %>% 
mutate(parsed_date= parse_date_time(inventory$date, orders= "mdy"), year=year(parsed_date))%>% 
select(-date)  # remove our original date column

#remove NWFSC stations written on bags which contain decimal values (which don't match any of our existing station data)-- 
  # these station values will come instead from the haul_info df based on haul # and year
  inventory$station <-  replace(x= inventory$station, list= grep(pattern = "\\.", x=inventory$station), values= "NA") # this changes all strings containing a "." to NA
  
  #convert from character to numeric
  inventory$station<- as.numeric(as.character(inventory$station)) # error message "NAs introduced by coercion" going to ignore
  inventory$haul_no <- as.numeric(as.character(inventory$haul_no)) # convert haul_no to numeric
  
  #sum(is.na(inventory$haul_no)) #still some samples missing haul numbers but some have their own station info on bags

  
  
  ### The next task involves filling in missing station information based on haul and date info 
  # should give station information and from station we can find lat long values)
  
    # create df of all stations including latitude and longitude values
    swfsc_stations <- read.csv("data/dbo_STANDARD_STATIONS.csv", na.strings= " ", header = TRUE) # note na.strings for blank cells to convert to NAs
    nwfsc_stations <- read.csv("data/dbo_NWFSC_STANDARD_STATIONS.csv", na.strings= " ",header = TRUE)
    
    # combine station lists from the SWFSC (stations) with the list from the NWFSC (nwfsc_stations) via rbind
    swfsc_stations<- swfsc_stations %>% # make sure formating is the same for both dfs
      select(-ACTIVE)%>% # remove "ACTIVE" column from swfsc_stations df, so that these two dfs are the same for the rbind merge
      mutate(SCI_CENTER= "SWFSC") # create a new columns for science center in case I ever wish to select by this later
    
    nwfsc_stations <-mutate(nwfsc_stations, SCI_CENTER="NWFSC") # create a new column for science center
    nwfsc_stations$LATITUDE <- as.numeric(as.character(nwfsc_stations$LATITUDE)) # convert latitude values from integer to numeric- check through str()
    
    stations<- rbind(swfsc_stations, nwfsc_stations) # combine rows from two dfs into one df, stations
    rm(nwfsc_stations, swfsc_stations)
    
    # convert degree minutes to decimal degrees
    stations <- stations %>% 
      rename(y = LATITUDE, x = LONGITUDE) %>% 
      mutate(lat.d  = as.numeric(str_sub(y, 1, 2)),
        lat.m  = as.numeric(str_sub(y, 3, 7)),
        LATITUDE    = lat.d + lat.m/60,
        long.d = as.numeric(str_sub(x, 1, 3)),
        long.m = as.numeric(str_sub(x, 4, 8)),
        LONGITUDE = -(long.d + long.m/60)) %>%
      select(-c(lat.d, long.d, lat.m, long.m, x, y))
    
    # note that this table has 414 stations, tho in all our catch data with standard stations, there are only 170 stations trawled, and likely a lot less for our data
    #sum(is.na(stations$STATION)) # at this point there aren't any stations without info
    
    ```
    
    ### Determine haul information (dbo_JUV_HAUL) and select for date, haul # and station information
    #(to merge with our inventory and in turn get lat long information)
   
    # create a list of haul #, year and lat long data to merge with our inventory info!
    
    haul <- read.csv("data/dbo_JUV_HAUL.csv", header = TRUE, stringsAsFactors = FALSE) # read in haul data from haul table 
    haul_info <- haul %>%
      select(HAUL_NO, HAUL_DATE, STATION) %>% 
      mutate(date=parse_date_time(HAUL_DATE, "mdy HM")) %>% # convert date and time format into something usable by {lubridate} note i only used hours minutes- instead of HMS so times may be off which I don't need for this project as i am interested in year
      mutate(year= year(date)) %>% # create a new column for year of survey so we can select by year and haul number later
      select(year, STATION, HAUL_NO) %>% 
      left_join(select(stations, STATION, LATITUDE, LONGITUDE), by= "STATION") %>% # add in lat long data from "stations" df
      rename(station=STATION, haul_no=HAUL_NO) # wanted the column name to match inventory df
    rm(haul) # only remove stations once i figure this out?!! 
    #sum(is.na(haul_info$station)) # and at this point there are 456 hauls without station info (ie that didn't merge based on year and haul number)
    # this created a df of year, haul, station and lat long for a given station 

    ### Now join these two tables and fill in missing values from haul_info
  
    # join two tables together (inventory df (where some station info is missing but we have haul # and year for most) and haul_info df (which contains lat long info and station year) 
    # add station information:
    inventory_merge <-  left_join(inventory, select(haul_info, year, station, haul_no), by=c("year", "haul_no")) %>% #joined by year and haul_no
      mutate(station= ifelse(is.na(station.y), station.x, station.y)) # fills in missing station information based on haul and year
    
    stations <-  stations %>% 
      select(STATION, LATITUDE, LONGITUDE) %>% 
      rename(station=STATION)
    # above: just selected station, latitude and longitude and converted the station name so it matches the file we would like to join to "station"
    
    inventory_coords <- left_join(inventory_merge, select(stations, LATITUDE, LONGITUDE, station), by="station") %>% # add lat long information to filled station list 
      # note that this is a merge with our simple "stations" df instead of the haul_info df which has multiple stations listed- need one with unique values
      select(-c(station.x, station.y ))
    
    #remove those samples without any coordinate info (~192/193) 
    inventory<- inventory_coords[complete.cases(inventory_coords$LATITUDE), ] # reduces our list of samples from 2,176 to 1,846
    #removing those 193 (not sure where I got an extra) samples without location information.
    
    rm(inventory_coords, inventory_merge, haul_info, stations)
    
    # note this reduces the many chances for error introduced by the written bags (using the haul_no and year, we can assign stations and avoid the issues with the station labels on the bag- mislabeling, trouble with legibitlity, ect.)
    #sum(is.na(inventory_coords$station)) # note that there are still some samples (192) that still appear to be missing station information. In some cases these are bongo samples without haul numbers or simply tissue samples collected from another trip. Either way, it is what it is- these samples will not be plotted 
    
    
    ### Transform df into a sf (spatial feature for mapping)
  
    inventory_sf <- st_as_sf(inventory, coords = c("LONGITUDE", "LATITUDE"))
    st_crs(inventory_sf)= 4326 # set the coordinate infomation to crs=4326 WGS84
    rm(inventory)
    
    # just need to run shiny app from here!