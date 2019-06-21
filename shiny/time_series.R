library(dplyr)
library(lubridate)      # for cleaner dates, we can use basic if we need

library(ggplot2)
library(plotly)         # for dynamic graph if I can figure out a better way to show names (geom_text maybe if I can figure it out)
library(ggpmisc)        # again for annotation and cleaner code


### Notes / To Do ###
# How can we make the labels work for other dates? 
# Dont know how to make the labels dynamic, this could be a problem and they may not be worth it.
# We would want to make the time serires basic (no labels etc) as a 'background'
# and then overlay the "dates of interest" as another plot on top
# no idea how to make, would need to learn unless you already know

# I do like the vline idea, only problem is the green is hard to see, thats why I liked the dots / names 


### Interesting Dates ### 

## Appearance dates / announcement dates / etc
## I think this might be a nice drop down to add into the time series 
## ie. default selection = fox news appearance, but we can add in MSNBC / CNN / announcement of candidacy as drop down options

### Dates of Interest

# Fox Appearance Dates
bernie_fox_appearance <- ymd("2019-04-15")
pete_fox_appearance <- ymd("2019-05-21")
gilli_fox_appearance <- ymd("2019-06-02")


### Loading the Data

# reading the csv created in data.R
candidate_interest <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQ7Wx65iktsXM6SzAcO0W4OWZzx5hgZ-Evx4fVsXty1XkiQ_l40of7IKai7lh_qXzSsMeuBrMSTiuWt/pub?output=csv")

# declaring data types
candidate_interest <- candidate_interest %>%
    as_tibble() %>%
    mutate(date = ymd(date), hits = as.numeric(hits))

### Appearances

# Function to find the gTrends "hits" score for any dates of interest
hits_of_interest <- function(data_set, candidate, date_of_interest){
    appearance <- data_set %>% 
        filter(keyword == candidate, date == date_of_interest) %>%
        select(hits) %>% 
        as.numeric()
    return(appearance)
}

# Bernie Sanders appeared on Fox News on April 15, 2019; 
bernie_appearance_hits <- hits_of_interest(candidate_interest, "Bernie Sanders", bernie_fox_appearance)

#Pete Buttigieg appeared on May 21, 2019;
pete_appearance_hits <- hits_of_interest(candidate_interest, "Pete Buttigieg", pete_fox_appearance) 

#Kirsten Gillibrand appeared on June 2, 2019.
gilli_appearance_hits <- hits_of_interest(candidate_interest, "Kirsten Gillibrand", gilli_fox_appearance)

# Create labels for the points 
fox_appearance_labels <- data.frame(
    candidate <- c("Sanders's Appearance", "Buttigieg's Appearance", "Gillibrand's Appearance"),
    date <- c(bernie_fox_appearance, pete_fox_appearance, gilli_fox_appearance),
    hits <- c(bernie_appearance_hits, pete_appearance_hits, gilli_appearance_hits), 
    stored_labels <- c("coral3", "dodgerblue3", "green3")
)

# Plot the time series
## Note: would be nice to break this apart, time series + dates of interest
## need to investigate how to do that
candidate_time_series <- function(selection){
    ggplot(candidate_interest, aes(date, hits, color = keyword)) +
        geom_line() +
        theme_minimal() +
        geom_point(data = fox_appearance_labels,
                   size = 2, 
                   x = date[selection], 
                   y = hits[selection], 
                   color = stored_labels[selection], 
                   fill = stored_labels[selection]) +
        geom_text(data = fox_appearance_labels, 
                  inherit.aes = FALSE,
                  mapping = aes(x = date[selection], y = hits[selection], label = candidate[selection]),
                  nudge_y = 3,
                  fontface = "bold")
}

# Produce the time series
#Test_TS <- candidate_time_series(selection = 3)
#Test_TS

