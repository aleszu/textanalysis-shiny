library(dplyr)
library(lubridate) # for cleaner dates, we can use basic if we need
library(plotly) # for dynamic graph if I can figure out a better way to show names (geom_text maybe if I can figure it out)
library(ggpmisc) # again for annotation and cleaner code


# If using the master data frame
# candidate_interest <- Master_gTrends$interest_over_time %>%
#          as_tibble() %>%
#          group_by(keyword) %>%
#          mutate(date = ymd(date), hits = as.numeric(hits))

# glimpse(candidate_interest)

### Interesting Dates ### 

## Appearance dates / announcement dates / etc
## I think this might be a nice drop down to add into the time series 
## ie. default selection = fox news appearance, but we can add in MSNBC / CNN / announcement of candidacy as drop down options

### Appearances ###

# I could / should probably functionize these appearances

# Bernie Sanders appeared on Fox News on April 15, 2019; 

#bernie_fox_appearance <- ymd("2019-04-15")
bernie_appearance_hits <- candidate_interest %>% 
    filter(keyword == "Bernie Sanders", date == bernie_fox_appearance) %>%
    select(hits) %>% 
    as.numeric()

#Pete Buttigieg appeared on May 21, 2019;
#pete_fox_appearance <- ymd("2019-05-21")
pete_appearance_hits <- candidate_interest %>% 
    filter(keyword == "Pete Buttigieg", date == pete_fox_appearance) %>%
    select(hits) %>% 
    as.numeric()

#Kirsten Gillibrand appeared on June 2, 2019.
#gilli_fox_appearance <- ymd("2019-06-02")
gilli_appearance_hits <- candidate_interest %>% 
    filter(keyword == "Kirsten Gillibrand", date == gilli_fox_appearance) %>%
    select(hits) %>% 
    as.numeric()

# Create labels for the points 
## Note: How can we make this dynamic? 
appearance_labels <- data.frame(
    candidate <- c("Sanders's Appearance", "Buttigieg's Appearance", "Gillibrand's Appearance"),
    date <- c(bernie_fox_appearance, pete_fox_appearance, gilli_fox_appearance),
    hits <- c(bernie_appearance_hits, pete_appearance_hits, gilli_appearance_hits), 
    stored_labels <- c("coral3", "dodgerblue3", "green3")
)

glimpse(candidate_interest)

candidate_interest$date <- as.Date(candidate_interest$date)
candidate_interest$hits <- as.numeric(candidate_interest$hits)
candidate_time_series <- ggplot(candidate_interest, aes(date, hits, color = keyword)) +
    geom_line() +
    theme_minimal() +
    geom_vline(xintercept = as.numeric(as.Date("2019-04-15")), linetype="dashed", color= "red") + # Bernie
    geom_vline(xintercept = as.numeric(as.Date("2019-05-21")), linetype="dashed", color= "dodgerblue") + # Pete
    geom_vline(xintercept = as.numeric(as.Date("2019-06-02")), linetype="dashed", color= "green")   # Gilli
candidate_time_series


candidate_time_series <- ggplot(candidate_interest, aes(date, hits, color = keyword)) +
    geom_line() +
    theme_minimal() +
    geom_point(size = 2, x = bernie_fox_appearance, y = bernie_appearance_hits, color = "coral3", fill = "coral1") +
    geom_point(size = 2, x = pete_fox_appearance, y = pete_appearance_hits, color = "dodgerblue3", fill = "dodgerblue1") +
    geom_point(size = 2, x = gilli_fox_appearance, y = gilli_appearance_hits, color = "green3", fill = "palegreen1") +    
    geom_text(data = appearance_labels, 
              inherit.aes = FALSE,
              mapping = aes(x = date, y = hits, label = candidate),
              nudge_y = 3,
              fontface = "bold")

candidate_time_series

ggplotly(candidate_time_series)   # Labels still do not dynamically display like I want them to in plotly,
# Namely, the dots/points disappear and then the names display the labels
# need help here, stuck on this idea.

# Here was another way with annotate
# Ctrl+Shift+C to bulk uncomment
# three_candidate_time_series <- ggplot(candidate_interest, aes(date, hits, color = keyword)) +
#     geom_line() +
#     theme_minimal() +
#     #Sanders
#     geom_point(size = 2, x = bernie_fox_appearance, y = bernie_appearance_hits, color = "coral3", fill = "coral1") +
#     annotate("text", x = bernie_fox_appearance+3, y = bernie_appearance_hits+3, label = "Sanders's Appearance") +
#     #Buttigieg
#     geom_point(size = 2, x = pete_fox_appearance, y = pete_appearance_hits, color = "dodgerblue3", fill = "dodgerblue1") +
#     annotate("text", x = pete_fox_appearance+3, y = pete_appearance_hits+4, label = "Buttigieg's Appearance") +
#     #Gillibrand
#     geom_point(size = 2, x = gilli_fox_appearance, y = gilli_appearance_hits, color = "green3", fill = "palegreen1") +
#     annotate("text", x = gilli_fox_appearance+3, y = gilli_appearance_hits+3, label = "Gillibrand's Appearance")
# 
# three_candidate_time_series
# 

