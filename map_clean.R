library(maps)
library(leaflet)

# function to capitalize words
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

# mapStates = map("state", fill = TRUE)
# data("us.cities")
map.cities(us.cities)
# leaflet(mapStates) %>% addTiles() %>% setView(lng = -97.00, lat = 38.00, zoom = 4)

# import and clean data
alumni <- read.csv("../TUSOM Residency Match List 2010-2015 - Sheet1.csv", stringsAsFactors = F, header = T)
# convert city to lowercase then capitalize it, then paste it together with the state
# we're doing this to match the alumni data to the us.cities data
alumni$city.state <- paste(capwords(tolower(alumni$City)), alumni$State)
head(alumni) #sanity check

# merge the data by the city.state column we just created and the name column in the us.cities data
alumni.map.data <- merge(alumni, us.cities, by.x = "city.state", by.y = "name")
sorted.data <- sort(unique(alumni.map.data$Primary.Specialty))

paste0('<a href = ', "https://www.google.com/#q=", "Leonard Walsh", ">", "Leonard Walsh", ">" )

# make the map
# formatted as html with a link to search google with the peron's name
alumni.map.vis <- leaflet(alumni.map.data) %>%
  addTiles() %>%
  addMarkers(
        popup = ~as.character(paste(sep = "<br/>",
                              paste0('<a href = ', #begin link format to search google
                                     "https://www.google.com/#q=",
                                     paste(alumni.map.data$First.Name,
                                           alumni.map.data$Last.Name, "Tulane", sep = "+"), #search elements
                                     ">",
                                     paste(alumni.map.data$First.Name, alumni.map.data$Last.Name, sep = " "),
                                     '</a>'),  # end link format to search google
                              paste("Class of", alumni.map.data$Class.Year, sep = " "), alumni.map.data$Program,
                              alumni.map.data$Primary.Specialty)),
  clusterOptions = markerClusterOptions()
)
alumni.map.vis

# With layer control
# map <- leaflet(alumni.map.data) %>% 
#   addTiles() %>% 
#   addCircles(
#     data=alumni.map.data, group = alumni.map.data$Primary.Specialty,
#     popup = ~as.character(paste(sep = "<br/>", 
#                                 paste0('<a href = ', #begin link format to search google
#                                        "https://www.google.com/#q=",
#                                        paste(alumni.map.data$First.Name, 
#                                              alumni.map.data$Last.Name, "Tulane", sep = "+"), #search elements
#                                        ">", 
#                                        paste(alumni.map.data$First.Name, alumni.map.data$Last.Name, sep = " "), 
#                                        '</a>'),  # end link format to search google
#                                 paste("Class of", alumni.map.data$Class.Year, sep = " "), alumni.map.data$Program, 
#                                 alumni.map.data$Primary.Specialty))
#   ) %>%
#   addLayersControl(
#     overlayGroups = alumni.map.data$Primary.Specialty,
#     options = layersControlOptions(collapsed = TRUE)
#   )