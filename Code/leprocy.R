rm(list = ls())
graphics.off()

library(data.table)
library(ggpubr)
library(plotly)
library(htmlwidgets)


### LOAD DATA ==========

cases <- fread("../Data/leprocy_by_country_WHO_accessed_251122.csv.gz")
gdp <- fread("../Data/GDP_by_coountry_datahub.io_accessed_251122.csv.gz")
population <- fread("../Data/population_by_country_UN_accessed_251122.csv.gz")


### FORMAT DATA ==========

# replace blankspace with _ in colnames
colnames(gdp) <- sub(" ", "_", colnames(gdp))

# subset relevant columns 
cases <- cases[, c("ParentLocation", "SpatialDimValueCode", "Location", "Period", "FactValueNumeric")]
gdp <- gdp[, c("Country_Code", "Year", "Value")]
population <- population[, c("ISO3_code", "Time", "PopTotal")]

# change column names
names(gdp)[names(gdp) == "Value"] <- "GDP"
names(cases) <- c("ParentLocation", "Country_Code", "Country_Name", "Year", "n_cases")
names(population) <- c("Country_Code", "Year", "PopTotal")

# merge data 
dt <- gdp[cases, on = c("Country_Code", "Year")]
dt <- population[dt, on = c("Country_Code", "Year")]

# calculate cases per capita
dt$PopTotal <- dt$PopTotal*1000
dt$n_cases_per_capita <- (dt$n_cases/dt$PopTotal)*100000
hist(dt$n_cases_per_capita)

# aggregate cases by parent location
agg <- aggregate(x = list(PopTotal = dt$PopTotal, n_cases = dt$n_cases), 
                 by = list(ParentLocation = dt$ParentLocation, Year = dt$Year), 
                 FUN = sum, na.rm = TRUE)
agg$n_cases_per_capita <- (agg$n_cases/agg$PopTotal)*100000


### PLOT ==========

## cases over time line graph
p_line <- ggline(agg, "Year", "n_cases_per_capita",
       linetype = "ParentLocation", color = "ParentLocation", palette = "jco") +
  xlab("Year") + ylab("Reported cases per 100,000 people") +
  rremove("legend.title")
p_line

## cases over time choropleth map
p_map <- plot_geo(dt) %>%
  add_trace(
  z = ~n_cases_per_capita, 
  color = ~n_cases_per_capita, colors = 'Reds',
  text = ~Country_Name, 
  locations = ~Country_Code, 
  # marker = list(line =  list(color = toRGB("grey"), width = 0.5)), # light grey boundaries
  frame=~Year
) %>%
  colorbar(title = 'Cases per 100,000') %>% 
  layout(
    geo = list(scope = "world", 
               showocean = TRUE, 
               oceancolor="lightblue", 
               showland = TRUE, 
               landcolor = "cccccc"),
    title = 'Reported leprocy cases<br>Source:<a href="https://www.who.int/health-topics/neglected-tropical-diseases#tab=tab_1">WHO</a>'
)
p_map


### EXPORT ==========

ggsave(filename = "../Results/cases_line_graph.jpg", plot = p_line, 
       width = 6, height = 5, units = "in")

saveWidget(p_map, "../Results/cases_chorograph_map.html", selfcontained = F, libdir = "../Results/lib")



