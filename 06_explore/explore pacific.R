# some illustrative analysis with the SPI data the bank
# has committed in the .Rdata object
#

#---------------funcationality and data prep---------------

library(spcstyle)
library(RColorBrewer)
library(tidyverse)

# load previously prepared version of the data
load(".RData")

# recommended for familiarisation: 
# View(SPI) 
# View(metadata) 
#
# noting the column names in metadata don't exactly match those in SPI! but they
# are mostly right.

picts <- c(
  "Australia",
  "New Zealand",                    
  "France",
  "Fiji",  
  "Samoa",
  "Solomon Islands",
  "Papua New Guinea",
  "Vanuatu",             
  "Kiribati",                      
  "Micronesia, Fed. Sts.",         
  "Marshall Islands",        
  "American Samoa",                
  "French Polynesia",              
  "Guam",             
  "Nauru",                         
  "New Caledonia",
  "Northern Mariana Islands",
  "Palau",     
  "Tonga",                        
  "Tuvalu"
)


# Pacific-only version of the SPI data:
spi_pac <- filter(SPI, country %in% picts)

#--------------------time series--------------
# Colour palette:
pal <- c(spc_cols(1:5), "black")

p <- spi_pac |>
  select(country, date:SPI.INDEX) |>
  gather(variable, value, -country, -date) |>
  left_join(metadata, by = c("variable" = "series")) |>
  mutate(isnt_pillar = !grepl("Overall Score", indicator_name)) |>
  filter(!is.na(value)) |>
  mutate(country = fct_reorder(country, value)) |>
  ggplot(aes(x = date, y = value, colour = indicator_name)) +
  geom_line(aes(linetype = isnt_pillar), linewidth = 1.1) +
  geom_line(linewidth = 0.5, alpha = 0.8) +
  facet_wrap(~country) +
  scale_colour_manual(values = pal) +
  scale_linetype_manual(guide = "none", values = c(1,0)) +
  labs(x = "", colour = "", y = "SPI Score",
       title = "Statistical Performance Indicator scores",
       subtitle = "All PICTs for whom data is available and selected other SPC members")

png("06_explore/spi_results_pac.png", 6000, 3000, res = 600, type = "cairo-png")
print(p)
dev.off()


#---------------------------------------some tables-----------

# which countries have which Pillar 4 indicators (ag survey,
# ag census, business census, business survey, etc)
spi_pac |>
  select(country, date, SPI.D4.1.1.POPU:SPI.D4.2.3.CRVS) |>
  gather(variable, value, -country, -date) |>
  group_by(variable, country) |>
  arrange(desc(date)) |>
  slice(1) |>
  mutate(variable = gsub(".*\\.", "", variable)) |>
  ungroup() |>
  select(country, variable, value) |>
  spread(variable, value)


# which year is hte latest? (always 2019)
spi_pac |>
  select(country, date, SPI.D4.1.1.POPU:SPI.D4.2.3.CRVS) |>
  gather(variable, value, -country, -date) |>
  filter(value != 0) |>
  group_by(variable, country) |>
  arrange(desc(date)) |>
  slice(1) |>
  mutate(variable = gsub(".*\\.", "", variable)) |>
  ungroup() |>
  select(country, variable, date) |>
  spread(variable, date)
