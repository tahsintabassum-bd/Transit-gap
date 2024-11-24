# Loading the necessary Libraries
library(RColorBrewer) 
library(ggplot2)      
library(colorspace)
library(dplyr)
library(tidyr)
library(readr)
library(sf)
library(terra)
# Read the shapefile of Loudoun County
loudoun_county <- st_read("Loudon_cnty_Albers.shp")
#Subset the shapefile to get the eastern portion
eastern_loudoun <- subset(loudoun_county, ZONE == "East")
# Read NLCD data for all years
nlcd_dt <- rast(c("NLCD_2001_Land_Cover_LoudonVA.tiff",
                  "NLCD_2004_Land_Cover_LoudonVA.tiff",
                  "NLCD_2006_Land_Cover_LoudonVA.tiff",
                  "NLCD_2008_Land_Cover_LoudonVA.tiff",
                  "NLCD_2011_Land_Cover_LoudonVA.tiff",
                  "NLCD_2013_Land_Cover_LoudonVA.tiff",
                  "NLCD_2016_Land_Cover_LoudonVA.tiff"))
# Defining the Function to plot raster data values
rasterdf <- function(x, aggregate = 1) {
  resampleFactor <- aggregate        
  inputRaster <- x    
  inCols <- ncol(inputRaster)
  inRows <- nrow(inputRaster)
  # Compute numbers of columns and rows in the resampled raster
  resampledRaster <- rast(ncol=(inCols / resampleFactor), 
                          nrow=(inRows / resampleFactor),
                          crs = crs(inputRaster))
  # Match to the extent of the original raster
  ext(resampledRaster) <- ext(inputRaster)
  # Resample data on the new raster
  y <- resample(inputRaster,resampledRaster,method='near')
  # Extract cell coordinates into a data frame
  coords <- xyFromCell(y, seq_len(ncell(y)))
  # Extract layer names
  dat <- stack(values(y, dataframe = TRUE))
  # Add names - 'value' for data, 'variable' for different
  # layer names in a multilayer raster
  names(dat) <- c('value', 'variable')
  dat <- cbind(coords, dat)
  dat
}
# Defining the Levels for NLCD data and Reclassified Data
LCnames <-c(
  "Water",
  "DevelopedOpen",
  "DevelopedLow",
  "DevelopedMed",
  "DevelopedHigh",
  "Barren",
  "DeciduousForest",
  "EvergreenForest",
  "MixedForest",
  "ShrubScrub",
  "GrassHerbaceous",
  "PastureHay",
  "CultCrops",
  "WoodyWetlands",
  "EmergentHerbWet")

newnames <- c("Water",
              "Developed",
              "Barren",
              "Forest",
              "GrassShrub",
              "Cropland",
              "Wetland")
newcols <- c("mediumblue", 
             "firebrick2", 
             "gray60", 
             "darkgreen", 
             "yellow2", 
             "orange4", 
             "paleturquoise2")
newcols2 <- desaturate(newcols, amount = 0.4)

lookup <- data.frame(LCcodes = c(11, 21, 22, 23, 24, 31, 41, 42, 43, 52, 71, 81, 82, 90, 95),
                     newclas = c(1, 2, 2, 2, 2, 3, 4, 4, 4, 5, 5, 5, 6, 7, 7))
# Reclassify NLCD Data
nlcd_rc <- classify (nlcd_dt, lookup)
names(nlcd_rc) <- c("2001", "2004", "2006", "2008", "2011", "2013", "2016")

nlcd_rc_df  <- rasterdf(nlcd_rc[[c("2001", "2006",  "2011", "2016")]])

ggplot(data = nlcd_rc_df) +
  geom_raster(aes(x = x, 
                  y = y, 
                  fill = as.character(value))) + 
  scale_fill_manual(name = "Land cover",
                    values = newcols2,
                    labels = newnames,
                    na.translate = FALSE) +
  coord_sf(expand = TRUE) +
  facet_wrap(facets = vars(variable), ncol = 2) +
  theme_void() +
  theme(strip.text.x = element_text(size = 12, face="bold"),
        legend.position="bottom")

## 1. Create a set of maps that show the changes in land cover patterns over time.


loudoun_county <- st_read("Loudon_cnty_Albers.shp", quiet = TRUE)
loudoun_county <- st_transform(loudoun_county, crs(nlcd_rc))

nlcd_rc_crp <- crop(nlcd_rc, vect(loudoun_county))
nlcd_rc_msk <- mask(nlcd_rc_crp, vect(loudoun_county))

nlcd_msk_df <- rasterdf(nlcd_rc_msk[[c("2001", "2006","2011", "2016")]])

ggplot(data = nlcd_msk_df) +
  geom_raster(aes(x = x, 
                  y = y, 
                  fill = as.character(value))) + 
  scale_fill_manual(name = "Land cover",
                    values = newcols2,
                    labels = newnames,
                    na.translate = FALSE) +
  coord_sf(expand = TRUE) +
  facet_wrap(facets = vars(variable), ncol = 2) +
  theme_void() +
  theme(strip.text.x = element_text(size=12, face="bold"),
        legend.position="bottom")

```

## 2. Create a line graph that shows changes in the areas of the land cover classes over time.

freq_df <- freq(nlcd_rc_msk, usenames=TRUE)
glimpse(freq_df)


nlcd_chg <- freq_df %>%
  mutate(km2 = count * 900 / 1000000,
         class = factor(value,
                        levels = 1:7,
                        labels = newnames),
         year = as.numeric(layer))

ggplot(data = nlcd_chg) +
  geom_line(aes(x = year, y = km2, color = class)) +
  geom_point(aes(x = year, y = km2, color = class)) +
  scale_color_manual(name = "Land Cover Class",
                     values = newcols) +
  labs(x = "Year", y = expression("Area(km"^2*")")) +
  theme_classic()


ggplot(data = nlcd_chg) +
  geom_line(aes(x = year, y = km2)) +
  facet_wrap(facets = vars(class), ncol = 4) +
  labs(x = "Year", y = expression("Area(km"^2*")")) +
  theme_bw()

ggplot(data = nlcd_chg) +
  geom_line(aes(x = year, y = km2)) +
  facet_wrap(facets = vars(class), 
             scales = "free_y", 
             ncol = 4) +
  labs(x = "Year", y = expression("ha")) +
  theme_bw()
## 3. Generate a land cover transmission matrix for change between 2001 and 2016. Create two bar charts that display the changes as (1) total areas, and (2) percent of class area in 2001.

```{r}

changeras <- c(nlcd_rc_msk[["2001"]], 
               nlcd_rc_msk[["2016"]])
changeout <- crosstab(changeras)
class(changeout)
changedf <- as_tibble(changeout)
class(changedf)

shortnames <- c("Wat", 
                "Dev", 
                "Bare", 
                "For", 
                "Grass", 
                "Crop", 
                "Wet")

changedf <- changedf %>%
  mutate(X2001 = factor(X2001,
                        levels = 1:7,
                        labels = shortnames),
         X2016 = factor(X2016,
                        levels = 1:7,
                        labels = shortnames),
         ha = n * 900/10000) %>%
  group_by(X2001) %>%
  mutate(tot2001 = sum(ha),
         perc = 100 * ha / tot2001)
changemat <- matrix(changedf$ha, 
                    nrow = 7, 
                    ncol = 7)
rownames(changemat) <- shortnames
colnames(changemat) <- shortnames

percmat <- matrix(changedf$perc, 
                  nrow = 7, 
                  ncol = 7)
rownames(percmat) <- shortnames
colnames(percmat) <- shortnames
changemat 
percmat
ggplot(data = changedf) +
  geom_bar(aes(x = X2001, y = ha, group = X2016, fill = X2016), color = "black", position = "dodge", stat = "identity") +
  scale_fill_manual(name = "2016 Land Cover", values = newcols) + # Ensure 'newcols' has appropriate color codes
  labs(x = "2001 Land Cover", y = expression("ha"), title = "Change in Land Cover from 2001 to 2016",) +
  theme_bw() + # Clean theme for better aesthetics
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(), # Removes major grid lines for a cleaner look
        panel.grid.minor = element_blank(), # Removes minor grid lines
        plot.background = element_rect(fill = "white", color = NA), # White plot background
        panel.border = element_rect(color = "black", fill = NA, size = 0.5), # Adds a border around the plot
        axis.text.x = element_text(angle = 45, hjust = 1), # Rotate X axis labels for better readability
        legend.title = element_text(face = "bold"), # Bold legend title
        legend.text = element_text(size = 10)) + # Adjust legend text size
  guides(fill = guide_legend(title.position = "left", title.hjust = 0.5)) 

```

```{r}

ggplot(data = changedf) +
  geom_bar(aes(x = X2001, 
               y = perc, 
               group = X2016, 
               fill = X2016),
           color = "black", position = "dodge", stat = "identity") +
  scale_fill_manual(name = "2016 Land Cover", values = newcols) + 
  labs(x = "2001 Land Cover", y = "% Area",
       title = "Change in Land Cover from 2001 to 2016",
       subtitle = "Comparative analysis of percentage area change by land cover type",
       caption = "Source: NLCD Data") +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        plot.background = element_rect(fill = "white", color = NA),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        legend.title = element_text(face = "bold"),
        legend.text = element_text(size = 10),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14), 
        plot.subtitle = element_text(hjust = 0.5, size = 12), 
        plot.caption = element_text(hjust = 1, face = "italic", size = 8)) + 
  guides(fill = guide_legend(title.position = "left", title.hjust = 0.5))

