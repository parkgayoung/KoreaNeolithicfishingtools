require("ncdf4")
require("lattice")
require("ggplot2")

# you'll need to get this file from
# https://figshare.com/articles/LateQuaternary_Environment_nc/12293345/3
# and move to our raw_data folder
file <- here::here("raw_data/LateQuaternary_Environment.nc")

env_nc      <- ncdf4::nc_open(file)
longitude   <- ncdf4::ncvar_get(env_nc, "longitude") 
latitude    <- ncdf4::ncvar_get(env_nc, "latitude")
years       <- ncdf4::ncvar_get(env_nc, "time")
months      <- ncdf4::ncvar_get(env_nc, "month")
temperature <- ncdf4::ncvar_get(env_nc, "temperature")
biome       <- ncdf4::ncvar_get(env_nc, "biome")
ncdf4::nc_close(env_nc)
 
mean_annual_temperature <- apply(temperature, c(1, 2, 4), mean)

# korean archaeological sites
library(tidyverse)
library(here)

site <-
  readxl::read_excel(here("raw_data/location_edit.xlsx"))

# early period
site_early <-
  site %>%
  select(site_name,
         lat_dd,
         long_dd,
         elevation,
         period) %>%
  filter(period == "early") %>% 
  rowwise()  %>%
  mutate(lonID = which.min(abs(longitude - long_dd)),
         latID = which.min(abs(latitude - lat_dd))) %>%
  mutate(mean_annual_temperature = list(tibble(year = years,
                                               mean_annual_temperature = mean_annual_temperature[lonID, latID, ]))) %>%
  unnest(mean_annual_temperature) %>%
  filter(between(year, -6450, -5450)) %>% 
  filter(mean_annual_temperature >0 )

# try to make boxplot
mat_per_site_plot <-
  ggplot(site_early) +
  aes(y = mean_annual_temperature,
      x = reorder(site_name,
                  mean_annual_temperature)) +
  geom_boxplot() +
  coord_flip() +
  xlab("") +
  ylab("Mean annual temperature (MAT, °C)") +
  theme_gray(base_family='Apple SD Gothic Neo')

#scatter plot
early <-ggplot(site_early) + 
  aes(y = mean_annual_temperature,
      x = reorder(site_name,
                  mean_annual_temperature)) +
  geom_point() +
  coord_flip() +
  xlab("") +
  ylab("Mean annual temperature: early (MAT, °C)") +
  theme_gray(base_family='Apple SD Gothic Neo')

#Korea map
library(ggmap)
library(ggrepel)
library(showtext)
showtext_auto()

# download background tiles for the map
map <-
  get_stamenmap(bbox = c(left = 125.5,
                         bottom = 34,
                         right = 	130,
                         top = 38.5),
                zoom = 9)

site_early <- tibble::rowid_to_column(site_early, "ID")

early_site_map_plot <-
  ggmap(map)  +
  geom_point(data = site_early,
             aes(long_dd ,
                 lat_dd,
                 colour = mean_annual_temperature),
             size = 2.8) +
  geom_text_repel(data = site_mid,
                  aes(long_dd ,
                      lat_dd,
                      label = ID),
                  size = 2,
                  bg.color = "white",
                  bg.r = 0.1) +
  scale_colour_viridis_c(name = "MAT") +
  theme_gray(base_family='Apple SD Gothic Neo')


###################

# mid period
site_mid <-
  site %>%
  select(site_name,
         lat_dd,
         long_dd,
         elevation,
         period) %>%
  filter(period == "mid") %>% 
  rowwise()  %>%
  mutate(lonID = which.min(abs(longitude - long_dd)),
         latID = which.min(abs(latitude - lat_dd))) %>%
  mutate(mean_annual_temperature = list(tibble(year = years,
                                               mean_annual_temperature = mean_annual_temperature[lonID, latID, ]))) %>%
  unnest(mean_annual_temperature) %>%
  filter(between(year, -5450, -4950))%>% 
  filter(mean_annual_temperature >0 )

mid_mat_time_series_plot <-
  ggplot() +
  geom_line(data = site_mid,
            aes(-year,
                mean_annual_temperature,
                group = site_name),
            colour = "grey90") +
  geom_line(data = site_mid %>%
              group_by(year) %>%
              summarise(mean_mean_annual_temperature = mean(mean_annual_temperature,
                                                            na.rm = TRUE)),
            aes(-year,
                mean_mean_annual_temperature),
            size = 2) +
  scale_x_continuous(labels = scales::comma) +
  coord_cartesian(ylim = c(1, 12)) +
  theme_gray(base_family='Apple SD Gothic Neo') +
  labs(y = "Mean annual temperature (MAT, °C)",
       x = "Year (BP)")

# try to make boxplot
mid_mat_per_site_plot <-
  ggplot(site_mid) +
  aes(y = mean_annual_temperature,
      x = reorder(site_name,
                  mean_annual_temperature)) +
  geom_boxplot() +
  coord_flip() +
  xlab("") +
  ylab("Mean annual temperature (MAT, °C)") +
  theme_gray(base_family='Apple SD Gothic Neo')

#scatter plot
mid<- 
ggplot(site_mid) +
  aes(y = mean_annual_temperature,
      x = reorder(site_name,
                  mean_annual_temperature)) +
  geom_point() +
  coord_flip() +
  xlab("") +
  ylab("Mean annual temperature: mid (MAT, °C)") +
  theme_gray(base_family='Apple SD Gothic Neo')


# map

site_mid <- tibble::rowid_to_column(site_mid, "ID")


table <- data.frame(cbind(ID=site_mid$ID, site_name=site_mid$site_name))
#data.tb <- tibble(x = 127, y = 38, tb = list(table))

#library(ggpmisc)
#library(gridExtra)

mid_site_map_plot <-
  ggmap(map)  +
  geom_point(data = site_mid,
             aes(long_dd ,
                 lat_dd,
                 colour = mean_annual_temperature),
             size = 2.8) +
  geom_text_repel(data = site_mid,
                  aes(long_dd ,
                      lat_dd,
                      label = ID),
                  size = 2,
                  bg.color = "white",
                  bg.r = 0.1) +
  scale_colour_viridis_c(name = "MAT") +
  theme_gray(base_family='Apple SD Gothic Neo')

#ggplot() + theme_void()+xlab(NULL)+ylab(NULL) +geom_table(data = data.tb, aes(x=0, y=0, label = tb)) 
#annotation_custom(tableGrob(table), xmin=126, xmax=130, ymin=34, ymax=38)
  
  
  #annotate(geom = "table", x =128, y = 36, label = list(table), vjust = 1, hjust = 0)
 
library(ggpubr)
# Summary table plot, medium orange theme
stable.p <- ggtexttable(table, rows = NULL, 
                        theme = ttheme("blank"))


ggarrange(mid_site_map_plot , stable.p, 
          ncol = 2, nrow = 1,
          heights = c(1, 0.5))

library(cowplot)

plot_grid(mid_site_map_plot , stable.p,
          ncol = 1)

###################

# late period
site_late <-
  site %>%
  select(site_name,
         lat_dd,
         long_dd,
         elevation,
         period) %>%
  filter(period == "late") %>% 
  rowwise()  %>%
  mutate(lonID = which.min(abs(longitude - long_dd)),
         latID = which.min(abs(latitude - lat_dd))) %>%
  mutate(mean_annual_temperature = list(tibble(year = years,
                                               mean_annual_temperature = mean_annual_temperature[lonID, latID, ]))) %>%
  unnest(mean_annual_temperature) %>%
  filter(between(year, -4950, -3000))%>% 
  filter(mean_annual_temperature >0 )


# try to make boxplot
late_mat_per_site_plot <-
  ggplot(site_late) +
  aes(y = mean_annual_temperature,
      x = reorder(site_name,
                  mean_annual_temperature)) +
  geom_boxplot() +
  coord_flip() +
  xlab("") +
  ylab("Mean annual temperature: late (MAT, °C)") +
  theme_gray(base_family='Apple SD Gothic Neo')

#scatter plot
late <- ggplot(site_late) +
  aes(y = mean_annual_temperature,
      x = reorder(site_name,
                  mean_annual_temperature)) +
  geom_point() +
  coord_flip() +
  xlab("") +
  ylab("Mean annual temperature: late (MAT, °C)") +
  theme_gray(base_family='Apple SD Gothic Neo')


#map for late
site_late <- tibble::rowid_to_column(site_late, "ID")

late_site_map_plot <-
  ggmap(map)  +
  geom_point(data = site_late,
             aes(long_dd ,
                 lat_dd,
                 colour = mean_annual_temperature),
             size = 2.8) +
  geom_text_repel(data = site_mid,
                  aes(long_dd ,
                      lat_dd,
                      label = ID),
                  size = 2,
                  bg.color = "white",
                  bg.r = 0.1) +
  scale_colour_viridis_c(name = "MAT") +
  theme_gray(base_family='Apple SD Gothic Neo')


#----------------------------------------------------------------------

# combine plots
library(cowplot)

plot_grid(early,
          mid,
          late,
          ncol = 1)

plot_grid(early_site_map_plot, 
          mid_site_map_plot, 
          late_site_map_plot, 
          ncol = 3) 

################

# initial-early
site_initial_early <-
  site %>%
  select(site_name,
         lat_dd,
         long_dd,
         elevation,
         period) %>%
  filter(period == c("initial","early")) %>% 
  rowwise()  %>%
  mutate(lonID = which.min(abs(longitude - long_dd)),
         latID = which.min(abs(latitude - lat_dd))) %>%
  mutate(mean_annual_temperature = list(tibble(year = years,
                                               mean_annual_temperature = mean_annual_temperature[lonID, latID, ]))) %>%
  unnest(mean_annual_temperature) %>%
  filter(between(year, -9950, -5450))%>% 
  filter(mean_annual_temperature >0 )


# try to make boxplot
initial_early_mat_per_site_plot <-
  ggplot(site_initial_early) +
  aes(y = mean_annual_temperature,
      x = reorder(site_name,
                  mean_annual_temperature)) +
  geom_boxplot() +
  coord_flip() +
  xlab("") +
  ylab("Mean annual temperature: initial&early (MAT, °C)") +
  theme_gray(base_family='Apple SD Gothic Neo')

#-------------

# mid-late
site_mid_late <-
  site %>%
  select(site_name,
         lat_dd,
         long_dd,
         elevation,
         period) %>%
  filter(period == c("mid","late")) %>% 
  rowwise()  %>%
  mutate(lonID = which.min(abs(longitude - long_dd)),
         latID = which.min(abs(latitude - lat_dd))) %>%
  mutate(mean_annual_temperature = list(tibble(year = years,
                                               mean_annual_temperature = mean_annual_temperature[lonID, latID, ]))) %>%
  unnest(mean_annual_temperature) %>%
  filter(between(year, -5450, -4150))%>% 
  filter(mean_annual_temperature >0 )


# try to make boxplot
mid_late_mat_per_site_plot <-
  ggplot(site_mid_late) +
  aes(y = mean_annual_temperature,
      x = reorder(site_name,
                  mean_annual_temperature)) +
  geom_boxplot() +
  coord_flip() +
  xlab("") +
  ylab("Mean annual temperature: mid&late (MAT, °C)") +
  theme_gray(base_family='Apple SD Gothic Neo')

# last
site_last <-
  site %>%
  select(site_name,
         lat_dd,
         long_dd,
         elevation,
         period) %>%
  filter(period == "last") %>% 
  rowwise()  %>%
  mutate(lonID = which.min(abs(longitude - long_dd)),
         latID = which.min(abs(latitude - lat_dd))) %>%
  mutate(mean_annual_temperature = list(tibble(year = years,
                                               mean_annual_temperature = mean_annual_temperature[lonID, latID, ]))) %>%
  unnest(mean_annual_temperature) %>%
  filter(between(year, -4150, -3450))%>% 
  filter(mean_annual_temperature >0 )


# try to make boxplot
last_mat_per_site_plot <-
  ggplot(site_last) +
  aes(y = mean_annual_temperature,
      x = reorder(site_name,
                  mean_annual_temperature)) +
  geom_boxplot() +
  coord_flip() +
  xlab("") +
  ylab("Mean annual temperature: mid&late (MAT, °C)") +
  theme_gray(base_family='Apple SD Gothic Neo')
