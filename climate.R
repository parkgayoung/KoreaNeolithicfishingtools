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
  geom_text_repel(data = site_early,
                  aes(long_dd ,
                      lat_dd,
                      label = ID),
                  size = 3,
                  #bg.color = "white",
                  bg.r = 0.1) +
  scale_colour_viridis_c(name = "MAT") +
  theme_gray(base_family='Apple SD Gothic Neo') +
  ggtitle("Site Location and MAT: early") +
  theme(plot.title = element_text(size=18))

dev.off()

###################
# initial period
site_initial <-
  site %>%
  select(site_name,
         lat_dd,
         long_dd,
         elevation,
         period) %>%
  filter(period == "initial") %>% 
  rowwise()  %>%
  mutate(lonID = which.min(abs(longitude - long_dd)),
         latID = which.min(abs(latitude - lat_dd))) %>%
  mutate(mean_annual_temperature = list(tibble(year = years,
                                               mean_annual_temperature = mean_annual_temperature[lonID, latID, ]))) %>%
  unnest(mean_annual_temperature) %>%
  filter(between(year, -7950, -6450))%>% 
  filter(mean_annual_temperature >0 )



# try to make boxplot
initial_mat_per_site_plot <-
  ggplot(site_initial) +
  aes(y = mean_annual_temperature,
      x = reorder(site_name,
                  mean_annual_temperature)) +
  geom_boxplot() +
  coord_flip() +
  xlab("") +
  ylab("Mean annual temperature: initial (MAT, °C)") +
  theme_gray(base_family='Apple SD Gothic Neo')

#scatter plot
initial<- 
  ggplot(site_initial) +
  aes(y = mean_annual_temperature,
      x = reorder(site_name,
                  mean_annual_temperature)) +
  geom_point() +
  coord_flip() +
  xlab("") +
  ylab("Mean annual temperature: initial (MAT, °C)") +
  theme_gray(base_family='Apple SD Gothic Neo')



#create mean for multiple MAT
site_initial_mean <- site_initial %>% 
  group_by(site_name) %>% 
  summarise(av_mat = mean(mean_annual_temperature),
            long_dd = mean(long_dd),
            lat_dd = mean(lat_dd))

site_initial_mean <- tibble::rowid_to_column(site_initial_mean, "ID")


early_site_map_plot <-
  ggmap(map)  +
  geom_point(data = site_initial_mean,
             aes(long_dd ,
                 lat_dd,
                 colour = av_mat),
             size = 2.8) +
  geom_text_repel(data = site_initial_mean,
                  aes(long_dd ,
                      lat_dd,
                      label = ID),
                  size = 3,
                  #bg.color = "white",
                  bg.r = 0.1) +
  scale_colour_viridis_c(name = "MAT") +
  theme_gray(base_family='Apple SD Gothic Neo') +
  ggtitle("Site Location and MAT: initial") +
  theme(plot.title = element_text(size=18))


############ mid period
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
                  size = 3,
                  #bg.color = "white",
                  bg.r = 0.1) +
  scale_colour_viridis_c(name = "MAT") +
  theme_gray(base_family='Apple SD Gothic Neo')+
  ggtitle("Site Location and MAT: mid") +
  theme(plot.title = element_text(size=18))

#ggplot() + theme_void()+xlab(NULL)+ylab(NULL) +geom_table(data = data.tb, aes(x=0, y=0, label = tb)) 
#annotation_custom(tableGrob(table), xmin=126, xmax=130, ymin=34, ymax=38)
  
  
  #annotate(geom = "table", x =128, y = 36, label = list(table), vjust = 1, hjust = 0)


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


#create mean for multiple MAT
site_late_mean <- site_late %>% 
  group_by(site_name) %>% 
  summarise(av_mat = mean(mean_annual_temperature),
            long_dd = mean(long_dd),
            lat_dd = mean(lat_dd))

site_late_mean <- tibble::rowid_to_column(site_late_mean, "ID")


map_later <-
  get_stamenmap(bbox = c(left = 125.5,
                         bottom = 33,
                         right = 	130,
                         top = 38.6),
                zoom = 9)

late_site_map_plot <-
  ggmap(map_later)  +
  geom_point(data = site_late_mean,
             aes(long_dd ,
                 lat_dd,
                 colour = av_mat),
             size = 2.8) +
  geom_text_repel(data = site_late_mean,
                  aes(long_dd ,
                      lat_dd,
                      label = ID),
                  size = 3,
                  # bg.color = "white",
                  bg.r = 0.1) +
  scale_colour_viridis_c(name = "MAT") +
  theme_gray(base_family='Apple SD Gothic Neo') +
  ggtitle("Site Location and MAT: late") +
  theme(plot.title = element_text(size=18))




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
  ylab("Mean annual temperature: late (MAT, °C)") +
  theme_gray(base_family='Apple SD Gothic Neo')

#scatter plot
last <- ggplot(site_last) +
  aes(y = mean_annual_temperature,
      x = reorder(site_name,
                  mean_annual_temperature)) +
  geom_point() +
  coord_flip() +
  xlab("") +
  ylab("Mean annual temperature: last (MAT, °C)") +
  theme_gray(base_family='Apple SD Gothic Neo')


#map for last
site_last <- tibble::rowid_to_column(site_last, "ID")

last_site_map_plot <-
  ggmap(map_later)  +
  geom_point(data = site_last,
             aes(long_dd ,
                 lat_dd,
                 colour = mean_annual_temperature),
             size = 2.8) +
  geom_text_repel(data = site_last,
                  aes(long_dd ,
                      lat_dd,
                      label = ID),
                  size = 3,
                  #bg.color = "white",
                  bg.r = 0.1) +
  scale_colour_viridis_c(name = "MAT") +
  theme_gray(base_family='Apple SD Gothic Neo')+
  ggtitle("Site Location and MAT: last") +
  theme(plot.title = element_text(size=18))


########comparison


site_all <-
  site %>%
  select(site_name,
         lat_dd,
         long_dd,
         period) %>%
  rowwise()  %>%
  mutate(lonID = which.min(abs(longitude - long_dd)),
         latID = which.min(abs(latitude - lat_dd))) %>%
  mutate(mean_annual_temperature = list(tibble(year = years,
                                               mean_annual_temperature = mean_annual_temperature[lonID, latID, ]))) %>%
  unnest(mean_annual_temperature) %>%
  filter(between(year, -9950, -3450)) %>% 
  filter(mean_annual_temperature >0 ) %>% 
group_by(site_name) %>% 
  summarise(av_mat_all = mean(mean_annual_temperature),
            long_dd = mean(long_dd),
            lat_dd = mean(lat_dd), period, year)

ggplot() +
  geom_line(data = site_all,
            aes(-year,
                av_mat_all,
                group = site_name),
            colour = "grey90") +
  geom_line(data = site_all %>%
              group_by(year) %>%
              summarise(av_mat_all = mean(av_mat_all, na.rm = TRUE)),
            aes(-year,
                av_mat_all),
            size = 2) +
  scale_x_continuous(labels = scales::comma) +
  coord_cartesian(ylim = c(1, 12)) +
  theme_minimal() +
  labs(y = "Mean annual temperature (MAT, °C)",
       x = "Year (BP)")


