library("dplyr")
library("tidyverse")
library("ggplot2")
library("maps")
library("mapproj")
library("patchwork")
library("stats")
library("tidyr")
library("reshape2")
install.packages("rmarkdown", type = "source")

#Loading data

incarceration <- read.csv("incarceration_trends.csv")
incarceration

a3_incarceration_df <- incarceration %>% 
  summarize(year, fips, state, county_name, black_pop_15to64, black_prison_pop, black_prison_adm_rate, 
                    black_prison_adm, black_prison_pop_rate, white_pop_15to64, 
                    white_prison_adm, white_prison_pop_rate, white_prison_adm_rate, female_prison_pop, female_prison_pop_rate,
            male_prison_pop_rate, aapi_prison_pop_rate, black_prison_pop_rate, latinx_prison_pop_rate, native_prison_pop_rate, 
            white_prison_pop_rate, female_prison_adm_rate, native_prison_adm_rate, 
            total_prison_adm_rate, male_prison_adm_rate, aapi_prison_adm_rate, black_prison_adm_rate, 
            latinx_prison_adm_rate, white_prison_adm_rate, black_female_prison_adm, black_male_prison_adm,
            white_female_prison_adm, female_prison_adm)
a3_incarceration_df


incarceration_df <- na.omit(a3_incarceration_df)
incarceration_df

#1) In what year was there the highest number of admitted black individuals in prison?

most_black_adm <- incarceration_df %>% 
  filter(black_prison_adm == max(na.omit(black_prison_adm))) %>% 
  pull(year)
most_black_adm

#2) In which state did this occur? 

most_black_adm_state <- incarceration_df %>% 
  filter(year == "1998") %>% 
  filter(black_prison_adm == max(na.omit(black_prison_adm))) %>% 
  pull(state)
most_black_adm_state

#3) How does the average number of black incarcerations in 2004 (in IL across all counties) compare to 
#that of the white incarcerations?

CA_1998 <- data.frame(
  incarceration_df %>% 
    filter(year == "1998") %>% 
    filter(state == "CA") %>% 
    select(year, county_name, black_prison_adm, white_prison_adm)
)
CA_1998

mean_black_adm <- mean(CA_1998$black_prison_adm)
mean_black_adm
#1590.417
mean_white_adm <- mean(CA_1998$white_prison_adm)
mean_white_adm
#1457.292


#4) What was the percent change in average incarceraton admissions (in both black and white populations) 
# from 1998 to 2010? 


#**1198**
df_1998 <- data.frame(
  incarceration_df %>% 
    filter(year == "1998") %>% 
    select(year, county_name, state, black_prison_adm, white_prison_adm)
)
df_1998

mean_black_adm <- mean(df_1998$black_prison_adm)
mean_black_adm
#513.1569
mean_white_adm <- mean(df_1998$white_prison_adm)
mean_white_adm
#394.8039

#**2010**

df_2010 <- data.frame(
  incarceration_df %>% 
    filter(year == "2010") %>% 
    select(year, county_name, state, black_prison_adm, white_prison_adm)
)
df_2010

mean_black_adm_2010 <- mean(df_2010$black_prison_adm)
mean_black_adm_2010
#259.046
mean_white_adm_2010 <- mean(df_2010$white_prison_adm)
mean_white_adm_2010
#270.3295

black_inc_percent_change <- mean_black_adm/mean_black_adm_2010
black_inc_percent_change
#1.739244
white_inc_percent_change <- mean_white_adm/mean_white_adm_2010
white_inc_percent_change
#1.460454

#5) How has the ratio of incarcerated black individuals to white individuals changed from 1998 to 2010?

ratio_1998 <- mean_white_adm/mean_black_adm
ratio_1998
#0.769363
ratio_2010 <- mean_white_adm_2010/mean_black_adm_2010
ratio_2010
#0.9162284


#data frame is showing the ratio between the average black inc. rate and average white inc. rate each year
#chart 1 

grouped_data_year <- group_by(incarceration_df, year)
grouped_data_year
grouped_data <- grouped_data_year %>% 
  summarize(mean_black_prison_adm_rate = mean(black_prison_adm_rate),
            mean_white_prison_adm_rate = mean(white_prison_adm_rate,
            mean_aapi_prison_adm_rate = mean(aapi_prison_adm_rate),
            mean_latinx_prison_adm_rate = mean(latinx_prison_adm_rate)))
grouped_data

gdata <- group_by(incarceration_df, year)
gdata
mean_data <- gdata %>% 
  summarise(mean.black.prison.amd.rate = mean(black_prison_adm_rate), 
            mean.white.prison.amd.rate = mean(white_prison_adm_rate), 
            mean.appi.prison.amd.rate = mean(aapi_prison_adm_rate),
            mean.latinx.prison.amd.rate = mean(latinx_prison_adm_rate))
mean_data


chart <- ggplot(mean_data, aes(x = year), group = race) + 
  geom_line(aes(y = mean.black.prison.amd.rate), color = "red", show.legend = TRUE) + 
  geom_line(aes(y = mean.white.prison.amd.rate), color = "blue", show.legend = TRUE) +
  geom_line(aes(y = mean.appi.prison.amd.rate), color = "yellow", show.legend = TRUE) +
  geom_line(aes(y = mean.latinx.prison.amd.rate), color = "green", show.legend = TRUE) + 
  xlab("Year") +
  ylab("Avgerage Prison Admission Rate") +
  scale_colour_manual(name = "Race",
                      values = c("Black Prison Amd Rate" = "red", "White Prison Amd Rate" = "blue", 
                                 "Appi Prison Amd Rate" = "yellow", "Latinx Prison Amd Rate" = "green")) +
  ggtitle("Average Prison Admission Rates Race Comparison 1990-2016") 
chart

#chart 2: The second chart that you'll create and include will show how two different (continuous) variables are related to one another.
#Again, think carefully about what such a comparison means, a
#nd want to communicate to your user (you may have to find relevant trends in the dataset first!). Here are some requirements to help guide your design:


scatterplot_data <- incarceration_df %>% 
  filter(year == "2015") %>% 
  summarize(year, female_prison_adm, black_female_prison_adm,
            white_female_prison_adm)
scatterplot_data

chart2 <- ggplot(scatterplot_data, aes(x = female_prison_adm)) +
  xlab("Female Prison Admissions") +
  ylab("Black and White Female Prison Admissions") + 
  geom_point(aes(y = black_female_prison_adm), color = "red", show.legend = TRUE) +
  geom_point(aes(y = white_female_prison_adm), color = "blue", show.legend = TRUE) +
  scale_colour_manual("Race",
                      values = c("Black Female Prison Adm"="blue", "White Female Prison Adm"="red")) +
  ggtitle("U.S. Use of Pesticides in Agriculture 1990-2011")
chart2
  
#chart 3 map

county_shapes <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by="polyname")
county_shapes

map_data <- county_shapes %>% 
  left_join(incarceration_df, by ="fips")
map_data

data_for_map <- na.omit(map_data) 
data_for_map

incarceration_map_df <- map_data %>% 
  summarize(long, lat, group, order, polyname, fips, black_prison_adm_rate)
incarceration_map_df

grouped_map_data <- group_by(incarceration_map_df, polyname)
grouped_map_data

data_map <- grouped_map_data %>% 
  summarise(long, lat, group, order, polyname, fips, black_prison_adm_rate,
            avg.black_prison_adm_rate = mean(black_prison_adm_rate))
data_map

chart_3 <- ggplot(data_map, aes(x = long, y = lat, group=group)) + 
  geom_polygon(aes(fill = avg.black_prison_adm_rate), color = "black") +
  ggtitle("U.S. Use of Pesticides in Agriculture 1990-2011")
chart_3





