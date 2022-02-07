########################################
## National and NHS Regional Analysis
## Olivia Bryant
########################################

library(tidyverse)
library(ggplot2)
library(dplyr)
library(gganimate)
library(gifski)
library(tsModel)
library(sf)
library(patchwork)
library(scales)
library(ggrepel)
library(ggthemes)
library(zoo)
library(ggtext)

setwd("~/Documents/LSHTM/Data Challenge/Final Work/")

theme_set(theme_fivethirtyeight())
theme_update(axis.title = element_text(),
             plot.caption = element_text(hjust = 0, vjust = 0),
             plot.background = element_rect(fill = "white", colour = "white"),
             panel.background = element_rect(fill = "white", colour = "white"),
             legend.background = element_rect(fill = "white", colour = "white"))
##
# IMPORTS AND WRANGLING --------------------------------------------------------
##
# import regional files
regions <- read.csv("Analysis Dataset/NHS_England_regions_corticosterioid_prescriptions.csv")
# calculate prescription rate per 1000
regions$prescription_rate_per_1000 <- regions$items/(regions$list_size/1000)
head(regions)

# import NHS England region boundaries
NHS_regions_bound <- read_sf("Boundary Files/NHS_England_Regions_(April_2019)_EN_BFC.shp")

# change the regional names to match the government shapefiles
regions <- regions %>% 
  mutate(name = case_when(name == "LONDON COMMISSIONING REGION" ~ "London",
                          name == "EAST OF ENGLAND COMMISSIONING REGION" ~ "East of England",
                          name == "NORTH WEST COMMISSIONING REGION"  ~ "North West",
                          name ==  "SOUTH WEST COMMISSIONING REGION" ~ "South West",
                          name ==  "SOUTH EAST COMMISSIONING REGION" ~ "South East",
                          name ==  "MIDLANDS COMMISSIONING REGION" ~ "Midlands",
                          name ==  "NORTH EAST AND YORKSHIRE COMMISSIONING REGION" ~ "North East and Yorkshire")
  )


# --------------------------------------------------------------------------------
### TIME-SERIES, REGIONAL PLOTS

## Plot 1: Line chart of all NHS regions across the entire time period
regions$date <- as.Date(regions$date)

region_line_plot <- ggplot(data=regions, aes(x=date, y=prescription_rate_per_1000, color=name)) +
  geom_line(aes(x=date, y=prescription_rate_per_1000, group=name)) +
  theme_fivethirtyeight()+
  theme(
        legend.box="vertical",
        legend.position="right",
        legend.justification = c(1, 1),
        legend.title = element_blank(),
        axis.title = element_text(),
        plot.caption = element_text(hjust = 0, vjust = 0),
        plot.background = element_rect(fill = "white", colour = "white"),
        panel.background = element_rect(fill = "white", colour = "white"),
        legend.background = element_rect(fill = "white", colour = "white"),
        panel.grid.major.x = element_blank() ,
        panel.grid.major.y = element_line(size=.1, color="black"))+
  # will set the axis ticks at Jan/July or every year
  scale_x_date(date_labels = "%b %Y", breaks = breaks_pretty(10),labels = scales::label_date_short())+
  labs(title = 'Systemic Corticosteroids Prescriptions by NHS England Region',
       subtitle = 'London\'s prescription rate has been consistently lower than other regions.' ,
       y = "Items prescribed per 1000 patients",
       x = element_blank(),
       caption = "Source: OpenPrescribing.net, EBM DataLab, University of Oxford, 2022")+
  guides(color=guide_legend(nrow=7, byrow=TRUE))+
  ylim(0, max(regions$prescription_rate_per_1000)+1)
  ggtitle("National Prescription Rate per 1000 Patients By NHS England Region") 

## Save plot to the size of a 16:9 PowerPoint slide
ggsave(plot = region_line_plot,'plots/NHS_England_region_line_chart.png', width = 10, height = 5.625, units = "in")
ggsave('plots/Report/NHS_England_region_line_chart_report.png',
       plot =  region_line_plot+ 
         theme(plot.title = element_markdown(size = 17),
               plot.subtitle = element_markdown(size = 12),
               axis.text.x = element_text(size=7)),
       width = 8,
       height = 4.5,
       units = "in")


## time-series line chart by region with labels at the end of the line, no legend
# dataset for the labels at the end of the lines
label_ends <- regions[regions$date == max(regions$date), ]

labels_end_of_line <- ggplot(data=regions, aes(x=date, y=prescription_rate_per_1000, color=name)) +
  geom_line(aes(x=date, y=prescription_rate_per_1000, group=name)) +
  theme_fivethirtyeight()+
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(hjust = 0, vjust = 0),
    plot.background = element_rect(fill = "white", colour = "white"),
    panel.background = element_rect(fill = "white", colour = "white"))+
  ylim(0, max(regions$prescription_rate_per_1000)+1)+
  scale_x_date(date_labels = "%b %Y", 
               breaks = seq(as.Date("2017-01-01"), 
                            as.Date("2021-10-01"), by = "6 months"),
               labels = scales::label_date_short(),
               # extend the axis to make room for the labels at the end but limit
               # the axis ticks to cover just the data
               lim = c(min(regions$date), as.Date("2022-11-01")))+
  labs(title = 'Systemic Corticosteroids Prescriptions by NHS England Region',
       subtitle = 'London\'s prescription rate has been consistently lower than other regions.' ,
       y = "Items prescribed per 1000 patients",
       x = element_blank(),
       caption = "Source: OpenPrescribing.net, EBM DataLab, University of Oxford, 2022")+
  guides(color=guide_legend(nrow=7, byrow=TRUE))+
  geom_label_repel(data=label_ends, 
                   aes(label = name),
                   segment.linetype = "dotted",
                   size = 3,
                   hjust=0,
                   nudge_y = 0,
                   # set ticks just to cover the data
                   xlim=c(as.Date("2021-09-01"), as.Date("2022-11-01")),
                   na.rm = TRUE) +
ggtitle("National Prescription Rate per 1000 Patients By NHS England Region")
labels_end_of_line
## Save plot to the size of a 16:9 PowerPoint slide
ggsave(plot=labels_end_of_line, 'plots/NHS_England_region_line_chart_no_legend.png', width = 10, height = 5.625, units = "in")


## plot with regional labels at the end of the line, annotations of the lockdowns
labels_end_annotation <- ggplot(data=regions, aes(x=date, y=prescription_rate_per_1000, color=name)) +
  geom_line(aes(x=date, y=prescription_rate_per_1000, group=name)) +
  theme_fivethirtyeight()+
  theme(
    legend.position = "none",
    axis.title = element_text(),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(hjust = 0, vjust = 0),
    plot.background = element_rect(fill = "white", colour = "white"),
    panel.background = element_rect(fill = "white", colour = "white"))+
  ylim(0, 23.75)+
  scale_x_date(date_labels = "%b %Y", 
               breaks = seq(as.Date("2017-01-01"), 
                            as.Date("2021-10-01"), by = "6 months"),
               labels = scales::label_date_short(),
               # extend the axis to make room for the labels at the end but limit
               # the axis ticks to cover just the data
               lim = c(min(regions$date), as.Date("2022-11-01")))+
  labs(title = 'Systemic Corticosteroids Prescriptions by NHS England Region',
       subtitle = 'London\'s prescription rate has been consistently lower than other regions.' ,
       y = "Items per 1000 patients",
       x = element_blank(),
       caption = "Source: OpenPrescribing.net, EBM DataLab, University of Oxford, 2022")+
  guides(color=guide_legend(nrow=7, byrow=TRUE))+
  geom_label_repel(data=label_ends, 
                   aes(label = name),
                   segment.linetype = "dotted",
                   size = 3,
                   hjust=0,
                   min.segment.length = 0.1,
                   seed = 1,
                   nudge_y = 0.5,
                   # set ticks just to cover the data
                   xlim=c(as.Date("2021-09-01"), as.Date("2022-11-01")),
                   ylim=c(6.366,22.5)) +
  geom_vline(xintercept = as.Date("2020-01-28"), # first case in the UK
             colour = "gray") +
  annotate("rect", # first lockdown
           fill = "gray", 
           alpha = 0.4,
           xmin = as.Date("2020-03-26"), 
           xmax = as.Date("2020-05-17"),
           ymin=-Inf, ymax=Inf) + 
  annotate("rect", # second lockdown
           fill = "gray", 
           alpha = 0.4, 
           xmin = as.Date("2020-11-05"), 
           xmax = as.Date("2020-12-02"),
           ymin=-Inf, 
           ymax=Inf) +
  annotate("rect", # third lockdown
           fill = "gray", 
           alpha = 0.4, 
           xmin = as.Date("2021-01-05"), 
           xmax = as.Date("2021-04-12"),
           ymin=-Inf, 
           ymax=Inf)
labels_end_annotation

## adding the text annotations onto the two types of plot separately 
ggsave(plot=labels_end_annotation+
         annotate("text", # labels
                  x = c(as.Date("2020-02-02"), 
                        as.Date("2020-03-29"), 
                        as.Date("2020-11-08"), 
                        as.Date("2021-01-08")), 
                  y = 22.1, 
                  label = c("1st C19 Case", 
                            "1st Lockdown",
                            "2nd Lockdown",
                            "3rd Lockdown") , 
                  size = 2.4,
                  alpha = 0.7,
                  angle = -90,
                  vjust = 0) ,
       'plots/NHS_England_region_lineplot_annotations.png', width = 10, height = 5.625, units = "in")
ggsave('plots/Report/NHS_England_region_line_chart_annotations_report.png',
       plot = labels_end_annotation+ 
         theme(plot.title = element_markdown(size = 17),
               plot.subtitle = element_markdown(size = 12),
               axis.text.x = element_text(size=8))+
         annotate("text", # labels
                  x = c(as.Date("2020-02-02"), 
                        as.Date("2020-03-29"), 
                        as.Date("2020-11-08"), 
                        as.Date("2021-01-08")), 
                  y = 20.5, 
                  label = c("1st C19 Case", 
                            "1st Lockdown",
                            "2nd Lockdown",
                            "3rd Lockdown") , 
                  size = 2.8,
                  alpha = 0.7,
                  angle = -90,
                  vjust = 0), # stops the labels from disappearing,
       width = 8,
       height = 4.5,
       units = "in")


# --------------------------------------------------------------------------------
### MAPS: NHS REGIONS

##
##  Choropleth map plotting function
##
# input: date in "YYYY-MM-DD" format, lower limit of scale, upper limit of scale, boundary
# file and data
choropleth_maker <- function(date, low_lim, up_lim, boundary, data){
  data.filtered <- data[data['date'] == date,]
  data.filtered <- left_join(boundary, data.filtered, by = c("nhser19nm" = "name"))
  choro <- ggplot(data.filtered, aes(fill = prescription_rate_per_1000)) +
    geom_sf(color = "#ffffff", size = 0.1) +
    scale_fill_distiller(direction = 1, palette='Blues', limits=c(low_lim,up_lim), name="Items per 1000 patients") +
    theme(
      plot.background = element_rect(fill = "white", colour = "white"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.text.y=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks=element_blank(),
    )
  choro
}

choropleth_maker("2020-03-01", 8, 18, NHS_regions_bound, regions)


## making plot comparing March 2019, 2020, 2021
march_comparison <- regions[regions$date == "2019-03-01" | regions$date == "2020-03-01" | regions$date == "2021-03-01",]
regions.filtered <- left_join(NHS_regions_bound, march_comparison, by = c("nhser19nm" = "name"))

facet_labels <- c(
  '2019-03-01'="March 2019",
  '2020-03-01'="March 2020",
  '2021-03-01'="March 2021"
)

# map comparison of March 2019, 2020, 2021 for presentation
march_facet_pres <- ggplot(regions.filtered, aes(fill = prescription_rate_per_1000)) +
  geom_sf(color = "#ffffff", size = 0.1) +
  scale_fill_distiller(direction = 1, palette='Blues') +
  facet_wrap(~ date, nrow = 1, labeller=as_labeller(facet_labels), shrink=TRUE) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position ="bottom",
    legend.title=element_text(size=10),
    plot.caption = element_text(hjust = 0, vjust = 0),
  )+
  labs(title = 'Impact of the First COVID-19 Lockdown on Systemic Corticosteroid\nPrescriptions in England',
       subtitle = 'March 2020—the start of the first lockdown—saw substantially higher rates of systemic corticosteroid prescriptions\ncompared with 2019 and 2021.' ,
       fill="Items per 1000 patients",
       caption = "Source: OpenPrescribing.net, EBM DataLab, University of Oxford, 2022")
march_facet_pres
ggsave(plot=march_facet_pres, 'plots/NHS_England_region_march_facet.png', width = 10, height = 5.625, units = "in")

march_facet_pres <- ggplot(regions.filtered, aes(fill = prescription_rate_per_1000)) +
  geom_sf(color = "#ffffff", size = 0.1) +
  scale_fill_distiller(direction = 1, palette='Blues') +
  facet_wrap(~ date, nrow = 1, labeller=as_labeller(facet_labels), shrink=TRUE) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position ="bottom",
    legend.title=element_text(size=10),
    plot.caption = element_text(hjust = 0, vjust = 0),
  )+
  labs(title = 'Impact of the First COVID-19 Lockdown on Systemic Corticosteroid\nPrescriptions in England',
       subtitle = 'March 2020—the start of the first lockdown—saw substantially higher rates of systemic corticosteroid prescriptions\ncompared with 2019 and 2021.' ,
       fill="Items per 1000 patients",
       caption = "Source: OpenPrescribing.net, EBM DataLab, University of Oxford, 2022")
march_facet_pres
ggsave(plot=march_facet_pres, 'plots/NHS_England_region_march_facet.png', width = 10, height = 5.625, units = "in")


march_facet_rep <- ggplot(regions.filtered, aes(fill = prescription_rate_per_1000)) +
  geom_sf(color = "#ffffff", size = 0.1) +
  scale_fill_distiller(direction = 1, palette='Blues') +
  facet_wrap(~ date, nrow = 1, labeller=as_labeller(facet_labels), shrink=TRUE) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position ="bottom",
    legend.title=element_text(size=10),
    plot.caption = element_text(hjust = 0),
    plot.title = element_text(size = 17),
    plot.subtitle = element_text(size = 12)
  )+
  labs(title = 'Impact of the First COVID-19 Lockdown on\nSystemic Corticosteroid Prescriptions in England',
       subtitle = 'March 2020—the start of the first lockdown—saw substantially\nhigher rates of systemic corticosteroid prescriptions compared with 2019 and 2021.' ,
       fill="Items per 1000 patients",
       caption = "Source: OpenPrescribing.net, EBM DataLab, University of Oxford, 2022")
march_facet_rep
ggsave(plot=march_facet_rep, 'plots/Report/NHS_England_region_march_facet_report.png', width = 8, height = 4.5, units = "in")



## Creating a GIF to show seasonal and regional trends quickly in a presentation
region.joined<- left_join(NHS_regions_bound, regions, by = c("nhser19nm" = "name"))
regions.filtered <- region.joined[region.joined$date < "2021-04-01" & region.joined$date > "2019-03-01",]
regions.filtered$date <- as.yearmon(regions.filtered$date)

p <- ggplot(regions.filtered, aes(fill = prescription_rate_per_1000, frame=date)) +
  geom_sf(color = "#ffffff", size = 0.1) +
  scale_fill_distiller(direction = 1, palette='Blues', limits=c(5,20), name="Items per 1000 patients") +
  theme_minimal() +
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.text.y=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks=element_blank(),
    legend.key.size = unit(1, 'cm'), 
    legend.key.height = unit(0.5, 'cm'),
    legend.key.width = unit(0.5, 'cm'),
    legend.title = element_text(size=14), 
    legend.text = element_text(size=10)
  )+
  labs(title = "Systemic Corticosteroids Prescriptions by NHS England Region: {current_frame} ",
       subtitle = 'London\'s prescription rate has been consistently lower than other regions.' ,
       x = element_blank(),
       caption = "Source: OpenPrescribing.net, EBM DataLab, University of Oxford, 2022")
  ggtitle('{current_frame}')
p <- p + 
  transition_manual(frames = date)

animate(p, renderer = gifski_renderer())
anim_save("plots/test.gif", p)



# --------------------------------------------------------------------------------
  ### TIME-SERIES, NATIONAL PLOTS
## line plots by covid period, by covid period with annotations and labels at the end of the lines
national_trend <- setNames(aggregate(list(regions$items, regions$list_size), by=list(regions$date), sum), c("date", "items", "total_patients"))
national_trend$prescription_rate_per_1000 <- national_trend$items/(national_trend$total_patients/1000)
national_trend$date <- as.Date(national_trend$date)
national_trend <- left_join(national_trend, regions[c("date","covid_period")], by = c("date" = "date"), match='first', keep=FALSE)
national_trend <- national_trend[!duplicated(national_trend$date), ]
national_trend$month <- format(national_trend$date,"%m")
national_trend$month.factor <- factor(national_trend$month, ordered = TRUE, 
                                      levels = c("03","04", "05", "06", "07","08","09","10","11","12","01","02"),
                                      labels=c("March","April","May","June","July","August","September","October","November","December","January","February"))

# change covid_period to match the Lancet's styling
national_trend <- national_trend %>% 
  mutate(covid_period = case_when(covid_period == "pre-covid year 4" ~ "pre-COVID-19 Year 4",
                                  covid_period == "pre-covid year 3" ~ "pre-COVID-19 Year 3",
                                  covid_period == "pre-covid year 2" ~ "pre-COVID-19 Year 2",
                                  covid_period == "pre-covid year 1" ~ "pre-COVID-19 Year 1",
                                  covid_period == "covid year 1" ~ "COVID-19 Year 1",
                                  covid_period == "covid year 2" ~ "COVID-19 Year 2"))

covid.year.data <- national_trend[national_trend$covid_period != "COVID-19 Year 2" & national_trend$covid_period != "pre-COVID-19 Year 4",]

# line plot by COVID year showing monthly changes
covid_period_line <-ggplot(data=covid.year.data, aes(x=month.factor, y=prescription_rate_per_1000, color=covid_period)) +
  geom_line(aes(x=month.factor, y=prescription_rate_per_1000, group=covid_period)) +
  theme_fivethirtyeight()+
  ylim(0, 18.5)+
  theme(legend.position = "right",
        legend.justification = c("right", "top"),
        legend.text = element_text(size = 8),
        legend.box="vertical",
        legend.title = element_blank(),
        plot.title = element_text(size=14),
        axis.text=element_text(size=8),
        axis.title = element_text(),
        plot.caption = element_text(hjust = 0, vjust = 0),
        plot.background = element_rect(fill = "white", colour = "white"),
        panel.background = element_rect(fill = "white", colour = "white"),
        legend.background = element_rect(fill = "white", colour = "white"),
        panel.grid.major.x = element_blank())+
  guides(color=guide_legend(nrow=7, byrow=TRUE))+
  labs(title = "National Prescription Rates of Systemic Corticosteroids per 1000 Patients ",
       subtitle = 'With the exception of March 2020, the first year of the COVID pandemic had lower prescription\nrates than the three previous years.' ,
       x = element_blank(),
       y = "Items per 1000 patients",
       caption = "Source: OpenPrescribing.net, EBM DataLab, University of Oxford, 2022")
covid_period_line
ggsave(plot=covid_period_line, 'plots/national_rate_by_covid_period.png', width = 10, height = 5.625, units = "in")
ggsave(plot=covid_period_line+
         theme(plot.title = element_text(size = 17),
               plot.subtitle = element_text(size = 12),
               axis.text.x = element_text(size=6))+
         ggtitle("National Prescription Rates of Systemic Corticosteroids\nper 1000 Patients"),
       'plots/Report/national_rate_by_covid_period_report.png', width = 8, height = 4.5, units = "in")


covid1 <- covid.year.data[covid.year.data$covid_period == "COVID-19 Year 1",]
march.covid1.val <- covid1[covid1$month.factor == "March", ]$prescription_rate_per_1000

labels <- covid.year.data[covid.year.data$month.factor == "February",]
covid_period_labels_end <- covid_period_line +  
  geom_label_repel(
    data = labels,
    aes(label = covid_period),
    segment.linetype = "dotted",
    min.segment.length = 0.1,
    size = 3,
    force_pull = 1.2,
    nudge_y = -0.5,
    ylim = c(6, 17.5), 
    na.rm = TRUE)+
  theme(
    legend.position = "none"
  )+
  annotate(
    geom = "curve", x = "April", y = 16.5, xend = "March", yend = march.covid1.val, 
    curvature = .3, 
    arrow = arrow(length = unit(2, "mm")),
    color = "grey"
  )
covid_period_labels_end
ggsave(plot=covid_period_labels_end+
         annotate(
           geom = "text", 
           x = "April", 
           y = 16.25, 
           label = "  March 2020 saw 93 million items dispensed overall,\n the highest number on record", 
           hjust = "left",
           color = "grey",
           size = 4), 'plots/national_rate_by_covid_period_annotations.png', width = 10, height = 5.625, units = "in")
ggsave(plot=covid_period_labels_end+
         theme(plot.title = element_text(size = 17),
               plot.subtitle = element_text(size = 12),
               axis.text.x = element_text(size=10))+
         annotate(
           geom = "text", 
           x = "April", 
           y = 16.75, 
           label = "  March 2020 saw 93 million items dispensed overall,\n the highest number on record", 
           hjust = "left",
           color = "grey",
           size = 4)+
         scale_x_discrete(labels=c("January" = "Jan", "February" = "Feb",
                                   "March" = "Mar", "April" = "Apr", "May" = "May", "June" = "June",
                                   "July" = "July", "August" = "Aug", "September" = "Sept", "October" = "Oct",
                                   "November" = "Nov", "December"="Dec"))+
         ggtitle("National Prescription Rates of Systemic Corticosteroids\nper 1000 Patients"),
       'plots/Report/national_rate_by_covid_period_labels_report.png', width = 8, height = 4.5, units = "in")

# --------------------------------------------------------------------------------
### SEASONALLY ADJUSTED REGRESSION MODEL
# based on a model from https://figshare.com/articles/dataset/Impact_of_COVID-19_on_corticosteroids_and_antibiotics_prescribing_in_England_an_interrupted_time_series_analysis/13482318?file=25875246
set.seed(100)

# train the model on the three-years before COVID
seasonal_model_data <- national_trend[national_trend$covid_period == "pre-COVID-19 Year 1" |
                                        national_trend$covid_period == "pre-COVID-19 Year 2" | 
                                        national_trend$covid_period == "pre-COVID-19 Year 3",]
seasonal_model <- lm(prescription_rate_per_1000 ~ month + harmonic(month,2,12), data=seasonal_model_data)

# generate a set of predictions to plot for 1 year
seasonal.preds <- predict(seasonal_model,type = "response", interval="confidence")[1:12]

# build dataframe with confidence intervals, predictions, and months
LoCI <- predict(seasonal_model,
                interval = "confidence", 
                level = 0.95)[, 2]
HiCI <- predict(seasonal_model, 
                interval = "confidence", 
                level = 0.95)[, 3]
preds <- data.frame(cbind(unique(seasonal_model_data$month.factor), seasonal.preds, LoCI, HiCI))
preds$covid_period <- "model prediction"
colnames(preds) <- c("month", "pred", "loCI", "hiCI", "covid_period")

# first plotting the seasonal trend and pre-covid data along with confidence intervals
model_precovid <- ggplot(data=seasonal_model_data, aes(x=month.factor, y=prescription_rate_per_1000, color=covid_period)) +
  geom_line(aes(x=month.factor, y=prescription_rate_per_1000, group=covid_period), color="dark grey") +
  theme_fivethirtyeight() +
  geom_line(data=preds, aes(x=month, y=pred), color="red")+
  geom_line(data=preds, aes(x=month, y=loCI), color="black", linetype="dotted")+
  geom_line(data=preds, aes(x=month, y=hiCI), color="black", linetype="dotted")+
  ylim(0, 18)+
  labs(title = "Seasonal Trends in Systemic Corticosteroid Prescription Rates\nPre-Pandemic in England ",
       subtitle = "<span style='color:grey'>Prior to COVID,</span> there were clear <span style='color:#d62728'>seasonal trends</span> with peaks in late autumn and sharp declines in February." ,
       x = element_blank(),
       y = "Items per 1000 patients",
       caption = "Source: OpenPrescribing.net, EBM DataLab, University of Oxford, 2022")+
  theme(
    legend.position = "right",
    plot.background = element_rect(fill = "white", colour = "white"),
    panel.background = element_rect(fill = "white", colour = "white"),
    legend.background = element_rect(fill = "white", colour = "white"),
    axis.title = element_text(),
    plot.subtitle = element_markdown(),
    plot.caption = element_text(hjust = 0, vjust = 0),
  )
ggsave(plot=model_precovid, "plots/seasonal_model_precovid.png", width = 10, height = 5.625, units = "in")
ggsave(plot=model_precovid+
          theme(plot.title = element_text(size = 17),
                plot.subtitle = element_markdown(size = 12))+
         scale_x_discrete(labels=c("January" = "Jan", "February" = "Feb",
                                   "March" = "Mar", "April" = "Apr", "May" = "May", "June" = "June",
                                   "July" = "July", "August" = "Aug", "September" = "Sept", "October" = "Oct",
                                   "November" = "Nov", "December"="Dec"))+
         labs(
           title="Seaonal Trends in Sysmetic Corticosteroid Prescription\nRates Pre-Pandemic in England",
           subtitle="<span style='color:grey'>Prior to COVID,</span> there were clear <span style='color:#d62728'>seasonal trends</span> with peaks in late autumn and sharp<br>declines in February."),
        "plots/Report/seasonal_model_precovid_report.png", width = 8, height = 4.5, units = "in")

# plotting model predictions, confidence intervals, and the first year of COVID
covid.data <- national_trend[national_trend$covid_period == "COVID-19 Year 1",]
model_covid <- ggplot(data=covid.data, aes(x=month.factor, y=prescription_rate_per_1000)) +
  geom_line(color="#17BECF", group=1) +
  theme_fivethirtyeight() +
  geom_line(data=preds, aes(x=month, y=pred), color="#d62728")+
  geom_line(data=preds, aes(x=month, y=loCI), color="black", linetype="dotted")+
  geom_line(data=preds, aes(x=month, y=hiCI), color="black", linetype="dotted")+
  ylim(0, 16)+
  scale_x_discrete(labels=c("Mar\n2020", "Apr\n2020", "May\n2020", "June\n2020", "July\n2020", "Aug\n2020", "Sept\n2020", "Oct\n2020", "Nov\n2020", "Dec\n2020", "Jan\n2021", "Feb\n2021"))+
  labs(title = "Pre-Pandemic Seasonal Trends in Systemic Corticosteroid Prescriptions<br>in England Compared with COVID-19 ",
       subtitle = "With the exception of March 2020, every month in the <span style = color:'#17BECF'>first year of the COVID-19 pandemic </span>saw a lower systemic <br> corticosteroid prescription rate in England compared with <span style='color:#d62728'>pre-pandemic levels.</span>" ,
       x = element_blank(),
       y = "Items per 1000 patients",
       caption = "Source: OpenPrescribing.net, EBM DataLab, University of Oxford, 2022")+
  theme(
    legend.position = "right",
    plot.background = element_rect(fill = "white", colour = "white"),
    panel.background = element_rect(fill = "white", colour = "white"),
    legend.background = element_rect(fill = "white", colour = "white"),
    panel.grid.minor=element_line(colour="light grey"),
    panel.grid.major.x = element_blank(),
    axis.title = element_text(),
    plot.title=element_markdown(),
    plot.subtitle = element_markdown(),
    plot.caption = element_text(hjust = 0, vjust = 0),
  )
model_covid
ggsave(plot=model_covid, "plots/seasonal_model_covid.png", width = 10, height = 5.625, units = "in")
ggsave(plot=model_covid+
         theme(plot.title = element_markdown(size = 17),
               plot.subtitle = element_markdown(size = 12))+
         labs(title = "Pre-Pandemic Seasonal Trends in Systemic Corticosteroid<br>Prescription in England Compared with COVID-19 ",
              subtitle = "With the exception of March 2020, every month in the <span style = color:'#17BECF'>first year of the COVID-19 pandemic </span>saw<br>a lower systemic corticosteroid prescription rate in England compared with <span style='color:#d62728'>pre-pandemic levels.</span>" ,
              x = element_blank(),
              y = "Items per 1000 patients",
              caption = "Source: OpenPrescribing.net, EBM DataLab, University of Oxford, 2022"),
       "plots/Report/seasonal_model_covid_report.png", width = 8, height = 4.5, units = "in")