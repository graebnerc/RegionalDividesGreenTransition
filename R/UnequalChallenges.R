library(data.table)
library(tidyverse)
library(here)
library(readxl)
library("ggsci")
library(countrycode)
source(here("R/country-setup.R"))

ger_group <- "Core" # Germany
fra_group <- "Periphery" # France
fig_width <- 6
fig_height <- 3
back_color <- rgb(245, 237, 239, maxColorValue=255)

country_set <- c(
  "Sweden", "Finland", "Latvia", "Lithuania", "France", "Austria",
  "Luxembourg", "Portugal", "Denmark", "Slovenia", "Slovakia", "EU-27",
  "Spain", "Belgium", "Hungary", "Bulgaria", "Croatia", "Germany",
  "Czechia", "Romania", "Italy", "Ireland", "Estonia", "Greece",
  "Cyprus", "Poland", "Malta", "Netherlands"
)

# Power sources---------
renewables_eu <- fread(
  here(paste0(
    "data/",
    "Share of renewables, low-carbon sources and fossil fuels in ",
    "power generation - European Union - 28.csv"))
) %>%
  rename(year=V1) %>%
  mutate(country="EU28",
         year=ifelse(year==2018, 2019, year))

renewables_de <- fread(
  here(paste0(
    "data/",
    "Share of renewables, low-carbon sources and fossil fuels in ",
    "power generation - Germany.csv"))
) %>%
  rename(year=V1) %>%
  mutate(country="Germany")

renewables_pl <- fread(
  here(paste0(
    "data/",
    "Share of renewables, low-carbon sources and fossil fuels in ",
    "power generation - Poland.csv"))
) %>%
  rename(year=V1) %>%
  mutate(country="Poland")

renewables <- rbind(renewables_eu, renewables_de, renewables_pl) %>%
  select(-Units) %>%
  pivot_longer(cols = -one_of("country", "year"),
               names_to = "Kind", values_to = "Share")

power_supply_2 <- renewables %>%
  mutate(
    year=as.character(year),
    Kind=gsub("Share of ", "", Kind),
    Kind=gsub(" in power generation", "", Kind)) %>%
  filter(Kind %in% c(
    "low carbon sources",
    "coal",
    "oil")) %>%
  pivot_wider(id_cols = all_of(c("year", "country")),
              names_from = "Kind", values_from = "Share") %>%
  mutate(coal_oil=coal+oil) %>%
  select(-all_of(c("coal", "oil"))) %>%
  rename(`Low carbon sources`=`low carbon sources`,
         `Coal and oil`=`coal_oil`) %>%
  pivot_longer(names_to = "Kind", values_to = "Share",
               cols = all_of(c("Low carbon sources", "Coal and oil"))
  ) %>%
  ggplot(., aes(x=year, y=Share, fill=country, color=country, group=country)) +
  #geom_bar(stat = "identity", position = position_dodge()) +
  geom_point() + geom_line(key_glyph = draw_key_rect) +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1),
    expand = expansion(add = c(2, 2))
  ) +
  scale_color_viridis_d(option = "turbo") +
  facet_wrap(~Kind, scales = "free") +
  labs(
    title="Challenges: Unequal reliance on harmful energy sources",
    caption = "Data: IEA.",
    y = "% of total power generation") +
  theme_bw() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size=12),
    panel.border = element_blank(),
    axis.line = element_line(),
    plot.title = element_text(size = 14, hjust = 0.5),
    legend.position = "bottom",
    axis.title.x = element_blank(),
    axis.title.y =element_text(size=11),
    axis.text.x = element_text(size=9, angle = 0, vjust = 0.8),
    axis.text.y = element_text(size=10),
    panel.grid.minor = element_blank(),
    legend.title = element_blank(), 
    plot.background = element_rect(
      fill = back_color,
      color = back_color),
    panel.background = element_rect(
      fill = back_color,
      color = back_color
    ),
    legend.background = element_rect(
      fill = back_color,
      color = back_color))
power_supply_2

ggsave(
  plot = power_supply_2, 
  width = fig_width+1, height = fig_height, 
  filename = here("output/PowerSupply.png"))

# Patents----------
oecd_countries <- countrycode(country_set, "country.name", "iso3c")
oecd_countries <- oecd_countries[!is.na(oecd_countries)]

pop_data <- fread(
  here("data/pop_data.csv"),
  colClasses = c("character", "double"))

patent_data2_cols <- c("LOCATION", "Country", "IPC", "Technology domains & IPC", "Value")

patent_data <- fread(here("data/PATS_IPC_new.csv")) %>%  
  filter(LOCATION %in% oecd_countries,
         KINDPATENT == "PCT_A", # Patent application field under the PCT
         KINDCOUNTRY=="INVENTORS",
         `Reference country`=="Inventor(s)'s country(ies) of residence",
         KINDDATE=="APPLICATION", `Reference Date`=="Application date",
         TIME==2017, IPC=="ENV_TECH"
         
  ) %>%
  select(all_of(patent_data2_cols)) %>% 
  left_join(., pop_data, by=c("Country"="country"))%>%
  mutate(Value_pc = Value/population,
         Country = ifelse(
           Country=="Czech Republic", "Czechia",
           ifelse(Country=="Slovak Republic", "Slovakia", Country))
  ) %>% 
  dplyr::mutate(
    ccode = countrycode(LOCATION, "iso3c", "iso2c"), 
    c_group = ifelse(
      ccode %in% countries_interest[["Germany"]], ger_group, ifelse(
        ccode %in% countries_interest[["France"]], fra_group, ifelse(
          ccode %in% countries_interest[["Core"]], "Core", ifelse(
            ccode %in% countries_interest[["Finance"]], "Finance", ifelse(
              ccode %in% countries_interest[["UK"]], "GBR", ifelse(
                ccode %in% countries_interest[["Catchup"]], "East", ifelse(
                  ccode %in% countries_interest[["Periphery"]], "Periphery", NA
                )))))))) 

patent_data_new <- patent_data %>%
  ggplot(
    data = ., 
    mapping = aes(
      x=reorder(Country, -Value_pc), 
      y=Value_pc, fill = c_group)
    ) +
  geom_bar(
    stat = "identity", color = back_color, width = 0.8) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15), add = 0),
    labels = scales::number_format(scale = 1000000)
    ) +
  scale_fill_viridis_d(option = "turbo") +
  labs(title = "Opportunities: Unequal expertise in green technologies",
       y = "Patents per million people",
       caption = "Source: OECD and World Bank; year: 2017.") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        axis.text.x = element_text(
          angle = 45, hjust = 1),
        legend.position = "none",
        plot.title = element_text(size = 14, hjust = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.title = element_blank(),
        plot.background = element_rect(
          fill = back_color,
          color = back_color),
        panel.background = element_rect(
          fill = back_color,
          color = back_color
        ),
        legend.background = element_rect(
          fill = back_color,
          color = back_color))
patent_data_new

ggsave(
  plot = patent_data_new, 
  width = fig_width, height = fig_height, 
  filename = here("output/Patents.png"))
