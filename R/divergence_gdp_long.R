here::i_am("R/divergence_gdp_long.R")
library(dplyr)
library(tidyr)
library(countrycode)
library(ggplot2)
library(here)
library(data.table)
library(ggpubr)
library(scales)
library(ggrepel)
source(here("R/country-setup.R"))
income_growth <- fread(here("data/wdi-inc-growth.csv"))

fig_width <- 4
fig_height <- 3
back_color <- rgb(245, 237, 239, maxColorValue=255)

if (FALSE){
  gdp_per_cap <- WDI::WDI(
    indicator = c("GDP_pc"="NY.GDP.PCAP.KD"), 
    country = unique(income_growth$iso2c))
  fwrite(gdp_per_cap, file = here("data/wdi_gdp_pc_NYGDPPCAPKD.csv"))
} else{
  gdp_per_cap <- fread(here("data/wdi_gdp_pc_NYGDPPCAPKD.csv"))
}

core_name <- "Central"
peri_name <- "South"
ger_group <- core_name # Germany
fra_group <- peri_name # France

income_growth_groups <- income_growth %>%
  # dplyr::filter(year>=filter_y_min, year<=filter_y_max) %>%
  full_join(y = select(gdp_per_cap, c("iso2c", "year", "GDP_pc")), 
            by = c("year", "iso2c")) %>% 
  dplyr::mutate(
    ccode = iso2c, 
    country=countrycode(ccode, "iso2c", "country.name")
    ) %>% 
  dplyr::mutate(
    c_group = ifelse(
      ccode %in% countries_interest[["Germany"]], ger_group, ifelse(
        ccode %in% countries_interest[["France"]], fra_group, ifelse(
          ccode %in% countries_interest[["Core"]], core_name, ifelse(
            ccode %in% countries_interest[["Finance"]], "Finance", ifelse(
              ccode %in% countries_interest[["UK"]], "GBR", ifelse(
                ccode %in% countries_interest[["Catchup"]], "East", ifelse(
                  ccode %in% countries_interest[["Periphery"]], peri_name, NA
                )))))))) 

income_growth_groups_agg <- income_growth_groups %>% 
  group_by(year, c_group) %>% 
  summarise(across(
    .cols = where(is.numeric), 
    .fns = ~ mean(.x, na.rm = TRUE)), 
    .groups = "drop") %>% 
  group_by(year) %>% 
  dplyr::mutate(
    GDP_pc_avg = mean(GDP_pc, na.rm=TRUE)
    ) %>% 
  ungroup()

gdp_pc_mean_core <- income_growth_groups_agg %>% 
  select(year, c_group, GDP_pc) %>% 
  dplyr::filter(c_group == core_name) %>% 
  select(-c_group) %>% 
  rename(GDP_pc_avg_core=GDP_pc)

income_growth_groups_agg <- income_growth_groups_agg %>% 
  left_join(gdp_pc_mean_core, by = c("year")) %>% 
  dplyr::mutate(
    GDP_pc_avg_dev = GDP_pc-GDP_pc_avg,
    GDP_pc_dev_core = GDP_pc-GDP_pc_avg_core,
    GDP_pc_dev_core_rel = GDP_pc/GDP_pc_avg_core)

# Individual time series-------------------------
set.seed(1234)
gdp_pc_plot_individual <- ggplot(
    data = income_growth_groups, 
    mapping = aes(x=year, y=GDP_pc, color=ccode)
  ) +
  labs(
    title = "Long-term dynamics of GDP per capita", 
    y = "GDP per capita", caption = "Data: World Bank.") +  
  scale_y_continuous(
    labels = scales::label_number(scale = 0.001, suffix = "k")
    ) +
  scale_x_continuous(
    limits = c(1960, 2070), 
    breaks = seq(1960, 2020, 10),
    labels = seq(1960, 2020, 10),
    expand = expansion()
    ) +
  scale_color_viridis_d(option = "turbo") + 
  geom_label_repel(
    data = dplyr::filter(income_growth_groups, year==2021),  
    mapping = aes(x = year, y=GDP_pc, label=country), size =2, 
    nudge_x = 5, max.overlaps = 20, max.iter = 100000, xlim = c(2020, 2070)
    ) + 
  geom_point(size=0.5, alpha=0.75) + geom_line() + theme_bw() +
  theme(
    legend.position = "none", 
    axis.title.x = element_blank(),
    plot.background = element_rect(
      fill = back_color,
      color = back_color),
    panel.background = element_rect(
      fill = back_color,
      color = back_color
    )
  )
gdp_pc_plot_individual
ggsave(
  filename = here("output/divergence_individual_countries.png"), 
  plot = gdp_pc_plot_individual,
  width = fig_width, height = fig_height)

# Aggregated time series-------------------------

set.seed(123)
gdp_pc_plot_agg <- ggplot(
  data = income_growth_groups_agg, 
  mapping = aes(x=year, y=GDP_pc, color=c_group)
) +
  labs(
    title = "Long-term dynamics of GDP per capita", 
    y = "GDP per capita", caption = "Data: World Bank.") +  
  scale_y_continuous(
    labels = scales::label_number(scale = 0.001, suffix = "k")
  ) +
  scale_x_continuous(
    limits = c(1960, 2030), 
    breaks = seq(1960, 2020, 10),
    labels = seq(1960, 2020, 10)
  ) +
  scale_color_viridis_d(option = "turbo") + 
  geom_label_repel(
    data = dplyr::filter(income_growth_groups_agg, year==2021),  
    mapping = aes(x = year, y=GDP_pc, label=c_group), 
    nudge_x = 5, max.overlaps = 15, max.iter = 80000
  ) + 
  geom_point() + geom_line() + theme_bw() +
  theme(
    legend.position = "none", 
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = 0.5), 
    plot.background = element_rect(
      fill = back_color,
      color = back_color),
    panel.background = element_rect(
      fill = back_color,
      color = back_color
    )
  )
gdp_pc_plot_agg
ggsave(
  filename = here("output/divergence_country_groups.png"), 
  plot = gdp_pc_plot_agg,
  width = fig_width, height = fig_height)

# Deviation from mean:

set.seed(123)
gdp_pc_dev_plot_agg <- ggplot(
  data = income_growth_groups_agg, 
  mapping = aes(x=year, y=GDP_pc_avg_dev, color=c_group)
) +
  labs(
    title = "Deviation from average GDP per capita", 
    y = "GDP per capita", caption = "Data: World Bank.") +  
  scale_y_continuous(
    labels = scales::label_number(scale = 0.001, suffix = "k")
  ) +
  scale_x_continuous(
    limits = c(1960, 2030), 
    breaks = seq(1960, 2020, 10),
    labels = seq(1960, 2020, 10)
  ) +
  geom_hline(yintercept = 0) +
  scale_color_viridis_d(option = "turbo") + 
  geom_label_repel(
    data = dplyr::filter(income_growth_groups_agg, year==2021),  
    mapping = aes(x = year, y=GDP_pc_avg_dev, label=c_group), 
    nudge_x = 5, max.overlaps = 15, max.iter = 80000
  ) + 
  geom_point() + geom_line() + theme_bw() +
  theme(
    legend.position = "none", 
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = 0.5), 
    plot.background = element_rect(
      fill = back_color,
      color = back_color),
    panel.background = element_rect(
      fill = back_color,
      color = back_color
    )
  )
gdp_pc_dev_plot_agg
ggsave(
  filename = here("output/divergence_country_groups_dev_mean.png"), 
  plot = gdp_pc_dev_plot_agg,
  width = fig_width, height = fig_height)

# Deviation from core:

set.seed(123)
gdp_pc_dev_core_plot_agg <- ggplot(
  data = income_growth_groups_agg, 
  mapping = aes(x=year, y=GDP_pc_dev_core_rel, color=c_group)
) +
  labs(
    title = "Converging with Central Europe?", 
    y = "GDP per capita", caption = "Data: World Bank.") +  
  scale_y_continuous(
    breaks = seq(0, 1.2, 0.2),
    labels = scales::label_number(
      scale = 100, suffix = "%")
  ) +
  scale_x_continuous(
    limits = c(1960, 2030), 
    breaks = seq(1960, 2020, 10),
    labels = seq(1960, 2020, 10)
  ) +
  scale_color_viridis_d(option = "turbo") + 
  geom_label_repel(
    data = dplyr::filter(income_growth_groups_agg, year==2021),  
    mapping = aes(x = year, y=GDP_pc_dev_core_rel, label=c_group), 
    nudge_x = 5, max.overlaps = 15, max.iter = 80000
  ) + 
  geom_point() + geom_line() + theme_bw() +
  theme(
    legend.position = "none", 
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.background = element_rect(
      fill = back_color,
      color = back_color),
    panel.background = element_rect(
      fill = back_color,
      color = back_color
    )
  )
gdp_pc_dev_core_plot_agg
ggsave(
  filename = here("output/divergence_country_groups_dev_core.png"), 
  plot = gdp_pc_dev_core_plot_agg,
  width = fig_width, height = fig_height)