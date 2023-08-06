
library(tidyverse)
library(devtools)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tm)
library(dplyr)
library(tidytext)
library(dplyr)
library(maps)
library(gridExtra)
library(reshape2)
library(ggalluvial)
library(lubridate)
library(janitor)
library(gganimate)
library(rcartocolor)
library(countrycode)
library(cowplot)
library(gapminder)
library(ggpubr)

# reading the data 

df <- read_csv("kivaData_augmented/kivaData_augmented.csv")
GDP_data <- read.csv("kivaData_augmented/GDP_per_capita_PPP.csv")
country_continent_dict <- read.csv("kivaData_augmented/country_continent_dict.txt")
dfs <- read.csv("kivaData_augmented/kivaData_augmented.csv")
data <- dfs[1:50000,]

## Plot 1 - wordcloud
#Create a vector containing only the text
text <- data$use
# Create a corpus  
docs <- Corpus(VectorSource(text))
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))

dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 

dfw <- data.frame(word = names(words),freq=words)
dfw <- dfw %>% filter(!word %in% c("buy","etc","purchase", "sell" , "pay"))
set.seed(123455) # for reproducibility 
wordcloud2::wordcloud2(data=dfw, size = 0.7, shape = 'pentagom')


#### PLOT 2 MAPS

colnames(GDP_data) <- c("a", "b", "region", "c", "GDP_per_capita_PPP2017")
GDP_data_cln <- GDP_data[,c(3,5)]
world <- map_data("world")

world[world$region=="USA", "region"] <- "United States"
world[world$region=="Russia", "region"] <- "Russian Federation"

world_df <- merge(x=world,y=GDP_data_cln,by="region", all.x=TRUE)
world_df$GDP_per_capita_PPP2017 <- as.numeric(world_df$GDP_per_capita_PPP2017)

grouped_data <- df %>% filter(useforspatial == 1) %>% 
  group_by(latitude, longitude) %>%
  summarize(total_funded_amount = sum(funded_amount))

plot <- ggplot() +
  geom_map(
    data = world_df, map = world,
    aes(long, lat, map_id = region, fill = log(GDP_per_capita_PPP2017)),
    color = "black", size = 0.1) +
  scale_fill_gradient2(low="red", mid = "yellow",  high="green", midpoint = 9, breaks = c(7, 8, 9, 10, 11), labels = c("low", "medium-low", "medium", "medium-high", "high")) +
  geom_point(data = grouped_data, aes(x = longitude, y = latitude, size = total_funded_amount, color = total_funded_amount, alpha = 0.5)) +
  scale_size_area(max_size = 10, breaks = c (500000, 2500000, 5000000, 7500000), labels = c("0.5", "2.5 mln", "5.0 mln", "7.5 mln")) + coord_sf(ylim = c(-45, 80), expand = FALSE) +
  guides(alpha = FALSE, color = FALSE, fill=guide_legend(title="logarithm of GDP per capita in PPP"), size=guide_legend(title="total amount funded")) +
  theme(legend.position = "right",
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

### South America
plot_latam <- ggplot() +
  geom_map(
    data = world_df, map = world,
    aes(long, lat, map_id = region, fill = log(GDP_per_capita_PPP2017)),
    color = "black", size = 0.1
  ) +
  scale_fill_gradient2(low="red", mid = "yellow",  high="green", midpoint = 9, breaks = c(7, 8, 9, 10, 11), labels = c("low", "medium-low", "medium", "medium-high", "high")) +
  guides(alpha = FALSE, color = FALSE) + 
  labs(fill='logarithm of GDP per capita for 2017', size = "total amount funded") +
  geom_point(data = grouped_data, aes(x = longitude, y = latitude, size = total_funded_amount, color = total_funded_amount, alpha = 0.5)) +
  scale_size_area(max_size = 10) + coord_sf(xlim = c(-110, -50), ylim = c(-30, 25), expand = FALSE) +
  geom_point(color = "red", shape = 21, fill = "white", alpha = 0.5) +
  ggtitle(" ") +
  theme(legend.position = "none",
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

### ASIA 
plot_latam2 <- ggplot() +
  geom_map(
    data = world_df, map = world,
    aes(long, lat, map_id = region, fill = log(GDP_per_capita_PPP2017)),
    color = "black", size = 0.1
  ) +
  scale_fill_gradient2(low="red", mid = "yellow",  high="green", midpoint = 9, breaks = c(7, 8, 9, 10, 11), labels = c("low", "medium-low", "medium", "medium-high", "high")) +
  guides(alpha = FALSE, color = FALSE) + 
  labs(fill='logarithm of GDP per capita for 2017', size = "total amount funded") +
  geom_point(data = grouped_data, aes(x = longitude, y = latitude, size = total_funded_amount, color = total_funded_amount, alpha = 0.5)) +
  scale_size_area(max_size = 10) + coord_sf(xlim = c(60, 150), ylim = c(-10, 40), expand = FALSE) +
  geom_point(color = "red", shape = 21, fill = "white", alpha = 0.5) +
  ggtitle(" ") +
  theme(legend.position = "none",
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

# Display the plot
ggarrange(plot,                                                 
          ggarrange(plot_latam, plot_latam2, ncol = 2, labels = c("Latin America", "Asia")), 
          nrow = 2, 
          labels = "locations of loans with founded amount"                                     
) 

## PLOT 3 - Animation 
dat <- dfs %>% 
  filter(country_code != 'XK') %>% 
  mutate(continent = countrycode(country_code, origin = "iso2c",destination = "continent")) %>% 
  transmute(continent, date = format(as.Date(date), "%Y-%m") , term_in_months, loan_amount) %>% 
  group_by(continent, date) %>% 
  summarise( mean_requested_instalment = sum(loan_amount)/sum(term_in_months)) %>% 
  arrange(date, .by_group = TRUE)%>%
  mutate(id = row_number()) %>%
  ungroup() %>% 
  filter(continent != 'NA') %>% 
  mutate(color_label = forcats::fct_collapse(continent,
                                             "darkgreen" = "Europe",
                                             "#888888" = "Asia",
                                             "navy" = "Africa",
                                             "#661100" = "Americas",
                                             "magenta" = "Oceania"),
                                             color_label = fct_relevel(color_label)) %>%
  arrange(color_label) %>%
  mutate(continent = fct_inorder(continent)) %>% 
  mutate(continent_label = as.character(continent))

annotations <- dat %>% 
  group_by(continent) %>% 
  filter(id == max(id))

animation <- ggplot(data = dat, 
                    mapping = aes(x = id, 
                                  y = mean_requested_instalment, 
                                  color = color_label,
                                  group = continent)) +
  geom_line(mapping = aes(linetype = 'dashed'), 
            size = 1.5, alpha =0.7) +
  scale_x_continuous(limits = c(0, max(dat$id) +3), breaks=c(1,13,25,37),
                     labels=c("2014-01", "2015-01", "2016-01", "2017-01"))+ 
  scale_y_log10(expand = expansion(add = c(0,0.1)), 
                breaks=c(30, 60, 90,  120, 150),
                labels=c('30$','60$', '90$', '120$','150$')) +
  theme_minimal() +
  scale_color_identity()+
  shadowtext::geom_shadowtext(data = dat, 
                              mapping = aes( 
                                y = mean_requested_instalment, 
                                label = continent),
                              hjust=-0.1, vjust = 0, bg.color = "white") +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    #plot.margin = margin(3,15,3,3,"mm"),
    plot.caption = ggtext::element_markdown()
  )+
  coord_cartesian(clip = "on") +
  labs(x = "Months in time", 
       y = "Requested instalments",
       title = "Mean level of requested instalments by continents",
       subtitle =  "By months from Kiva start",
       caption = "<span style = 'font-size:8pt;color:#888888'>Michal Kunstler <br> Jaroslaw Leski<br> </span>") +
  transition_reveal(id)
animate(animation, height = 800, width =1200)
anim_save("instalments.gif")


#### PLOT 4 - Distributions
df_continents <- merge(df, country_continent_dict, by.x = "country", by.y = "Country", all.x = TRUE)
df_continents2 <- df_continents[!is.na(df_continents$Continent),]

df_continents2 %>%filter(funded_amount <3000) -> df_continents3

ggplot(data = df_continents3, aes(x = (funded_amount), ..scaled..)) +
  geom_density(aes(fill = Continent)) +
  #geom_density(bw = 0.5, size = 1) +
  facet_wrap(~Continent) +
  labs(x = "Funded Amount") +
  ggtitle("Distributions of the size of loans between the continents") +
  guides(fill = FALSE) + 
  theme(axis.title.x = element_text(face = "bold", family = "mono"),
        axis.title.y = element_blank(),
        strip.text.x = element_text(face = "bold", family = "mono"),
        axis.text.y = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5, family = "mono"),
        strip.background = element_rect(fill = "#f7d8b5"))

## PLOT 5 - Heatmap 

df_continent_counts <- df_continents %>% 
  select(id, Continent) %>% 
  group_by(Continent) %>% 
  summarise(counter_cont = n()) %>% 
  ungroup()

df_heatmap_init <- df_continents %>% 
  select(id, Continent, sector) %>% 
  group_by(Continent, sector) %>% 
  summarise(counter = n())

df_heatmap <- merge(x = df_heatmap_init, y = df_continent_counts, all.x = TRUE, by = "Continent")
df_heatmap$perc_share <- df_heatmap$counter/df_heatmap$counter_cont

df_heatmap2 <- rbind(df_heatmap, c("Oceania", "Health", 0, 0, 0.0))
df_heatmap3 <- df_heatmap2[!is.na(df_heatmap2$Continent),]

df_heatmap3$perc_share <- as.numeric(df_heatmap3$perc_share)
# create the heatmap 
ggplot(df_heatmap3, aes(sector, Continent)) + 
  geom_tile(aes(fill = perc_share), colour = "white") + 
  ggtitle("Distribution of shares of loans by number of loans in each continent") + 
  geom_text(aes(label = paste(round(perc_share,2)*100, '%')), color = "white", size = 4) +
  scale_fill_gradient2(low = "#960019", mid = "#2e8b57", high = "#3bb143", midpoint = 0.25, breaks = c(0, 0.1, 0.2, 0.3, 0.4), labels = c("0%", "10%", "20%", "30%", "40%")) +
  labs(fill='percentage share') +
  theme(legend.position = 'top', legend.text = element_text(family = "mono", hjust = 0.5), legend.title = element_text(family = "mono", vjust = 0.7),
        axis.text.x = element_text(angle = 330, hjust = 0, family = "mono"), axis.title = element_blank(),
        axis.text.y = element_text(hjust = 1, family = "mono"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, family = "mono", face = "bold"))

