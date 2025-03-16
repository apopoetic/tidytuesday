library(tidyverse)
library(showtext)

tuesdata <- tidytuesdayR::tt_load('2025-03-11')

#getwd()
pixar_films <- tuesdata$pixar_films
public_response <- tuesdata$public_response

pixar_join <- full_join(pixar_films, public_response, by = "film") |> mutate(year = year(release_date))
write_csv(pixar_join, "pixar.csv")

pixar <- read_csv("pixar.csv") 

pixar$release_date <- lubridate::mdy(pixar$release_date)

pixar$position <- 0

pixar$dummy <- sample(c(-30:-10, 10:35), size = nrow(pixar), replace = TRUE)

glimpse(pixar)
# Get fonts ---------------------------------------------------------------

font_add_google('Rock Salt', 'rs')
showtext_auto()


# Plotting ----------------------------------------------------------------
# simple timeline
ggplot(data = pixar, aes(x = release_date, y = position)) + 
  geom_point() + theme_classic() + geom_point(aes(x = release_date, y = dummy), color = "red",
                                              shape = 20, size = 3, alpha = 0.5) +
  geom_segment(aes(x = release_date, xend = release_date, y = position, yend = dummy, color = film), 
                alpha = 0.5) +
  guides(color = FALSE) +
  geom_hline(yintercept = 0, color = "pink") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + labs(x = "Release Year") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title.y = element_blank(), 
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 20, family = "rs")) +
  ggrepel::geom_text_repel(aes(x = release_date, y = dummy, label = film, colour = film), hjust = 0.5,
            vjust = -0.5, size = 9, family = "rs") +
  annotate(geom = "text", x = as.Date("1999-01-01"), y = -15, label = "Timeline of Pixar Films", 
           colour = "purple", size = 15, family = "rs")

ggsave("pixar.png", width = 10, height = 6, dpi = 300)

ggplot(data = pixar, aes(x = release_date, y = run_time)) + 
  geom_smooth(se = FALSE, method = "loess", linetype = "dashed", colour = "#85929e") + 
  geom_point(aes(colour = film)) + theme_classic() +
  ggrepel::geom_text_repel(aes(x = release_date, y = run_time, label = film, colour = film), hjust = 0.5,
            vjust = -0.5, size = 9, family = "rs",
            position=position_jitter(width=1,height=1)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + labs(x = "Release Year")+
  scale_y_continuous(breaks = seq(from = 80, to = 150, by = 5)) +
  guides(colour = FALSE) + labs(x = "Release Year", y = "Run Time (minutes)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        # axis.title.y = element_blank(),
        # axis.line.y = element_blank(),
        # axis.ticks.y = element_blank(),
        # axis.text.y = element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 20, family = "rs")) +
  annotate(geom = "text", x = as.Date("2004-01-01"), y = 85, label = "Shortest Film", 
           colour = "#c0392b", size = 10, family = "rs") +
  annotate(geom = "curve", x = as.Date("2004-01-01"), y = 83, xend = as.Date("1996-3-22"), 
           yend = 81, colour = "#c0392b", size = 1, curvature = -0.2,
           arrow = arrow(type = "closed", length = unit(0.1, "inches")))



ggplot(data = pixar, aes(x = run_time, y = rotten_tomatoes)) + 
  geom_smooth(se = FALSE, method = "loess", linetype = "dashed", colour = "#85929e") +
  geom_point(aes(colour = film)) + theme_classic() +
  ggrepel::geom_text_repel(aes(x = run_time, y = rotten_tomatoes, label = film, colour = film), hjust = 0.5,
            vjust = -0.5, size = 9, family = "rs",
            position=position_jitter(width=1,height=1)) +
  #scale_color_brewer(palette = "BuPu")+
  scale_x_continuous(breaks = seq(from = 80, to = 150, by = 5)) +
  scale_y_continuous(breaks = seq(from = 0, to = 100, by = 5)) +
  guides(colour = FALSE) + coord_cartesian(ylim = c(65, 105)) + 
  labs(x = "Run Time (minutes)", y = "Rotten Tomatoes Score") +
  theme(# axis.title.y = element_blank(),
        # axis.line.y = element_blank(),
        # axis.ticks.y = element_blank(),
        # axis.text.y = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        text = element_text(size = 20, family = "rs"))



ggsave("run_score.png", width = 10, height = 6, dpi = 300)
