library(tidyverse)
library(lubridate)
library(cowplot)
library(ggdist)
library(ggrepel)
theme_set(theme_minimal())
# turn off warnings for now
options(warn = -1)
# clear all variables and plots
rm(list = ls())

chosen_palette <- wes_palette(name = "GrandBudapest1")
general_palette <- wes_palette(name = "Rushmore")
long_palette <- c(wes_palette(name = "GrandBudapest1"),wes_palette(name = "GrandBudapest2"))
four_palette <- wes_palette(name = 'GrandBudapest2')
three_palette <- wes_palette(name = "Royal2")[c(1, 3, 5)]
two_palette <- wes_palette(n = 5, name = "FantasticFox1")[c(3, 5)]
remark_color <- wes_palette(name = "Chevalier1")[1]

source('quadrant_plot.R')

books <- read.csv("books.csv", sep = ',') %>%
  mutate(
    is_release = ymd(is_release),
    est_publication = ymd(est_publication),
    next_release = lead(is_release),
    time_to_release = next_release - is_release,
  )

books %>% summarise(
  avg_time_to_release = mean(time_to_release, na.rm = TRUE),
  avg_page_count = mean(pages),
  avg_chapters = mean(chapters),
  sd_page_count = sd(pages),
  sum_page_count = sum(pages),
  sum_chapters = sum(chapters),
  n = n(),
)

# change in translator
translator <- books %>%
  group_by(translator) %>%
  summarise(n = n(), pages = sum(pages),
            first_date = min(est_publication),
            last_date = max(est_publication),
            period = last_date - first_date,
            # convert period to years
            period = as.numeric(period, units = "days") / 365.25,
  ) %>%
  arrange(first_date) %>%
  summarise(
    started = first(interaction(translator, n, sep = ' #')),
    finished = last(interaction(translator, n, sep = ' #')),
    change = as.Date('1987-03-06')
  )

df <- books %>%
  rename(release = est_publication, var11 = pages, var22 = year, var21 = time_to_release, group = id) %>%
  mutate(var21 = var21 / 7, color = as.factor(chapters))

create_plots(
  df,
  label11 = 'Page Count', label22 = 'Books',
  title21 = 'Inter-Book Wait', label21 = 'Weeks',
  title11 = 'Publications',
  title22 = 'Books Published', title12 = 'Page Count',
  output_filename = 'margit_count',
  extra_layers_p11 = list(
    geom_smooth(alpha = .2),
    theme(legend.position = c(0, 0), legend.justification = c(0, 0), legend.box.just = "left"),
    guides(color = guide_legend("Chapters", nrow = 2, byrow = TRUE)),
    geom_vline(xintercept = translator$change, color = "gray20", linetype = 'dashed'),
    annotate("text", x = translator$change, y = Inf, label = translator$started, vjust = -1, hjust = 1, color = 'gray20',
             angle = 90),
    annotate("text", x = translator$change, y = Inf, label = translator$finished, vjust = 1.5, hjust = 1, color =
      'gray20', angle = 90),
    scale_color_manual(values = general_palette)
  )) -> p

books %>%
  filter(!is.na(is_release)) %>%
  mutate(
    weekday_release = wday(is_release, label = TRUE),
  ) %>%
  group_by(weekday_release) %>%
  tally()

storytel <- read.csv('storytel_isfolkid.csv') %>%
  mutate(
    audiobook = as.Date(audiobook), ebook = as.Date(ebook),
    reader = factor(reader, levels = unique(reader)),
    hours = length / 60,
    next_release = lead(audiobook),
    time_to_next = next_release - audiobook
  )
storytel <- books %>%
  select(id, pages) %>%
  merge(storytel, by = 'id')

# change in reader
readers <- storytel %>%
  group_by(reader) %>%
  summarise(n = n(), pages = sum(pages), length = sum(length),
            read_pace = pages / length,
            first_date = min(audiobook),
            last_date = max(audiobook),
            period = last_date - first_date,
            # convert period to months
            period = as.numeric(period, units = "days") / 30,
            # weighted average of rating wrt reviews
            rating = sum(rating * reviews) / sum(reviews),
            rating = as.factor(round(rating, 1)),
  ) %>%
  arrange(first_date) %>%
  mutate(
    time_to_next = (lead(first_date) - last_date),
    split_date = last_date + ifelse(is.na(time_to_next), 0, time_to_next / 2)
  )

storytel %>%
  group_by(time_to_next) %>%
  tally()

storytel %>%
  mutate(weekday = wday(audiobook, label = TRUE)) %>%
  group_by(weekday) %>%
  tally()

storytel %>% ggplot(aes(x = audiobook, y = reviews)) +
  geom_line() +
  geom_point(aes(color = as.factor(rating), size = hours)) +
  labs(x = NULL, y = "Number of Reviews", title = "Number of Reviews by Audiobook Release Date",
       color = 'Rating', size = 'Running time (hours)') +
  geom_vline(data = readers %>% filter(!is.na(time_to_next)),
             aes(xintercept = split_date), color = "gray20", linetype = 'dashed') +
  geom_text(data = readers,
            aes(x = split_date, y = Inf, label = paste(reader, ' #', n), color = rating),
            vjust = -1, hjust = 1,
            angle = 90) +
  theme(legend.position = "bottom")

df <- storytel %>%
  mutate(group = reader,
         var22 = # find the first letter of each word in reader
           str_extract_all(reader, "\\b\\w") %>%
             map_chr(~paste0(.x, collapse = "")),
         var22 = factor(var22, levels = unique(var22)),
         var11 = length / 60, var21 = time_to_next,
         color = reader) %>%
  rename(release = audiobook)

p <- create_plots2(
  df, title11 = 'Running Time', label11 = 'Hours Read',
  extra_layers_p11 = list(
    scale_color_manual(values = four_palette),
    theme(legend.position = c(1, 0), legend.justification = c(1, 0), legend.box.just = "right"),
    guides(color = guide_legend("Reader", nrow = 4, byrow = TRUE))
  )
)
p$p21 = storytel %>%
  ggplot(aes(hours, pages, color = reader)) +
  geom_smooth(method = 'lm', alpha = 0.2) +
  geom_point(size = 2) +
  scale_color_manual(values = four_palette) +
  labs(x = 'Hours Read', y = 'Pages Read', title = 'Reading Speed', color = 'Reader') +
  theme(legend.position = 'none')
p$p22 <- df %>%
  ggplot(aes(var22)) +
  geom_histogram(stat = 'count', aes(fill = var22), show.legend = F) +
  # write the percentage and count on top of the bars = cant use statbin because it doesnt work with factors
  geom_text(stat = 'count', aes(label = paste0(round(..count.. / sum(..count..) * 100, 1), "%\n#", ..count..)),
            vjust = +0.5) +
  scale_y_continuous(expand = c(0, 2.5)) +
  scale_fill_manual(values = four_palette) +
  labs(title = 'Books Read', x = NULL, y = 'Books')
plot_grid(p$plot, plot_grid(p$p21, p$p22, ncol = 1), ncol = 2, rel_widths = c(1, 0.5))
ggsave(filename = 'charts/storytel_readers.png', width = 10, height = 5, units = 'in', dpi = 300)

p <- create_plots2(storytel %>% rename(release = audiobook, var11 = reviews),
                   title11 = 'Number of Reviews by Audiobook Release Date', label11 = 'Number of Reviews',
                   title12 = 'Reviews', scale_y_log10 = TRUE,
                   output_filename = 'storytel_reviews'
)
p <- create_plots2(storytel %>% rename(release = audiobook, var11 = rating),
                   title11 = 'Mean Rating by Audiobook Release Date', label11 = 'Mean Rating',
                   title12 = 'Rating', output_filename = 'storytel_ratings')

# ------------------------------------------------------------
alvarpid <- read.csv('alvarpid.csv') %>%
  mutate(
    release = as.Date(release, format = '%Y-%m-%d'),
    next_release = lead(release),
    weekday = weekdays(release),
    days_to_next = as.numeric(next_release - release)
  ) %>%
  merge(books %>%
          select(id, is_title) %>%
          rename(book = id), by = 'book', all.x = TRUE) %>%
  mutate(is_title = factor(is_title, levels = unique(is_title)))

df = alvarpid %>%
  rename(var22 = book, var11 = listeners, var21 = days_to_next / 7) %>%
  mutate(
    var22 = as.factor(var22))
create_plots(
  df,
  label11 = 'Streams', label22 = 'Episodes', label21 = 'Weeks',
  title11 = "Listeners on Mixcloud", title12 = 'Streams',
  title21 = 'Inter-Episode Wait', title22 = 'Episodes per Book',
  output_filename = "alvarpid_listeners", scale_y_log10 = TRUE, scale_x_log10 = TRUE,
  extra_layers_p11 = list(
    geom_point(data = df %>% filter(!is.na(var22)),
               aes(color = interaction(var22, is_title, sep = ') ')), size = 3),
    geom_text(data = df %>% filter(id == 0),
              aes(label = paste0('Introduction: #', var11)), color = remark_color,
              hjust = -0.1, vjust = 1),
    geom_text(data = df %>% filter(id == max(alvarpid$id)),
              aes(label = paste0('Final Episode: #', var11)), color = remark_color,
              angle = 90, hjust = -0.1, vjust = 0.5),
    labs(color = NULL),
    scale_color_manual(values = long_palette),
    theme(legend.position = c(0.8, 0.75))
  )) -> p

alvarpid %>% summarise(
  n = n(),
  mean.length = mean(length, na.rm = TRUE),
  med.length = median(length, na.rm = TRUE),
  tot.length = sum(length, na.rm = TRUE),
  mean.listeners = mean(listeners, na.rm = TRUE),
  med.listeners = median(listeners, na.rm = TRUE),
  mean.releases = mean(days_to_next, na.rm = TRUE),
  med.releases = median(days_to_next, na.rm = TRUE),
  running = max(release) - min(release),
)
alvarpid %>% group_by(weekday) %>% tally()
# ------------------------------------------------------------
iskisur <- read.csv('storytel_iskisur.csv') %>%
  mutate(
    release = as.Date(release, format = '%Y-%m-%d'),
    weekday = weekdays(release),
    part = factor(part, levels = c(0, 1, 2), labels = c('Special', 'Part 1', 'Part 2'))
  ) %>%
  arrange(release, episode) %>%
  mutate(days_to_next = c(NA, diff(release)))

df <- iskisur %>% rename(var11 = reviews)
p <- create_plots2(
  df, scale_y_log10 = T,
  title11 = 'Reviews by Podcast Release', label11 = 'Reviews', label12 = 'Review Distribution',
  extra_layers_p11 = list(
    geom_text(data = df %>% filter(episode == 0),
              aes(label = paste0('Introduction: #', var11)), color = remark_color,
              hjust = -0.1, vjust = 1,
    ),
    # annotate the last point with text
    geom_text(data = df %>% filter(episode > 47 * 2),
              aes(label = paste0('Final Remarks: #', var11)), color = remark_color,
              angle = 90, hjust = -0.1, vjust = 0.5,
    )
  ),
  output_filename = 'iskisur_reviews'
)

df <- iskisur %>% rename(var11 = rating)
p <- create_plots2(
  df,
  title11 = 'Ratings by Podcast Release', label11 = 'Ratings', label12 = 'Rating Distribution',
  output_filename = 'iskisur_ratings'
)
df <- iskisur %>%
  rename(var11 = length) %>%
  filter(!is.na(release))
p <- create_plots2(
  df,
  title11 = 'Podcast Length by Release Date', label11 = 'Minutes', title12 = 'Episode Length',
  output_filename = 'iskisur_length'
)

# create a sequence with numbers 1:47 where the same number is repeated twice
iskisur$book <- c(NA, rep(1:47, each = 2), NA, NA)
iskisur <- iskisur %>%
  merge(books %>%
          select(id, is_title) %>%
          rename(book = id), by = 'book', all.x = T) %>%
  filter(!is.na(release)) %>%
  arrange(release)

iskisur %>%
  filter(release < as.Date('2019-07-19', format = '%Y-%m-%d')) %>%
  select(release, book, part, episode, is_title) %>%
  tail()

iskisur %>%
  summarise(
    n = n(),
    tot.length = sum(length), avg.length = mean(length), max.length = max(length),
    tot.reviews = sum(reviews), med.reviews = median(reviews), max.reviews = max(reviews),
    med.rating = mean(rating), min.rating = min(rating), top.rating = sum(rating == 5),
    n.below4 = sum(rating < 4),
    running = max(release) - min(release)
  )


# ------------------------------------------------------------
alvarpid %>%
  filter(!is.na(release)) %>%
  summarise(
    type = 'Alvarp', n = n(),
    books = max(book, na.rm = T),
    sum(length), mean(length),
    mean(days_to_next, na.rm = T), max(days_to_next, na.rm = T), min(days_to_next[days_to_next > 1], na.rm = T),
    median(days_to_next, na.rm = T),
    max(release), min(release), timespan = max(release) - min(release)) %>%
  rbind(
    iskisur %>%
      filter(!is.na(release)) %>%
      summarise(
        type = 'Storytel', n = n(),
        books = max(book, na.rm = T),
        sum(length), mean(length),
        mean(days_to_next, na.rm = T), max(days_to_next, na.rm = T), min(days_to_next[days_to_next > 1], na.rm = T),
        median(days_to_next, na.rm = T),
        max(release), min(release), timespan = max(release) - min(release))
  )
# ------------------------------------------------------------

timeline <- books %>%
  rename(date = est_publication, book = id) %>%
  mutate(type = 'book', subtype = 'written', event = paste0(book, ') ', is_title)) %>%
  select(date, event, book, type, subtype)

timeline <- storytel %>%
  rename(date = audiobook, event = is_title, book = id) %>%
  mutate(type = 'book', subtype = 'audio', event = paste0(book, ') ', event)) %>%
  select(date, event, book, type, subtype) %>%
  rbind(timeline)

timeline <- iskisur %>%
  rename(date = release) %>%
  mutate(event = ifelse(episode == 0, 'Special: Introduction',
                        ifelse(episode > 47 * 2, 'Special: Final Remarks',
                               paste0(book, ') ', is_title))),
         type = 'podcast', subtype = 'Storytel') %>%
  select(date, event, type, subtype, book) %>%
  rbind(timeline)


timeline <- alvarpid %>%
  filter(!is.na(release)) %>%
  mutate(type = 'podcast', subtype = 'Alvarp') %>%
  rename(event = episode_title, date = release) %>%
  select(date, event, type, subtype, book) %>%
  rbind(timeline)

timeline <- timeline %>%
  mutate(
    period = ifelse(year(date) < 1990, '80s', 'Present Day'),
  ) %>%
  arrange(date)

timeline %>% group_by(type, subtype) %>% tally()

source('timeline.R')

# ------------------------------------------------------------
family <- read.csv('family.csv', sep = ',') %>%
  mutate(age = ifelse(is.na(death), 1960, death) - birth,
         alpha = ifelse(is.na(death), TRUE, FALSE),
         ancestry = ifelse(!is.na(mom) & !is.na(dad), 'Pure',
                           ifelse(!is.na(mom) | !is.na(dad), 'Half', 'No')),
         ancestry = factor(ancestry, levels = c('No', 'Half', 'Pure'),
                           labels = c('Muggles', 'Half-blood', 'Pure-blood')),
         chosen_one = factor(chosen_one, levels = c('good', 'converted', 'evil'),
                             labels = c('Good', 'Converted', 'Evil')
         )
  )
df <- family %>%
  filter(!is.na(age)) %>%
  rename(var11 = age, var22 = ancestry, release = birth) %>%
  mutate(var21 = release, group = id) %>%
  mutate(
    color = var22,
    gender = ifelse(is.na(gender), 'U', gender),
    shape = factor(gender, levels = c('M', 'F', 'U'), labels = c('Male', 'Female', 'Unknown')),
  )
df %>% group_by(gender) %>% tally()
df %>% group_by(chosen_one) %>% tally()

p <- create_plots(
  df, title11 = 'Lifespan', label11 = 'Age', title12 = 'Age', title21 = 'Births',
  label22 = 'Ancestry',
  extra_layers_p11 = list(
    labs(shape = 'Gender', x = NULL, y = 'Age', color = 'Ancestry', alpha = 'Alive'),
    scale_alpha_discrete(range = c(0.5, 1)),
    # center bottom of the plot, but inside the plot area, align to bottom
    theme(legend.position = c(0.5, 0), legend.justification = c(0.5, 0),
          legend.direction = 'vertical', legend.box = 'horizontal',
          legend.key.size = unit(0.5, 'cm'), legend.key.height = unit(0.5, 'cm'),
    ),
    guides(shape = guide_legend(order = 2), color = guide_legend(order = 1)),
    scale_color_manual(values = three_palette)
  ),
  output_filename = 'family_birth'
)

families <- c('af att Isfolksins', 'Meiden', 'Paladin', 'Stege', 'Tark', 'Grip', 'Gard', 'Brink', 'Skogsrud', 'Volden',
              'Lind')
family <- family %>%
  mutate(
    first_name = str_extract(name, '^[^ ]+'),
    family_name = iconv(name, to = "ASCII//TRANSLIT"),
    family_names = map(family_name, ~str_extract_all(., families) %>%
      unlist() %>%
      unique()),
    family_name = map_chr(family_names, ~paste(., collapse = ', '))
  )

family %>%
  select(id, name, family_name) %>%
  unnest(family_name) %>%
  filter(!is.na(family_name)) %>%
  group_by(family_name) %>%
  tally() %>%
  arrange(desc(n))


gantt <- read.csv('gantt_order.csv') %>%
  merge(family, by = 'id', all.x = T) %>%
  mutate(machine = as.factor(machine))

# Create the Gantt chart
p$gantt <- ggplot(gantt, aes(y = machine, yend = machine, x = birth, xend = ifelse(is.na(death), 1960, death))) +
  geom_segment(size = 1, color = 'gray') +
  geom_segment(data = gantt %>% filter(!is.na(chosen_one)), size = 1, aes(color = chosen_one)) +
  geom_text(data = gantt %>% filter(gender == 'M'),
            aes(label = first_name), hjust = 0, size = 3, vjust = -0.3, show.legend = FALSE, color = two_palette[1]) +
  geom_text(data = gantt %>% filter(gender == 'F'),
            aes(label = first_name), hjust = 0, size = 3, vjust = -0.3, show.legend = FALSE, color = two_palette[2]) +
  labs(x = NULL, y = '', title = NULL) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = c(0, .5), legend.justification = c(0, 0.5),
  ) +
  expand_limits(y = 31) +
  scale_color_manual('Chosen One', values = three_palette)

chosen_ones <- family %>%
  select(birth, death, first_name, chosen_one) %>%
  filter(!is.na(chosen_one) & !is.na(birth)) %>%
  mutate(
    death = ifelse(is.na(death), 1960, death),
    year = map2(birth, death, ~seq(.x, .y, by = 1))) %>%
  unnest(year)

p$chosen <- chosen_ones %>% ggplot() +
  geom_histogram(
    aes(x = year, fill = chosen_one), position = 'stack',
    bins = max(chosen_ones$year) - min(chosen_ones$year),
    show.legend = FALSE
  ) +
  theme(panel.grid.minor.y = element_blank(),
        legend.position = c(0, .5), legend.justification = c(0, 0.5),
  ) +
  labs(x = NULL, y = 'Count', title = 'Chosen Ones') +
  scale_fill_manual(values = three_palette)

plot_grid(p$gantt, p$chosen, nrow = 2, rel_heights = c(1, 0.3))
ggsave('charts/family_gantt.png', width = 10, height = 6, dpi = 300)


family %>%
  filter(!is.na(death)) %>%
  summarise(mean(age, na.rm = TRUE))

family %>% group_by(gender) %>% tally()

fab_five <- family %>% filter(id %in% c(134, 135, 97, 116, 125))

# -------
parents <- family %>%
  mutate(parent_id = mom, parent = 'Mother', child_born = birth) %>%
  select(parent_id, parent, child_born) %>%
  merge(family %>% select(id, birth), by.x = 'parent_id', by.y = 'id') %>%
  rbind(
    family %>%
      mutate(parent_id = dad, parent = 'Father', child_born = birth) %>%
      select(parent_id, parent, child_born) %>%
      merge(family %>% select(id, birth), by.x = 'parent_id', by.y = 'id')
  ) %>%
  filter(!is.na(birth) & !is.na(child_born)) %>%
  group_by(parent_id, parent) %>%
  mutate(age = child_born - birth, parent = factor(parent, levels = c('Father', 'Mother')))

df <- parents %>% mutate(var11 = age, release = birth, color = parent, group = parent_id)
p <- create_plots2(
  df, title12 = 'Age', title11 = 'Parental Age', label11 = 'Age Distribution',
  extra_layers_p11 = list(
    geom_hline(data = df %>% filter(color == 'Father'), aes(yintercept = mean(age), color = color)),
    geom_hline(data = df %>% filter(color == 'Mother'), aes(yintercept = mean(age), color = color)),
    scale_color_manual('Parent', values = two_palette),
    theme(legend.position = c(1, 1), legend.justification = c(1, 1))
  ),
  output_filename = 'family_parent_age'
)

parents.stat <- parents %>%
  group_by(parent_id, parent) %>%
  summarise(
    kids = n(),
    mean_kid = mean(age),
    first_kid = min(age),
    last_kid = max(age)
  )

p$p21 <-
  ggplot(parents.stat, aes(x = kids, fill = parent)) +
    geom_histogram(bins = max(parents.stat$kids), position = "dodge") +
    geom_text(stat = 'count', aes(label = ..count.., color = parent, hjust = ifelse(parent == 'Mother', -1, 1)),
              vjust = -1) +
    scale_fill_manual("Parent", values = two_palette) +
    scale_color_manual("Parent", values = two_palette) +
    theme(legend.position = c(1, 1), legend.justification = c(1, 1),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank()) +
    labs(x = 'Number of Children', y = 'Parents', title = 'Number of Children per Parent') +
    scale_y_continuous(expand = c(0, 45 * .2))

marriage <- read.csv('marriage.csv') %>%
  merge(family %>%
          select(id, birth) %>%
          rename(female_born = birth), by.x = 'female', by.y = 'id') %>%
  merge(family %>%
          select(id, birth) %>%
          rename(male_born = birth), by.x = 'male', by.y = 'id') %>%
  mutate(age_difference = abs(female_born - male_born), older = ifelse(male_born < female_born, 'Male',
                                                                       ifelse(male_born > female_born, 'Female', NA)))
p$p22 <- ggplot(marriage, aes(x = age_difference)) +
  geom_histogram(bins = 10, aes(fill = older)) +
  labs(x = 'Years', y = 'Number of Marriages', title = 'Age Difference Between Spouses',
       fill = 'Older Spouse') +
  scale_x_continuous(breaks = seq(-20, 20, 5), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = two_palette) +
  theme(legend.position = c(1, 1), legend.justification = c(1, 1),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank())

p$new <- plot_grid(p$plot, plot_grid(p$p22, p$p21, ncol = 1), ncol = 2, rel_widths = c(1, 0.5))
ggsave('charts/family_parent_age.png', plot = p$new, width = 12, height = 5, dpi = 300)


parents.stat %>%
  group_by(parent) %>%
  summarise(
    mean_age = mean(mean_kid),
    age_first = mean(first_kid),
    age_last = mean(last_kid),
    only_one = sum(kids == 1),
    more_than_one = sum(kids > 1),
    max_kids = max(kids),
    mean_kids = mean(kids)
  )

fab_five

# -----------------
source('pedigree.R')
print('done')
