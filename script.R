# load libraries
library(tidyverse)

gapminder <- read.csv("data/gapminder_data.csv")

head(gapminder)

# select examples
gapminder_select <- select(gapminder, country, year, lifeExp, pop)
head(gapminder_select)

select(gapminder, country, year, lifeExp, pop)


# pipes
gapminder_select <- gapminder %>% select(., country, year, lifeExp, pop)

# pipes with no placeholder
gapminder_select <- gapminder %>%
  select(country, year, lifeExp, pop)

# challenge question
x <- gapminder %>%
  select(continent, gdpPercap, lifeExp, year)


# filter example
gapminder_canada <- gapminder %>%
  filter(country == "Canada")

# next filter example
gapminder_LE <- gapminder %>%
  filter(lifeExp > 50)
head(gapminder_LE)


# another example with multiple filters
gapminder_CE <- gapminder %>%
  filter(country == "Canada", lifeExp >= 80)
head(gapminder_CE)

# filter for two countries
y <- gapminder %>%
  filter(country %in% c("Canada", "Mexico"))

gapminder %>%
  filter(country %in% c("Cana", "Mexico"))


# Another challenger appears!!
x <- gapminder %>%
  filter(continent == "Africa", year >= 1980)


# create new columns
gapminder_gdpbil <- gapminder %>%
  mutate(gdp_billion = gdpPercap * pop / 10^9)
head(gapminder_gdpbil)


# chaining commands with pipes

gapminder_new <- gapminder %>%
  select(country, year, pop) %>%
  filter(country == "Canada")
head(gapminder_new)


# more chaining examples
gapminder_new2 <- gapminder %>%
  filter(country == "Canada") %>%
  select(country, continent, gdpPercap, pop) %>%
  mutate(gdp_billion = gdpPercap * pop / 10^9) %>%
  filter(gdp_billion >= 9)


# group_by() and summmarize()
gapminder_avgLifeExp <- gapminder %>%
  group_by(country) %>%
  summarise(mean_lifeExp = mean(lifeExp))

gapminder_lifeExp_stats <- gapminder %>%
  group_by(country) %>%
  summarise(mean_lifeExp = mean(lifeExp), sd_lifeExp = sd(lifeExp))

gapminder_lifeExp_stats <- gapminder %>%
  group_by(country) %>%
  summarise(num = n())

gapminder_lifeExp_stats <- gapminder %>%
  group_by(country) %>%
  mutate(num = n())


# using left_join()
fruits1 <- read.csv("data/fruits_table1.csv", stringsAsFactors = FALSE)
fruits2 <- read.csv("data/fruits_table2.csv", stringsAsFactors = FALSE)

fruits_joined <- left_join(fruits1, fruits2, by = "FruitID")


# final exercise
gapminder_final <- gapminder %>%
  select(country, year, pop, gdpPercap) %>%
  filter(year >= 1980) %>%
  mutate(gdpBil = gdpPercap * pop / 10^9) %>%
  group_by(country) %>%
  summarise(mean_GdpBil = mean(gdpBil), sd_GdpBil = sd(gdpBil))

write_csv(gapminder_final, path = "gapminder_summary_gdpbil.csv")






# ggplot2
ggplot( data=gapminder,
        aes(x=year,y=lifeExp,group=country, col=continent) ) +
  geom_line() +
  geom_point(col='black')


ggplot( data=gapminder,
        aes(x=gdpPercap,y=lifeExp,group=country, col=continent) ) +
  geom_point() +
  scale_x_log10()


x <- ggplot( data=gapminder,
        aes(x=gdpPercap,y=lifeExp,col=continent,group=1) )

x <- x + geom_point( alpha=0.25) +
  scale_x_log10() +
  geom_smooth(method="lm")

rgb(0,0,0,0.25)



gapminder_small <- gapminder %>%
  filter(country %in% c("Canada", "United States", "France", "Australia"))


x <- ggplot( gapminder_small, aes(year, lifeExp, colour = continent)) +
  geom_line() +
  facet_wrap(~country)

ggsave(plot = x, filename = "x.png", units = "cm", width = 12, height = 10, dpi = 300)
