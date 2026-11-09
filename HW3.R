# Install packages once in the Console if needed:
# install.packages(c("ggplot2", "dplyr", "tidyr", "readr", "sf"))
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(sf)
vehicles <- ggplot2::mpg # Full dataset
gems <- ggplot2::diamonds # Full dataset
# Problem 1A
ggplot(vehicles, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv), alpha = 0.6) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "blue") +
  facet_wrap(~ drv) +
  scale_x_continuous(limits = c(1.5, 5)) +
  labs(x = "Engine displacement (litres)",
       y = "Highway fuel economy (mpg)") +
  theme_bw()

# facet_wrap(~ drv) puts each drive type in its own panel.
# geom_smooth() fits one line per panel, so there are three fitted lines even though only the points use drive type for color.

# Problem 1B: Counts
ggplot(vehicles, aes(x = drv, fill = factor(cyl))) +
  geom_bar() +
  theme_bw()

# Problem 1B: Proportions
ggplot(vehicles, aes(x = drv, fill = factor(cyl))) +
  geom_bar(position = "fill") +
  labs(y = "Proportion within drive type") +
  theme_bw()

# The proportion chart divides each cylinder count by the total
# for its drive type. Every bar adds up to 1, even though the
# drive types have different totals

# Problem 1C
gems_small <- filter(gems, carat <= 2.5)

# Report how many diamonds were kept and excluded
nrow(gems_small)
nrow(gems) - nrow(gems_small)

p_fine <- ggplot(gems_small, aes(x = carat)) +
  geom_histogram(binwidth = 0.01, boundary = 0, closed = "right") +
  coord_cartesian(xlim = c(0, 2.5)) +
  labs(title = "Bin width: 0.01 carats", x = "Carat", y = "Count") +
  theme_bw()

p_coarse <- ggplot(gems_small, aes(x = carat)) +
  geom_histogram(binwidth = 0.5, boundary = 0, closed = "right") +
  coord_cartesian(xlim = c(0, 2.5)) +
  labs(title = "Bin width: 0.5 carats", x = "Carat", y = "Count") +
  theme_bw()

print(p_fine)
print(p_coarse)

# Count exact weights using the full dataset
gems %>%
  filter(carat %in% c(0.99, 1.00)) %>%
  count(carat)

# The narrow bins show a spike at 1.00 carat that the wider bins hide.
# In the full dataset, 1,558 diamonds weigh exactly 1.00 carat, while only 23 weigh exactly 0.99 carat
#PROBLEM 2

libraries_raw <- jsonlite::fromJSON("data/raw/libraries.json")
areas_raw <- st_read("data/raw/community_areas.geojson", quiet = TRUE)

nrow(libraries_raw)
nrow(areas_raw)

library_locations <- tibble(
  site_id = sprintf("CPL%03d", seq_len(nrow(libraries_raw))),
  facility_name = libraries_raw$branch_,
  longitude = as.numeric(libraries_raw$location$longitude),
  latitude = as.numeric(libraries_raw$location$latitude)
)

clean_locations <- library_locations %>%
  filter(
    !is.na(facility_name), trimws(facility_name) != "",
    is.finite(longitude), between(longitude, -180, 180),
    is.finite(latitude), between(latitude, -90, 90)
  )

nrow(clean_locations)
nrow(library_locations) - nrow(clean_locations)
sum(duplicated(clean_locations[c("longitude", "latitude")]))

sites_m <- st_as_sf(
  clean_locations, coords = c("longitude", "latitude"),
  crs = 4326, remove = FALSE
) %>%
  st_transform(26916)

areas_m <- st_transform(areas_raw, 26916)

p_sites <- ggplot() +
  geom_sf(data = areas_m, fill = "grey95", color = "grey70") +
  geom_sf(data = sites_m, aes(color = "Public library"), size = 1.5) +
  labs(
    title = "Chicago Public Library Locations",
    color = NULL,
    caption = "Sources: Chicago Public Library and City of Chicago. Downloaded Nov. 6, 2026."
  ) +
  theme_bw()

print(p_sites)
buffers_500 <- st_union(st_buffer(sites_m, dist = 500, nQuadSegs = 100))
buffers_1000 <- st_union(st_buffer(sites_m, dist = 1000, nQuadSegs = 100))

buffer_summary <- tibble(
  radius_m = c(500, 1000),
  union_area_km2 = c(
    as.numeric(st_area(buffers_500)),
    as.numeric(st_area(buffers_1000))
  ) / 1e6
)

print(buffer_summary)
stopifnot(buffer_summary$union_area_km2[2] >=
            buffer_summary$union_area_km2[1])

p_buffers <- p_sites +
  geom_sf(
    data = st_sf(geometry = buffers_500),
    aes(fill = "500 m buffer"),
    color = NA,
    alpha = 0.4
  ) +
  labs(title = "Area Within 500 m of a Chicago Public Library", fill = NULL)

print(p_buffers)

# The 1,000 m buffer covers more area than the 500 m buffer.
# Both calculations include any portions that extend outside Chicago.
# Problem 3
areas_m <- areas_m %>%
  transmute(
    district_id = as.character(area_numbe),
    district_name = community
  ) %>%
  st_make_valid()

matches <- st_intersects(sites_m, areas_m)

site_match_audit <- tibble(
  site_id = sites_m$site_id,
  n_matches = lengths(matches)
)

write_csv(site_match_audit, "site_match_audit.csv")
print(count(site_match_audit, n_matches))
print(filter(site_match_audit, n_matches != 1))

assigned <- lengths(matches) == 1
area_index <- unlist(matches[assigned])

district_map <- areas_m %>%
  mutate(
    n_sites = tabulate(area_index, nbins = nrow(areas_m)),
    area_km2 = as.numeric(st_area(areas_m)) / 1e6,
    sites_per_km2 = n_sites / area_km2
  )

district_counts <- st_drop_geometry(district_map) %>%
  as_tibble()

stopifnot(all(district_counts$area_km2 > 0))
stopifnot(sum(district_counts$n_sites) == sum(assigned))

print(district_counts, n = Inf)
map_caption <- paste(
  "Sources: Chicago Public Library and City of Chicago. Downloaded Nov. 6, 2026.",
  "Libraries left unassigned:", sum(!assigned)
)

p_count <- ggplot(district_map) +
  geom_sf(aes(fill = n_sites), color = "grey55", linewidth = 0.2) +
  scale_fill_viridis_c(name = "Library count", na.value = "magenta") +
  labs(title = "Libraries by Chicago Community Area", caption = map_caption) +
  theme_bw()

p_density <- ggplot(district_map) +
  geom_sf(aes(fill = sites_per_km2), color = "grey55", linewidth = 0.2) +
  scale_fill_viridis_c(name = "Libraries per km²", na.value = "magenta") +
  labs(title = "Library Density by Chicago Community Area", caption = map_caption) +
  theme_bw()

print(p_count)
print(p_density)

print(slice_max(district_counts, n_sites, n = 1))
print(slice_max(district_counts, sites_per_km2, n = 1))
# Austin, Humboldt Park, Near West Side, New City, and Portage Park tied for the highest count, with 3 libraries each.
# Douglas had the highest density, with 0.468 libraries per km².
# Dividing by area shows that a smaller community area can rank higher in density even if it has fewer libraries.
write_csv(buffer_summary, "buffer_summary.csv")
write_csv(district_counts, "district_counts.csv")

ggsave("library_locations.png", p_sites, width = 8, height = 7)
ggsave("library_buffers.png", p_buffers, width = 8, height = 7)
ggsave("library_counts.png", p_count, width = 8, height = 7)
ggsave("library_density.png", p_density, width = 8, height = 7)