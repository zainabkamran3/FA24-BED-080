# Question 3: literacy by age band and sex, Punjab rural

library(ggplot2)
library(svglite)

# 1-3. read, merge, keep my variant (same as script 02)
person <- read.csv("data/pslm_person_subset.csv")
hh     <- read.csv("data/pslm_hh_subset.csv")
hh_small <- hh[, c("hhid", "hhweight", "prov_code", "urban_rural")]
d <- merge(person, hh_small, by = "hhid")
d <- d[d$prov_code == 2 & d$urban_rural == 1, ]

# 4. literacy sample: age 10-64, literate not missing
l <- d[d$age >= 10 & d$age <= 64 & !is.na(d$literate), ]

# 5. age band: 10 for 10-14, 15 for 15-19, ...
l$band_start <- floor(l$age / 5) * 5

# 6. loop over sex and band: count people, weighted literacy rate
rows <- data.frame()
for (sx in c(1, 2)) {
  for (b in seq(10, 60, by = 5)) {
    g <- l[l$sex == sx & l$band_start == b, ]
    rate <- 100 * sum(g$hhweight * g$literate) / sum(g$hhweight)
    one_row <- data.frame(sex  = ifelse(sx == 1, "Men", "Women"),
                          band = paste0(b, "-", b + 4),
                          mid  = b + 2,
                          n    = nrow(g),
                          rate = round(rate, 1))
    rows <- rbind(rows, one_row)
  }
}
rows$thin <- rows$n < 30     # thin cell = fewer than 30 observations

# 7. band table (one row per band) for the README
men   <- rows[rows$sex == "Men", ]
women <- rows[rows$sex == "Women", ]
band_table <- data.frame(band = men$band, mid = men$mid,
                         n_men = men$n,     rate_men = men$rate,
                         n_women = women$n, rate_women = women$rate)
band_table$gap <- round(band_table$rate_men - band_table$rate_women, 1)
print(band_table)
write.csv(band_table, "figures/band_table_r.csv", row.names = FALSE)

# 8. band with the largest gap
i <- which.max(abs(band_table$gap))
big <- band_table[i, ]
cat("Largest gap:", big$band, "with", big$gap, "points\n")

# 9. the figure
p <- ggplot(rows, aes(x = mid, y = rate, colour = sex, group = sex)) +
  geom_line() +
  geom_point(aes(shape = thin), size = 3, stroke = 1) +
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 1)) +
  geom_text(data = rows[rows$mid == 62, ], aes(label = sex),
            nudge_x = 1, hjust = 0) +
  annotate("segment", x = big$mid, xend = big$mid,
           y = big$rate_women, yend = big$rate_men) +
  annotate("text", x = big$mid + 1, y = (big$rate_women + big$rate_men) / 2,
           hjust = 0, label = paste0("Largest gap\n", big$gap, " points\n(ages ", big$band, ")")) +
  scale_x_continuous(breaks = seq(12, 62, by = 10), limits = c(10, 70)) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(title = paste0("Women's literacy falls faster with age than men's: the gap is ",
                      big$gap, " points at ages ", big$band),
       x = "Age band (plotted at midpoint)",
       y = "Literate, weighted percent",
       caption = paste0("Source: PSLM 2019-20. Sample: Punjab rural persons aged 10-64 ",
                        "with literacy recorded (n = ", nrow(l), "). Weighted by hhweight. ",
                        "Hollow markers = band with fewer than 30 observations.")) +
  theme_minimal() +
  theme(legend.position = "none")      # no legend, series are labelled directly

ggsave("figures/literacy_by_age.svg", p, width = 9, height = 5.5)
