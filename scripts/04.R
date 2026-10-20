# Question 4: R's default figure, untouched
library(ggplot2)
library(svglite)

person <- read.csv("data/pslm_person_subset.csv")
hh     <- read.csv("data/pslm_hh_subset.csv")

hh_small <- hh[, c("hhid", "hhweight", "prov_code", "urban_rural")]
d <- merge(person, hh_small, by = "hhid")
d <- d[d$prov_code == 2 & d$urban_rural == 1, ]           # variant 0: Punjab rural
d <- d[d$age >= 10 & d$age <= 64 & !is.na(d$literate), ]  # literacy sample

p <- ggplot(d, aes(age, literate)) + geom_point()

dir.create("figures", showWarnings = FALSE)
ggsave("figures/default.svg", p)
