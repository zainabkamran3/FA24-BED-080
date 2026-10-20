# Question 2: histogram of education spending, Punjab rural students

library(ggplot2)
library(svglite)

# 1. read the data
person <- read.csv("data/pslm_person_subset.csv")
hh     <- read.csv("data/pslm_hh_subset.csv")

# 2. add weight, province and area to the person file
hh_small <- hh[, c("hhid", "hhweight", "prov_code", "urban_rural")]
stopifnot(!any(duplicated(hh_small$hhid)))
d <- merge(person, hh_small, by = "hhid")
cat("Rows before/after merge:", nrow(person), nrow(d), "\n")

# 3. keep my variant: Punjab (2), rural (1)
d <- d[d$prov_code == 2 & d$urban_rural == 1, ]
stopifnot(nrow(d) > 0)

# 4. students = spending above zero (drop missing spending and weights)
s <- d[!is.na(d$ed_total) & d$ed_total > 0 & !is.na(d$hhweight), ]

# 5. weighted quantile
wq <- function(x, w, p) {
  o <- order(x)
  x <- x[o]
  w <- w[o]
  share <- cumsum(w) / sum(w)
  x[share >= p][1]
}

n_students <- nrow(s)
w_median   <- wq(s$ed_total, s$hhweight, 0.5)
u_median   <- median(s$ed_total)
p10        <- wq(s$ed_total, s$hhweight, 0.1)
p90        <- wq(s$ed_total, s$hhweight, 0.9)

# 6. log10 of spending, and students outside Rs 100 to Rs 1,000,000
s$lx <- log10(s$ed_total)
n_excluded <- sum(s$lx < 2 | s$lx > 6)

cat("Students:", n_students, "\n")
cat("Weighted median:", w_median, "  Unweighted median:", u_median, "\n")
cat("P10:", p10, "  P90:", p90, "  Ratio:", round(p90 / p10, 2), "\n")
cat("Excluded:", n_excluded, "\n")

# 7. the figure
fmt <- function(v) format(round(v), big.mark = ",", scientific = FALSE, trim = TRUE)

s_plot <- s[s$lx >= 2 & s$lx <= 6, ]

p <- ggplot(s_plot, aes(x = lx, y = after_stat(density), weight = hhweight)) +
  geom_histogram(breaks = seq(2, 6, by = 0.1), closed = "left",
                 fill = "grey55", colour = "white") +
  geom_vline(xintercept = log10(c(w_median, p90)), linetype = "dashed") +
  annotate("text", x = log10(w_median), y = Inf, vjust = 1.5, hjust = 1.1,
           label = paste("Weighted median: Rs", fmt(w_median))) +
  annotate("text", x = log10(p90), y = Inf, vjust = 1.5, hjust = -0.1,
           label = paste("90th percentile: Rs", fmt(p90))) +
  scale_x_continuous(breaks = 2:6,
                     labels = c("Rs 100", "Rs 1,000", "Rs 10,000",
                                "Rs 100,000", "Rs 1,000,000")) +
  coord_cartesian(xlim = c(2, 6)) +
  labs(title = paste("A typical rural Punjab student costs about Rs", fmt(w_median),
                     "a year; the top tenth cost Rs", fmt(p90), "or more"),
       x = "Annual spending on education (log scale)",
       y = "Density per log10 unit",
       caption = paste0("Source: PSLM 2019-20. Sample: Punjab rural students, ",
                        "ed_total > 0 (n = ", n_students, "). Weighted by hhweight. ",
                        n_excluded, " students outside Rs 100 to Rs 1,000,000 excluded.")) +
  theme_minimal()

# 8. save the figure (after p is built)
dir.create("figures", showWarnings = FALSE)

print(p)   # if this errors, the problem is in the plot, not the saving

ggsave(filename = "figures/distribution.svg",
       plot     = p,
       device   = svglite::svglite,
       width    = 9,
       height   = 5.5)

# optional PNG backup
ggsave("figures/distribution.png", p, width = 9, height = 5.5, dpi = 300)


#  summary table
summary_table <- data.frame(
  Statistic = c("Students (n)",
                "Weighted median (Rs)",
                "Unweighted median (Rs)",
                "10th percentile, weighted (Rs)",
                "90th percentile, weighted (Rs)",
                "P90 / P10 ratio",
                "Students outside Rs 100 to Rs 1,000,000"),
  Value = c(n_students,
            w_median,
            u_median,
            p10,
            p90,
            round(p90 / p10, 2),
            n_excluded)
)

print(summary_table, row.names = FALSE)

dir.create("tables", showWarnings = FALSE)
write.csv(summary_table, "tables/summary_table.csv", row.names = FALSE)
