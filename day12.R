library("readr")
library("combinat")
library("purrr")

dat <- read_lines("input12.txt") %>%
  strsplit(" ") %>%
  lapply(as.list)

rows <- lapply(dat, `[[`, 1) %>%
  lapply(function(x) strsplit(x, "")[[1]])

sizes <- lapply(dat, `[[`, 2) %>%
  lapply(function(x) strsplit(x, ",")[[1]]) %>%
  lapply(as.numeric)

get_candidates <- function(row, sizes) {
  poss_vals <- c(
    rep("#", sum(sizes)),
    rep(".", length(row) - sum(sizes))
  )
  perms <- unique(permn(poss_vals))
  candidate <- keep(perms, \(x) all(x == row | row == "?"))
}

is_candidate_consistent <- function(candidate, sizes) {
  cand <- gsub("\\.", "0", candidate) %>%
    gsub("\\#", "1", .) %>%
    as.numeric()
  groups <- split(cand, cumsum(c(1, diff(cand) != 0)))
  sums <- vapply(groups, sum, numeric(1))
  identical(unname(sums[sums != 0]), sizes)
}

