library("readr")
library("stringr")

dat <- read_lines("input06.txt") %>%
  str_extract_all("\\d+") %>%
  lapply(as.numeric) %>%
  setNames(c("time", "duration"))

race_result <- function(hold_time, duration) {
  move_time <- duration - hold_time
  hold_time * move_time
}

get_possible_results <- function(duration) {
  vapply(0:duration, race_result, numeric(1), duration = duration)
}

get_winning_times <- function(poss_results, record) {
  sum(poss_results > record)
}

## Part 1
poss_results <- lapply(dat[["time"]], get_possible_results)
prod(mapply(get_winning_times, poss_results, dat[["duration"]]))

## Part 2
dat2 <- lapply(dat, function(x) as.numeric(paste0(x, collapse = "")))

poss_results2 <- get_possible_results(dat2[["time"]])
prod(get_winning_times(poss_results2, dat2[["duration"]]))
