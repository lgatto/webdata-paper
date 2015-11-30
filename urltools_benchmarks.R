# This code generates visualisations and benchmarks highlighting the difference in speed between urltools and non-urltools
# equivalent functions, for inclusion in the paper "R Packages to Aid in Handling Web Access Logs"

library(httr) # only existing parser equivalent
library(microbenchmark) # benchmarking
library(ggplot2) # Plotting benchmarks
library(wmf) # theming plots
library(urltools) # doy

# First, decoding. This is fairly simple.
# Set up the functions
urltools_decode <- urltools::url_decode
r_base_decode   <- function(x){
  vapply(x, URLdecode, "character")
}

# Generate the data. We want a moderately complex but not unnecessarily so URL, and we want 1m of them.
# Let's use the urltools example
example_urls <- rep("https://en.wikipedia.org/wiki/File:Vice_City_Public_Radio_%28logo%29.jpg", 1000000)

# Generate benchmarks
decoding_benchmarks <- microbenchmark({(urltools_decode(example_urls))},
                                      {(r_base_decode(example_urls))})

# Test parsing. Also fairly simple
# Set up the functions:
urltools_parse <- urltools::url_parse
httr_parse     <- function(x){
  lapply(x, httr::parse_url)
}

parsing_benchmarks <- microbenchmark({(urltools_parse(example_urls))},
                                     {(httr_parse(example_urls))})

# Save the results to disc, because nobody wants to lose benchmarks that take _this long_ to compute.
save(decoding_benchmarks, parsing_benchmarks, file = "urltools_benchmarks.RData")
