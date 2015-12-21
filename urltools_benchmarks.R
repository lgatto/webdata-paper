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

# Reset levels
levels(decoding_benchmarks$expr)[grepl(x = levels(decoding_benchmarks$expr), pattern = "urltools")] <- "urltools"
levels(decoding_benchmarks$expr)[levels(decoding_benchmarks$expr) != "urltools"] <- "base R"

# Original Plot
ggsave(file = "./paper/decoding_benchmarks.png",
       plot = autoplot(decoding_benchmarks) + theme_fivethirtynine(base_size = 14) +
         labs(title = "Decoding 1m URLs, base R versus urltools (log-scaled)"))

# Test parsing. Also fairly simple
# Set up the functions:
urltools_parse <- urltools::url_parse
httr_parse     <- function(x){
  lapply(x, httr::parse_url)
}

# Run
parsing_benchmarks <- microbenchmark({(urltools_parse(example_urls))},
                                     {(httr_parse(example_urls))})

# Reset levels
levels(parsing_benchmarks$expr)[grepl(x = levels(parsing_benchmarks$expr), pattern = "urltools")] <- "urltools"
levels(parsing_benchmarks$expr)[levels(parsing_benchmarks$expr) != "urltools"] <- "httr"

# Plot
ggsave(file = "./paper/parsing_benchmarks.png",
       plot = autoplot(parsing_benchmarks) + theme_fivethirtynine(base_size = 14) +
         labs(title = "Parsing 1m URLs, httr versus urltools (log-scaled)"))

# Save the results to disc, because nobody wants to lose benchmarks that take _this long_ to compute.
save(decoding_benchmarks, parsing_benchmarks, file = "urltools_benchmarks.RData")

### Jay doing some updates
load(file = "urltools_benchmarks.RData")

dcode <- as.data.frame(decoding_benchmarks)
dcode$time.sec <- dcode$time/1e+9

mbrk <- c(seq(2,9), seq(20,90,by=10))
mbrk.df <- data.frame(y=mbrk, expr=NA, time.sec=NA)
brk <- c(1, 10, 100, 200)
gg <- ggplot(dcode, aes(expr, time.sec)) + 
  geom_hline(data=mbrk.df, aes(yintercept=y), color="gray95") +
  geom_violin(fill="steelblue", color="steelblue") +
  scale_y_log10("Time [seconds]", breaks=brk, limits=c(1,200), expand=c(0,0)) +
  coord_flip() +
  ggtitle("Decoding 1m URLs, base R versus urltools (log-scaled)") +
  theme(panel.background=element_rect(fill=NA, color="gray75"),
        panel.grid.major=element_line(color="gray75"),
        panel.grid.minor=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks=element_blank())
ggsave(gg, file = "./paper/decoding_benchmarks-jay.png")
       

## parsing_benchmarks
pcode <- as.data.frame(parsing_benchmarks)
pcode$time.sec <- pcode$time/1e+9

mbrk <- c(seq(2,9), seq(20,90,by=10), seq(200,900,100), seq(2000, 9000, 1000))
mbrk.df <- data.frame(y=mbrk, expr=NA, time.sec=NA)
brk <- c(1, 10, 100, 1000, 10000)
gg <- ggplot(pcode, aes(expr, time.sec)) + 
  geom_hline(data=mbrk.df, aes(yintercept=y), color="gray95") +
  geom_violin(fill="steelblue", color="steelblue") +
  scale_y_log10("Time [seconds]", breaks=brk, limits=c(1,10000), expand=c(0,0)) +
  coord_flip() +
  ggtitle("Parsing 1m URLs, httr versus urltools (log-scaled)") +
  theme(panel.background=element_rect(fill=NA, color="gray75"),
        panel.grid.major=element_line(color="gray75"),
        panel.grid.minor=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks=element_blank())
gg
ggsave(gg, file = "./paper/parsing_benchmarks-jay.png")
