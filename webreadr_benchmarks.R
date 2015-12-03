# This code generates visualisations and benchmarks highlighting the difference in speed between webreadr and base R
# equivalent functions, for inclusion in the paper "R Packages to Aid in Handling Web Access Logs"

library(httr) # only existing parser equivalent
library(microbenchmark) # benchmarking
library(ggplot2) # Plotting benchmarks
library(wmf) # theming plots
library(webreadr) # doy

# We already have an example file, expanded_squid_log.squid, containing 600000 values. Let's use that as a basis.

# Create the functions
webreadr_read_squid <- webreadr::read_squid
r_base_read_squid <- function(filename){
  names <- c("timestamp", "time_elapsed", "ip_address", "status_code",
             "bytes_sent","http_method", "url","remote_user_ident","peer_info")
  data <- read.delim(filename, sep = " ", header = FALSE, as.is = TRUE, col.names = names)
  data$timestamp <- as.POSIXlt(data$timestamp, origin = "1970-01-01", tz = "UTC")
  return(data)
}

# Benchmark
reading_benchmarks <- microbenchmark({(webreadr_read_squid("expanded_squid_log.squid"))},
                                     {(r_base_read_squid("expanded_squid_log.squid"))})

# Relevel
levels(reading_benchmarks$expr)[grepl(x = levels(reading_benchmarks$expr), pattern = "webreadr")] <- "webreadr"
levels(reading_benchmarks$expr)[levels(reading_benchmarks$expr) != "webreadr"] <- "base R"

# Plot
ggsave(file = "./paper/reading_benchmarks.png",
       autoplot(reading_benchmarks) + theme_fivethirtynine(base_size = 14)  + theme_fivethirtynine(base_size = 14) + 
         scale_y_continuous(expand=c(0,0)) + 
         labs(y = "Time [seconds]", title = "Reading 600k lines of access log, base R versus webreadr"))

# Save benchmarks
save(reading_benchmarks, file = "webreadr_benchmarks.RData")

