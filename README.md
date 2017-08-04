# luketools
Luke Davis's personal package of useful R functions.

## Exported Functions
- `stat_mode`: computes the mode (most frequently occurring value) of vectors
- `run_time`: simple, easy to use function that tells you how long a command took to run. Intended for interactive or script use, not benchmarking (use [`microbenchmark`](https://cran.r-project.org/web/packages/microbenchmark/index.html) for that).
- `parallelize`: manages code for setting up and spinning down CPU clusters, cleaning up your code and preventing mistakes
- `rolling`: computes functions over rolling windows. Can be used to create rolling means or medians with variable look back      or look forward windows, or more creatively for general purpose computation over windows.
- `holiday_dates`: vectorized holiday date lookup. Deals with the weirdness of the `timeDate` package's interface, making it  easy to pull all holiday dates for a given country and year en masse.

## Installation
You can use the `devtools` package to install `luketools`. First, install `devtools` (if you don't already have it) by running `install.packages("devtools")`. Next, use `devtools` to install the package from github by running `devtools::install_github("lukerobert/luketools")`.
