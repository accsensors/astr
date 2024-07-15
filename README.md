
<!-- README.md is generated from README.Rmd. Please edit that file -->

# astr

<!-- badges: start -->
<!-- badges: end -->

The goal of astr is to make working with Access Sensor Technologies
(AST) air sampler log files easy.

## Installation

The `astr` package can be installed from our
[GitHub](https://github.com/accsensors/astr). You must have the
`devtools` package installed. Use the following commands to install the
`devtools` and `astr` packages.

``` r
install.packages("devtools")
devtools::install_github("accsensors/astr")
```

## Reading Air Sampler Log Files into R

Once the astr package has been installed, you can load the package using
the following command:

``` r
library(astr)
```

Two functions are used to read log files into R: read_ast_header and
read_ast_log. These functions work for UPASv2, UPASv2x, and Home Health
Box (HHB) log files. To view the documentation for these two functions,
use either of the following commands:

``` r
?read_ast_header
?read_ast_log
```

To read the header data from multiple UPAS log files and create a data
frame with one row for each file:

``` r
data_upas_h <- list.files(path = ".", pattern="^PS.*.txt$",  full.names = TRUE)%>%
  lapply(read_ast_header, update_names = TRUE)%>%
  dplyr::bind_rows()
```

To read the sample logs from multiple UPAS log files and combine them
all into a single data frame:

``` r
data_upas <- list.files(path = ".", pattern="^PS.*.txt$",  full.names = TRUE)%>%
  lapply(read_ast_log, update_names = TRUE)%>%
  dplyr::bind_rows()
```

For the examples above, you will need to update the `path` argument in
the `list.files` function to indicate the location where your log files
are stored, relative to your working directory. Specifying
`pattern = "^PS.*.txt$"`, will choose only UPAS log files, since UPASv2
and UPASv2x log files are prefixed by `"PS"` and `"PSP"` respectively.
To read only UPASv2, UPASv2x, or HHB files edit the `pattern` argument
in `list.files` as shown below:

``` r
# UPASv2 files
data_upasv2 <- list.files(path = ".", pattern="^PS[1-9].*.txt$",  full.names = TRUE)%>%
  lapply(read_ast_log, update_names = FALSE)%>%
  dplyr::bind_rows()

# UPASv2x files
data_upasv2x <- list.files(path = ".", pattern="^PSP.*.txt$",  full.names = TRUE)%>%
  lapply(read_ast_log, update_names = FALSE)%>%
  dplyr::bind_rows()

# HHB files
data_hhb <- list.files(path = ".", pattern="^HHB.*.csv$",  full.names = TRUE)%>%
  lapply(read_ast_log, update_names = TRUE)%>%
  dplyr::bind_rows()
```

This package also includes many example log files to test your scripts.
To see a list of the example log files included with this package, use
the command below:

``` r
system.file("extdata", package = "astr", mustWork = TRUE) |>
    list.files(pattern=NULL, full.names = TRUE)
#>  [1] "C:/Users/GabeNeymark/AppData/Local/Temp/RtmpIPY2oZ/temp_libpath6868442b4e4b/astr/extdata/HHB00032_LOG_2024-07-01T18_20UTC.csv"                              
#>  [2] "C:/Users/GabeNeymark/AppData/Local/Temp/RtmpIPY2oZ/temp_libpath6868442b4e4b/astr/extdata/PS0166_LOG_2021-09-29T17_37_09UTC_test_______________---.txt"      
#>  [3] "C:/Users/GabeNeymark/AppData/Local/Temp/RtmpIPY2oZ/temp_libpath6868442b4e4b/astr/extdata/PS1422_LOG_2020-06-02T18_26_25UTC_rev100-norm________---.txt"      
#>  [4] "C:/Users/GabeNeymark/AppData/Local/Temp/RtmpIPY2oZ/temp_libpath6868442b4e4b/astr/extdata/PS1422_LOG_2020-06-02T18_29_11UTC_DIAGNOSTIC____________.txt"      
#>  [5] "C:/Users/GabeNeymark/AppData/Local/Temp/RtmpIPY2oZ/temp_libpath6868442b4e4b/astr/extdata/PS1771_LOG_2024-06-13T21_20_17UTC_GPSoutside_________Eng.txt"      
#>  [6] "C:/Users/GabeNeymark/AppData/Local/Temp/RtmpIPY2oZ/temp_libpath6868442b4e4b/astr/extdata/PS1771_LOG_2024-06-13T21_31_26UTC_DIAGNOSTIC____________.txt"      
#>  [7] "C:/Users/GabeNeymark/AppData/Local/Temp/RtmpIPY2oZ/temp_libpath6868442b4e4b/astr/extdata/PS1771_LOG_2024-06-18T02_46_00UTC_NoGPS______________Eng.txt"      
#>  [8] "C:/Users/GabeNeymark/AppData/Local/Temp/RtmpIPY2oZ/temp_libpath6868442b4e4b/astr/extdata/PS1786_LOG_2023-03-02T21_45_43UTC_DIAGNOSTIC____________.txt"      
#>  [9] "C:/Users/GabeNeymark/AppData/Local/Temp/RtmpIPY2oZ/temp_libpath6868442b4e4b/astr/extdata/PSP00024_LOG_2021-08-11T18_18_03UTC_test____________test______.txt"
#> [10] "C:/Users/GabeNeymark/AppData/Local/Temp/RtmpIPY2oZ/temp_libpath6868442b4e4b/astr/extdata/PSP00030_LOG_2022-05-11T23_24_01UTC_---------------_----------.txt"
#> [11] "C:/Users/GabeNeymark/AppData/Local/Temp/RtmpIPY2oZ/temp_libpath6868442b4e4b/astr/extdata/PSP00055_LOG_2022-02-24T19_26_03UTC_test1___________----------.txt"
#> [12] "C:/Users/GabeNeymark/AppData/Local/Temp/RtmpIPY2oZ/temp_libpath6868442b4e4b/astr/extdata/PSP00055_LOG_2022-03-24T18_05_32UTC_DIAGNOSTIC________________.txt"
#> [13] "C:/Users/GabeNeymark/AppData/Local/Temp/RtmpIPY2oZ/temp_libpath6868442b4e4b/astr/extdata/PSP00270_LOG_2024-06-10T21_50_55UTC_name____________eng_______.txt"
#> [14] "C:/Users/GabeNeymark/AppData/Local/Temp/RtmpIPY2oZ/temp_libpath6868442b4e4b/astr/extdata/PSP00270_LOG_2024-06-13T16_24_47UTC_DIAGNOSTIC________________.txt"
#> [15] "C:/Users/GabeNeymark/AppData/Local/Temp/RtmpIPY2oZ/temp_libpath6868442b4e4b/astr/extdata/PSP00270_LOG_2024-06-14T18_36_07UTC_GPSin-out-in____eng_______.txt"
#> [16] "C:/Users/GabeNeymark/AppData/Local/Temp/RtmpIPY2oZ/temp_libpath6868442b4e4b/astr/extdata/PSP00270_LOG_2024-06-14T18_54_44UTC_NoGPS___________----------.txt"
#> [17] "C:/Users/GabeNeymark/AppData/Local/Temp/RtmpIPY2oZ/temp_libpath6868442b4e4b/astr/extdata/PSP00270_LOG_2024-06-25T21_37_48UTC_GPS-in-out______----------.txt"
#> [18] "C:/Users/GabeNeymark/AppData/Local/Temp/RtmpIPY2oZ/temp_libpath6868442b4e4b/astr/extdata/PSP00270_LOG_2024-07-02T22_28_20UTC_fail____________----------.txt"
#> [19] "C:/Users/GabeNeymark/AppData/Local/Temp/RtmpIPY2oZ/temp_libpath6868442b4e4b/astr/extdata/PSP00270_LOG_2024-07-10T19_30_20UTC_StartOnNext_____----------.txt"
#> [20] "C:/Users/GabeNeymark/AppData/Local/Temp/RtmpIPY2oZ/temp_libpath6868442b4e4b/astr/extdata/PSP00270_LOG_2024-07-10T19_43_31UTC_000354__________----------.txt"
#> [21] "C:/Users/GabeNeymark/AppData/Local/Temp/RtmpIPY2oZ/temp_libpath6868442b4e4b/astr/extdata/PSP00270_LOG_2024-07-11T18_01_22UTC_PM_CO2_Map______----------.txt"
#> [22] "C:/Users/GabeNeymark/AppData/Local/Temp/RtmpIPY2oZ/temp_libpath6868442b4e4b/astr/extdata/SH00007_LOG_2021-12-13T13_28_41UTC_---------------_-----.txt"      
#> [23] "C:/Users/GabeNeymark/AppData/Local/Temp/RtmpIPY2oZ/temp_libpath6868442b4e4b/astr/extdata/SH00009_LOG_2022-02-14T17_02_32UTC_---------------_5VX__.txt"
```

You will notice there are log files for UPASv2 (prefixed by `"PS"`),
UPASv2x (prefixed by `"PSP"`), and HHB (prefixed by `"HHB"`). Use the
commands below to generate example data frames using the example UPAS
log files. Change the `pattern` argument in `list.files` as previously
described to select the types of log file read
(e.g. `pattern = "^HHB.*.csv$"` for HHB log files.

``` r
data_upas_examples_h <- system.file("extdata", package = "astr", mustWork = TRUE) |>
    list.files(pattern="^PS.*.txt$", full.names = TRUE) %>%
    lapply(read_ast_header, update_names = TRUE) %>%
    dplyr::bind_rows()
```

``` r
data_upas_examples <- system.file("extdata", package = "astr", mustWork = TRUE) |>
    list.files(pattern="^PS.*.txt$", full.names = TRUE) %>%
    lapply(read_ast_log, update_names = TRUE) %>%
    dplyr::bind_rows()
```

Describe the other functions Describe shiny functions and
recommendations for use Describe map functions and maybe show example
Edit the read_ast_log and read_ast_header documentation to also specify
some of the extra info stated here
