# format_hhb_samples works with all HHBv2 firmwares

    Code
      format_hhb_samples(hhb_header_240111)
    Output
        HHBserial                          LogFileName SampleName Channel ChannelType
      1  HHB00032 HHB00032_LOG_2024-07-01T18_20UTC.csv   20240701       A      Filter
      2  HHB00032 HHB00032_LOG_2024-07-01T18_20UTC.csv   20240701       B      Filter
      3  HHB00032 HHB00032_LOG_2024-07-01T18_20UTC.csv   20240701       C     Sorbent
      4  HHB00032 HHB00032_LOG_2024-07-01T18_20UTC.csv   20240701       D     Sorbent
        PumpPCB    CID PumpStartingVolume PumpStartingRuntime CartridgeStartingVolume
      1 FP00003    BJ0           13.72000               0.166                       0
      2 FP00162    BJ4           16.98000               0.196                       0
      3 SP00004 808677            0.16996               0.442                       0
      4 SP00004 808678            0.18975               0.282                       0
        CartridgeStartingRuntime VolumetricFlowRate DutyCycle ShutdownMode
      1                        0              2.000       100            1
      2                        0              2.000       100            1
      3                        0              0.003       100            1
      4                        0              0.003       100            1
        SampledRunTime SampledVolume AverageVolumetricFlowRate
      1          0.499      59.96000                   1.99900
      2          0.499      59.94000                   1.99800
      3          0.499       0.09154                   0.00305
      4          0.499       0.08792                   0.00293

---

    Code
      format_hhb_samples(hhb_header_250529)
    Output
        HHBserial                          LogFileName   SampleName Channel
      1  HHB00087 HHB00087_LOG_2025-06-03T20_55UTC.csv FirmwareTest       A
      2  HHB00087 HHB00087_LOG_2025-06-03T20_55UTC.csv FirmwareTest       B
      3  HHB00087 HHB00087_LOG_2025-06-03T20_55UTC.csv FirmwareTest       C
      4  HHB00087 HHB00087_LOG_2025-06-03T20_55UTC.csv FirmwareTest       D
        ChannelType PumpPCB  CID PumpStartingVolume PumpStartingRuntime
      1      Filter FP00084 <NA>        82312.80000             680.640
      2      Filter FP00049 <NA>        43568.73000             364.351
      3     Sorbent SP00076 <NA>           96.47840             516.256
      4     Sorbent SP00076 <NA>           68.47116             366.037
        CartridgeStartingVolume CartridgeStartingRuntime VolumetricFlowRate DutyCycle
      1             20116.46000                      168              2.000         0
      2             20113.09000                      168              2.000         0
      3                30.45342                      168              0.003         0
      4                30.67436                      168              0.003         0
        ProgrammedStartDelay SampleProgrammedStartTime SampleProgrammedRuntime
      1                   NA                      <NA>                      NA
      2                   NA                      <NA>                      NA
      3                   NA                      <NA>                      NA
      4                   NA                      <NA>                      NA
        SampleStartDateTimeUTC SampleEndDateTimeUTC ShutdownMode SampledRunTime
      1    2025-06-03 20:55:00  2025-06-03 21:00:00            1          0.083
      2    2025-06-03 20:55:00  2025-06-03 21:00:00            1          0.083
      3    2025-06-03 20:55:00  2025-06-03 21:00:00            1          0.083
      4    2025-06-03 20:55:00  2025-06-03 21:00:00            1          0.083
        SampledVolume AverageVolumetricFlowRate
      1       9.93000                   1.99900
      2       9.90000                   1.99300
      3       0.01452                   0.00292
      4       0.01518                   0.00306

# format_hhb_sensors works with all HHBv2 firmwares

    Code
      format_hhb_sensors(hhb_log_240111, hhb_header_240111)
    Output
      # A tibble: 61 x 9
         HHBserial LogFileName           Position ID    SampleTime DateTimeUTC        
         <chr>     <chr>                    <dbl> <chr> <drtn>     <dttm>             
       1 HHB00032  HHB00032_LOG_2024-07~        1 2021~   0 secs   2024-07-01 18:20:00
       2 HHB00032  HHB00032_LOG_2024-07~        1 2021~  30 secs   2024-07-01 18:20:30
       3 HHB00032  HHB00032_LOG_2024-07~        1 2021~  60 secs   2024-07-01 18:21:00
       4 HHB00032  HHB00032_LOG_2024-07~        1 2021~  90 secs   2024-07-01 18:21:30
       5 HHB00032  HHB00032_LOG_2024-07~        1 2021~ 120 secs   2024-07-01 18:22:00
       6 HHB00032  HHB00032_LOG_2024-07~        1 2021~ 150 secs   2024-07-01 18:22:30
       7 HHB00032  HHB00032_LOG_2024-07~        1 2021~ 180 secs   2024-07-01 18:23:00
       8 HHB00032  HHB00032_LOG_2024-07~        1 2021~ 210 secs   2024-07-01 18:23:30
       9 HHB00032  HHB00032_LOG_2024-07~        1 2021~ 240 secs   2024-07-01 18:24:00
      10 HHB00032  HHB00032_LOG_2024-07~        1 2021~ 270 secs   2024-07-01 18:24:30
      # i 51 more rows
      # i 3 more variables: G.SCD30_Temp <dbl>, WE <dbl>, AUX <dbl>

---

    Code
      format_hhb_sensors(hhb_log_250529, hhb_header_250529)
    Output
      # A tibble: 33 x 18
         HHBserial LogFileName   Position ID    Type  ISB_Gain Sensitivity   WEt   WEe
         <chr>     <chr>            <dbl> <chr> <chr>    <dbl>       <dbl> <dbl> <dbl>
       1 HHB00087  HHB00087_LOG~        1 2021~ NO2-~    -0.73       -297.   235   236
       2 HHB00087  HHB00087_LOG~        1 2021~ NO2-~    -0.73       -297.   235   236
       3 HHB00087  HHB00087_LOG~        1 2021~ NO2-~    -0.73       -297.   235   236
       4 HHB00087  HHB00087_LOG~        1 2021~ NO2-~    -0.73       -297.   235   236
       5 HHB00087  HHB00087_LOG~        1 2021~ NO2-~    -0.73       -297.   235   236
       6 HHB00087  HHB00087_LOG~        1 2021~ NO2-~    -0.73       -297.   235   236
       7 HHB00087  HHB00087_LOG~        1 2021~ NO2-~    -0.73       -297.   235   236
       8 HHB00087  HHB00087_LOG~        1 2021~ NO2-~    -0.73       -297.   235   236
       9 HHB00087  HHB00087_LOG~        1 2021~ NO2-~    -0.73       -297.   235   236
      10 HHB00087  HHB00087_LOG~        1 2021~ NO2-~    -0.73       -297.   235   236
      # i 23 more rows
      # i 9 more variables: AEt <dbl>, AEe <dbl>, SampleTime <drtn>,
      #   DateTimeUTC <dttm>, G.SCD30_Temp <dbl>, WE <dbl>, AUX <dbl>, Alg <dbl>,
      #   ppb <dbl>

