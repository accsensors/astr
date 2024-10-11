# read_ast_header works with all UPASv2 firmwares

    Code
      read_ast_header(upasv2_rev100_file, update_names = FALSE)
    Output
        ASTSampler UPASserial
      1  UPAS_v2_0     PS1422
                                                                             Firmware
      1 UPAS_v2_0-rev0100-L152RE_20180405_mbed116.bin compiledApr  6 2018_00:19:25UTC
        FirmwareRev                                                  LogFilename
      1         100 PS1422_LOG_2020-06-02T18_26_25UTC_rev100-norm________---.txt
        PowerCycles CumulativeSamplingTime StartOnNextPowerUp       StartDateTime
      1           8                    5.7              FALSE 2020-06-02 18:26:25
        GPSUTCOffset ProgrammedStartDelay ProgrammedRuntime VolumetricFlowRate
      1           -6                    0            172800                  1
        DutyCycle DutyCycleWindow GPSEnabled LogFileMode LogInterval AppLock
      1       100              30       TRUE      normal          30       0
        AppVersion ShutdownMode       ShutdownReason StartBatteryCharge
      1     i2.1.9            1 user pushbutton stop                 53
        StartBatteryVoltage EndBatteryCharge EndBatteryVoltage SampledVolume
      1                2.83               53              2.85           0.9
        SampledRuntime LoggedRuntime AverageVolumetricFlow
      1          0.015         0.015                 1.016

---

    Code
      read_ast_header(upasv2_rev100_diag_file, update_names = FALSE)
    Output
        ASTSampler UPASserial
      1  UPAS_v2_0     PS1422
                                                                             Firmware
      1 UPAS_v2_0-rev0100-L152RE_20180405_mbed116.bin compiledApr  6 2018_00:19:25UTC
        FirmwareRev                                                  LogFilename
      1         100 PS1422_LOG_2020-06-02T18_29_11UTC_DIAGNOSTIC____________.txt
        PowerCycles CumulativeSamplingTime      MF4      MF3       MF2      MF1
      1          10                   5.72 0.015635 0.606876 -1.676746 2.649093
              MF0 MFSVoltMin MFSVoltMax MFSVoltMaxEst MFSMFMin MFSMFMax MFSMFMaxEst
      1 -1.030556   0.525875    2.15325          2.99    2e-04   3.3004     9.37197
              CalUNIXTIME CalDateTime FlowOffset StartOnNextPowerUp
      1 1584561792.000000        <NA>          0              FALSE
              StartDateTime GPSUTCOffset ProgrammedStartDelay ProgrammedRuntime
      1 2020-06-02 18:29:11           -6                    0            172800
        VolumetricFlowRate DutyCycle DutyCycleWindow GPSEnabled LogFileMode
      1                  1         0              30       TRUE       debug
        LogInterval AppLock AppVersion ShutdownMode ShutdownReason StartBatteryCharge
      1           1       0     i2.1.9            0  unknown error                 53
        StartBatteryVoltage EndBatteryCharge EndBatteryVoltage SampledVolume
      1                2.83               53              2.88           1.5
        SampledRuntime LoggedRuntime AverageVolumetricFlow MFSDIAGVoutBlocked
      1          0.025         0.025                 1.001             0.6195
        MFSDIAGVoutMax MFSDIAGVoutMin MFSDIAGMFBlocked MFSDIAGMFMax MFSDIAGMFMin
      1       2.073125       0.944375                0     2.951006     0.499354
        MFSDIAGPumpVBoostMax MFSDIAGPumpVBoostMin MFSDIAGPDeadhead
      1             28.01557             7.109706         546.1166

---

    Code
      read_ast_header(upasv2_rev125_file, update_names = FALSE)
    Output
        ASTSampler UPASserial
      1  UPAS_v2_0     PS0166
                                                                             Firmware
      1 UPAS_v2_0-rev0125-L152RE_20210925_mbed116.bin compiledSep 25 2021_17:35:48UTC
        FirmwareRev LifetimeSampleCount LifetimeSampleRuntime
      1         125                   3                     0
                                                         LogFilename SampleName
      1 PS0166_LOG_2021-09-29T17_37_09UTC_test_______________---.txt       test
        CartridgeID GPSUTCOffset StartOnNextPowerUp ProgrammedStartDelay
      1          NA           -6              FALSE                    0
        ProgrammedRuntime VolumetricFlowRate FlowOffset DutyCycle DutyCycleWindow
      1            172800                  1          0       100              30
        GPSEnabled LogFileMode LogInterval AppLock AppVersion    StartDateTimeUTC
      1       TRUE      normal          30       0     i2.2.2 2021-09-29 17:37:09
          LocalTZ  StartDateTimeLocal StartBatteryCharge StartBatteryVoltage
      1 Etc/GMT+6 2021-09-29 11:37:09                 54                3.58
             EndDateTimeUTC    EndDateTimeLocal EndBatteryCharge EndBatteryVoltage
      1 2021-09-29 17:41:30 2021-09-29 11:41:30               54              3.54
        ShutdownMode       ShutdownReason SampledVolume SampledRuntime LoggedRuntime
      1            1 user pushbutton stop          4.28           0.07         0.069
        AverageVolumetricFlowRate UserTZ
      1                     1.022  FALSE

---

    Code
      read_ast_header(upasv2_rev130_diag_file, update_names = FALSE)
    Output
        ASTSampler UPASserial
      1  UPAS_v2_0     PS1786
                                                                             Firmware
      1 UPAS_v2_0-rev0130-L152RE_20220310_mbed116.bin compiledMar 10 2022_20:39:40UTC
        FirmwareRev LifetimeSampleCount LifetimeSampleRuntime
      1         130                   1                     0
                                                         LogFilename SampleName
      1 PS1786_LOG_2023-03-02T21_45_43UTC_DIAGNOSTIC____________.txt DIAGNOSTIC
        CartridgeID GPSUTCOffset StartOnNextPowerUp ProgrammedStartDelay
      1          NA           -8              FALSE                    0
        ProgrammedRuntime VolumetricFlowRate FlowOffset DutyCycle DutyCycleWindow
      1            172800                  1          0         0              30
        GPSEnabled LogFileMode LogInterval AppLock AppVersion    StartDateTimeUTC
      1       TRUE       debug           1       0     i2.2.2 2023-03-02 21:45:43
          LocalTZ  StartDateTimeLocal StartBatteryCharge StartBatteryVoltage
      1 Etc/GMT+8 2023-03-02 13:45:43                 99                3.56
             EndDateTimeUTC    EndDateTimeLocal EndBatteryCharge EndBatteryVoltage
      1 2023-03-02 21:48:17 2023-03-02 13:48:17               99               3.5
        ShutdownMode ShutdownReason SampledVolume SampledRuntime LoggedRuntime
      1            0  unknown error          0.97          0.011         0.023
        AverageVolumetricFlowRate      MF4       MF3       MF2      MF1       MF0
      1                     1.538 0.329437 -0.478687 -0.141832 1.783128 -0.814451
        MFSVoltMin MFSVoltMax MFSVoltMaxEst MFSMFMin MFSMFMax MFSMFMaxEst
      1    0.49375    1.89925       1.89925        0   3.0629      3.0629
              CalUNIXTIME         CalDateTime MFSDIAGVoutBlocked MFSDIAGVoutMax
      1 1677782528.000000 2023-03-02 18:42:08               0.54       1.914125
        MFSDIAGVoutMin MFSDIAGMFBlocked MFSDIAGMFMax MFSDIAGMFMin
      1         0.8405                0       3.0629     0.464254
        MFSDIAGPumpVBoostMax MFSDIAGPumpVBoostMin MFSDIAGPDeadhead UserTZ
      1             28.00899             7.160145          1638.35  FALSE

---

    Code
      read_ast_header(upasv2_rev138_file, update_names = FALSE)
    Output
        ASTSampler UPASserial
      1  UPAS_v2_0     PS1771
                                                                             Firmware
      1 UPAS_v2_0-rev0138-L152RE_20230510_mbed116.bin compiledMay 11 2023_02:06:55UTC
        FirmwareRev LifetimeSampleCount LifetimeSampleRuntime
      1         138                  43                 532.6
                                                         LogFilename SampleName
      1 PS1771_LOG_2024-06-13T21_20_17UTC_GPSoutside_________Eng.txt GPSoutside
        CartridgeID GPSUTCOffset StartOnNextPowerUp ProgrammedStartDelay
      1         Eng           -6              FALSE                    0
        ProgrammedRuntime VolumetricFlowRate FlowOffset DutyCycle DutyCycleWindow
      1           3.6e+08                  1          0       100              30
        GPSEnabled LogFileMode LogInterval AppLock AppVersion    StartDateTimeUTC
      1       TRUE      normal          30       0     i2.2.2 2024-06-13 21:20:17
          LocalTZ  StartDateTimeLocal StartBatteryCharge StartBatteryVoltage
      1 Etc/GMT+6 2024-06-13 15:20:17                 93                4.04
             EndDateTimeUTC    EndDateTimeLocal EndBatteryCharge EndBatteryVoltage
      1 2024-06-13 21:25:30 2024-06-13 15:25:30               93              4.01
        ShutdownMode       ShutdownReason SampledVolume SampledRuntime LoggedRuntime
      1            1 user pushbutton stop          5.02          0.084         0.084
        AverageVolumetricFlowRate UserTZ
      1                     0.997  FALSE

# read_ast_header works with all UPASv2x firmwares

    Code
      read_ast_header(upasv2x_rev81_file, update_names = FALSE)
    Output
        ASTSampler UPASserial
      1  UPAS_v2_x   PSP00024
                                                                                       Firmware
      1 UPAS_v2_x-rev_00081-L476RE_20210806_mbedOS-6_13_0.bin compiled Aug  6 2021_21:59:53 UTC
        FirmwareRev LifetimeSampleCount LifetimeSampleRuntime
      1          81                  20                524.25
                                                                        LogFilename
      1 20210811/PSP00024_LOG_2021-08-11T18_18_03UTC_test____________test______.txt
        SampleName CartridgeID GPSUTCOffset StartOnNextPowerUp ProgrammedStartTime
      1       test        test           -4              FALSE                   0
        ProgrammedRuntime SizeSelectiveInlet FlowRateSetpoint FlowOffset
      1                NA              PM2.5                1          0
        FlowCheckMeterReadingPreSample FlowCheckMeterReadingPostSample FlowDutyCycle
      1                             NA                              NA            20
        DutyCycleWindow GPSEnabled PMSensorInterval      PMSensorOperation
      1              30      FALSE                1 Continuous Measurement
        RTGasSampleState LogInterval PowerSaveMode AppLock AppVersion
      1            FALSE          30          TRUE   FALSE     i1.0.0
           StartDateTimeUTC   LocalTZ  StartDateTimeLocal      EndDateTimeUTC
      1 2021-08-11 18:18:03 Etc/GMT+4 2021-08-11 14:18:03 2021-08-12 01:04:11
           EndDateTimeLocal OverallDuration PumpingDuration OverallFlowRateAverage
      1 2021-08-11 21:04:11           6.763           1.353                    0.2
        PumpingFlowRateAverage SampledVolume StartBatteryCharge EndBatteryCharge
      1                  0.999         81.06                 99               86
        StartBatteryVoltage EndBatteryVoltage ShutdownMode       ShutdownReason
      1                4.13              3.96            1 user pushbutton stop
                 MFSCalDate MFSCalPerson MFSCalVoutBlocked MFSCalVoutMin
      1 2021-04-22 00:44:48         <NA>                NA          0.57
        MFSCalVoutMax MFSCalMFBlocked MFSCalMFMin MFSCalMFMax MFSCalPumpVBoostMin
      1         2.146              NA       2e-04       3.577                  NA
        MFSCalPumpVBoostMax MFSCalPDeadhead      MF4       MF3      MF2      MF1
      1                  NA              NA 0.228346 -0.384661 0.094702 1.251266
            MF0 UserTZ
      1 -0.5875  FALSE

---

    Code
      read_ast_header(upasv2x_rev117_file, update_names = FALSE)
    Output
        ASTSampler UPASserial UPASpcbRev UPASexpRev                   PMserial
      1  UPAS_v2_x   PSP00030          0          1 1F3F67618F0F615B_2.2_7_2.0
                                                                                       Firmware
      1 UPAS_v2_x-rev_00117-L476RE_20220511_mbedOS-6_15_1.bin compiled May 11 2022_21:25:01 UTC
        FirmwareRev LifetimeSampleCount LifetimeSampleRuntime
      1         117                 285                351.46
                                                                        LogFilename
      1 20220511/PSP00030_LOG_2022-05-11T23_24_01UTC_---------------_----------.txt
        SampleName CartridgeID GPSUTCOffset StartOnNextPowerUp ProgrammedStartTime
      1         NA          NA           -7              FALSE                   0
        ProgrammedRuntime SizeSelectiveInlet FlowRateSetpoint FlowOffset
      1              5.03              PM2.5                1          1
        FlowDutyCycle DutyCycleWindow GPSEnabled PMSensorInterval
      1            20              30       TRUE               17
                          PMSensorOperation RTGasSampleState CO2SampleState
      1 15s Warmup 5s Measurement 40s Sleep             TRUE           TRUE
        LogInterval PowerSaveMode AppLock AppVersion    StartDateTimeUTC   LocalTZ
      1          30         FALSE   FALSE     i1.0.1 2022-05-11 23:24:01 Etc/GMT+7
         StartDateTimeLocal      EndDateTimeUTC    EndDateTimeLocal
      1 2022-05-11 16:24:01 2022-05-11 23:31:17 2022-05-11 16:31:17
        FlowCheckMeterReadingPreSample FlowCheckMeterReadingPostSample
      1                             NA                              NA
        OverallDuration PumpingDuration OverallFlowRateAverage PumpingFlowRateAverage
      1            0.11           0.022                  0.198                  0.989
        SampledVolume StartBatteryCharge EndBatteryCharge StartBatteryVoltage
      1           1.3                 99               99                4.18
        EndBatteryVoltage ShutdownMode       ShutdownReason          CO2CalDate
      1              4.16            1 user pushbutton stop 2022-05-10 23:42:09
        CO2CalTarget CO2CalOffset          MFSCalDate MFSCalPerson MFSCalVoutBlocked
      1          420          230 2022-01-27 19:31:12         <NA>                NA
        MFSCalVoutMin MFSCalVoutMax MFSCalMFBlocked MFSCalMFMin MFSCalMFMax
      1          0.48        2.1515              NA    0.002244      3.5224
        MFSCalPumpVBoostMin MFSCalPumpVBoostMax MFSCalPDeadhead      MF4      MF3
      1                  NA                  NA              NA 0.119098 0.154664
             MF2      MF1       MF0 UserTZ
      1 -0.84873 1.899014 -0.737162  FALSE

---

    Code
      read_ast_header(upasv2x_rev110_diag_file, update_names = FALSE)
    Output
        ASTSampler UPASserial                   PMserial
      1  UPAS_v2_x   PSP00055 B7D1537B9ADF2439_2.3_7_2.0
                                                                                       Firmware
      1 UPAS_v2_x-rev_00110-L476RE_20220323_mbedOS-6_15_1.bin compiled Mar 23 2022_22:33:23 UTC
        FirmwareRev LifetimeSampleCount LifetimeSampleRuntime
      1         110                   2                  0.17
                                                                        LogFilename
      1 20220324/PSP00055_LOG_2022-03-24T18_05_32UTC_DIAGNOSTIC________________.txt
        SampleName CartridgeID GPSUTCOffset StartOnNextPowerUp ProgrammedStartTime
      1 DIAGNOSTIC          NA           -6              FALSE                   0
        ProgrammedRuntime SizeSelectiveInlet FlowRateSetpoint FlowOffset
      1              0.33               <NA>                0          0
        FlowDutyCycle DutyCycleWindow GPSEnabled PMSensorInterval
      1            50              30       TRUE                1
             PMSensorOperation RTGasSampleState LogInterval PowerSaveMode AppLock
      1 Continuous Measurement            FALSE           1         FALSE   FALSE
        AppVersion    StartDateTimeUTC   LocalTZ  StartDateTimeLocal
      1     i1.0.0 2022-03-24 18:05:32 Etc/GMT+6 2022-03-24 12:05:32
             EndDateTimeUTC    EndDateTimeLocal FlowCheckMeterReadingPreSample
      1 2022-03-24 18:08:32 2022-03-24 12:08:32                             NA
        FlowCheckMeterReadingPostSample OverallDuration PumpingDuration
      1                              NA           0.051           0.025
        OverallFlowRateAverage PumpingFlowRateAverage SampledVolume
      1                  0.003                  0.007          0.01
        StartBatteryCharge EndBatteryCharge StartBatteryVoltage EndBatteryVoltage
      1                 31               31                3.59              3.56
        ShutdownMode ShutdownReason          MFSCalDate MFSCalPerson
      1           94     RTOS crash 2022-03-22 17:10:24         <NA>
        MFSCalVoutBlocked MFSCalVoutMin MFSCalVoutMax MFSCalMFBlocked MFSCalMFMin
      1                NA          0.52       1.97675              NA    0.011037
        MFSCalMFMax MFSCalPumpVBoostMin MFSCalPumpVBoostMax MFSCalPDeadhead      MF4
      1      3.4043                  NA                  NA              NA 1.191545
              MF3      MF2       MF1      MF0 MFSDIAGVoutBlocked MFSDIAGVoutMax
      1 -4.507419 6.382635 -2.589087 0.178156             0.7455       1.872625
        MFSDIAGVoutMin MFSDIAGMFBlocked MFSDIAGMFMax MFSDIAGMFMin
      1       0.916625         0.295771     2.765242     0.537403
        MFSDIAGPumpVBoostMax MFSDIAGPumpVBoostMin MFSDIAGPDeadhead UserTZ
      1             28.45308             7.633833          1638.35  FALSE

---

    Code
      read_ast_header(upasv2x_rev157_file, update_names = FALSE)
    Output
        ASTSampler UPASserial UPASpcbRev UPASexpRev MotionID
      1  UPAS_v2_x   PSP00270          1       0BR2     0x6B
                          PMserial      CO2serial      Gasserial
      1 B1D3251112E134C8_2.3_7_2.0 0x63ea7f073b80 0x000003da2859
                                                                                       Firmware
      1 UPAS_v2_x-rev_00157-L476RE_20230830_mbedOS-6_15_1.bin compiled Aug 30 2023_21:55:00 UTC
        FirmwareRev LifetimeSampleCount LifetimeSampleRuntime LifetimeBatteryRuntime
      1         157                 330                573.15                   3.36
        LifetimeSamplePumptime LifetimePMSensorFanStartCount LifetimePMSensorFanHours
      1                2415.83                          1648                   450.53
        LifetimePMSensorPMMC LifetimeCO2SensorHours LifetimeVOCSensorHours
      1              5.2e-05                 572.48                 572.46
                                                                        LogFilename
      1 20240625/PSP00270_LOG_2024-06-25T21_37_48UTC_GPS-in-out______----------.txt
        SampleName CartridgeID GPSUTCOffset StartOnNextPowerUp ProgrammedStartTime
      1 GPS-in-out          NA           -6              FALSE                   0
        ProgrammedRuntime SizeSelectiveInlet FlowRateSetpoint FlowOffset
      1                NA              PM2.5                1          0
        FlowDutyCycle DutyCycleWindow GPSEnabled PMSensorInterval
      1           100              30       TRUE                1
             PMSensorOperation RTGasSampleState CO2SampleState LogInterval
      1 Continuous Measurement             TRUE           TRUE          30
        SamplerConfiguration ExternalPowerMode PowerSaveMode AppLock AppVersion
      1                    0             FALSE         FALSE   FALSE     i1.0.4
           StartDateTimeUTC   LocalTZ  StartDateTimeLocal      EndDateTimeUTC
      1 2024-06-25 21:37:48 Etc/GMT+6 2024-06-25 15:37:48 2024-06-25 22:21:13
           EndDateTimeLocal FlowCheckMeterReadingPreSample
      1 2024-06-25 16:21:13                             NA
        FlowCheckMeterReadingPostSample OverallDuration PumpingDuration
      1                              NA           0.714           0.714
        OverallFlowRateAverage PumpingFlowRateAverage SampledVolume PercentTimeWorn
      1                  0.994                  0.994         42.57            48.6
        StartBatteryCharge EndBatteryCharge StartBatteryVoltage EndBatteryVoltage
      1                 99               96                4.08              4.02
        ShutdownMode       ShutdownReason          CO2CalDate CO2CalTarget
      1            1 user pushbutton stop 2024-05-20 13:53:12          417
        CO2CalOffset          MFSCalDate MFSCalPerson MFSCalVoutBlocked MFSCalVoutMin
      1         -331 2023-01-17 19:29:04         <NA>                NA           0.5
        MFSCalVoutMax MFSCalMFBlocked MFSCalMFMin MFSCalMFMax MFSCalPumpVBoostMin
      1       1.94575              NA    0.006072      3.0542                  NA
        MFSCalPumpVBoostMax MFSCalPDeadhead      MF4      MF3      MF2      MF1
      1                  NA              NA 0.639249 -1.96428 2.275305 0.071015
             MF0 UserTZ
      1 -0.39268  FALSE

---

    Code
      read_ast_header(upasv2x_rev158_diag_file, update_names = FALSE)
    Output
        ASTSampler UPASserial UPASpcbRev UPASexpRev MotionID
      1  UPAS_v2_x   PSP00270          1       0BR2     0x6B
                          PMserial      CO2serial      Gasserial
      1 B1D3251112E134C8_2.3_7_2.0 0x63ea7f073b80 0x000003da2859
                                                                                       Firmware
      1 UPAS_v2_x-rev_00158-L476RE_20231201_mbedOS-6_15_1.bin compiled Mar 29 2024_13:50:34 UTC
        FirmwareRev LifetimeSampleCount LifetimeSampleRuntime LifetimeBatteryRuntime
      1         158                 313                571.32                   1.54
        LifetimeSamplePumptime LifetimePMSensorFanStartCount LifetimePMSensorFanHours
      1                2414.19                          1626                   448.85
        LifetimePMSensorPMMC LifetimeCO2SensorHours LifetimeVOCSensorHours
      1              4.8e-05                 570.65                 570.63
                                                                        LogFilename
      1 20240613/PSP00270_LOG_2024-06-13T16_24_47UTC_DIAGNOSTIC________________.txt
        SampleName CartridgeID GPSUTCOffset StartOnNextPowerUp ProgrammedStartTime
      1 DIAGNOSTIC          NA           -6              FALSE                   0
        ProgrammedRuntime SizeSelectiveInlet FlowRateSetpoint FlowOffset
      1              0.05              PM2.5                1          0
        FlowDutyCycle DutyCycleWindow GPSEnabled PMSensorInterval
      1           100              30       TRUE                1
             PMSensorOperation RTGasSampleState CO2SampleState LogInterval
      1 Continuous Measurement             TRUE           TRUE           1
        SamplerConfiguration ExternalPowerMode PowerSaveMode AppLock AppVersion
      1                    0             FALSE         FALSE   FALSE     i1.0.4
           StartDateTimeUTC   LocalTZ  StartDateTimeLocal      EndDateTimeUTC
      1 2024-06-13 16:24:47 Etc/GMT+6 2024-06-13 10:24:47 2024-06-13 16:27:26
           EndDateTimeLocal FlowCheckMeterReadingPreSample
      1 2024-06-13 10:27:26                             NA
        FlowCheckMeterReadingPostSample OverallDuration PumpingDuration
      1                              NA           0.037           0.037
        OverallFlowRateAverage PumpingFlowRateAverage SampledVolume PercentTimeWorn
      1                   0.29                   0.29          0.65               0
        StartBatteryCharge EndBatteryCharge StartBatteryVoltage EndBatteryVoltage
      1                 99               99                3.95              3.91
        ShutdownMode       ShutdownReason          CO2CalDate CO2CalTarget
      1            1 user pushbutton stop 2024-05-20 13:53:12          417
        CO2CalOffset          MFSCalDate MFSCalPerson MFSCalVoutBlocked MFSCalVoutMin
      1         -331 2023-01-17 19:29:04         <NA>                NA           0.5
        MFSCalVoutMax MFSCalMFBlocked MFSCalMFMin MFSCalMFMax MFSCalPumpVBoostMin
      1       1.94575              NA    0.006072      3.0542                  NA
        MFSCalPumpVBoostMax MFSCalPDeadhead      MF4      MF3      MF2      MF1
      1                  NA              NA 0.639249 -1.96428 2.275305 0.071015
             MF0 MFSDIAGVoutBlocked MFSDIAGVoutMax MFSDIAGVoutMin MFSDIAGMFBlocked
      1 -0.39268           2.011125          2.011       1.037125           3.0542
        MFSDIAGMFMax MFSDIAGMFMin MFSDIAGPumpVBoostMax MFSDIAGPumpVBoostMin
      1       3.0542     0.676676             27.93224             7.155759
        MFSDIAGPDeadhead UserTZ
      1           626.85  FALSE

# read_ast_header works with all HHBv2 firmwares

    Code
      read_ast_header(hhb_file)
    Output
        HHBserial     SEN55_Serial HHBslot1 HHBslot2 HHBslot3 HHBslot4 HHBslot5
      1  HHB00032 826B5FD0165CA542  SP00004  FP00003    Empty  FP00162    Empty
        HHBslot6 G.Alphasense1_ID           G.SCD30_Serial   G.SFA30_Serial
      1  GS00008       0202121325 557739-9558747-133234424 214551794F073BB7
                          Firmware                          LogFileName SampleName
      1 HHBv2_Jan_11_2024_08:35:16 HHB00032_LOG_2024-07-01T18_20UTC.csv   20240701
        D.SorbentCID C.SorbentCID A.FilterCID B.FilterCID    D.SorbentCalDate
      1       808678       808677         BJ0         BJ4 2024-06-27 15:37:00
        D.SorbentCalVoutMin D.SorbentCalVoutMax D.SorbentCalMFMin D.SorbentCalMFMax
      1               0.451             1.37469             0.014            35.842
        D.SorbentMF4 D.SorbentMF3 D.SorbentMF2 D.SorbentMF1 D.SorbentMF0
      1      2.74919      5.34928      -11.211      34.2365     -13.7709
           C.SorbentCalDate C.SorbentCalVoutMin C.SorbentCalVoutMax C.SorbentCalMFMin
      1 2024-06-27 15:43:00            0.445187             1.43144             0.005
        C.SorbentCalMFMax C.SorbentMF4 C.SorbentMF3 C.SorbentMF2 C.SorbentMF1
      1             47.28     -4.05113      33.6009      -46.687      55.8697
        C.SorbentMF0     A.FilterCalDate A.FilterCalVoutMin A.FilterCalVoutMax
      1     -18.4814 2024-06-27 15:49:00           0.585375            1.81038
        A.FilterCalMFMin A.FilterCalMFMax A.FilterMF4 A.FilterMF3 A.FilterMF2
      1           0.2842           3.1235    0.741438    -2.22835     2.49903
        A.FilterMF1 A.FilterMF0     B.FilterCalDate B.FilterCalVoutMin
      1    0.317727   -0.395998 2024-06-27 15:53:00             0.6065
        B.FilterCalVoutMax B.FilterCalMFMin B.FilterCalMFMax B.FilterMF4 B.FilterMF3
      1            1.85062           0.2911           3.1219     1.09846    -4.01808
        B.FilterMF2 B.FilterMF1 B.FilterMF0 UTCOffset ProgrammedRuntime SEN55_Runtime
      1     5.59904    -1.98744    0.191105        -6              1800         4.087
        SEN55_FanRuntime D.SorbentPumpStartingVolume D.SorbentPumpStartingRuntime
      1            4.087                     0.18975                        0.282
        D.SorbentCartridgeStartingVolume D.SorbentCartridgeStartingRuntime
      1                                0                                 0
        D.SorbentVolumetricFlowRate D.SorbentDutyCycle C.SorbentPumpStartingVolume
      1                       0.003                100                     0.16996
        C.SorbentPumpStartingRuntime C.SorbentCartridgeStartingVolume
      1                        0.442                                0
        C.SorbentCartridgeStartingRuntime C.SorbentVolumetricFlowRate
      1                                 0                       0.003
        C.SorbentDutyCycle A.FilterPumpStartingVolume A.FilterPumpStartingRuntime
      1                100                      13.72                       0.166
        A.FilterCartridgeStartingVolume A.FilterCartridgeStartingRuntime
      1                               0                                0
        A.FilterVolumetricFlowRate A.FilterDutyCycle B.FilterPumpStartingVolume
      1                          2               100                      16.98
        B.FilterPumpStartingRuntime B.FilterCartridgeStartingVolume
      1                       0.196                               0
        B.FilterCartridgeStartingRuntime B.FilterVolumetricFlowRate B.FilterDutyCycle
      1                                0                          2               100
        G.Alphasense1_Runtime G.FanRuntime G.SCD30_Runtime G.SFA30_Runtime
      1                 3.575        3.971           3.992           3.989
           StartDateTimeUTC      EndDateTimeUTC HHBSampledRuntime
      1 2024-07-01 18:20:00 2024-07-01 18:50:00               0.5
        D.SorbentShutdownMode D.SorbentSampledRunTime D.SorbentSampledVolume
      1                     1                   0.499                0.08792
        D.SorbentAverageVolumetricFlowRate C.SorbentShutdownMode
      1                            0.00293                     1
        C.SorbentSampledRunTime C.SorbentSampledVolume
      1                   0.499                0.09154
        C.SorbentAverageVolumetricFlowRate A.FilterShutdownMode
      1                            0.00305                    1
        A.FilterSampledRunTime A.FilterSampledVolume
      1                  0.499                 59.96
        A.FilterAverageVolumetricFlowRate B.FilterShutdownMode B.FilterSampledRunTime
      1                             1.999                    1                  0.499
        B.FilterSampledVolume B.FilterAverageVolumetricFlowRate
      1                 59.94                             1.998

