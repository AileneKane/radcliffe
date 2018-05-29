Data downloaded August 25, 2017 from http://harvardforest.fas.harvard.edu:8080/exist/apps/datasets/showData.html?id=hf004
hf004-02: filled data

    datetime: date and time
    year: year
    month: month
    doy: day of year; January 1 = 1 (unit: nominalDay)
    hour: hour of day in EST; (EST=GMT-5) (unit: nominalHour)
    seq.day.90: sequential day starting in 1990, 1 = Jan. 1 1990 (unit: nominalDay)
    seq.time.90: sequential time in fractional days; 1.5 is 12 noon January 1 1990 (unit: nominalDay)
    obs.nee: observerd NEE (FCO2 + storage) in micromoles CO2/m2/s (unit: micromolePerMeterSquaredPerSecond / missing value: NA)
    obs.fco2: observed FCO2 (CO2 eddy covariance flux) in micromoleCO2/m2/s (unit: micromolePerMeterSquaredPerSecond / missing value: NA)
    fco2.corr: observed FCO2 + storage correction (unit: micromolePerMeterSquaredPerSecond / missing value: NA)
    ustar: observed u* (u* = sqrt(-1 * <u'w'>) in cm/s (unit: centimetersPerSecond / missing value: NA)
    nee: net ecosystem exchange filled by model in micromole CO2 /m2/s (unit: micromolePerMeterSquaredPerSecond / missing value: NA)
    resp.e: ecosystem respiration filled and derived (see below) e-6mol/m2/s (unit: micromolePerMeterSquaredPerSecond / missing value: NA)
    gee: gross ecosystem exchange derived and filled (unit: micromolePerMeterSquaredPerSecond / missing value: NA)
    c.flag1: 1 when NEE is filled (nee.obs is NA)
        1: NEE is filled
    c.flag2: 1 when estimated NEE = observed FCO2 + ‘storage adjustment’
        1: estimated NEE = observed FCO2 + ‘storage adjustment’
    obs.ta.27m: observed air temperature at 27 m (top of tower) (unit: celsius / missing value: NA)
    ta.27m.filled: filled air temperature with missing points replaced by available data (unit: celsius / missing value: NA)
    ta1.flag1: 1when Tair interpolated from adjacent profile heights
        1: Tair interpolated from adjacent profile heights
    ta1.flag2: 1when Tair filled from sonic temperature
        1: Tair filled from sonic temperature
    ta1.flag3: 1when Tair filled from Fisher Meterological Station data
        1: Tair filled from Fisher Meterological Station data
    ta1.flag4: 1when Tair from daily mean at Shaler Met Station and mean 24-hour cycle
        1: Tair from daily mean at Shaler Met Station and mean 24-hour cycle
    ta.2.5m: observed air temperature at 2.5m above ground (unit: celsius / missing value: NA)
    ta.2.5m.filled: filled air temperature at 2.5 m above ground (unit: celsius / missing value: NA)
    ta5.flag1: 1when Tair interpolated from adjacent profile heights
        1: Tair interpolated from adjacent profile heights
    ta5.flag2: 1when Tair filled from sonic temperature
        1: Tair filled from sonic temperature
    ta5.flag3: 1when Tair filled from Fisher Meterological Station data
        1: Tair filled from Fisher Meterological Station data
    ta5.flag4: 1when Tair from daily mean at Shaler Met Station and mean 24-hour cycle
        1: Tair from daily mean at Shaler Met Station and mean 24-hour cycle
    par.28m: observed photosynthetically active radiation (PPFD) at 28m (above the canopy) (unit: micromolePerMeterSquaredPerSecond / missing value: NA)
    par.28m.filled: filled photosynthetically active radiation (PPFD) (unit: micromolePerMeterSquaredPerSecond / missing value: NA)
    p.flag1: 1when missing nighttime points set to 0
        1: missing nighttime points set to 0
    p.flag2: 1when filled from SUNY-ASRC data
        1: filled from SUNY-ASRC data
    p.flag3: 1when estimated from global radiation at Fisher Met Station
        1: estimated from global radiation at Fisher Met Station
    p.flag4: 1when filled with hourly mean in a 30 day window
        1: filled with hourly mean in a 30 day window
    par.tot.ue.ms.2: total PAR at 29 meters (unit: microeinsteinPerMeterSquaredPerSecond / missing value: NA)
    par.dfs.ue.ms.2: diffuse component of PAR at 29 meters (unit: microeinsteinPerMeterSquaredPerSecond / missing value: NA)


hf006-01: soil respiration

    datetime: date and time
    date: date
    jd: Julian date (unit: nominalDay)
    doy: sequential day of year over multiple years (unit: nominalDay / missing value: NA)
    flux: CO2 flux (unit: milligramPerMeterSquaredPerHour / missing value: NA)
    se: standard error of CO2 flux (unit: milligramPerMeterSquaredPerHour / missing value: NA)
    soilt: soil temperature at 10cm depth (unit: celsius / missing value: NA)
    vsm: volumetric soil moisture over the top 15cm of soil (unit: gramsPerGram / missing value: NA)
    time: time of sampling
    site: site name
    drainage: drainage class


