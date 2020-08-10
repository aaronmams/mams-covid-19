Overview
--------

This is project I set up mostly for teaching purposes to illustrate some
different data visualization techniques. The meaty part of this
repository is a "heatmap" of Covid-19 cases by county for the state of
California. This "heatmap" is created with `CA-Covid-19-countymap.Rmd`.

The data sources I use are detailed in the `Data` Section.

Data
----

Probably the most important thing to note about this project is the data
sources. Everything else is more-or-less just me playing with different
data visualizations. The primary data sources are as follows:

1.  [Eli Holmes CoV-19 R Package](https://github.com/eeholmes/CoV19):
    Eli has compiled and curated the data coming out of Johns Hopkins.

2.  [The NY Times GitHub
    Repo](https://github.com/nytimes/covid-19-data). This repository has
    .csv files for US counties and US states.

3.  \[California County Boundary Shapefiles\]
    (<https://catalog.data.gov/dataset/tiger-line-shapefile-2016-state-california-current-county-subdivision-state-based>)

4.  County-level population estimates for Oregon and California. I
    basically just googled "Oregon county population" and "California
    county population" and grabbed the 1st .csv file I found.
