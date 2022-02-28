# Map matching in R

## Overview
This repository contains the source files for the talk that I gave on February 24, 2022 at the Graphics Group in the Department of Statistics at Iowa State University.

## Abstract
Map matching is an interesting problem that involves snapping noisy GPS traces to a road network with a high degree of accuracy. It is used by ride-sharing services such as Uber and Lyft, transportation researchers, highway agencies, auto insurance companies, and many others for gaining insights into driver behavior and travel patterns as well as improving operational efficiency. There are various commercial and open source map matching solutions available for use such as Google's Snap to Roads API, Mapbox's Map Matching API, and QGIS's map matching plugin; however, these options are either very expensive (e.g., costs ~$4k to match 500k GPS points using Google's API) or are very feature-rich (e.g., QGIS) with many more features than we typically need. The fast map matching (FMM) is a lightweight standalone tool for map matching that addresses these issues and is blazing fast. It also has excellent support for large spatial datasets. In this talk, I will discuss the internal specifics of my forthcoming "mapmatchr" R package that provides an R wrapper for the FMM tool.

## License
The overall presentation is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International license (CC BY-SA 4.0)](https://creativecommons.org/licenses/by-sa/4.0/), and the source files are licensed under the [MIT license](https://ashirwad-barnwal.mit-license.org/).
