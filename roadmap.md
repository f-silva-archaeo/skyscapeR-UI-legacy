---
output:
  word_document: default
  html_document: default
---

# skyscapeR Roadmap (14 IV 2017)

_Recently implemented:_

* BUG: horizon orbit of zenith passing star for epoch range fixed
* BUG: horizon orbit of circumpolar for epoch range fixed
* added georeferencing to Stellarium panorama download
* change Epoch to a range and display appropriately
* plot data-points (with error margins) in Horizon module
* added tooltip on Display Celestial Features Epoch stating BC as negative
* load Horizons from data entered
* download dataset after dec calc
* plot .Unc as per dataset inmported
* add an input to show any given dec on Visualization and Horizon module
* calc dec uncertainty from az.unc and alt.unc
* plot individual datapoint uncertainty when available

_Current priorities:_

* standardize variable and object names
* download options (celestial objects, etc) as .ini file ??
* add EFM data

_Next:_

* implement conditional Tab for data viz
* add HWT copyright disclaimer
* create HeyWhatsThat panorama from within the app
* create multiple Horizon modules with dynamic tabs
* add back-end of import horizon facility
* right-clicking on plot provides more info, when data-points visible
* new Mapping module that plots data in Google Maps

**--- v0.5 release**

* add tools for null hypothesis testing

**--- v0.6 release**

* add Inference module, including inferantial uncertainty

**--- v0.7 release**

* add a customization menu to allow the user to choose plotting colours, etc, but also astronomical equations and factors used

**--- v0.8 release**