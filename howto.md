---
output:
  word_document: default
  html_document: default
---

# skyscapeR How-To Guide
v0.3 (3 I 2017) -- beta build

**skyscapeR** is an online app focused on the visualization and analysis of archaeoastronomical data. It is built in [_R_](https://www.r-project.org) using [_RStudio_](https://www.rstudio.com) and [_Shiny_](https://shiny.rstudio.com). It also uses the _R_ packages [_astrolibR_](https://cran.r-project.org/web/packages/astrolibR/index.html) and [_palinsol_](https://cran.r-project.org/web/packages/palinsol/index.html) for its astronomical calculations.

This is still under construction: help pages, more information and more modules will be added later.

## Where to find it?
The app is accesible by clicking [_here_](https://app.skyscaper.net). Soon this will be password-protected so that only MA CAA tutors, student and alumni can access it, during its beta testing phase. 

The AWS server is currently running on its free tier mode. This means that computations will be slower than usual. Eventually this will be upgraded, but as it incurs extra costs this hasn't been done yet.

## What is it?
In its present incarnation the app completely replaces the MA CAA Archaeoastronomy Calculations spreadsheet. All the calculations previously available are present in skyscapeR, which also provides new features and will continue to be updated with more advanced features thorughout 2017.

At present the following features (called _modules_) are available:

* **Data Entry**: this allows users to either choose a published dataset, which is useful to learn how to use the app, or upload their own field data. Field data should be input into the provided template spreadsheet. When this is done, the system automatically calculates declinations.
* **Visualization**: once a dataset has been selected or uploaded (see above), this module allows for visualization of the data. Three options are available at the present: data points, where each declination is represented by a point, with an uncertainty/error bar attached; histogram, where bars shows the frequency of each range of declinations and bin size and start can be changed; and curvugram/SPD, where each measurement's probability distribution is summed and the total displayed, uncertainties can be changed directly. Furthermore, bars representing the declinations of key solar, lunar and stellar events can be added to the graphs, and the epoch they relate to edited (where negative years correspond to BCE).
* **Horizon**: this allows the user to download a previously created HeyWhatThat panorama into _skyscapeR_ by inputting the HeyWhatsThatID of the panorama. The user is then given the choice to download it in a format that can be imported into Stellarium, or continue to play with it by visualizing the orbits of key solar, lunar and stellar events for any epoch (just as above).
* **Tools :: Unit Conversion**: a small app to convert degrees from their decimal point form (e.g. 23.4555º) to deg-min-sec format (e.g. 23º 27' 19.8").
* **Tools :: Coordinate Conversion**: a small app to convert horizontal (azimuth and altitude) to equatorial (declination and right ascension) coordinates and vice-versa.
* **Tools :: Horizon Altitude**: a small app to calculate the horizon altitude (in degrees) from two altitude measurements and the distance.
* **Tools :: Magnetic Declination**: a small app to estimate the magnetic declination at a site using the radial GPS technique of Silva (2010).
* **Tools :: Solar Declination Table**: a small app to produce and download a table of daily solar declination (at noon) for a given epoch.

Any figures produced by the app can be downloaded by right-clicking on them, just like any other image on the web. Furthermore, if the user left-clicks on the figures a set of number of the bottom-left of the figure will display the coordinates of the place where the image was clicked, which is handy to find out, for example, the declination of curvigram peaks and or the azimuth and altitude of horizon features.

## Where is it going next?
There's a long road ahead for this app. The aim is to keep it in closed beta for at least the next 6 months, to receive feedback from MA CAA alumni and current dissertation and sksycape students. In the meantime the interface will be simplified according to user feedback and more features will be added.


Future versions will be developed according to the following tentative roadmap:

* v0.4 will include the option to create a HeyWhatsThat panorama from with the **Horizon** module;
* v0.5 will include tools and visualization options relating to uncertainty (both in measurement and dating);
* v0.6 will include null hypothesis testing (useful for arguing for intentionality);
* v0.7 will include a statistical inference module (including inferential uncertainty);
* v0.8 will include a customization menu to allow the user to choose plotting colours, etc, but also astronomical equations and factors used;
* v1.0 will be the first official, fully open release (estimated end of 2017).