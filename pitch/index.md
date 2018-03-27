---
title       : "The Shiny Rio Triathletes app"
subtitle    : "27 March 2018"
author      : "wlvm"
date        : 27-03-2018
widgets     : [bootstrap, mathjax, interactive, plotly]
ext_widgets : {rCharts: [libraries/nvd3, libraries/leaflet, libraries/dygraphs]}
mode        : selfcontained # {standalone, draft}
logo        : Rio.png
#logo     : OlympicRings.png
knit        : slidify::knit2slides
---




## About

- Learn more about the *2016 Olympic triathletes* with this app [here](https://wlvm.shinyapps.io/shinyRio/) and on [github](https://github.com/wlvm/ShinyRio)
- Have fun randomly rendering their home countries on a map
- Check if your *Body Mass Index* (BMI) is like that of a swim-bike-runner!

<img src="https://stillmed.olympic.org/media/Images/OlympicOrg/News/2016/09/07/2016-09-07-triathlon-inside-02.jpg" 
  width=45% alt="The Brownlee brothers" class="center">

Upfront credit goes to: [Rio Olympics website](https://www.rio2016.com/) for data, logos and photo of the Brownlee brothers. 
Photo credits go to New York Times for title photo of my hero [Gwen Jorgenson](http://www.gwenjorgensen.com/).


---


## Triathlete country distribution (with plotly)
![plot of chunk unnamed-chunk-1](assets/fig/unnamed-chunk-1-1.png)

---

## Distribution of triathletes (Age and BMI)
![plot of chunk unnamed-chunk-2](assets/fig/unnamed-chunk-2-1.png)


--- 


## Scatterplot of normal person vs super triathlete

![plot of chunk unnamed-chunk-3](assets/fig/unnamed-chunk-3-1.png)




