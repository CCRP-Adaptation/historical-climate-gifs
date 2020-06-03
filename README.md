# historical-climate-gifs
Parsing PRISM data and looping through all parks to create gifs visualizing historical climate.
Currently in two scripts so modifications to plots can be made (incl. updates with current data) without requiring reparsing.

1. Parsing script
For all parks, loops through PRISM data and saves .csv file with annual ppt and tmean.
*currently parses through 800m monthly data, modify to use Annie's script to get 4k from web

2. Plotting script
Loops through park PRISM files and creates two gifs illustrating historical climate trends.
*currently only full figure, need to add simplified figure
