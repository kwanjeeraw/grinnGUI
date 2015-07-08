# grinnGUI
Graphical user interface for grinn R package

Version: 1.0 (08 July 2015)

Description
=========
grinnGUI is the graphical user interface for [grinn](https://github.com/kwanjeeraw/grinn) R package.
The user interface is implemented using [Shiny](http://shiny.rstudio.com/) and runs on a web browser (Safari, Chrome, Firefox and etc.).

Install and Run
=========
* Require [shiny](http://shiny.rstudio.com/) and [grinn](https://github.com/kwanjeeraw/grinn). 
* Download grinnGUI .zip file and unzip 
* Run the following code

```
library(shiny)
runApp("[path to grinnGUI-master]/grinnGUI-master")
```

Documentation
=========
list of support functions:
* convertToGrinnID
* fetchCorrGrinnNetwork
* fetchCorrNetwork
* fetchDiffCorrGrinnNetwork
* fetchDiffCorrNetwork
* fetchGrinnCorrNetwork
* fetchGrinnDiffCorrNetwork
* fetchGrinnNetwork

see grinn [homepage](http://kwanjeeraw.github.io/grinn/) for the function information.

Updates
=========
#### version 1.0 (08/07/15)
* Initial version

License
=========
[GNU General Public License (v3)](https://github.com/kwanjeeraw/grinnGUI/blob/master/LICENSE)

