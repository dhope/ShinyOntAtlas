# Ontario Breeding Bird Atlas Shiny - CWS Ontario  

Code is essentially a knockoff of this Shiny example: [https://github.com/dhope/Shiny_Map](https://github.com/dhope/Shiny_Map). Thanks to those authors for sharing their code.

To run the app, enter the following code into your R console:

`shiny::runGitHub("dhope/ShinyOntAtlas")`.

You will need to have at a minimum the following packages:

`req_pkgs <- c("here", "shiny", "dplyr", "ggplot2")`

`missing <- req_pkgs[!req_pkgs %in% installed.packages() ]`

`install.packages(missing)`

Details about the folders:

File | Description
---|----------------------------------------------------------
R | Contains functions and files that are sourced
data | contains data from other projects or sources. Data locations are described in the README of this folder

