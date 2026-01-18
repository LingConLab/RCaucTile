# RCaucTile

[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![CRAN version](https://www.r-pkg.org/badges/version/RCaucTile)](https://cran.r-project.org/package=RCaucTile)

`RCaucTile` provides an R implementation of the East Caucasian Languages template, designed for generating tile maps for East Caucasian Languages in `ggplot2` (see [`PyCaucTile`](https://github.com/LingConLab/PyCaucTile/) for the Python version). It draws inspiration from the Typological Atlas of the Languages of Daghestan ([TALD](https://lingconlab.ru/tald/)). The TALD project initially features three distinct map visualizations. This package extends this by introducing a new tile map visualization, which displays a rectangle for each language and allows for color-coding based on specific linguistic features. The detailed tutorial is available [here](https://lingconlab.github.io/RCaucTile/).

Here are several examples of the result tile maps.

![](docs/index_files/figure-html/unnamed-chunk-1-1.png)

![](docs/index_files/figure-html/unnamed-chunk-16-1.png)

![](docs/index_files/figure-html/unnamed-chunk-13-3.png)

I would like to thank Yura Koryakov, Kostya Filatov, Ira Politova and Vlada Termus for the help with preparing language template.

To cite RCaucTile in publications, please use

```
Moroz, George (2025). RCaucTile: Tile Grid Maps for East Caucasian Languages. R package. doi:10.32614/CRAN.package.RCaucTile

A BibTeX entry for LaTeX users is

@Manual{moroz2025RCaucTile,
  title = {{RCaucTile}: {T}ile {G}rid {M}aps for {E}ast {C}aucasian {L}anguages},
  author = {George Moroz},
  doi = {10.32614/CRAN.package.RCaucTile},
  url = {https://CRAN.R-project.org/package=RCaucTile},
  year = {2025}
}
```
