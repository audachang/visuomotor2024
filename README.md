# Visuomotor Rotation Data Analysis

## Description

This repository contains an R script for analyzing visuomotor rotation experimental data. It leverages R and Python for data processing, statistical analysis, and visualization. The script handles data importation, preprocessing, intersection computations, and graphical representation using `ggplot2`.

## Prerequisites

Ensure R is installed with the following packages:
- `rio`
- `reticulate`
- `dplyr`
- `ggplot2`
- `tidyr`
- `REdaS`

Python must be installed and set up to interface with R via the `reticulate` package.

## Installation

1. Clone the repository or download its contents to your machine.
2. Install the required R packages with the command: 
   ```R
   install.packages(c("rio", "reticulate", "dplyr", "ggplot2", "tidyr", "REdaS"))
   ```
3. Ensure Python and any dependencies for `compute_intersect.py` are installed.

## Usage

1. Place the `ana_path.R` script in the `vrot_scripts` directory.
2. Update the `droot1` and `droot2` variables in the script to point to the respective data directories, typically found within `vrot_first` and `vrot_second`. For example:
   ```R
   droot1 <- '../vrot_first/data'
   droot2 <- '../vrot_second/data'
   ```
   Confirm that the `data` subdirectories exist and contain your datasets.
3. Execute the script within your R environment, modifying any paths or parameters to align with your specific dataset structures and analysis objectives.

## Contributing

Your input is valuable. For enhancements or fixes, please submit an issue or a pull request.

## License

This project is licensed under the [MIT License](https://opensource.org/licenses/MIT).

## Contact

For support, please file an issue on the GitHub repository.
