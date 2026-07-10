# ST558_Project2

# NBA Play-by-Play Data Exploration & Interactive Analysis App

## Application Overview & Objective
This interactive R Shiny application provides a comprehensive analytical dashboard for exploring, subsetting, 
and visualizing the **2019-20 NBA Play-by-Play dataset**. 

Designed for statistical exploration and quantitative reporting, the app enables users to filter tens of thousands 
of play-by-play events across regular season and play off games, perform dynamic categorical and numerical statistical summaries,
and interactive plots. 

---
## Core Features & Architecture

### 1. Sidebar Layout & Subsetting Control
- **Multi-Dimensional Categorical Filtering**: Filter by **GameType**, **ShotOutcome**, **ShotType**, **Quarter**, and **FoulType**, complete 
with instant **Select All** shortcuts.
- **Dynamic Numerical Range Slider**: Dual-ended sliders(`renderUI`) adapting automatically to min/max values of chosen numeric columns.
- **Controlled Reactivity**: Strictly triggered via an **Apply Filters** `actionButton()` using `reactiveValues`. 

### 2. Main Panel Tabs

#### Tab 1: About
- Project overview, user guide, and source data documentation links. 

#### Tab 2: Data Download 
- Presents filtered data using paginated `DT::dataTableOutput` and includes a dedicated `csv` export handler(`downloadButton`).

#### Tab 3: Data Exploration 
1. **Categorcial Summaries**: Generates one-way & two-way contingency tables
2. **Numerical Summaries**: Grouped descriptions stats(`n`, `Mean`, `Median`, `SD`,`Min`, `Max`)
3. **Interactive Visualization**: Features **7 Plotly charts** arranged in spacious full width stacked layout
  - **Plot 1**: Univariate Bar Chart(`ShotType` counts)
  - **Plot 2**: Univariate Density Plot(`Shot Distance`)
  - **Plot 3**: Multivariate Side-by-Side Bar Chart(`Shot Outcome` by `Game Type`)
  - **Plot 4**: Multivariate Box Plot ( `Shot Distance` by `Shot Outcome`)
  - **Plot 5**: Multivariate & Faceted Histogram (`Shot Distance` facted by `Quarter`)
  - **Plot 6**: Multivariate Scatter Plot (`Shot Distance` vs. `Seconds Left`, colored by `ShotOutcome`)
  - **Plot 7**: Non-parametric HDR Plot
  
### Error handling 
- Equipped with `shinycssloaders` loading spinners, informative `validate()` error prompts, and automated sampling optimization for instant rendering. 

### Running the App
- Open `app.R` in RStudio and run 