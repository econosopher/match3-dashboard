# Match-3 Game Balancing Dashboard

This R Shiny dashboard provides a suite of tools for mobile game designers and analysts to balance the progression and economy of a match-3 game. By uploading level-specific data, users can visualize key performance metrics, analyze difficulty and economy, and identify recurring patterns in level design.

## Features

- **CSV Data Upload**: Upload your level data in CSV format directly from the sidebar.
- **Level Range Filter**: Filter the analysis to a specific range of levels using the sidebar slider.
- **Modular Analysis Tabs**: Each major analysis area (Difficulty, Core Gameplay, Economy, Sequence) is a separate, maintainable module.
- **Interactive Metrics Table**: Explore a sortable, filterable table of level metrics with color-coded highlights.
- **Sequence Analysis**: Visualize win and churn rates by level sequence groupings.
- **Modern UI/UX**: Sidebar navigation, Inter font, high-contrast color palettes, and responsive layout.

## Project Structure

- `app.R`: Main app entry point, sources modules and helpers, and wires up global controls in the sidebar.
- `modules/`: Shiny modules for each major dashboard tab (e.g., `difficulty_analysis.R`, `sequence_analysis.R`).
- `utils/`: Utility scripts for plotting, data wrangling, and configuration (e.g., `plotting_helpers.R`, `config.R`).
- `www/`: Static assets (custom CSS, fonts, etc.).
- `examples/`: Sample data files.

## Extending the App

To add a new feature or tab:
1. Create a new module in `modules/` (see `difficulty_analysis.R` for a template).
2. Add any new plotting or data helpers to `utils/`.
3. Source your module in `app.R` and add its UI/server calls.

## Contributing

- Fork the repository and create a feature branch for your changes.
- Keep each module focused on a single analysis area.
- Use helpers from `utils/` for shared logic.
- Store global constants and color palettes in `utils/config.R`.
- Test modules independently before integrating.
- Submit a pull request with a clear description of your changes.

## Setup & Installation

To run this dashboard, you need to have R installed on your system. RStudio is a recommended IDE for a smoother experience.

1. **Clone the repository** (if from GitHub):
    ```sh
    git clone https://github.com/econosopher/match3-dashboard.git
    cd match3-dashboard
    ```
2. **Open R/RStudio**: Launch an R session in the project directory.
3. **Run the App**: Execute the following command in the R console:
    ```R
    shiny::runApp()
    ```
   The application uses the `pacman` package to automatically install and load all required dependencies. The first time you run it, it may take a moment to install the necessary packages.

## How to Use

1. **Upload Data**: In the sidebar, use the "Upload Level Data CSV" button to select your data file.
2. **Filter Levels**: Use the "Filter Level Range" slider in the sidebar to focus the analysis on specific levels.
3. **Explore Tabs**:
    - **Level Metrics Table**: View and sort the original uploaded data and key metrics.
    - **Difficulty Analysis**: Analyze win rates, churn, and attempts by difficulty.
    - **Core Gameplay**: Explore attempts per success, win streaks, and churn trends.
    - **Economy Analysis**: Visualize extra moves, near-win/loss rates, and gold inventory.
    - **Sequence Analysis**: See win and churn rates by level sequence groupings (the tab now shows only these two charts for clarity).

### Required CSV Format

The application expects a CSV file with the following headers (see `examples/sample_level_data.csv` for a template):

```
level_number, ... (see app for full list of required columns)
```

If any required columns are missing, a warning will appear in the dashboard.

---

## License

MIT License. See `LICENSE` file for details.

## Issues & Feedback

If you encounter bugs or have feature requests, please open an issue or submit a pull request on GitHub. 