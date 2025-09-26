# Visualization System Documentation

This directory contains the visualization system for the Endo-Review project, designed to create consistent, publication-ready plots from research data.

## 🏗️ Architecture Overview

```
scripts/04_analysis/visualization/
├── shared_utils.R              # Common utilities and functions
├── visualize_taxa_results.R    # Research-focused taxonomic visualizations
├── visualize_extraction_results.R  # Data quality and methodology visualizations
├── temporal_trend_analysis.R   # Time-based analysis plots
├── run_taxa_visualizations.R   # Entry point for research visualizations
├── config.R                    # Shared configuration (future)
└── README.md                   # This documentation
```

## 📊 Visualization Categories

### 1. **Research Visualizations** (`visualize_taxa_results.R`)
- **Purpose**: Publication-ready plots for research findings
- **Output**: 24 phylum-based plots + 3 geographic visualizations
- **Features**: Main vs supplementary analysis modes
- **Users**: Researchers preparing manuscripts

### 2. **Quality Assessment** (`visualize_extraction_results.R`)
- **Purpose**: Pipeline evaluation and data quality metrics
- **Output**: 19+ data extraction quality visualizations
- **Features**: Comprehensive methodology assessment
- **Users**: Pipeline developers and methodologists

## 🛠️ Shared Utilities

### Core Functions (`shared_utils.R`)

#### Data Loading
```r
# Load data consistently across scripts
data <- load_comprehensive_data()
```

#### Plot Saving
```r
# Save plots with organized directory structure
save_plot_organized(my_plot, "my_plot.png", category = "main")
```

#### Filtering
```r
# Apply mycorrhizal filtering consistently
main_data <- filter_mycorrhizal_papers(data, include_mycorrhizal_only = FALSE)
supp_data <- filter_mycorrhizal_papers(data, include_mycorrhizal_only = TRUE)
```

#### Theming
```r
# Apply consistent theme to plots
my_plot + get_visualization_theme()
```

#### Data Summary
```r
# Create standardized data summaries
summary <- create_data_summary(data)
```

## 📁 Output Organization

```
plots/
├── main/                      # Main analysis (excluding mycorrhizal-only)
│   ├── plantae_*.png         # Plant taxonomic representations
│   └── fungi_*.png           # Fungal taxonomic representations
├── supplementary/             # Including mycorrhizal-only papers
│   ├── plantae_*.png         # Plant taxonomic representations
│   └── fungi_*.png           # Fungal taxonomic representations
├── geographic/               # Geographic and research patterns
│   ├── geographic_*.png      # Geographic distribution plots
│   └── publication_*.png     # Research intensity patterns
├── extraction/               # Data quality and methodology
│   ├── species_*.png         # Species detection analysis
│   ├── methods_*.png         # Research methods analysis
│   └── quality_*.png         # Prediction quality metrics
└── README.md                 # Detailed organization guide
```

## 🚀 Usage Examples

### Basic Workflow

```r
# 1. Source shared utilities
source("scripts/04_analysis/visualization/shared_utils.R")

# 2. Load and prepare data
data <- load_comprehensive_data()
summary <- create_data_summary(data)

# 3. Apply filtering for analysis modes
main_data <- filter_mycorrhizal_papers(data, include_mycorrhizal_only = FALSE)
supp_data <- filter_mycorrhizal_papers(data, include_mycorrhizal_only = TRUE)

# 4. Create visualizations
main_plot <- ggplot(main_data, aes(x = category, y = value)) +
  geom_bar() +
  get_visualization_theme()

# 5. Save with proper organization
save_plot_organized(main_plot, "main_analysis.png", category = "main")
```

### For Research Publications

```r
# Generate main manuscript figures
source("scripts/04_analysis/visualization/visualize_taxa_results.R")
# Creates: plots/main/*.png for primary manuscript

# Generate supplementary figures
# (Same script automatically creates plots/supplementary/*.png)
```

### For Methodology Assessment

```r
# Generate quality assessment plots
source("scripts/04_analysis/visualization/visualize_extraction_results.R")
# Creates: plots/extraction/*.png for methods evaluation
```

## 🎨 Styling and Themes

### Color Palettes
- **Primary**: Colorblind-friendly palette from `plot_utils.R`
- **Semantic**: Consistent colors for presence/absence, found/not found
- **Categorical**: Standardized colors for kingdoms and taxonomic levels

### Themes
- **Base Theme**: `endo_theme()` from `plot_utils.R`
- **Enhanced Theme**: `get_visualization_theme()` from `shared_utils.R`
- **Consistent**: All plots use the same styling parameters

## 🔧 Configuration

### Directory Structure
```r
PLOT_DIRECTORIES <- list(
  main = "plots/main/",
  supplementary = "plots/supplementary/",
  geographic = "plots/geographic/",
  extraction = "plots/extraction/"
)
```

### File Naming Convention
- **Main**: `{kingdom}_{level}_representation_main_by_phylum_{type}.png`
- **Supplementary**: `{kingdom}_{level}_representation_supplementary_by_phylum_{type}.png`
- **Geographic**: `geographic_{analysis_type}.png`
- **Extraction**: `{focus_area}_{analysis_type}.png`

## 🧪 Testing

Each visualization script should be tested independently:

```r
# Test taxa visualizations
source("scripts/04_analysis/tests/test_plot_taxa_visualizations.R")

# Test extraction visualizations
source("scripts/04_analysis/tests/test_plot_extraction_visualizations.R")
```

## 🔄 Development Workflow

### Adding New Visualizations

1. **Determine Category**: Choose appropriate subfolder (main/supplementary/geographic/extraction)
2. **Use Shared Utilities**: Source `shared_utils.R` for common functions
3. **Follow Naming**: Use established naming conventions
4. **Apply Themes**: Use `get_visualization_theme()` for consistency
5. **Test**: Verify output in correct directory

### Modifying Existing Visualizations

1. **Update Scripts**: Modify in respective script files
2. **Test Changes**: Run script and verify output location
3. **Update Documentation**: Modify this README if interfaces change
4. **Validate**: Ensure consistent styling and organization

## 📈 Performance Considerations

- **Large Datasets**: Both scripts handle large datasets efficiently
- **Memory Usage**: Scripts are designed to process data in chunks when needed
- **Plot Generation**: Vector-based output (PNG) for high-quality publications
- **Caching**: Consider caching intermediate results for repeated runs

## 🤝 Contributing

When adding new visualization functionality:

1. **Evaluate Placement**: Determine if new script needed or existing script appropriate
2. **Use Shared Utils**: Leverage `shared_utils.R` for common operations
3. **Follow Patterns**: Maintain consistent code structure and naming
4. **Document**: Update this README with new functionality
5. **Test**: Ensure new visualizations integrate with existing system

## 📞 Support

For questions about:
- **Research plots**: See `visualize_taxa_results.R`
- **Quality metrics**: See `visualize_extraction_results.R`
- **Shared utilities**: See `shared_utils.R`
- **Architecture**: See this README

---

*This visualization system provides a robust, scalable foundation for creating publication-ready plots while maintaining consistency and reducing code duplication.*