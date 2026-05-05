#!/usr/bin/env python3
"""
Plot sensitivity analysis: understudied country enrichment in biodiversity priority regions.

Shows what % of understudied endophyte countries rank at different priority thresholds.
"""
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from pathlib import Path

# Setup
ROOT = Path('.').resolve()
INPUT_SENSITIVITY = ROOT / 'results' / 'biodiversity_priority_overlap' / 'sensitivity_analysis.csv'
OUTPUT_PLOT = ROOT / 'results' / 'biodiversity_priority_overlap' / 'priority_overlap_sensitivity.png'
OUTPUT_PLOT.parent.mkdir(parents=True, exist_ok=True)

# Load data
sensitivity = pd.read_csv(INPUT_SENSITIVITY)

# Convert quantiles to percentile labels (top X%)
sensitivity['priority_label'] = (100 * (1 - sensitivity['quantile'])).round(0).astype(int).astype(str) + '%'

# Sort by priority_label for logical order (top 10% to top 90%)
label_order = ['10%', '25%', '50%', '75%']
sensitivity['priority_label_cat'] = pd.Categorical(sensitivity['priority_label'], categories=label_order, ordered=True)
sensitivity = sensitivity.sort_values('priority_label_cat')

# Create figure
fig, ax = plt.subplots(figsize=(10, 6))

# Main plot: % understudied overlapping at each threshold
colors = ['#2ecc71', '#f39c12', '#e74c3c', '#c0392b']  # Green to red gradient
bars = ax.bar(range(len(sensitivity)), sensitivity['pct_understudied_overlapping'], 
               color=colors, alpha=0.8, edgecolor='black', linewidth=1.5)

# Add value labels on bars
for i, (bar, val) in enumerate(zip(bars, sensitivity['pct_understudied_overlapping'])):
    ax.text(bar.get_x() + bar.get_width()/2, bar.get_height() + 1.5, 
            f'{val:.1f}%', ha='center', va='bottom', fontsize=11, fontweight='bold')

# Styling
ax.set_xlabel('Priority Level (World Bank Biodiversity Metrics)', fontsize=12, fontweight='bold')
ax.set_ylabel('Understudied Endophyte Countries (%)', fontsize=12, fontweight='bold')
ax.set_title('Enrichment of Understudied Regions in High-Priority Biodiversity Areas', 
             fontsize=13, fontweight='bold', pad=20)
ax.set_xticks(range(len(sensitivity)))
ax.set_xticklabels(['Top ' + lbl for lbl in sensitivity['priority_label']], fontsize=11)
ax.set_ylim(0, 80)
ax.grid(axis='y', alpha=0.3, linestyle='--')
ax.set_axisbelow(True)

# Add horizontal reference line for random expectation
# Random expectation would be the proportion of countries at each threshold
max_random_expectation = sensitivity['n_priority_countries'].max() / 84 * 100  # 84 = total understudied
ax.axhline(y=max_random_expectation, color='gray', linestyle=':', linewidth=2, 
           label=f'Random expectation (~{max_random_expectation:.0f}%)', alpha=0.7)

ax.legend(loc='upper right', fontsize=10)

# Add bottom annotation
fig.text(0.5, 0.02, 
         'Data: 84 understudied endophyte countries compared to World Bank endemic species counts and threatened probability metrics.\n' +
         'Finding: 70% of understudied countries rank in top 25% global priority, suggesting strong conservation-research alignment.',
         ha='center', fontsize=9, style='italic', wrap=True)

plt.tight_layout(rect=[0, 0.08, 1, 1])
plt.savefig(OUTPUT_PLOT, dpi=300, bbox_inches='tight')
print(f"Plot saved to: {OUTPUT_PLOT}")

# Also create a version showing counts (for supplementary)
fig2, (ax1, ax2) = plt.subplots(1, 2, figsize=(14, 5))

# Left: count of countries at each priority
x_pos = np.arange(len(sensitivity))
ax1.bar(x_pos, sensitivity['n_priority_countries'], color='#3498db', alpha=0.7, 
        edgecolor='black', label='Total countries', linewidth=1.5)
ax1.bar(x_pos, sensitivity['n_overlap_countries'], color='#e74c3c', alpha=0.8, 
        edgecolor='black', label='Understudied overlap', linewidth=1.5)
ax1.set_xlabel('Priority Level', fontsize=11, fontweight='bold')
ax1.set_ylabel('Number of Countries', fontsize=11, fontweight='bold')
ax1.set_title('Absolute Counts at Each Priority Threshold', fontsize=12, fontweight='bold')
ax1.set_xticks(x_pos)
ax1.set_xticklabels(['Top ' + lbl for lbl in sensitivity['priority_label']], fontsize=10)
ax1.legend(fontsize=10)
ax1.grid(axis='y', alpha=0.3, linestyle='--')
ax1.set_axisbelow(True)

# Right: stacked percentage
ax2.bar(x_pos, sensitivity['pct_understudied_overlapping'], color='#e74c3c', alpha=0.8, 
        edgecolor='black', label='Understudied overlap %', linewidth=1.5)
ax2.bar(x_pos, 100-sensitivity['pct_understudied_overlapping'], bottom=sensitivity['pct_understudied_overlapping'],
        color='#95a5a6', alpha=0.6, edgecolor='black', label='Other countries', linewidth=1.5)
ax2.set_xlabel('Priority Level', fontsize=11, fontweight='bold')
ax2.set_ylabel('Percentage of Priority Countries (%)', fontsize=11, fontweight='bold')
ax2.set_title('Composition of Priority Countries at Each Threshold', fontsize=12, fontweight='bold')
ax2.set_xticks(x_pos)
ax2.set_xticklabels(['Top ' + lbl for lbl in sensitivity['priority_label']], fontsize=10)
ax2.set_ylim(0, 100)
ax2.legend(fontsize=10)
ax2.grid(axis='y', alpha=0.3, linestyle='--')
ax2.set_axisbelow(True)

plt.tight_layout()
plt.savefig(OUTPUT_PLOT.parent / 'priority_overlap_sensitivity_detailed.png', dpi=300, bbox_inches='tight')
print(f"Detailed plot saved to: {OUTPUT_PLOT.parent / 'priority_overlap_sensitivity_detailed.png'}")
print("\nPlots complete")
