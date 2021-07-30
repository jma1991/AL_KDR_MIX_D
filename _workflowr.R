# Step 1: Commit analysis files

system("git add analysis/about.Rmd")
system("git add analysis/index.Rmd")
system("git add analysis/license.Rmd")
system("git add analysis/01-quality-control.Rmd")
system("git add analysis/02-normalization.Rmd")
system("git add analysis/03-feature-selection.Rmd")
system("git add analysis/04-reduced-dimensions.Rmd")
system("git add analysis/05-clustering.Rmd")
system("git add analysis/06-doublet-detection.Rmd")
system("git add analysis/07-cell-cycle.Rmd")
system("git add analysis/08-marker-detection.Rmd")
system("git add analysis/09-cell-annotation.Rmd")
system("git add analysis/signaling-analysis.Rmd")
system("git commit -m 'Build'")

# Step 2: Build HTML files

wflow_build("analysis/about.Rmd")
wflow_build("analysis/index.Rmd")
wflow_build("analysis/license.Rmd")
wflow_build("analysis/01-quality-control.Rmd")
wflow_build("analysis/02-normalization.Rmd")
wflow_build("analysis/03-feature-selection.Rmd")
wflow_build("analysis/04-reduced-dimensions.Rmd")
wflow_build("analysis/05-clustering.Rmd")
wflow_build("analysis/06-doublet-detection.Rmd")
wflow_build("analysis/07-cell-cycle.Rmd")
wflow_build("analysis/08-marker-detection.Rmd")
wflow_build("analysis/09-cell-annotation.Rmd")
wflow_build("analysis/signaling-analysis.Rmd")

# Step 3: Commit HTML files

system("git add docs/about.html")
system("git add docs/index.html")
system("git add docs/license.html")
system("git add docs/01-quality-control.html")
system("git add docs/02-normalization.html")
system("git add docs/03-feature-selection.html")
system("git add docs/04-reduced-dimensions.html")
system("git add docs/05-clustering.html")
system("git add docs/06-doublet-detection.html")
system("git add docs/07-cell-cycle.html")
system("git add docs/08-marker-detection.html")
system("git add docs/09-cell-annotation.html")
system("git add docs/signaling-analysis.html")
system("git commit -m 'Build'")

# Step 4: Commit PNG files

system("git add docs/figure/01-quality-control.Rmd")
system("git add docs/figure/02-normalization.Rmd")
system("git add docs/figure/03-feature-selection.Rmd")
system("git add docs/figure/04-reduced-dimensions.Rmd")
system("git add docs/figure/05-clustering.Rmd")
system("git add docs/figure/06-doublet-detection.Rmd")
system("git add docs/figure/07-cell-cycle.Rmd")
system("git add docs/figure/08-marker-detection.Rmd")
system("git add docs/figure/09-cell-annotation.Rmd")
system("git add docs/figure/signaling-analysis.Rmd")
system("git commit -m 'Build'")

# Step 5: Commit docs files

system("git add docs/site_libs")
system("git add docs/.nojekyll")
system("git commit -m 'Build'")

# Step 6: Push to master

system("git push origin master")
