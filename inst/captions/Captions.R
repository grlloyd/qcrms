#' @import captioner
#'

figs <- captioner::captioner(prefix="Figure S", auto_space = F)
tbls <- captioner::captioner(prefix="Table S", auto_space = F)

figs(name="S1","The relationship between sample injection order and total ion
     intensity per sample of raw spectral data.")

figs(name="S2","The relationship between sample injection order and total
     ion intensity per sample after deconvolution with XCMS software.")

figs (name="S3", "The relationship between raw intensity signal and intensity signal extracted
      by XCMS. High correlation between data points in this figure indicates acceptable performance
      of peak picking step. ")

figs (name="S4", "The relationship of the number of extracted features by XCMS versus the total
      extracted signal intensity.")

figs (name="S5", "Scores plot of the first two principal components of PCA model for all QC and
      biological samples. Signal intensities scaled to unit variance (UV) and k-nearest (knn)
      method used (k=5) to fill in missing values.")

figs (name="S6", "Scores plot of the first two principal components of PCA model for QC samples only.
      Signal intensities scaled to unit variance (UV) and k-nearest (knn) method used (k=5) to fill
      in missing values.")

figs (name="S7", " Scores plot of the first two principal components of PCA model for QC samples
only and the first 5 QCs removed. Signal intensities scaled to unit variance (UV) and k-nearest
(knn) method used (k=5) to fill
      in missing values.")

figs (name="S8", "Scores plot of the first two principal components of PCA model for  all biological
      samples and QC samples and the first 5 QCs removed. Signal intensities scaled to unit variance
      (UV) and k-nearest (knn) method used (k=5) to fill in missing values.")

figs (name="S9", "Examples of extracted ion chromatogram (EICs). The figure is divided in three panels.
      Top pane includes chromatograms of the peak group with highest intensity for peak sets detected
      in at least 80% of samples. Bottom panes are showing peak sets from the closest m/z values of
      the main one to visualise potential drift in m/z.")

figs (name="S10", "Number of detection rate (percentage) across samples and extracted features.")

figs (name="S11", "Violin plot of Relative Standard Deviation (RSD%) per sample group. Equilibration
      QC samples 1-5 were excluded from RSD% calculations. Horisontal lines indicate 25%, 50% and 75%
      quantiles.")

figs (name="S12", "RSD% values of metabolic feature across QC samples in comparison to RSD% values of
      metabolic feature across biological samples. The plot visualises the difference between technical
      and biological variability of the spectral data.")

figs (name="S13", "Box plot of the features of all QC samples (per analytical batch if more than one
      is present) scaled to unit variance. Thick line in each bar represents median value of the
      scaled features and is expected to be close to 0 and for QC samples should stay within a range
      of +-2 (2 standard deviations).")

figs (name="S14", "Box plot of the features of  all QC and biological samples scaled to unit variance.
      This figure is indicative for a stability of the whole analytical batch.")

figs (name="S15", "Scores plot of the first two principal components of PCA model for  biological and
      QC (first 5 QCs removed) samples after peak matrix filtering. Data are normalised using
      PQN normalisation and k-nearest (knn) method used (k=5) to fill in missing values.
      Glog transformation used to scale signal intensities.")

figs (name="S16", "Scores plot of the first two principal components of PCA model for
QC (first 5 QCs removed) samples after peak matrix filtering. K-nearest (knn) method used (k=5) to fill in
      missing values and signal intensities are scaled to unit variance.")

figs (name="S17", "Scores plot of the first two principal components of PCA model for  biological and
      QC (first 5 QCs removed) samples after peak matrix filtering, and signal/batch correction.
      Data are normalised using PQN normalisation and k-nearest (knn) method used (k=5) to fill
      in missing values. Glog transformation used to scale signal intensities.")

figs (name="S18", "Scores plot of the first two principal components of PCA model for
      QC (first 5 QCs removed) samples after peak matrix filtering, and signal/batch correction.
      K-nearest (knn) method used (k=5) to fill in missing values and signal intensities are scaled to unit variance.")

figs (name="S19", "Violin plot of Relative Standard Deviation (RSD%) per sample group after peak
      matrix filtering. Equilibration QC samples were excluded from RSD% calculations. Horizontal
      lines indicate 25%, 50% and 75% quantiles.")

figs (name="S19a", "Violin plot of Relative Standard Deviation (RSD%) of QC samples per batch after 
      peak matrix filtering. Equilibration QC samples were excluded from RSD% calculations. Horizontal
      lines indicate 25%, 50% and 75% quantiles.")

figs (name="S20", "Violin plot of Relative Standard Deviation (RSD%) per sample group after 
      peak matrix filtering and signal/batch correction. Equilibration QC samples 1-5 were excluded
      from RSD% calculations. Horizontal lines indicate 25%, 50% and 75% quantiles.")

figs (name="RT_MAD", "Violin plots of MAD (meadian absolute deviation) of median RT per chromatographic feature.")

figs (name="peak_width", "Violin plot of meadian peak width per chromatographic feature in s.")

figs (name="mz_precision", "Violin plot of meadian of mass precision per chromatographic feature in ppm. Values below -5 and
      above 5 ppm are excluded from a plot.")

figs(name="size_vs_peaknr", "The relationship of the number of extracted features by XCMS versus the mzML file size.
     This figure should be useful to identify possible technical issues during sample collection.")

tbls (name="S1", "A summary of the Pearson's product-moment correlation coefficients for the data
      shown in Figures S2-S4 (blank samples are excluded from calculations). High correlation should not beens observed
      between measurement order and total signal intensity.")

tbls (name="S2", "A summary of RSD% for data shown in  Figure S11.")

tbls (name="S3", "A summary of RSD% values for data shown in  Figure S17.")

tbls (name="S4", "A summary of RSD% values for data shown in  Figure S18.")

tbls (name="S5", "Summary of the sample metadata of the analytical batch; Sample names, group labels,
      sample measurement time and number of features detected by XCMS for each sample.")

tbls (name="RT_RSD", "A summary of RSD% of median RT per chromatographic feature.")

tbls (name="RT_MAD", "A summary of MAD (meadian absolute deviation) of median RT per chromatographic feature.")

tbls (name="peak_width", "A summary of meadian peak width per chromatographic feature in s.")

tbls (name="mz_precision", "A summary of meadian of mass precision per chromatographic feature in ppm.")

tbls (name="filtering", "Several filters on extracted peak matrix are applied to asses data quality.
   1) Blank filter; 2) Missing value (MV) filter across samples; 3) MV filter across features for QC
   samples; 4) MV filter across feature and across all samples. 
   Parameters and thresholds used are listed in table below. Column 'Number of features' shows how many 
   features are retained after each filtering step, column 'Number of samples' how many samples are
   retained and column 'Applied' if the filter was applied.")
