.PHONY: all cleanall chol chol_corse chol_fine fit fit_corse fit_fine fit_simple_fine fit_simple_corse

Rscript = Rscript-3.3.1
RBATCH = R-3.3.1 CMD BATCH --vanilla --args

## data preparation ----------------------------------------------
dat_prep: 6_model.Rout

cleanall:
	rm Rout/* *.pdf  -f

## 1-5 can be skipped
Rout/1_prepare_ndvi.Rout data/ndvi-model-1.gri: 1_prepare_ndvi.R  
	mkdir -p data
	$(RBATCH) 1_prepare_ndvi.R Rout/1_prepare_ndvi.Rout
	mv Rplots.pdf 1_prepare_ndvi.pdf  

Rout/2_prepare_dist2coast.Rout: 2_prepare_dist2coast.R data/ndvi-model-1.gri
	$(RBATCH) 2_prepare_dist2coast.R Rout/2_prepare_dist2coast.Rout
	mv Rplots.pdf 2_prepare_dist2coast.pdf

Rout/3_prepare_elevation.Rout: 3_prepare_elevation.R data/ndvi-model-1.gri
	$(RBATCH) 3_prepare_elevation.R Rout/3_prepare_elevation.Rout
	mv Rplots.pdf 3_prepare_elevation.pdf

Rout/4_prepare_merge.Rout: 4_prepare_merge.R 3_prepare_elevation.Rout 2_prepare_dist2coast.Rout
	$(RBATCH) 4_prepare_merge.R Rout/4_prepare_merge.Rout
#	mv Rplots.pdf 4_export_samples.pdf

Rout/5_prepare_subsets.Rout: 5_prepare_subsets.R 4_prepare_merge.Rout
	$(RBATCH) 5_prepare_subsets.R Rout/5_prepare_subsets.Rout
#	mv Rplots.pdf 4_export_samples.pdf



# 6 prepare data
Rout/6_preprocess.Rout data/data.RData: 6_preprocess.R #5_prepare_subsets.Rout
	$(RBATCH) 6_preprocess.R Rout/6_preprocess.Rout
#	mv Rplots.pdf 6_model.pdf




## model fit ----------------------------------------------
## 7. get Cholseky
chol: chol_fine chol_corse

chol_corse: data/chol_0.RData data/chol_0.1.RData data/chol_0.2.RData data/chol_0.3.RData data/chol_0.4.RData data/chol_0.5.RData data/chol_0.6.RData data/chol_0.7.RData data/chol_0.8.RData data/chol_0.9.RData data/chol_1.RData

chol_fine: data/chol_0.52.RData data/chol_0.54.RData data/chol_0.56.RData data/chol_0.58.RData data/chol_0.62.RData data/chol_0.64.RData data/chol_0.66.RData data/chol_0.68.RData

## 8. fit models
fit: fit_corse fit_fine fit_simple fit_simple_fine

fit_corse: data/out_0.RData data/out_0.1.RData data/out_0.2.RData data/out_0.3.RData data/out_0.4.RData data/out_0.5.RData data/out_0.6.RData data/out_0.7.RData data/out_0.8.RData data/out_0.9.RData data/out_1.RData 

fit_fine: data/out_0.52.RData data/out_0.54.RData data/out_0.56.RData data/out_0.58.RData data/out_0.62.RData data/out_0.64.RData data/out_0.66.RData data/out_0.68.RData


fit_simple: data/out_simple_0.RData data/out_simple_0.1.RData data/out_simple_0.2.RData data/out_simple_0.3.RData data/out_simple_0.4.RData data/out_simple_0.5.RData data/out_simple_0.6.RData data/out_simple_0.7.RData data/out_simple_0.8.RData data/out_simple_0.9.RData data/out_simple_1.RData 

fit_simple_fine: data/out_simple_0.52.RData data/out_simple_0.54.RData data/out_simple_0.56.RData data/out_simple_0.58.RData data/out_simple_0.62.RData data/out_simple_0.64.RData data/out_simple_0.66.RData data/out_simple_0.68.RData


## ---------------------------------------------

data/chol_0.RData: 7_chol.R data/data.RData
	$(RBATCH) -tau=0    7_chol.R Rout/7_chol_0.Rout  
data/chol_0.1.RData: 7_chol.R data/data.RData
	$(RBATCH) -tau=0.1  7_chol.R Rout/7_chol_0.1.Rout
data/chol_0.2.RData: 7_chol.R data/data.RData
	$(RBATCH) -tau=0.2  7_chol.R Rout/7_chol_0.2.Rout
data/chol_0.3.RData: 7_chol.R data/data.RData
	$(RBATCH) -tau=0.3  7_chol.R Rout/7_chol_0.3.Rout
data/chol_0.4.RData: 7_chol.R data/data.RData
	$(RBATCH) -tau=0.4  7_chol.R Rout/7_chol_0.4.Rout
data/chol_0.5.RData: 7_chol.R data/data.RData
	$(RBATCH) -tau=0.5  7_chol.R Rout/7_chol_0.5.Rout
data/chol_0.6.RData: 7_chol.R data/data.RData
	$(RBATCH) -tau=0.6  7_chol.R Rout/7_chol_0.6.Rout
data/chol_0.7.RData: 7_chol.R data/data.RData
	$(RBATCH) -tau=0.7  7_chol.R Rout/7_chol_0.7.Rout
data/chol_0.8.RData: 7_chol.R data/data.RData
	$(RBATCH) -tau=0.8  7_chol.R Rout/7_chol_0.8.Rout
data/chol_0.9.RData: 7_chol.R data/data.RData
	$(RBATCH) -tau=0.9  7_chol.R Rout/7_chol_0.9.Rout
data/chol_1.RData: 7_chol.R data/data.RData
	$(RBATCH) -tau=1    7_chol.R Rout/7_chol_1.Rout




data/chol_0.52.RData: 7_chol.R	data/data.RData
	$(RBATCH) -tau=0.52  7_chol.R Rout/7_chol_0.52.Rout
data/chol_0.54.RData: 7_chol.R  data/data.RData
	$(RBATCH) -tau=0.54  7_chol.R Rout/7_chol_0.54.Rout
data/chol_0.56.RData: 7_chol.R  data/data.RData
	$(RBATCH) -tau=0.56  7_chol.R Rout/7_chol_0.56.Rout
data/chol_0.58.RData: 7_chol.R  data/data.RData
	$(RBATCH) -tau=0.58  7_chol.R Rout/7_chol_0.58.Rout
data/chol_0.62.RData: 7_chol.R  data/data.RData
	$(RBATCH) -tau=0.62  7_chol.R Rout/7_chol_0.62.Rout
data/chol_0.64.RData: 7_chol.R  data/data.RData
	$(RBATCH) -tau=0.64  7_chol.R Rout/7_chol_0.64.Rout
data/chol_0.66.RData: 7_chol.R  data/data.RData
	$(RBATCH) -tau=0.66  7_chol.R Rout/7_chol_0.66.Rout
data/chol_0.68.RData: 7_chol.R  data/data.RData
	$(RBATCH) -tau=0.68  7_chol.R Rout/7_chol_0.68.Rout




data/out_0.RData: 8_fit.R
	$(RBATCH) -tau=0    8_fit.R Rout/8_fit_0.Rout
data/out_0.1.RData: 8_fit.R
	$(RBATCH) -tau=0.1  8_fit.R Rout/8_fit_0.1.Rout
data/out_0.2.RData: 8_fit.R
	$(RBATCH) -tau=0.2  8_fit.R Rout/8_fit_0.2.Rout
data/out_0.3.RData: 8_fit.R
	$(RBATCH) -tau=0.3  8_fit.R Rout/8_fit_0.3.Rout
data/out_0.4.RData: 8_fit.R
	$(RBATCH) -tau=0.4  8_fit.R Rout/8_fit_0.4.Rout
data/out_0.5.RData: 8_fit.R
	$(RBATCH) -tau=0.5  8_fit.R Rout/8_fit_0.5.Rout
data/out_0.6.RData: 8_fit.R
	$(RBATCH) -tau=0.6  8_fit.R Rout/8_fit_0.6.Rout
data/out_0.7.RData: 8_fit.R
	$(RBATCH) -tau=0.7  8_fit.R Rout/8_fit_0.7.Rout
data/out_0.8.RData: 8_fit.R
	$(RBATCH) -tau=0.8  8_fit.R Rout/8_fit_0.8.Rout
data/out_0.9.RData: 8_fit.R
	$(RBATCH) -tau=0.9  8_fit.R Rout/8_fit_0.9.Rout
data/out_1.RData: 8_fit.R
	$(RBATCH) -tau=1  8_fit.R Rout/8_fit_1.Rout

data/out_0.52.RData: 8_fit.R
	$(RBATCH) -tau=0.52  8_fit.R Rout/8_fit_0.52.Rout
data/out_0.54.RData: 8_fit.R
	$(RBATCH) -tau=0.54  8_fit.R Rout/8_fit_0.54.Rout
data/out_0.56.RData: 8_fit.R
	$(RBATCH) -tau=0.56  8_fit.R Rout/8_fit_0.56.Rout
data/out_0.58.RData: 8_fit.R
	$(RBATCH) -tau=0.58  8_fit.R Rout/8_fit_0.58.Rout
data/out_0.62.RData: 8_fit.R
	$(RBATCH) -tau=0.62  8_fit.R Rout/8_fit_0.62.Rout
data/out_0.64.RData: 8_fit.R
	$(RBATCH) -tau=0.64  8_fit.R Rout/8_fit_0.64.Rout
data/out_0.66.RData: 8_fit.R
	$(RBATCH) -tau=0.66  8_fit.R Rout/8_fit_0.66.Rout
data/out_0.68.RData: 8_fit.R
	$(RBATCH) -tau=0.68  8_fit.R Rout/8_fit_0.68.Rout


data/out_simple_0.RData: 8_fit_simple.R
	$(RBATCH) -tau=0     8_fit_simple.R Rout/8_fit_simple_0.Rout
data/out_simple_0.1.RData: 8_fit_simple.R
	$(RBATCH) -tau=0.1   8_fit_simple.R Rout/8_fit_simple_0.1.Rout
data/out_simple_0.2.RData: 8_fit_simple.R
	$(RBATCH) -tau=0.2   8_fit_simple.R Rout/8_fit_simple_0.2.Rout
data/out_simple_0.3.RData: 8_fit_simple.R
	$(RBATCH) -tau=0.3   8_fit_simple.R Rout/8_fit_simple_0.3.Rout
data/out_simple_0.4.RData: 8_fit_simple.R
	$(RBATCH) -tau=0.4   8_fit_simple.R Rout/8_fit_simple_0.4.Rout
data/out_simple_0.5.RData: 8_fit_simple.R
	$(RBATCH) -tau=0.5   8_fit_simple.R Rout/8_fit_simple_0.5.Rout
data/out_simple_0.6.RData: 8_fit_simple.R
	$(RBATCH) -tau=0.6   8_fit_simple.R Rout/8_fit_simple_0.6.Rout
data/out_simple_0.7.RData: 8_fit_simple.R
	$(RBATCH) -tau=0.7   8_fit_simple.R Rout/8_fit_simple_0.7.Rout
data/out_simple_0.8.RData: 8_fit_simple.R
	$(RBATCH) -tau=0.8   8_fit_simple.R Rout/8_fit_simple_0.8.Rout
data/out_simple_0.9.RData: 8_fit_simple.R
	$(RBATCH) -tau=0.9   8_fit_simple.R Rout/8_fit_simple_0.9.Rout
data/out_simple_1.RData: 8_fit_simple.R
	$(RBATCH) -tau=1   8_fit_simple.R Rout/8_fit_simple_1.Rout

data/out_simple_0.52.RData: 8_fit_simple.R
	$(RBATCH) -tau=0.52   8_fit_simple.R Rout/8_fit_simple_0.52.Rout
data/out_simple_0.54.RData: 8_fit_simple.R
	$(RBATCH) -tau=0.54   8_fit_simple.R Rout/8_fit_simple_0.54.Rout
data/out_simple_0.56.RData: 8_fit_simple.R	
	$(RBATCH) -tau=0.56   8_fit_simple.R Rout/8_fit_simple_0.56.Rout
data/out_simple_0.58.RData: 8_fit_simple.R
	$(RBATCH) -tau=0.58   8_fit_simple.R Rout/8_fit_simple_0.58.Rout
data/out_simple_0.62.RData: 8_fit_simple.R
	$(RBATCH) -tau=0.62   8_fit_simple.R Rout/8_fit_simple_0.62.Rout
data/out_simple_0.64.RData: 8_fit_simple.R
	$(RBATCH) -tau=0.64   8_fit_simple.R Rout/8_fit_simple_0.64.Rout
data/out_simple_0.66.RData: 8_fit_simple.R
	$(RBATCH) -tau=0.66   8_fit_simple.R Rout/8_fit_simple_0.66.Rout
data/out_simple_0.68.RData: 8_fit_simple.R
	$(RBATCH) -tau=0.68   8_fit_simple.R Rout/8_fit_simple_0.68.Rout

# 9 summary fit 1
Rout/9_summary_simple.Rout: 9_summary_simple.R
	$(RBATCH) 9_summary_simple.R Rout/9_summary_simple.Rout

# 10 summary fit 2
Rout/10_summary.Rout: 10_summary.R Rout/9_summary_simple.Rout
	$(RBATCH) 10_summary.R Rout/10_summary.Rout



