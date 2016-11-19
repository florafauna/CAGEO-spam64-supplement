

Rscript = Rscript-3.3.1 --vanilla 
R = R-3.3.1
RBATCH = $(R) CMD BATCH --vanilla --args

all: Rout/summary.Rout 





## ---------------------------------------------

Rout/summary.Rout figs/performance-times.pdf figs/performance-memory.pdf figs/performance-label.pdf: summary.R time memory
	$(RBATCH) summary.R Rout/summary.Rout
	rm -f Rplots.pdf

## ---------------------------------------------
memory: memory_base memory_spam-legacy memory_spam64 memory_spam64force
memory_base: data/memory_base_1.RData data/memory_base_2.RData data/memory_base_3.RData data/memory_base_4.RData data/memory_base_5.RData data/memory_base_6.RData data/memory_base_7.RData

data/memory_base_1.RData: memory_base.R setup.R
	$(RBATCH) -density_=1 -nr_=1 memory_base.R Rout/memory_base_1.Rout

data/memory_base_2.RData: memory_base.R setup.R
	$(RBATCH) -density_=0.316227766 -nr_=2 memory_base.R Rout/memory_base_2.Rout

data/memory_base_3.RData: memory_base.R setup.R
	$(RBATCH) -density_=0.1 -nr_=3 memory_base.R Rout/memory_base_3.Rout

data/memory_base_4.RData: memory_base.R setup.R
	$(RBATCH) -density_=0.031622777 -nr_=4 memory_base.R Rout/memory_base_4.Rout  

data/memory_base_5.RData: memory_base.R setup.R
	$(RBATCH) -density_=0.01 -nr_=5 memory_base.R Rout/memory_base_5.Rout

data/memory_base_6.RData: memory_base.R setup.R
	$(RBATCH) -density_=0.003162278 -nr_=6 memory_base.R Rout/memory_base_6.Rout

data/memory_base_7.RData: memory_base.R setup.R
	$(RBATCH) -density_=0.001 -nr_=7 memory_base.R Rout/memory_base_7.Rout

## ---------------------------------------------

memory_spam-legacy: data/memory_spam-legacy_1.RData data/memory_spam-legacy_2.RData data/memory_spam-legacy_3.RData data/memory_spam-legacy_4.RData data/memory_spam-legacy_5.RData data/memory_spam-legacy_6.RData data/memory_spam-legacy_7.RData

data/memory_spam-legacy_1.RData: memory_spam-legacy.R setup.R
	$(RBATCH) -density_=1 -nr_=1 memory_spam-legacy.R Rout/memory_spam-legacy_1.Rout

data/memory_spam-legacy_2.RData: memory_spam-legacy.R setup.R
	$(RBATCH) -density_=0.316227766 -nr_=2 memory_spam-legacy.R Rout/memory_spam-legacy_2.Rout

data/memory_spam-legacy_3.RData: memory_spam-legacy.R setup.R
	$(RBATCH) -density_=0.1 -nr_=3 memory_spam-legacy.R Rout/memory_spam-legacy_3.Rout

data/memory_spam-legacy_4.RData: memory_spam-legacy.R setup.R
	$(RBATCH) -density_=0.031622777 -nr_=4 memory_spam-legacy.R Rout/memory_spam-legacy_4.Rout

data/memory_spam-legacy_5.RData: memory_spam-legacy.R setup.R
	$(RBATCH) -density_=0.010000000 -nr_=5 memory_spam-legacy.R Rout/memory_spam-legacy_5.Rout

data/memory_spam-legacy_6.RData: memory_spam-legacy.R setup.R
	$(RBATCH) -density_=0.003162278 -nr_=6 memory_spam-legacy.R Rout/memory_spam-legacy_6.Rout

data/memory_spam-legacy_7.RData: memory_spam-legacy.R setup.R
	$(RBATCH) -density_=0.001 -nr_=7 memory_spam-legacy.R Rout/memory_spam-legacy_7.Rout

## ---------------------------------------------

memory_spam64: data/memory_spam64_1.RData data/memory_spam64_2.RData data/memory_spam64_3.RData data/memory_spam64_4.RData data/memory_spam64_5.RData data/memory_spam64_6.RData data/memory_spam64_7.RData

data/memory_spam64_1.RData: memory_spam64.R setup.R
	$(RBATCH) -density_=1 -nr_=1 memory_spam64.R Rout/memory_spam64_1.Rout

data/memory_spam64_2.RData: memory_spam64.R setup.R
	$(RBATCH) -density_=0.316227766 -nr_=2 memory_spam64.R Rout/memory_spam64_2.Rout

data/memory_spam64_3.RData: memory_spam64.R setup.R
	$(RBATCH) -density_=0.1 -nr_=3 memory_spam64.R Rout/memory_spam64_3.Rout

data/memory_spam64_4.RData: memory_spam64.R setup.R
	$(RBATCH) -density_=0.031622777 -nr_=4 memory_spam64.R Rout/memory_spam64_4.Rout

data/memory_spam64_5.RData: memory_spam64.R setup.R
	$(RBATCH) -density_=0.01 -nr_=5 memory_spam64.R Rout/memory_spam64_5.Rout

data/memory_spam64_6.RData: memory_spam64.R setup.R
	$(RBATCH) -density_=0.003162278 -nr_=6 memory_spam64.R Rout/memory_spam64_6.Rout

data/memory_spam64_7.RData: memory_spam64.R setup.R
	$(RBATCH) -density_=0.001 -nr_=7 memory_spam64.R Rout/memory_spam64_7.Rout


## ---------------------------------------------
memory_spam64force: data/memory_spam64force_1.RData data/memory_spam64force_2.RData data/memory_spam64force_3.RData data/memory_spam64force_4.RData data/memory_spam64force_5.RData data/memory_spam64force_6.RData data/memory_spam64force_7.RData

data/memory_spam64force_1.RData: memory_spam64force.R setup.R
	$(RBATCH) -density_=1 -nr_=1 memory_spam64force.R Rout/memory_spam64force_1.Rout

data/memory_spam64force_2.RData: memory_spam64force.R setup.R
	$(RBATCH) -density_=0.316227766 -nr_=2 memory_spam64force.R Rout/memory_spam64force_2.Rout

data/memory_spam64force_3.RData: memory_spam64force.R setup.R
	$(RBATCH) -density_=0.1 -nr_=3 memory_spam64force.R Rout/memory_spam64force_3.Rout

data/memory_spam64force_4.RData: memory_spam64force.R setup.R
	$(RBATCH) -density_=0.031622777 -nr_=4 memory_spam64force.R Rout/memory_spam64force_4.Rout

data/memory_spam64force_5.RData: memory_spam64force.R setup.R
	$(RBATCH) -density_=0.01 -nr_=5 memory_spam64force.R Rout/memory_spam64force_5.Rout

data/memory_spam64force_6.RData: memory_spam64force.R setup.R
	$(RBATCH) -density_=0.003162278 -nr_=6 memory_spam64force.R Rout/memory_spam64force_6.Rout

data/memory_spam64force_7.RData: memory_spam64force.R setup.R
	$(RBATCH) -density_=0.001 -nr_=7 memory_spam64force.R Rout/memory_spam64force_7.Rout

## ---------------------------------------------
time: time_base time_spam-legacy time_spam64 time_spam64force
time_base: data/time_base_1.RData data/time_base_2.RData data/time_base_3.RData data/time_base_4.RData data/time_base_5.RData data/time_base_6.RData data/time_base_7.RData

data/time_base_1.RData: time_base.R setup.R
	$(RBATCH) -density_=1 -nr_=1 time_base.R Rout/time_base_1.Rout

data/time_base_2.RData: time_base.R setup.R
	$(RBATCH) -density_=0.316227766 -nr_=2 time_base.R Rout/time_base_2.Rout

data/time_base_3.RData: time_base.R setup.R
	$(RBATCH) -density_=0.1 -nr_=3 time_base.R Rout/time_base_3.Rout

data/time_base_4.RData: time_base.R setup.R
	$(RBATCH) -density_=0.031622777 -nr_=4 time_base.R Rout/time_base_4.Rout  

data/time_base_5.RData: time_base.R setup.R
	$(RBATCH) -density_=0.01 -nr_=5 time_base.R Rout/time_base_5.Rout

data/time_base_6.RData: time_base.R setup.R
	$(RBATCH) -density_=0.003162278 -nr_=6 time_base.R Rout/time_base_6.Rout

data/time_base_7.RData: time_base.R setup.R
	$(RBATCH) -density_=0.001 -nr_=7 time_base.R Rout/time_base_7.Rout

## ---------------------------------------------

time_spam-legacy: data/time_spam-legacy_1.RData data/time_spam-legacy_2.RData data/time_spam-legacy_3.RData data/time_spam-legacy_4.RData data/time_spam-legacy_5.RData data/time_spam-legacy_6.RData data/time_spam-legacy_7.RData

data/time_spam-legacy_1.RData: time_spam-legacy.R setup.R
	$(RBATCH) -density_=1 -nr_=1 time_spam-legacy.R Rout/time_spam-legacy_1.Rout

data/time_spam-legacy_2.RData: time_spam-legacy.R setup.R
	$(RBATCH) -density_=0.316227766 -nr_=2 time_spam-legacy.R Rout/time_spam-legacy_2.Rout

data/time_spam-legacy_3.RData: time_spam-legacy.R setup.R
	$(RBATCH) -density_=0.1 -nr_=3 time_spam-legacy.R Rout/time_spam-legacy_3.Rout

data/time_spam-legacy_4.RData: time_spam-legacy.R setup.R
	$(RBATCH) -density_=0.031622777 -nr_=4 time_spam-legacy.R Rout/time_spam-legacy_4.Rout

data/time_spam-legacy_5.RData: time_spam-legacy.R setup.R
	$(RBATCH) -density_=0.010000000 -nr_=5 time_spam-legacy.R Rout/time_spam-legacy_5.Rout

data/time_spam-legacy_6.RData: time_spam-legacy.R setup.R
	$(RBATCH) -density_=0.003162278 -nr_=6 time_spam-legacy.R Rout/time_spam-legacy_6.Rout

data/time_spam-legacy_7.RData: time_spam-legacy.R setup.R
	$(RBATCH) -density_=0.001 -nr_=7 time_spam-legacy.R Rout/time_spam-legacy_7.Rout

## ---------------------------------------------

time_spam64: data/time_spam64_1.RData data/time_spam64_2.RData data/time_spam64_3.RData data/time_spam64_4.RData data/time_spam64_5.RData data/time_spam64_6.RData data/time_spam64_7.RData

data/time_spam64_1.RData: time_spam64.R setup.R
	$(RBATCH) -density_=1 -nr_=1 time_spam64.R Rout/time_spam64_1.Rout

data/time_spam64_2.RData: time_spam64.R setup.R
	$(RBATCH) -density_=0.316227766 -nr_=2 time_spam64.R Rout/time_spam64_2.Rout

data/time_spam64_3.RData: time_spam64.R setup.R
	$(RBATCH) -density_=0.1 -nr_=3 time_spam64.R Rout/time_spam64_3.Rout

data/time_spam64_4.RData: time_spam64.R setup.R
	$(RBATCH) -density_=0.031622777 -nr_=4 time_spam64.R Rout/time_spam64_4.Rout

data/time_spam64_5.RData: time_spam64.R setup.R
	$(RBATCH) -density_=0.01 -nr_=5 time_spam64.R Rout/time_spam64_5.Rout

data/time_spam64_6.RData: time_spam64.R setup.R
	$(RBATCH) -density_=0.003162278 -nr_=6 time_spam64.R Rout/time_spam64_6.Rout

data/time_spam64_7.RData: time_spam64.R setup.R
	$(RBATCH) -density_=0.001 -nr_=7 time_spam64.R Rout/time_spam64_7.Rout


## ---------------------------------------------
time_spam64force: data/time_spam64force_1.RData data/time_spam64force_2.RData data/time_spam64force_3.RData data/time_spam64force_4.RData data/time_spam64force_5.RData data/time_spam64force_6.RData data/time_spam64force_7.RData

data/time_spam64force_1.RData: time_spam64force.R setup.R
	$(RBATCH) -density_=1 -nr_=1 time_spam64force.R Rout/time_spam64force_1.Rout

data/time_spam64force_2.RData: time_spam64force.R setup.R
	$(RBATCH) -density_=0.316227766 -nr_=2 time_spam64force.R Rout/time_spam64force_2.Rout

data/time_spam64force_3.RData: time_spam64force.R setup.R
	$(RBATCH) -density_=0.1 -nr_=3 time_spam64force.R Rout/time_spam64force_3.Rout

data/time_spam64force_4.RData: time_spam64force.R setup.R
	$(RBATCH) -density_=0.031622777 -nr_=4 time_spam64force.R Rout/time_spam64force_4.Rout

data/time_spam64force_5.RData: time_spam64force.R setup.R
	$(RBATCH) -density_=0.01 -nr_=5 time_spam64force.R Rout/time_spam64force_5.Rout

data/time_spam64force_6.RData: time_spam64force.R setup.R
	$(RBATCH) -density_=0.003162278 -nr_=6 time_spam64force.R Rout/time_spam64force_6.Rout

data/time_spam64force_7.RData: time_spam64force.R setup.R
	$(RBATCH) -density_=0.001 -nr_=7 time_spam64force.R Rout/time_spam64force_7.Rout



clean-data:
	rm -f data/*
	rm -f *.Rout
	rm -f Rout/*
	rm -f figs/*
	rm -f Rplots.pdf

.PHONY: clean-data all \
	time time_base time_spam-legacy time_spam64 time_spam64force \
	memory 	memory_base memory_spam-legacy memory_spam64 memory_spam64force
