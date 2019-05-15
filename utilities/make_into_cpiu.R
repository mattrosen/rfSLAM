# load cpiu file
source("cpiu.R")
library("readstata13")
library("tidyverse")
library("lubridate")

# load updated.corrected
corrected <- readRDS("../../Desktop/RF-SLAM_data/new.full.dat.updated.rds")

# load LVEF data
lvef.data <- readRDS("../../Desktop/RF-SLAM_data/new.lvef.t.updated.rds")

# merge row-wise
corrected <- merge(corrected, lvef.data, all.x=TRUE)

# compute start time of study
corrected$start.time1 <- as.Date(as.character(corrected$dateofmri),format = "%m/%d/%Y")

# add for lvef
corrected$nuc1date  <- corrected$nucdate
corrected$nuc1ef    <- corrected$nucef
corrected$echo0date <- corrected$dateofmri
corrected$echo0ef   <- corrected$otheref

# column s.t. the i-th value is min(deathtime[i], timescd1[i])
corrected$min.death.sca <- pmin(corrected$deathtime, 
								corrected$timescd1, 
								na.rm=TRUE)

# ditto for above, but with min(deathtime, timehf1)
corrected$min.death.hf <- pmin(corrected$deathtime,
							   corrected$timehf1, 
							   na.rm=TRUE)

lvef.list <- list(times=c(sprintf("echo%ddate", 0:11), sprintf("nuc%ddate", 1:3)), 
				  values=c(sprintf("echo%def", 0:11),  sprintf("nuc%def", 1:3)))

# list of variables to create for CPIU'd data ending at sca1
to.create.msc <- list(t.hf=sprintf("timehf",seq(1:10)),
					  t.sca="timescd1",
					  c.lvef=lvef.list,
					  n.hf=sprintf("timehf",seq(1:10)),
					  n.sca="timescd1",
					  i.hf=sprintf("timehf",seq(1:10)),
					  i.sca="timescd1",
					  ni.hf=sprintf("timehf",seq(1:10)),
					  ni.sca="timescd1",
					  i.death='timescd1')

# list of variables to create for CPIU'd data ending at hf1
to.create.mhf <- list(t.hf="timehf1",
					  t.sca="timescd1",
					  c.lvef=lvef.list,
					  n.hf="timehf1",
					  n.sca="timescd1",
					  i.hf="timehf1",
					  i.sca="timescd1",
					  ni.hf="timehf1",
					  ni.sca="timescd1",
					  i.death='timehf1')

# ditto for above, but for intervals ending in death
to.create.mdeath <- list(t.hf=sprintf("timehf%dpsca",seq(1:10)),
						 t.sca="timescd1",
						 c.lvef=lvef.list,
						 n.hf=sprintf("timehf%dpsca",seq(1:10)),
						 n.sca="timescd1",
						 i.hf=sprintf("timehf%dpsca",seq(1:10)),
						 i.sca="timescd1",
						 ni.hf=sprintf("timehf%dpsca",seq(1:10)),
						 ni.sca="timescd1")

# make the CPIUs
for (k in c(0, 2)) {

	# new files
	interval.by.sca   <- cpiu.fc(corrected, 
								 t.outcome="min.death.sca", 
								 to.create=to.create.msc, 
								 k.to.persist=k)
	interval.by.hf    <- cpiu.fc(corrected, 
								 t.outcome="min.death.hf",  
								 to.create=to.create.mhf, 
								 k.to.persist=k)
	interval.by.death <- cpiu.fc(corrected, 
								 t.outcome="deathtime", 
								 to.create=to.create.mdeath,  
								 k.to.persist=k)


	# save
	s1 <- saveRDS(interval.by.sca, sprintf("../../Desktop/RF-SLAM_data/for_shannon/interval_by_sca1_general_m%d.rds", k))
	s2 <- saveRDS(interval.by.hf, sprintf("../../Desktop/RF-SLAM_data/for_shannon/interval_by_hf1_general_m%d.rds", k))
	s3 <- saveRDS(interval.by.death, sprintf("../../Desktop/RF-SLAM_data/for_shannon/interval_by_death_general_m%d.rds", k))
}

