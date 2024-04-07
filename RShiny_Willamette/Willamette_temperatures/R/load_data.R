# Annual time series tab

load("data/North Santiam.retro.ts.RData")
load("data/South Santiam.retro.ts.RData")
load("data/McKenzie.retro.ts.RData")
load("data/Middle Fork.retro.ts.RData")
load("data/Mainstem.retro.ts.RData")

for(p in c("NorthSantiam", "SouthSantiam", "McKenzie", "MiddleFork", "Mainstem")){
  for(c in c("CanESM2", "CCSM4", "CNRM-CM5", "CSIRO-Mk3-6-0", "GDFL-ESM2M", "HadGEM2-CC", "HadGEM2-ES", "inmcm4", "IPSL-CM5A-MR", "MIROC", "ST_med")){
      load(paste0("data/", p, ".", c, ".data.list.RData"))
  }
}

# Whole time series tab

load("data/retro.means.RData")

climate_scenarios <- c("CanESM2", "CCSM4", "CNRM-CM5", "CSIRO-Mk3-6-0", "GDFL-ESM2M", "HadGEM2-CC", "HadGEM2-ES", "inmcm4", "IPSL-CM5A-MR", "MIROC", "ST_med")
climate_scenarios <- gsub("-", "_", climate_scenarios)
for(c in climate_scenarios){
  load(paste0("data/", c, ".means.RData"))
}