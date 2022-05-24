# PART 4: Meta-Analysis
nh_meta_analysis <- read_dta("Replication/NH Meta-analysis/NH Meta-Analysis Data4OSF.dta")

# Model 1
lm.cluster(effect100 ~ major_party_general + primary, 
           data = nh_meta_analysis, cluster = "RaceID")
# Model 2: high-visibility (media) model
lm.cluster(effect100 ~ race_media0to1SQRT, 
           data = nh_meta_analysis, cluster = "RaceID")       
# Model 3: incumbent model
lm.cluster(effect100 ~ race_has_incumbent, 
           data = nh_meta_analysis, cluster = "RaceID")
# Model 4: Margin of Victory model
lm.cluster(effect100 ~ mov0to1SQRT, 
           data = nh_meta_analysis, cluster = "RaceID")
# Model 5: Combined model
lm.cluster(effect100 ~ major_party_general + primary + race_media0to1SQRT + race_has_incumbent + mov0to1SQRT, 
           data = nh_meta_analysis, cluster = "RaceID")