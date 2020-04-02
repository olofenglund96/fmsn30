# Load and init log transformation
load('Data/Pb_mossa.rda')
Pb_mossa$nyear = I(Pb_mossa$year-1975)
summary(Pb_mossa)
Pb_log = Pb_mossa
Pb_log$Pb <- log(Pb_mossa$Pb)
Pb_log$nyear <- Pb_mossa$nyear

(Pb_log.model <- lm(Pb ~ nyear, data = Pb_log))
