library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)


# set up ----
rmultinorm<-data.table(id = rep(1:nrow(microsim_pop),61),
                  cycle=rep(1:61, times=nrow(microsim_pop)))
rmultinorm[,c("rmultinorm.no.fls.trt.choice.spine.male"):= 0]
rmultinorm[,c("rmultinorm.fls.trt.choice.spine.male"):= 0]
rmultinorm[,c("rmultinorm.no.fls.trt.choice.hip.male"):= 0]
rmultinorm[,c("rmultinorm.fls.trt.choice.hip.male"):= 0]
rmultinorm[,c("rmultinorm.no.fls.trt.choice.other.male"):= 0]
rmultinorm[,c("rmultinorm.fls.trt.choice.other.male"):= 0]
rmultinorm[,c("rmultinorm.no.fls.trt.choice.spine.female"):= 0]
rmultinorm[,c("rmultinorm.fls.trt.choice.spine.female"):= 0]
rmultinorm[,c("rmultinorm.no.fls.trt.choice.hip.female"):= 0]
rmultinorm[,c("rmultinorm.fls.trt.choice.hip.female"):= 0]
rmultinorm[,c("rmultinorm.no.fls.trt.choice.other.female"):= 0]
rmultinorm[,c("rmultinorm.fls.trt.choice.other.female"):= 0]
rmultinorm[,c("rmultinorm.no.fls.prob_identification.spine"):= 0]
rmultinorm[,c("rmultinorm.fls.prob_identification.spine"):= 0]
rmultinorm[,c("rmultinorm.no.fls.prob_identification.hip"):= 0]
rmultinorm[,c("rmultinorm.fls.prob_identification.hip"):= 0]
rmultinorm[,c("rmultinorm.no.fls.prob_identification.other"):= 0]
rmultinorm[,c("rmultinorm.fls.prob_identification.other"):= 0]
rmultinorm[,c("rmultinorm.no.fls.trt.spine.male"):= 0]
rmultinorm[,c("rmultinorm.fls.trt.spine.male"):= 0]
rmultinorm[,c("rmultinorm.no.fls.trt.hip.male"):= 0]
rmultinorm[,c("rmultinorm.fls.trt.hip.male"):= 0]
rmultinorm[,c("rmultinorm.no.fls.trt.other.male"):= 0]
rmultinorm[,c("rmultinorm.fls.trt.other.male"):= 0]
rmultinorm[,c("rmultinorm.no.fls.trt.spine.female"):= 0]
rmultinorm[,c("rmultinorm.fls.trt.spine.female"):= 0]
rmultinorm[,c("rmultinorm.no.fls.trt.hip.female"):= 0]
rmultinorm[,c("rmultinorm.fls.trt.hip.female"):= 0]
rmultinorm[,c("rmultinorm.no.fls.trt.other.female"):= 0]
rmultinorm[,c("rmultinorm.fls.trt.other.female"):= 0]
rmultinorm[,c("rmultinorm.romo.to.no.fls.male"):= 0]
rmultinorm[,c("rmultinorm.romo.to.fls.male"):= 0]
rmultinorm[,c("rmultinorm.romo.to.no.fls.female"):= 0]
rmultinorm[,c("rmultinorm.romo.to.fls.female"):= 0]
rmultinorm[,c("rmultinorm.abaloparatide.to.no.fls.male"):= 0]
rmultinorm[,c("rmultinorm.abaloparatide.to.fls.male"):= 0]
rmultinorm[,c("rmultinorm.abaloparatide.to.no.fls.female"):= 0]
rmultinorm[,c("rmultinorm.abaloparatide.to.fls.female"):= 0]
rmultinorm[,c("rmultinorm.teriparatide.to.no.fls.male"):= 0]
rmultinorm[,c("rmultinorm.teriparatide.to.fls.male"):= 0]
rmultinorm[,c("rmultinorm.teriparatide.to.no.fls.female"):= 0]
rmultinorm[,c("rmultinorm.teriparatide.to.fls.female"):= 0]
rmultinorm[,c("rmultinorm.no.fls.monitored.spine.4m.male"):= 0]
rmultinorm[,c("rmultinorm.fls.monitored.spine.4m.male"):= 0]
rmultinorm[,c("rmultinorm.no.fls.monitored.hip.4m.male"):= 0]
rmultinorm[,c("rmultinorm.fls.monitored.hip.4m.male"):= 0]
rmultinorm[,c("rmultinorm.no.fls.monitored.other.4m.male"):= 0]
rmultinorm[,c("rmultinorm.fls.monitored.other.4m.male"):= 0]
rmultinorm[,c("rmultinorm.no.fls.monitored.spine.4m.female"):= 0]
rmultinorm[,c("rmultinorm.fls.monitored.spine.4m.female"):= 0]
rmultinorm[,c("rmultinorm.no.fls.monitored.hip.4m.female"):= 0]
rmultinorm[,c("rmultinorm.fls.monitored.hip.4m.female"):= 0]
rmultinorm[,c("rmultinorm.no.fls.monitored.other.4m.female"):= 0]
rmultinorm[,c("rmultinorm.fls.monitored.other.4m.female"):= 0]
rmultinorm[,c("rmultinorm.no.fls.monitored.spine.12m.male"):= 0]
rmultinorm[,c("rmultinorm.fls.monitored.spine.12m.male"):= 0]
rmultinorm[,c("rmultinorm.no.fls.monitored.hip.12m.male"):= 0]
rmultinorm[,c("rmultinorm.fls.monitored.hip.12m.male"):= 0]
rmultinorm[,c("rmultinorm.no.fls.monitored.other.12m.male"):= 0]
rmultinorm[,c("rmultinorm.fls.monitored.other.12m.male"):= 0]
rmultinorm[,c("rmultinorm.no.fls.monitored.spine.12m.female"):= 0]
rmultinorm[,c("rmultinorm.fls.monitored.spine.12m.female"):= 0]
rmultinorm[,c("rmultinorm.no.fls.monitored.hip.12m.female"):= 0]
rmultinorm[,c("rmultinorm.fls.monitored.hip.12m.female"):= 0]
rmultinorm[,c("rmultinorm.no.fls.monitored.other.12m.female"):= 0]
rmultinorm[,c("rmultinorm.fls.monitored.other.12m.female"):= 0]
rmultinorm[,c("rmultinorm.not.monitored.4m_adh_risedronate.male"):= 0]
rmultinorm[,c("rmultinorm.not.monitored.4m_adh_strontium.male"):= 0]
rmultinorm[,c("rmultinorm.not.monitored.4m_adh_ibandronate.male"):= 0]
rmultinorm[,c("rmultinorm.not.monitored.4m_adh_denosumab.male"):= 0]
rmultinorm[,c("rmultinorm.not.monitored.4m_adh_zoledronate.male"):= 0]
rmultinorm[,c("rmultinorm.not.monitored.4m_adh_teriparatide.male"):= 0]
rmultinorm[,c("rmultinorm.not.monitored.4m_adh_abaloparatide.male"):= 0]
rmultinorm[,c("rmultinorm.not.monitored.4m_adh_romo.male"):= 0]
rmultinorm[,c("rmultinorm.not.monitored.12m_adh_alendronate.male"):= 0]
rmultinorm[,c("rmultinorm.not.monitored.12m_adh_risedronate.male"):= 0]
rmultinorm[,c("rmultinorm.not.monitored.12m_adh_strontium.male"):= 0]
rmultinorm[,c("rmultinorm.not.monitored.12m_adh_ibandronate.male"):= 0]
rmultinorm[,c("rmultinorm.not.monitored.12m_adh_denosumab.male"):= 0]
rmultinorm[,c("rmultinorm.not.monitored.12m_adh_zoledronate.male"):= 0]
rmultinorm[,c("rmultinorm.not.monitored.12m_adh_teriparatide.male"):= 0]
rmultinorm[,c("rmultinorm.not.monitored.12m_adh_abaloparatide.male"):= 0]
rmultinorm[,c("rmultinorm.not.monitored.4m_adh_alendronate.female"):= 0]
rmultinorm[,c("rmultinorm.not.monitored.4m_adh_risedronate.female"):= 0]
rmultinorm[,c("rmultinorm.not.monitored.4m_adh_strontium.female"):= 0]
rmultinorm[,c("rmultinorm.not.monitored.4m_adh_ibandronate.female"):= 0]
rmultinorm[,c("rmultinorm.not.monitored.4m_adh_denosumab.female"):= 0]
rmultinorm[,c("rmultinorm.not.monitored.4m_adh_zoledronate.female"):= 0]
rmultinorm[,c("rmultinorm.not.monitored.4m_adh_teriparatide.female"):= 0]
rmultinorm[,c("rmultinorm.not.monitored.4m_adh_abaloparatide.female"):= 0]
rmultinorm[,c("rmultinorm.not.monitored.4m_adh_romo.female"):= 0]
rmultinorm[,c("rmultinorm.not.monitored.12m_adh_alendronate.female"):= 0]
rmultinorm[,c("rmultinorm.not.monitored.12m_adh_risedronate.female"):= 0]
rmultinorm[,c("rmultinorm.not.monitored.12m_adh_strontium.female"):= 0]
rmultinorm[,c("rmultinorm.not.monitored.12m_adh_ibandronate.female"):= 0]
rmultinorm[,c("rmultinorm.not.monitored.12m_adh_denosumab.female"):= 0]
rmultinorm[,c("rmultinorm.not.monitored.12m_adh_zoledronate.female"):= 0]
rmultinorm[,c("rmultinorm.not.monitored.12m_adh_teriparatide.female"):= 0]
rmultinorm[,c("rmultinorm.not.monitored.12m_adh_abaloparatide.female"):= 0]
rmultinorm[,c("rmultinorm.monitored.4m_adh_alendronate.male"):= 0]
rmultinorm[,c("rmultinorm.monitored.4m_adh_risedronate.male"):= 0]
rmultinorm[,c("rmultinorm.monitored.4m_adh_strontium.male"):= 0]
rmultinorm[,c("rmultinorm.monitored.4m_adh_ibandronate.male"):= 0]
rmultinorm[,c("rmultinorm.monitored.4m_adh_denosumab.male"):= 0]
rmultinorm[,c("rmultinorm.monitored.4m_adh_zoledronate.male"):= 0]
rmultinorm[,c("rmultinorm.monitored.4m_adh_teriparatide.male"):= 0]
rmultinorm[,c("rmultinorm.monitored.4m_adh_abaloparatide.male"):= 0]
rmultinorm[,c("rmultinorm.monitored.4m_adh_romo.male"):= 0]
rmultinorm[,c("rmultinorm.monitored.12m_adh_alendronate.male"):= 0]
rmultinorm[,c("rmultinorm.monitored.12m_adh_risedronate.male"):= 0]
rmultinorm[,c("rmultinorm.monitored.12m_adh_strontium.male"):= 0]
rmultinorm[,c("rmultinorm.monitored.12m_adh_ibandronate.male"):= 0]
rmultinorm[,c("rmultinorm.monitored.12m_adh_denosumab.male"):= 0]
rmultinorm[,c("rmultinorm.monitored.12m_adh_zoledronate.male"):= 0]
rmultinorm[,c("rmultinorm.monitored.12m_adh_teriparatide.male"):= 0]
rmultinorm[,c("rmultinorm.monitored.12m_adh_abaloparatide.male"):= 0]
rmultinorm[,c("rmultinorm.monitored.4m_adh_alendronate.female"):= 0]
rmultinorm[,c("rmultinorm.monitored.4m_adh_risedronate.female"):= 0]
rmultinorm[,c("rmultinorm.monitored.4m_adh_strontium.female"):= 0]
rmultinorm[,c("rmultinorm.monitored.4m_adh_ibandronate.female"):= 0]
rmultinorm[,c("rmultinorm.monitored.4m_adh_denosumab.female"):= 0]
rmultinorm[,c("rmultinorm.monitored.4m_adh_zoledronate.female"):= 0]
rmultinorm[,c("rmultinorm.monitored.4m_adh_teriparatide.female"):= 0]
rmultinorm[,c("rmultinorm.monitored.4m_adh_abaloparatide.female"):= 0]
rmultinorm[,c("rmultinorm.monitored.4m_adh_romo.female"):= 0]
rmultinorm[,c("rmultinorm.monitored.12m_adh_alendronate.female"):= 0]
rmultinorm[,c("rmultinorm.monitored.12m_adh_risedronate.female"):= 0]
rmultinorm[,c("rmultinorm.monitored.12m_adh_strontium.female"):= 0]
rmultinorm[,c("rmultinorm.monitored.12m_adh_ibandronate.female"):= 0]
rmultinorm[,c("rmultinorm.monitored.12m_adh_denosumab.female"):= 0]
rmultinorm[,c("rmultinorm.monitored.12m_adh_zoledronate.female"):= 0]
rmultinorm[,c("rmultinorm.monitored.12m_adh_teriparatide.female"):= 0]
rmultinorm[,c("rmultinorm.monitored.12m_adh_abaloparatide.female"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_alendronate.spine.no.fls.male"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_alendronate.spine.fls.male"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_risedronate.spine.no.fls.male"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_risedronate.spine.fls.male"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_strontium.spine.no.fls.male"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_strontium.spine.fls.male"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_ibandronate.spine.no.fls.male"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_ibandronate.spine.fls.male"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_denosumab.spine.no.fls.male"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_denosumab.spine.fls.male"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_zoledronate.spine.no.fls.male"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_zoledronate.spine.fls.male"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_teriparatide.spine.no.fls.male"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_teriparatide.spine.fls.male"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_abaloparatide.spine.no.fls.male"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_abaloparatide.spine.fls.male"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_romo.spine.no.fls.male"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_romo.spine.fls.male"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_alendronate.hip.no.fls.male"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_alendronate.hip.fls.male"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_risedronate.hip.no.fls.male"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_risedronate.hip.fls.male"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_strontium.hip.no.fls.male"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_strontium.hip.fls.male"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_ibandronate.hip.no.fls.male"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_ibandronate.hip.fls.male"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_denosumab.hip.no.fls.male"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_denosumab.hip.fls.male"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_zoledronate.hip.no.fls.male"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_zoledronate.hip.fls.male"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_teriparatide.hip.no.fls.male"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_teriparatide.hip.fls.male"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_abaloparatide.hip.no.fls.male"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_abaloparatide.hip.fls.male"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_romo.hip.no.fls.male"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_romo.hip.fls.male"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_alendronate.other.no.fls.male"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_alendronate.other.fls.male"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_risedronate.other.no.fls.male"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_risedronate.other.fls.male"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_strontium.other.no.fls.male"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_strontium.other.fls.male"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_ibandronate.other.no.fls.male"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_ibandronate.other.fls.male"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_denosumab.other.no.fls.male"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_denosumab.other.fls.male"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_zoledronate.other.no.fls.male"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_zoledronate.other.fls.male"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_teriparatide.other.no.fls.male"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_teriparatide.other.fls.male"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_abaloparatide.other.no.fls.male"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_abaloparatide.other.fls.male"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_romo.other.no.fls.male"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_romo.other.fls.male"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_alendronate.spine.no.fls.female"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_alendronate.spine.fls.female"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_risedronate.spine.no.fls.female"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_risedronate.spine.fls.female"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_strontium.spine.no.fls.female"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_strontium.spine.fls.female"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_ibandronate.spine.no.fls.female"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_ibandronate.spine.fls.female"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_denosumab.spine.no.fls.female"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_denosumab.spine.fls.female"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_zoledronate.spine.no.fls.female"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_zoledronate.spine.fls.female"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_teriparatide.spine.no.fls.female"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_teriparatide.spine.fls.female"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_abaloparatide.spine.no.fls.female"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_abaloparatide.spine.fls.female"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_romo.spine.no.fls.female"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_romo.spine.fls.female"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_alendronate.hip.no.fls.female"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_alendronate.hip.fls.female"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_risedronate.hip.no.fls.female"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_risedronate.hip.fls.female"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_strontium.hip.no.fls.female"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_strontium.hip.fls.female"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_ibandronate.hip.no.fls.female"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_ibandronate.hip.fls.female"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_denosumab.hip.no.fls.female"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_denosumab.hip.fls.female"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_zoledronate.hip.no.fls.female"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_zoledronate.hip.fls.female"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_teriparatide.hip.no.fls.female"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_teriparatide.hip.fls.female"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_abaloparatide.hip.no.fls.female"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_abaloparatide.hip.fls.female"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_romo.hip.no.fls.female"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_romo.hip.fls.female"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_alendronate.other.no.fls.female"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_alendronate.other.fls.female"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_risedronate.other.no.fls.female"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_risedronate.other.fls.female"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_strontium.other.no.fls.female"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_strontium.other.fls.female"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_ibandronate.other.no.fls.female"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_ibandronate.other.fls.female"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_denosumab.other.no.fls.female"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_denosumab.other.fls.female"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_zoledronate.other.no.fls.female"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_zoledronate.other.fls.female"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_teriparatide.other.no.fls.female"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_teriparatide.other.fls.female"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_abaloparatide.other.no.fls.female"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_abaloparatide.other.fls.female"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_romo.other.no.fls.female"):= 0]
rmultinorm[,c("rmultinorm.primary.adh_romo.other.fls.female"):= 0]
rmultinorm[,c("rmultinorm.not.monitored.4m_adh_alendronate.male"):= 0]
rmultinorm[,c("rmultinorm.adh_annual_decline_alendronate.no.fls.male"):= 0]
rmultinorm[,c("rmultinorm.adh_annual_decline_alendronate.fls.male"):= 0]
rmultinorm[,c("rmultinorm.adh_annual_decline_risedronate.no.fls.male"):= 0]
rmultinorm[,c("rmultinorm.adh_annual_decline_risedronate.fls.male"):= 0]
rmultinorm[,c("rmultinorm.adh_annual_decline_strontium.no.fls.male"):= 0]
rmultinorm[,c("rmultinorm.adh_annual_decline_strontium.fls.male"):= 0]
rmultinorm[,c("rmultinorm.adh_annual_decline_ibandronate.no.fls.male"):= 0]
rmultinorm[,c("rmultinorm.adh_annual_decline_ibandronate.fls.male"):= 0]
rmultinorm[,c("rmultinorm.adh_annual_decline_denosumab.no.fls.male"):= 0]
rmultinorm[,c("rmultinorm.adh_annual_decline_denosumab.fls.male"):= 0]
rmultinorm[,c("rmultinorm.adh_annual_decline_zoledronate.no.fls.male"):= 0]
rmultinorm[,c("rmultinorm.adh_annual_decline_zoledronate.fls.male"):= 0]
rmultinorm[,c("rmultinorm.adh_annual_decline_alendronate.no.fls.female"):= 0]
rmultinorm[,c("rmultinorm.adh_annual_decline_alendronate.fls.female"):= 0]
rmultinorm[,c("rmultinorm.adh_annual_decline_risedronate.no.fls.female"):= 0]
rmultinorm[,c("rmultinorm.adh_annual_decline_risedronate.fls.female"):= 0]
rmultinorm[,c("rmultinorm.adh_annual_decline_strontium.no.fls.female"):= 0]
rmultinorm[,c("rmultinorm.adh_annual_decline_strontium.fls.female"):= 0]
rmultinorm[,c("rmultinorm.adh_annual_decline_ibandronate.no.fls.female"):= 0]
rmultinorm[,c("rmultinorm.adh_annual_decline_ibandronate.fls.female"):= 0]
rmultinorm[,c("rmultinorm.adh_annual_decline_denosumab.no.fls.female"):= 0]
rmultinorm[,c("rmultinorm.adh_annual_decline_denosumab.fls.female"):= 0]
rmultinorm[,c("rmultinorm.adh_annual_decline_zoledronate.no.fls.female"):= 0]
rmultinorm[,c("rmultinorm.adh_annual_decline_zoledronate.fls.female"):= 0]
rmultinorm[,c("rmultinorm.fx.hosp.hip"):= 0]
rmultinorm[,c("rmultinorm.fx.hosp.other"):= 0]
rmultinorm[,c("rmultinorm.fx.hosp.spine"):= 0]
rmultinorm[,c("rmultinorm.fx.hosp.surgery.hip"):= 0]
rmultinorm[,c("rmultinorm.fx.hosp.surgery.other"):= 0]
rmultinorm[,c("rmultinorm.fx.hosp.surgery.spine"):= 0]
rmultinorm[,c("rmultinorm.fx.not.admitted.surgery.spine"):= 0]
rmultinorm[,c("rmultinorm.discharged.hip.all"):= 0]
rmultinorm[,c("rmultinorm.discharged.hip.temp.rehab"):= 0]
rmultinorm[,c("rmultinorm.discharged.hip.from.home.no.support"):= 0]
rmultinorm[,c("rmultinorm.discharged.hip.from.home.support"):= 0]
rmultinorm[,c("rmultinorm.discharged.hip.from.family.home"):= 0]
rmultinorm[,c("rmultinorm.discharged.spine.all"):= 0]
rmultinorm[,c("rmultinorm.discharged.spine.temp.rehab"):= 0]
rmultinorm[,c("rmultinorm.discharged.spine.from.home.no.support"):= 0]
rmultinorm[,c("rmultinorm.discharged.spine.from.home.support"):= 0]
rmultinorm[,c("rmultinorm.discharged.spine.from.family.home"):= 0]
rmultinorm[,c("rmultinorm.discharged.other.all"):= 0]
rmultinorm[,c("rmultinorm.discharged.other.temp.rehab"):= 0]
rmultinorm[,c("rmultinorm.discharged.other.from.home.no.support"):= 0]
rmultinorm[,c("rmultinorm.discharged.other.from.home.support"):= 0]
rmultinorm[,c("rmultinorm.discharged.other.from.family.home"):= 0]
rmultinorm[,c("rmultinorm.lab.test.no.fls.hip"):= 0]
rmultinorm[,c("rmultinorm.lab.test.no.fls.spine"):= 0]
rmultinorm[,c("rmultinorm.lab.test.no.fls.other"):= 0]
rmultinorm[,c("rmultinorm.lab.test.fls.hip"):= 0]
rmultinorm[,c("rmultinorm.lab.test.fls.spine"):= 0]
rmultinorm[,c("rmultinorm.lab.test.fls.other"):= 0]
rmultinorm[,c("rmultinorm.dxa.no.fls.hip"):= 0]
rmultinorm[,c("rmultinorm.dxa.no.fls.spine"):= 0]
rmultinorm[,c("rmultinorm.dxa.no.fls.other"):= 0]
rmultinorm[,c("rmultinorm.dxa.fls.hip"):= 0]
rmultinorm[,c("rmultinorm.dxa.fls.spine"):= 0]
rmultinorm[,c("rmultinorm.dxa.fls.other"):= 0]
rmultinorm[,c("rmultinorm.trans"):= 0]

# add -------



input<-user_inputs
for (i in 1:61) {
print(paste0("get random samples for time: ", i))
# trt type ------
sample<- get.sample.no.fls_fls(
   probs.no.fls=c(input$no.fls.trt.alendronate.spine.male,
            input$no.fls.trt.risedronate.spine.male,
            input$no.fls.trt.strontium.spine.male,
            input$no.fls.trt.ibandronate.spine.male,
            input$no.fls.trt.denosumab.spine.male,
            input$no.fls.trt.zoledronate.spine.male,
            input$no.fls.trt.teriparatide.spine.male,
            input$no.fls.trt.abaloparatide.spine.male,
            input$no.fls.trt.romo.spine.male),
   probs.fls=c(input$fls.trt.alendronate.spine.male,
            input$fls.trt.risedronate.spine.male,
            input$fls.trt.strontium.spine.male,
            input$fls.trt.ibandronate.spine.male,
            input$fls.trt.denosumab.spine.male,
            input$fls.trt.zoledronate.spine.male,
            input$fls.trt.teriparatide.spine.male,
            input$fls.trt.abaloparatide.spine.male,
            input$fls.trt.romo.spine.male)
   )  

rmultinorm[cycle == i, rmultinorm.no.fls.trt.choice.spine.male := unlist(sample[1])] 
rmultinorm[cycle == i, rmultinorm.fls.trt.choice.spine.male := unlist(sample[2])] 
rm(sample)
}










#####


names(rmultinorm)<-
  c("id", "cycle",
)





i<-1

install.packages("bigmemory")
library(bigmemory)


# set up matrix ----
rmultinorm.mat<-matrix(  
   nrow=length(rep(1:nrow(microsim_pop),61)), 
   ncol=303) 
rmultinorm.mat[,1]<-rep(1:nrow(microsim_pop),61) # ids
rmultinorm.mat[,2]<-rep(1:61, times=nrow(microsim_pop)) # cycles
# column names ----
colnames(rmultinorm.mat)<-
  c("id", "cycle",
"rmultinorm.no.fls.trt.choice.spine.male",
"rmultinorm.fls.trt.choice.spine.male","rmultinorm.no.fls.trt.choice.hip.male",
"rmultinorm.fls.trt.choice.hip.male","rmultinorm.no.fls.trt.choice.other.male",
"rmultinorm.fls.trt.choice.other.male","rmultinorm.no.fls.trt.choice.spine.female",
"rmultinorm.fls.trt.choice.spine.female","rmultinorm.no.fls.trt.choice.hip.female",
"rmultinorm.fls.trt.choice.hip.female","rmultinorm.no.fls.trt.choice.other.female",
"rmultinorm.fls.trt.choice.other.female","rmultinorm.no.fls.prob_identification.spine",
"rmultinorm.fls.prob_identification.spine","rmultinorm.no.fls.prob_identification.hip",
"rmultinorm.fls.prob_identification.hip","rmultinorm.no.fls.prob_identification.other",
"rmultinorm.fls.prob_identification.other","rmultinorm.no.fls.trt.spine.male",
"rmultinorm.fls.trt.spine.male","rmultinorm.no.fls.trt.hip.male",
"rmultinorm.fls.trt.hip.male","rmultinorm.no.fls.trt.other.male",
"rmultinorm.fls.trt.other.male","rmultinorm.no.fls.trt.spine.female",
"rmultinorm.fls.trt.spine.female","rmultinorm.no.fls.trt.hip.female",
"rmultinorm.fls.trt.hip.female","rmultinorm.no.fls.trt.other.female",
"rmultinorm.fls.trt.other.female","rmultinorm.romo.to.no.fls.male",
"rmultinorm.romo.to.fls.male","rmultinorm.romo.to.no.fls.female",
"rmultinorm.romo.to.fls.female","rmultinorm.abaloparatide.to.no.fls.male",
"rmultinorm.abaloparatide.to.fls.male","rmultinorm.abaloparatide.to.no.fls.female",
"rmultinorm.abaloparatide.to.fls.female","rmultinorm.teriparatide.to.no.fls.male",
"rmultinorm.teriparatide.to.fls.male","rmultinorm.teriparatide.to.no.fls.female",
"rmultinorm.teriparatide.to.fls.female","rmultinorm.no.fls.monitored.spine.4m.male",
"rmultinorm.fls.monitored.spine.4m.male","rmultinorm.no.fls.monitored.hip.4m.male",
"rmultinorm.fls.monitored.hip.4m.male","rmultinorm.no.fls.monitored.other.4m.male",
"rmultinorm.fls.monitored.other.4m.male","rmultinorm.no.fls.monitored.spine.4m.female",
"rmultinorm.fls.monitored.spine.4m.female","rmultinorm.no.fls.monitored.hip.4m.female",
"rmultinorm.fls.monitored.hip.4m.female","rmultinorm.no.fls.monitored.other.4m.female",
"rmultinorm.fls.monitored.other.4m.female","rmultinorm.no.fls.monitored.spine.12m.male",
"rmultinorm.fls.monitored.spine.12m.male","rmultinorm.no.fls.monitored.hip.12m.male",
"rmultinorm.fls.monitored.hip.12m.male","rmultinorm.no.fls.monitored.other.12m.male",
"rmultinorm.fls.monitored.other.12m.male","rmultinorm.no.fls.monitored.spine.12m.female",
"rmultinorm.fls.monitored.spine.12m.female","rmultinorm.no.fls.monitored.hip.12m.female",
"rmultinorm.fls.monitored.hip.12m.female","rmultinorm.no.fls.monitored.other.12m.female",
"rmultinorm.fls.monitored.other.12m.female","rmultinorm.not.monitored.4m_adh_risedronate.male",
"rmultinorm.not.monitored.4m_adh_strontium.male","rmultinorm.not.monitored.4m_adh_ibandronate.male",
"rmultinorm.not.monitored.4m_adh_denosumab.male","rmultinorm.not.monitored.4m_adh_zoledronate.male",
"rmultinorm.not.monitored.4m_adh_teriparatide.male","rmultinorm.not.monitored.4m_adh_abaloparatide.male",
"rmultinorm.not.monitored.4m_adh_romo.male","rmultinorm.not.monitored.12m_adh_alendronate.male",
"rmultinorm.not.monitored.12m_adh_risedronate.male","rmultinorm.not.monitored.12m_adh_strontium.male",
"rmultinorm.not.monitored.12m_adh_ibandronate.male","rmultinorm.not.monitored.12m_adh_denosumab.male",
"rmultinorm.not.monitored.12m_adh_zoledronate.male","rmultinorm.not.monitored.12m_adh_teriparatide.male",
"rmultinorm.not.monitored.12m_adh_abaloparatide.male","rmultinorm.not.monitored.4m_adh_alendronate.female",
"rmultinorm.not.monitored.4m_adh_risedronate.female","rmultinorm.not.monitored.4m_adh_strontium.female",
"rmultinorm.not.monitored.4m_adh_ibandronate.female","rmultinorm.not.monitored.4m_adh_denosumab.female",
"rmultinorm.not.monitored.4m_adh_zoledronate.female","rmultinorm.not.monitored.4m_adh_teriparatide.female",
"rmultinorm.not.monitored.4m_adh_abaloparatide.female","rmultinorm.not.monitored.4m_adh_romo.female",
"rmultinorm.not.monitored.12m_adh_alendronate.female","rmultinorm.not.monitored.12m_adh_risedronate.female",
"rmultinorm.not.monitored.12m_adh_strontium.female","rmultinorm.not.monitored.12m_adh_ibandronate.female",
"rmultinorm.not.monitored.12m_adh_denosumab.female","rmultinorm.not.monitored.12m_adh_zoledronate.female",
"rmultinorm.not.monitored.12m_adh_teriparatide.female","rmultinorm.not.monitored.12m_adh_abaloparatide.female",
"rmultinorm.monitored.4m_adh_alendronate.male","rmultinorm.monitored.4m_adh_risedronate.male",
"rmultinorm.monitored.4m_adh_strontium.male","rmultinorm.monitored.4m_adh_ibandronate.male",
"rmultinorm.monitored.4m_adh_denosumab.male","rmultinorm.monitored.4m_adh_zoledronate.male",
"rmultinorm.monitored.4m_adh_teriparatide.male","rmultinorm.monitored.4m_adh_abaloparatide.male",
"rmultinorm.monitored.4m_adh_romo.male","rmultinorm.monitored.12m_adh_alendronate.male",
"rmultinorm.monitored.12m_adh_risedronate.male","rmultinorm.monitored.12m_adh_strontium.male",
"rmultinorm.monitored.12m_adh_ibandronate.male","rmultinorm.monitored.12m_adh_denosumab.male",
"rmultinorm.monitored.12m_adh_zoledronate.male","rmultinorm.monitored.12m_adh_teriparatide.male",
"rmultinorm.monitored.12m_adh_abaloparatide.male","rmultinorm.monitored.4m_adh_alendronate.female",
"rmultinorm.monitored.4m_adh_risedronate.female","rmultinorm.monitored.4m_adh_strontium.female",
"rmultinorm.monitored.4m_adh_ibandronate.female","rmultinorm.monitored.4m_adh_denosumab.female",
"rmultinorm.monitored.4m_adh_zoledronate.female","rmultinorm.monitored.4m_adh_teriparatide.female",
"rmultinorm.monitored.4m_adh_abaloparatide.female","rmultinorm.monitored.4m_adh_romo.female",
"rmultinorm.monitored.12m_adh_alendronate.female","rmultinorm.monitored.12m_adh_risedronate.female",
"rmultinorm.monitored.12m_adh_strontium.female","rmultinorm.monitored.12m_adh_ibandronate.female",
"rmultinorm.monitored.12m_adh_denosumab.female","rmultinorm.monitored.12m_adh_zoledronate.female",
"rmultinorm.monitored.12m_adh_teriparatide.female","rmultinorm.monitored.12m_adh_abaloparatide.female",
"rmultinorm.primary.adh_alendronate.spine.no.fls.male","rmultinorm.primary.adh_alendronate.spine.fls.male",
"rmultinorm.primary.adh_risedronate.spine.no.fls.male","rmultinorm.primary.adh_risedronate.spine.fls.male",
"rmultinorm.primary.adh_strontium.spine.no.fls.male","rmultinorm.primary.adh_strontium.spine.fls.male",
"rmultinorm.primary.adh_ibandronate.spine.no.fls.male","rmultinorm.primary.adh_ibandronate.spine.fls.male",
"rmultinorm.primary.adh_denosumab.spine.no.fls.male","rmultinorm.primary.adh_denosumab.spine.fls.male",
"rmultinorm.primary.adh_zoledronate.spine.no.fls.male","rmultinorm.primary.adh_zoledronate.spine.fls.male",
"rmultinorm.primary.adh_teriparatide.spine.no.fls.male","rmultinorm.primary.adh_teriparatide.spine.fls.male",
"rmultinorm.primary.adh_abaloparatide.spine.no.fls.male","rmultinorm.primary.adh_abaloparatide.spine.fls.male",
"rmultinorm.primary.adh_romo.spine.no.fls.male","rmultinorm.primary.adh_romo.spine.fls.male",
"rmultinorm.primary.adh_alendronate.hip.no.fls.male","rmultinorm.primary.adh_alendronate.hip.fls.male",
"rmultinorm.primary.adh_risedronate.hip.no.fls.male","rmultinorm.primary.adh_risedronate.hip.fls.male",
"rmultinorm.primary.adh_strontium.hip.no.fls.male","rmultinorm.primary.adh_strontium.hip.fls.male",
"rmultinorm.primary.adh_ibandronate.hip.no.fls.male","rmultinorm.primary.adh_ibandronate.hip.fls.male",
"rmultinorm.primary.adh_denosumab.hip.no.fls.male","rmultinorm.primary.adh_denosumab.hip.fls.male",
"rmultinorm.primary.adh_zoledronate.hip.no.fls.male","rmultinorm.primary.adh_zoledronate.hip.fls.male",
"rmultinorm.primary.adh_teriparatide.hip.no.fls.male","rmultinorm.primary.adh_teriparatide.hip.fls.male",
"rmultinorm.primary.adh_abaloparatide.hip.no.fls.male","rmultinorm.primary.adh_abaloparatide.hip.fls.male",
"rmultinorm.primary.adh_romo.hip.no.fls.male","rmultinorm.primary.adh_romo.hip.fls.male",
"rmultinorm.primary.adh_alendronate.other.no.fls.male","rmultinorm.primary.adh_alendronate.other.fls.male",
"rmultinorm.primary.adh_risedronate.other.no.fls.male","rmultinorm.primary.adh_risedronate.other.fls.male",
"rmultinorm.primary.adh_strontium.other.no.fls.male","rmultinorm.primary.adh_strontium.other.fls.male",
"rmultinorm.primary.adh_ibandronate.other.no.fls.male","rmultinorm.primary.adh_ibandronate.other.fls.male",
"rmultinorm.primary.adh_denosumab.other.no.fls.male","rmultinorm.primary.adh_denosumab.other.fls.male",
"rmultinorm.primary.adh_zoledronate.other.no.fls.male","rmultinorm.primary.adh_zoledronate.other.fls.male",
"rmultinorm.primary.adh_teriparatide.other.no.fls.male","rmultinorm.primary.adh_teriparatide.other.fls.male",
"rmultinorm.primary.adh_abaloparatide.other.no.fls.male","rmultinorm.primary.adh_abaloparatide.other.fls.male",
"rmultinorm.primary.adh_romo.other.no.fls.male","rmultinorm.primary.adh_romo.other.fls.male",
"rmultinorm.primary.adh_alendronate.spine.no.fls.female","rmultinorm.primary.adh_alendronate.spine.fls.female",
"rmultinorm.primary.adh_risedronate.spine.no.fls.female","rmultinorm.primary.adh_risedronate.spine.fls.female",
"rmultinorm.primary.adh_strontium.spine.no.fls.female","rmultinorm.primary.adh_strontium.spine.fls.female",
"rmultinorm.primary.adh_ibandronate.spine.no.fls.female","rmultinorm.primary.adh_ibandronate.spine.fls.female",
"rmultinorm.primary.adh_denosumab.spine.no.fls.female","rmultinorm.primary.adh_denosumab.spine.fls.female",
"rmultinorm.primary.adh_zoledronate.spine.no.fls.female","rmultinorm.primary.adh_zoledronate.spine.fls.female",
"rmultinorm.primary.adh_teriparatide.spine.no.fls.female","rmultinorm.primary.adh_teriparatide.spine.fls.female",
"rmultinorm.primary.adh_abaloparatide.spine.no.fls.female","rmultinorm.primary.adh_abaloparatide.spine.fls.female",
"rmultinorm.primary.adh_romo.spine.no.fls.female","rmultinorm.primary.adh_romo.spine.fls.female",
"rmultinorm.primary.adh_alendronate.hip.no.fls.female","rmultinorm.primary.adh_alendronate.hip.fls.female",
"rmultinorm.primary.adh_risedronate.hip.no.fls.female","rmultinorm.primary.adh_risedronate.hip.fls.female",
"rmultinorm.primary.adh_strontium.hip.no.fls.female","rmultinorm.primary.adh_strontium.hip.fls.female",
"rmultinorm.primary.adh_ibandronate.hip.no.fls.female","rmultinorm.primary.adh_ibandronate.hip.fls.female",
"rmultinorm.primary.adh_denosumab.hip.no.fls.female","rmultinorm.primary.adh_denosumab.hip.fls.female",
"rmultinorm.primary.adh_zoledronate.hip.no.fls.female","rmultinorm.primary.adh_zoledronate.hip.fls.female",
"rmultinorm.primary.adh_teriparatide.hip.no.fls.female","rmultinorm.primary.adh_teriparatide.hip.fls.female",
"rmultinorm.primary.adh_abaloparatide.hip.no.fls.female","rmultinorm.primary.adh_abaloparatide.hip.fls.female",
"rmultinorm.primary.adh_romo.hip.no.fls.female","rmultinorm.primary.adh_romo.hip.fls.female",
"rmultinorm.primary.adh_alendronate.other.no.fls.female","rmultinorm.primary.adh_alendronate.other.fls.female",
"rmultinorm.primary.adh_risedronate.other.no.fls.female","rmultinorm.primary.adh_risedronate.other.fls.female",
"rmultinorm.primary.adh_strontium.other.no.fls.female","rmultinorm.primary.adh_strontium.other.fls.female",
"rmultinorm.primary.adh_ibandronate.other.no.fls.female","rmultinorm.primary.adh_ibandronate.other.fls.female",
"rmultinorm.primary.adh_denosumab.other.no.fls.female","rmultinorm.primary.adh_denosumab.other.fls.female",
"rmultinorm.primary.adh_zoledronate.other.no.fls.female","rmultinorm.primary.adh_zoledronate.other.fls.female",
"rmultinorm.primary.adh_teriparatide.other.no.fls.female","rmultinorm.primary.adh_teriparatide.other.fls.female",
"rmultinorm.primary.adh_abaloparatide.other.no.fls.female","rmultinorm.primary.adh_abaloparatide.other.fls.female",
"rmultinorm.primary.adh_romo.other.no.fls.female","rmultinorm.primary.adh_romo.other.fls.female",
"rmultinorm.not.monitored.4m_adh_alendronate.male","rmultinorm.adh_annual_decline_alendronate.no.fls.male",
"rmultinorm.adh_annual_decline_alendronate.fls.male","rmultinorm.adh_annual_decline_risedronate.no.fls.male",
"rmultinorm.adh_annual_decline_risedronate.fls.male","rmultinorm.adh_annual_decline_strontium.no.fls.male",
"rmultinorm.adh_annual_decline_strontium.fls.male","rmultinorm.adh_annual_decline_ibandronate.no.fls.male",
"rmultinorm.adh_annual_decline_ibandronate.fls.male","rmultinorm.adh_annual_decline_denosumab.no.fls.male",
"rmultinorm.adh_annual_decline_denosumab.fls.male","rmultinorm.adh_annual_decline_zoledronate.no.fls.male",
"rmultinorm.adh_annual_decline_zoledronate.fls.male","rmultinorm.adh_annual_decline_alendronate.no.fls.female",
"rmultinorm.adh_annual_decline_alendronate.fls.female","rmultinorm.adh_annual_decline_risedronate.no.fls.female",
"rmultinorm.adh_annual_decline_risedronate.fls.female","rmultinorm.adh_annual_decline_strontium.no.fls.female",
"rmultinorm.adh_annual_decline_strontium.fls.female","rmultinorm.adh_annual_decline_ibandronate.no.fls.female",
"rmultinorm.adh_annual_decline_ibandronate.fls.female","rmultinorm.adh_annual_decline_denosumab.no.fls.female",
"rmultinorm.adh_annual_decline_denosumab.fls.female","rmultinorm.adh_annual_decline_zoledronate.no.fls.female",
"rmultinorm.adh_annual_decline_zoledronate.fls.female","rmultinorm.fx.hosp.hip",
"rmultinorm.fx.hosp.other","rmultinorm.fx.hosp.spine",
"rmultinorm.fx.hosp.surgery.hip","rmultinorm.fx.hosp.surgery.other",
"rmultinorm.fx.hosp.surgery.spine","rmultinorm.fx.not.admitted.surgery.spine",
"rmultinorm.discharged.hip.all","rmultinorm.discharged.hip.temp.rehab",
"rmultinorm.discharged.hip.from.home.no.support","rmultinorm.discharged.hip.from.home.support",
"rmultinorm.discharged.hip.from.family.home","rmultinorm.discharged.spine.all",
"rmultinorm.discharged.spine.temp.rehab","rmultinorm.discharged.spine.from.home.no.support",
"rmultinorm.discharged.spine.from.home.support","rmultinorm.discharged.spine.from.family.home",
"rmultinorm.discharged.other.all","rmultinorm.discharged.other.temp.rehab",
"rmultinorm.discharged.other.from.home.no.support","rmultinorm.discharged.other.from.home.support",
"rmultinorm.discharged.other.from.family.home","rmultinorm.lab.test.no.fls.hip",
"rmultinorm.lab.test.no.fls.spine","rmultinorm.lab.test.no.fls.other",
"rmultinorm.lab.test.fls.hip","rmultinorm.lab.test.fls.spine",
"rmultinorm.lab.test.fls.other","rmultinorm.dxa.no.fls.hip",
"rmultinorm.dxa.no.fls.spine","rmultinorm.dxa.no.fls.other",
"rmultinorm.dxa.fls.hip","rmultinorm.dxa.fls.spine",
"rmultinorm.dxa.fls.other","rmultinorm.trans")

# ----
i<-1
input<-user_inputs
for (i in 1:61) {
# trt type ------
sample<- get.sample.no.fls_fls(
   probs.no.fls=c(input$no.fls.trt.alendronate.spine.male,
            input$no.fls.trt.risedronate.spine.male,
            input$no.fls.trt.strontium.spine.male,
            input$no.fls.trt.ibandronate.spine.male,
            input$no.fls.trt.denosumab.spine.male,
            input$no.fls.trt.zoledronate.spine.male,
            input$no.fls.trt.teriparatide.spine.male,
            input$no.fls.trt.abaloparatide.spine.male,
            input$no.fls.trt.romo.spine.male),
   probs.fls=c(input$fls.trt.alendronate.spine.male,
            input$fls.trt.risedronate.spine.male,
            input$fls.trt.strontium.spine.male,
            input$fls.trt.ibandronate.spine.male,
            input$fls.trt.denosumab.spine.male,
            input$fls.trt.zoledronate.spine.male,
            input$fls.trt.teriparatide.spine.male,
            input$fls.trt.abaloparatide.spine.male,
            input$fls.trt.romo.spine.male)
   )  

rmultinorm.mat[which(rmultinorm.mat[,"cycle"]=={{i}}),][,"rmultinorm.no.fls.trt.choice.spine.male"]<-unlist(sample[1])
rmultinorm.mat[which(rmultinorm.mat[,"cycle"]=={{i}}),][,"rmultinorm.fls.trt.choice.spine.male"]<-unlist(sample[2])

}


















# data frame with each id for each cycle
rmultinorm<-data.frame(cycle= rep(1:61,
                          times=nrow(microsim_pop)),
                          id=rep(1:nrow(microsim_pop),61))
# set up required vars
rmultinorm<-rmultinorm %>% 
  mutate(rmultinorm.no.fls.trt.choice.spine.male=NA,
         rmultinorm.fls.trt.choice.spine.male=NA) 



i<-1
input<-user_inputs

# trt type ------
sample<- get.sample.no.fls_fls(
   probs.no.fls=c(input$no.fls.trt.alendronate.spine.male,
            input$no.fls.trt.risedronate.spine.male,
            input$no.fls.trt.strontium.spine.male,
            input$no.fls.trt.ibandronate.spine.male,
            input$no.fls.trt.denosumab.spine.male,
            input$no.fls.trt.zoledronate.spine.male,
            input$no.fls.trt.teriparatide.spine.male,
            input$no.fls.trt.abaloparatide.spine.male,
            input$no.fls.trt.romo.spine.male),
   probs.fls=c(input$fls.trt.alendronate.spine.male,
            input$fls.trt.risedronate.spine.male,
            input$fls.trt.strontium.spine.male,
            input$fls.trt.ibandronate.spine.male,
            input$fls.trt.denosumab.spine.male,
            input$fls.trt.zoledronate.spine.male,
            input$fls.trt.teriparatide.spine.male,
            input$fls.trt.abaloparatide.spine.male,
            input$fls.trt.romo.spine.male)
   )  


rmultinorm<-rmultinorm %>% 
  mutate(rmultinorm.no.fls.trt.choice.spine.male=
           ifelse(cycle=={{i}}, unlist(sample[1]), rmultinorm.no.fls.trt.choice.spine.male))
rmultinorm<-rmultinorm %>% 
  mutate(rmultinorm.fls.trt.choice.spine.male=
           ifelse(cycle=={{i}}, unlist(sample[2]), rmultinorm.fls.trt.choice.spine.male))





                              
for (i in 1:61) {
  print(paste0("get random samples for time: ", i))
  rmultinorm[[i]]<-get.rmultinorm1(input=user_inputs, n_microsimulation=n_microsimulation, microsim_pop=microsim_pop)
}
rmultinorm<-bind_rows(rmultinorm, .id="cycle")

# ----
get.rmultinorm1<-function(rmultinorm,input, n_microsimulation, microsim_pop){
   rmultinorm<-data.frame(id=1:nrow(microsim_pop))
  # browser()
  
# trt type ------
sample<- get.sample.no.fls_fls(
   probs.no.fls=c(input$no.fls.trt.alendronate.spine.male,
            input$no.fls.trt.risedronate.spine.male,
            input$no.fls.trt.strontium.spine.male,
            input$no.fls.trt.ibandronate.spine.male,
            input$no.fls.trt.denosumab.spine.male,
            input$no.fls.trt.zoledronate.spine.male,
            input$no.fls.trt.teriparatide.spine.male,
            input$no.fls.trt.abaloparatide.spine.male,
            input$no.fls.trt.romo.spine.male),
   probs.fls=c(input$fls.trt.alendronate.spine.male,
            input$fls.trt.risedronate.spine.male,
            input$fls.trt.strontium.spine.male,
            input$fls.trt.ibandronate.spine.male,
            input$fls.trt.denosumab.spine.male,
            input$fls.trt.zoledronate.spine.male,
            input$fls.trt.teriparatide.spine.male,
            input$fls.trt.abaloparatide.spine.male,
            input$fls.trt.romo.spine.male)
   )  

rmultinorm$rmultinorm.no.fls.trt.choice.spine.male<-unlist(sample[1])
rmultinorm$rmultinorm.fls.trt.choice.spine.male<-unlist(sample[2])
 
sample<- get.sample.no.fls_fls(
   probs.no.fls=c(input$no.fls.trt.alendronate.hip.male,
            input$no.fls.trt.risedronate.hip.male,
            input$no.fls.trt.strontium.hip.male,
            input$no.fls.trt.ibandronate.hip.male,
            input$no.fls.trt.denosumab.hip.male,
            input$no.fls.trt.zoledronate.hip.male,
            input$no.fls.trt.teriparatide.hip.male,
            input$no.fls.trt.abaloparatide.hip.male,
            input$no.fls.trt.romo.hip.male),
   probs.fls=c(input$fls.trt.alendronate.hip.male,
            input$fls.trt.risedronate.hip.male,
            input$fls.trt.strontium.hip.male,
            input$fls.trt.ibandronate.hip.male,
            input$fls.trt.denosumab.hip.male,
            input$fls.trt.zoledronate.hip.male,
            input$fls.trt.teriparatide.hip.male,
            input$fls.trt.abaloparatide.hip.male,
            input$fls.trt.romo.hip.male)
   )  

rmultinorm$rmultinorm.no.fls.trt.choice.hip.male<-unlist(sample[1])
rmultinorm$rmultinorm.fls.trt.choice.hip.male<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(input$no.fls.trt.alendronate.other.male,
            input$no.fls.trt.risedronate.other.male,
            input$no.fls.trt.strontium.other.male,
            input$no.fls.trt.ibandronate.other.male,
            input$no.fls.trt.denosumab.other.male,
            input$no.fls.trt.zoledronate.other.male,
            input$no.fls.trt.teriparatide.other.male,
            input$no.fls.trt.abaloparatide.other.male,
            input$no.fls.trt.romo.other.male),
   probs.fls=c(input$fls.trt.alendronate.other.male,
            input$fls.trt.risedronate.other.male,
            input$fls.trt.strontium.other.male,
            input$fls.trt.ibandronate.other.male,
            input$fls.trt.denosumab.other.male,
            input$fls.trt.zoledronate.other.male,
            input$fls.trt.teriparatide.other.male,
            input$fls.trt.abaloparatide.other.male,
            input$fls.trt.romo.other.male)
   )  

rmultinorm$rmultinorm.no.fls.trt.choice.other.male<-unlist(sample[1])
rmultinorm$rmultinorm.fls.trt.choice.other.male<-unlist(sample[2])





sample<- get.sample.no.fls_fls(
   probs.no.fls=c(input$no.fls.trt.alendronate.spine.female,
            input$no.fls.trt.risedronate.spine.female,
            input$no.fls.trt.strontium.spine.female,
            input$no.fls.trt.ibandronate.spine.female,
            input$no.fls.trt.denosumab.spine.female,
            input$no.fls.trt.zoledronate.spine.female,
            input$no.fls.trt.teriparatide.spine.female,
            input$no.fls.trt.abaloparatide.spine.female,
            input$no.fls.trt.romo.spine.female),
   probs.fls=c(input$fls.trt.alendronate.spine.female,
            input$fls.trt.risedronate.spine.female,
            input$fls.trt.strontium.spine.female,
            input$fls.trt.ibandronate.spine.female,
            input$fls.trt.denosumab.spine.female,
            input$fls.trt.zoledronate.spine.female,
            input$fls.trt.teriparatide.spine.female,
            input$fls.trt.abaloparatide.spine.female,
            input$fls.trt.romo.spine.female)
   )  

rmultinorm$rmultinorm.no.fls.trt.choice.spine.female<-unlist(sample[1])
rmultinorm$rmultinorm.fls.trt.choice.spine.female<-unlist(sample[2])
 
sample<- get.sample.no.fls_fls(
   probs.no.fls=c(input$no.fls.trt.alendronate.hip.female,
            input$no.fls.trt.risedronate.hip.female,
            input$no.fls.trt.strontium.hip.female,
            input$no.fls.trt.ibandronate.hip.female,
            input$no.fls.trt.denosumab.hip.female,
            input$no.fls.trt.zoledronate.hip.female,
            input$no.fls.trt.teriparatide.hip.female,
            input$no.fls.trt.abaloparatide.hip.female,
            input$no.fls.trt.romo.hip.female),
   probs.fls=c(input$fls.trt.alendronate.hip.female,
            input$fls.trt.risedronate.hip.female,
            input$fls.trt.strontium.hip.female,
            input$fls.trt.ibandronate.hip.female,
            input$fls.trt.denosumab.hip.female,
            input$fls.trt.zoledronate.hip.female,
            input$fls.trt.teriparatide.hip.female,
            input$fls.trt.abaloparatide.hip.female,
            input$fls.trt.romo.hip.female)
   )  

rmultinorm$rmultinorm.no.fls.trt.choice.hip.female<-unlist(sample[1])
rmultinorm$rmultinorm.fls.trt.choice.hip.female<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(input$no.fls.trt.alendronate.other.female,
            input$no.fls.trt.risedronate.other.female,
            input$no.fls.trt.strontium.other.female,
            input$no.fls.trt.ibandronate.other.female,
            input$no.fls.trt.denosumab.other.female,
            input$no.fls.trt.zoledronate.other.female,
            input$no.fls.trt.teriparatide.other.female,
            input$no.fls.trt.abaloparatide.other.female,
            input$no.fls.trt.romo.other.female),
   probs.fls=c(input$fls.trt.alendronate.other.female,
            input$fls.trt.risedronate.other.female,
            input$fls.trt.strontium.other.female,
            input$fls.trt.ibandronate.other.female,
            input$fls.trt.denosumab.other.female,
            input$fls.trt.zoledronate.other.female,
            input$fls.trt.teriparatide.other.female,
            input$fls.trt.abaloparatide.other.female,
            input$fls.trt.romo.other.female)
   )  

rmultinorm$rmultinorm.no.fls.trt.choice.other.female<-unlist(sample[1])
rmultinorm$rmultinorm.fls.trt.choice.other.female<-unlist(sample[2])

# identification  ------

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.prob_identification.spine,
            input$no.fls.prob_identification.spine),
   probs.fls=c(1-input$fls.prob_identification.spine,
            input$fls.prob_identification.spine))  
rmultinorm$rmultinorm.no.fls.prob_identification.spine<-unlist(sample[1])
rmultinorm$rmultinorm.fls.prob_identification.spine<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.prob_identification.hip,
            input$no.fls.prob_identification.hip),
   probs.fls=c(1-input$fls.prob_identification.hip,
            input$fls.prob_identification.hip))  
rmultinorm$rmultinorm.no.fls.prob_identification.hip<-unlist(sample[1])
rmultinorm$rmultinorm.fls.prob_identification.hip<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.prob_identification.other,
            input$no.fls.prob_identification.other),
   probs.fls=c(1-input$fls.prob_identification.other,
            input$fls.prob_identification.other))  
rmultinorm$rmultinorm.no.fls.prob_identification.other<-unlist(sample[1])
rmultinorm$rmultinorm.fls.prob_identification.other<-unlist(sample[2])


# trt  ------


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.trt.spine.male,
            input$no.fls.trt.spine.male),
   probs.fls=c(1-input$fls.trt.spine.male,
            input$fls.trt.spine.male))  
rmultinorm$rmultinorm.no.fls.trt.spine.male<-unlist(sample[1])
rmultinorm$rmultinorm.fls.trt.spine.male<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.trt.hip.male,
            input$no.fls.trt.hip.male),
   probs.fls=c(1-input$fls.trt.hip.male,
            input$fls.trt.hip.male))  
rmultinorm$rmultinorm.no.fls.trt.hip.male<-unlist(sample[1])
rmultinorm$rmultinorm.fls.trt.hip.male<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.trt.other.male,
            input$no.fls.trt.other.male),
   probs.fls=c(1-input$fls.trt.other.male,
            input$fls.trt.other.male))  
rmultinorm$rmultinorm.no.fls.trt.other.male<-unlist(sample[1])
rmultinorm$rmultinorm.fls.trt.other.male<-unlist(sample[2])



sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.trt.spine.female,
            input$no.fls.trt.spine.female),
   probs.fls=c(1-input$fls.trt.spine.female,
            input$fls.trt.spine.female))  
rmultinorm$rmultinorm.no.fls.trt.spine.female<-unlist(sample[1])
rmultinorm$rmultinorm.fls.trt.spine.female<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.trt.hip.female,
            input$no.fls.trt.hip.female),
   probs.fls=c(1-input$fls.trt.hip.female,
            input$fls.trt.hip.female))  
rmultinorm$rmultinorm.no.fls.trt.hip.female<-unlist(sample[1])
rmultinorm$rmultinorm.fls.trt.hip.female<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.trt.other.female,
            input$no.fls.trt.other.female),
   probs.fls=c(1-input$fls.trt.other.female,
            input$fls.trt.other.female))  
rmultinorm$rmultinorm.no.fls.trt.other.female<-unlist(sample[1])
rmultinorm$rmultinorm.fls.trt.other.female<-unlist(sample[2])


# romo.to  ------


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(
            input$romo.to.nothing.no.fls.male,
            input$romo.to.alendronate.no.fls.male*(1-input$romo.to.nothing.no.fls.male),
            input$romo.to.risedronate.no.fls.male*(1-input$romo.to.nothing.no.fls.male),
            input$romo.to.strontium.no.fls.male*(1-input$romo.to.nothing.no.fls.male),
            input$romo.to.ibandronate.no.fls.male*(1-input$romo.to.nothing.no.fls.male),
            input$romo.to.denosumab.no.fls.male*(1-input$romo.to.nothing.no.fls.male),
            input$romo.to.zoledronate.no.fls.male*(1-input$romo.to.nothing.no.fls.male)),
   probs.fls=c(
            input$romo.to.nothing.fls.male,
            input$romo.to.alendronate.fls.male*(1-input$romo.to.nothing.no.fls.male),
            input$romo.to.risedronate.fls.male*(1-input$romo.to.nothing.no.fls.male),
            input$romo.to.strontium.fls.male*(1-input$romo.to.nothing.no.fls.male),
            input$romo.to.ibandronate.fls.male*(1-input$romo.to.nothing.no.fls.male),
            input$romo.to.denosumab.fls.male*(1-input$romo.to.nothing.no.fls.male),
            input$romo.to.zoledronate.fls.male*(1-input$romo.to.nothing.no.fls.male))
   )  

rmultinorm$rmultinorm.romo.to.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.romo.to.fls.male<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(
            input$romo.to.nothing.no.fls.female,
            input$romo.to.alendronate.no.fls.female*(1-input$romo.to.nothing.no.fls.female),
            input$romo.to.risedronate.no.fls.female*(1-input$romo.to.nothing.no.fls.female),
            input$romo.to.strontium.no.fls.female*(1-input$romo.to.nothing.no.fls.female),
            input$romo.to.ibandronate.no.fls.female*(1-input$romo.to.nothing.no.fls.female),
            input$romo.to.denosumab.no.fls.female*(1-input$romo.to.nothing.no.fls.female),
            input$romo.to.zoledronate.no.fls.female*(1-input$romo.to.nothing.no.fls.female)),
   probs.fls=c(
            input$romo.to.nothing.fls.female,
            input$romo.to.alendronate.fls.female*(1-input$romo.to.nothing.no.fls.female),
            input$romo.to.risedronate.fls.female*(1-input$romo.to.nothing.no.fls.female),
            input$romo.to.strontium.fls.female*(1-input$romo.to.nothing.no.fls.female),
            input$romo.to.ibandronate.fls.female*(1-input$romo.to.nothing.no.fls.female),
            input$romo.to.denosumab.fls.female*(1-input$romo.to.nothing.no.fls.female),
            input$romo.to.zoledronate.fls.female*(1-input$romo.to.nothing.no.fls.female))
   )  

rmultinorm$rmultinorm.romo.to.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.romo.to.fls.female<-unlist(sample[2])

# abaloparatide.to  ------

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(
            input$abaloparatide.to.nothing.no.fls.male,
            input$abaloparatide.to.alendronate.no.fls.male*(1-input$abaloparatide.to.nothing.no.fls.male),
            input$abaloparatide.to.risedronate.no.fls.male*(1-input$abaloparatide.to.nothing.no.fls.male),
            input$abaloparatide.to.strontium.no.fls.male*(1-input$abaloparatide.to.nothing.no.fls.male),
            input$abaloparatide.to.ibandronate.no.fls.male*(1-input$abaloparatide.to.nothing.no.fls.male),
            input$abaloparatide.to.denosumab.no.fls.male*(1-input$abaloparatide.to.nothing.no.fls.male),
            input$abaloparatide.to.zoledronate.no.fls.male*(1-input$abaloparatide.to.nothing.no.fls.male)),
   probs.fls=c(
            input$abaloparatide.to.nothing.fls.male,
            input$abaloparatide.to.alendronate.fls.male*(1-input$abaloparatide.to.nothing.fls.male),
            input$abaloparatide.to.risedronate.fls.male*(1-input$abaloparatide.to.nothing.fls.male),
            input$abaloparatide.to.strontium.fls.male*(1-input$abaloparatide.to.nothing.fls.male),
            input$abaloparatide.to.ibandronate.fls.male*(1-input$abaloparatide.to.nothing.fls.male),
            input$abaloparatide.to.denosumab.fls.male*(1-input$abaloparatide.to.nothing.fls.male),
            input$abaloparatide.to.zoledronate.fls.male*(1-input$abaloparatide.to.nothing.fls.male))
   )  

rmultinorm$rmultinorm.abaloparatide.to.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.abaloparatide.to.fls.male<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(
            input$abaloparatide.to.nothing.no.fls.female,
            input$abaloparatide.to.alendronate.no.fls.female*(1-input$abaloparatide.to.nothing.no.fls.female),
            input$abaloparatide.to.risedronate.no.fls.female*(1-input$abaloparatide.to.nothing.no.fls.female),
            input$abaloparatide.to.strontium.no.fls.female*(1-input$abaloparatide.to.nothing.no.fls.female),
            input$abaloparatide.to.ibandronate.no.fls.female*(1-input$abaloparatide.to.nothing.no.fls.female),
            input$abaloparatide.to.denosumab.no.fls.female*(1-input$abaloparatide.to.nothing.no.fls.female),
            input$abaloparatide.to.zoledronate.no.fls.female*(1-input$abaloparatide.to.nothing.no.fls.female)),
   probs.fls=c(
            input$abaloparatide.to.nothing.fls.female,
            input$abaloparatide.to.alendronate.fls.female*(1-input$abaloparatide.to.nothing.fls.female),
            input$abaloparatide.to.risedronate.fls.female*(1-input$abaloparatide.to.nothing.fls.female),
            input$abaloparatide.to.strontium.fls.female*(1-input$abaloparatide.to.nothing.fls.female),
            input$abaloparatide.to.ibandronate.fls.female*(1-input$abaloparatide.to.nothing.fls.female),
            input$abaloparatide.to.denosumab.fls.female*(1-input$abaloparatide.to.nothing.fls.female),
            input$abaloparatide.to.zoledronate.fls.female*(1-input$abaloparatide.to.nothing.fls.female))
   )  

rmultinorm$rmultinorm.abaloparatide.to.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.abaloparatide.to.fls.female<-unlist(sample[2])



# teriparatide.to  ------
sample<- get.sample.no.fls_fls(
   probs.no.fls=c(
            input$teriparatide.to.nothing.no.fls.male,
            input$teriparatide.to.alendronate.no.fls.male*(1-input$teriparatide.to.nothing.no.fls.male),
            input$teriparatide.to.risedronate.no.fls.male*(1-input$teriparatide.to.nothing.no.fls.male),
            input$teriparatide.to.strontium.no.fls.male*(1-input$teriparatide.to.nothing.no.fls.male),
            input$teriparatide.to.ibandronate.no.fls.male*(1-input$teriparatide.to.nothing.no.fls.male),
            input$teriparatide.to.denosumab.no.fls.male*(1-input$teriparatide.to.nothing.no.fls.male),
            input$teriparatide.to.zoledronate.no.fls.male*(1-input$teriparatide.to.nothing.no.fls.male)),
   probs.fls=c(
            input$teriparatide.to.nothing.fls.male,
            input$teriparatide.to.alendronate.fls.male*(1-input$teriparatide.to.nothing.fls.male),
            input$teriparatide.to.risedronate.fls.male*(1-input$teriparatide.to.nothing.fls.male),
            input$teriparatide.to.strontium.fls.male*(1-input$teriparatide.to.nothing.fls.male),
            input$teriparatide.to.ibandronate.fls.male*(1-input$teriparatide.to.nothing.fls.male),
            input$teriparatide.to.denosumab.fls.male*(1-input$teriparatide.to.nothing.fls.male),
            input$teriparatide.to.zoledronate.fls.male*(1-input$teriparatide.to.nothing.fls.male))
   )  

rmultinorm$rmultinorm.teriparatide.to.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.teriparatide.to.fls.male<-unlist(sample[2])



sample<- get.sample.no.fls_fls(
   probs.no.fls=c(
            input$teriparatide.to.nothing.no.fls.female,
            input$teriparatide.to.alendronate.no.fls.female*(1-input$teriparatide.to.nothing.no.fls.female),
            input$teriparatide.to.risedronate.no.fls.female*(1-input$teriparatide.to.nothing.no.fls.female),
            input$teriparatide.to.strontium.no.fls.female*(1-input$teriparatide.to.nothing.no.fls.female),
            input$teriparatide.to.ibandronate.no.fls.female*(1-input$teriparatide.to.nothing.no.fls.female),
            input$teriparatide.to.denosumab.no.fls.female*(1-input$teriparatide.to.nothing.no.fls.female),
            input$teriparatide.to.zoledronate.no.fls.female*(1-input$teriparatide.to.nothing.no.fls.female)),
   probs.fls=c(
            input$teriparatide.to.nothing.fls.female,
            input$teriparatide.to.alendronate.fls.female*(1-input$teriparatide.to.nothing.fls.female),
            input$teriparatide.to.risedronate.fls.female*(1-input$teriparatide.to.nothing.fls.female),
            input$teriparatide.to.strontium.fls.female*(1-input$teriparatide.to.nothing.fls.female),
            input$teriparatide.to.ibandronate.fls.female*(1-input$teriparatide.to.nothing.fls.female),
            input$teriparatide.to.denosumab.fls.female*(1-input$teriparatide.to.nothing.fls.female),
            input$teriparatide.to.zoledronate.fls.female*(1-input$teriparatide.to.nothing.fls.female))
   )  

rmultinorm$rmultinorm.teriparatide.to.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.teriparatide.to.fls.female<-unlist(sample[2])



# monitored.4m.to  ------

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.monitored.spine.4m.male,
              input$no.fls.monitored.spine.4m.male),
   probs.fls=c(1-input$fls.monitored.spine.4m.male,
              input$fls.monitored.spine.4m.male)
   )  
rmultinorm$rmultinorm.no.fls.monitored.spine.4m.male<-unlist(sample[1])
rmultinorm$rmultinorm.fls.monitored.spine.4m.male<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.monitored.hip.4m.male,
              input$no.fls.monitored.hip.4m.male),
   probs.fls=c(1-input$fls.monitored.hip.4m.male,
              input$fls.monitored.hip.4m.male)
   )  
rmultinorm$rmultinorm.no.fls.monitored.hip.4m.male<-unlist(sample[1])
rmultinorm$rmultinorm.fls.monitored.hip.4m.male<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.monitored.other.4m.male,
              input$no.fls.monitored.other.4m.male),
   probs.fls=c(1-input$fls.monitored.other.4m.male,
              input$fls.monitored.other.4m.male)
   )  
rmultinorm$rmultinorm.no.fls.monitored.other.4m.male<-unlist(sample[1])
rmultinorm$rmultinorm.fls.monitored.other.4m.male<-unlist(sample[2])



sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.monitored.spine.4m.female,
              input$no.fls.monitored.spine.4m.female),
   probs.fls=c(1-input$fls.monitored.spine.4m.female,
              input$fls.monitored.spine.4m.female)
   )  
rmultinorm$rmultinorm.no.fls.monitored.spine.4m.female<-unlist(sample[1])
rmultinorm$rmultinorm.fls.monitored.spine.4m.female<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.monitored.hip.4m.female,
              input$no.fls.monitored.hip.4m.female),
   probs.fls=c(1-input$fls.monitored.hip.4m.female,
              input$fls.monitored.hip.4m.female)
   )  
rmultinorm$rmultinorm.no.fls.monitored.hip.4m.female<-unlist(sample[1])
rmultinorm$rmultinorm.fls.monitored.hip.4m.female<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.monitored.other.4m.female,
              input$no.fls.monitored.other.4m.female),
   probs.fls=c(1-input$fls.monitored.other.4m.female,
              input$fls.monitored.other.4m.female)
   )  
rmultinorm$rmultinorm.no.fls.monitored.other.4m.female<-unlist(sample[1])
rmultinorm$rmultinorm.fls.monitored.other.4m.female<-unlist(sample[2])

# monitored.12m.to  ------

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.monitored.spine.12m.male,
              input$no.fls.monitored.spine.12m.male),
   probs.fls=c(1-input$fls.monitored.spine.12m.male,
              input$fls.monitored.spine.12m.male)
   )  
rmultinorm$rmultinorm.no.fls.monitored.spine.12m.male<-unlist(sample[1])
rmultinorm$rmultinorm.fls.monitored.spine.12m.male<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.monitored.hip.12m.male,
              input$no.fls.monitored.hip.12m.male),
   probs.fls=c(1-input$fls.monitored.hip.12m.male,
              input$fls.monitored.hip.12m.male)
   )  
rmultinorm$rmultinorm.no.fls.monitored.hip.12m.male<-unlist(sample[1])
rmultinorm$rmultinorm.fls.monitored.hip.12m.male<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.monitored.other.12m.male,
              input$no.fls.monitored.other.12m.male),
   probs.fls=c(1-input$fls.monitored.other.12m.male,
              input$fls.monitored.other.12m.male)
   )  
rmultinorm$rmultinorm.no.fls.monitored.other.12m.male<-unlist(sample[1])
rmultinorm$rmultinorm.fls.monitored.other.12m.male<-unlist(sample[2])



sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.monitored.spine.12m.female,
              input$no.fls.monitored.spine.12m.female),
   probs.fls=c(1-input$fls.monitored.spine.12m.female,
              input$fls.monitored.spine.12m.female)
   )  
rmultinorm$rmultinorm.no.fls.monitored.spine.12m.female<-unlist(sample[1])
rmultinorm$rmultinorm.fls.monitored.spine.12m.female<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.monitored.hip.12m.female,
              input$no.fls.monitored.hip.12m.female),
   probs.fls=c(1-input$fls.monitored.hip.12m.female,
              input$fls.monitored.hip.12m.female)
   )  
rmultinorm$rmultinorm.no.fls.monitored.hip.12m.female<-unlist(sample[1])
rmultinorm$rmultinorm.fls.monitored.hip.12m.female<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$no.fls.monitored.other.12m.female,
              input$no.fls.monitored.other.12m.female),
   probs.fls=c(1-input$fls.monitored.other.12m.female,
              input$fls.monitored.other.12m.female)
   )  
rmultinorm$rmultinorm.no.fls.monitored.other.12m.female<-unlist(sample[1])
rmultinorm$rmultinorm.fls.monitored.other.12m.female<-unlist(sample[2])




# monitored  ------


rmultinorm$rmultinorm.not.monitored.4m_adh_risedronate.male<-
 get.sample(c(1-input$not.monitored.4m_adh_risedronate.male,
              input$not.monitored.4m_adh_risedronate.male))
rmultinorm$rmultinorm.not.monitored.4m_adh_strontium.male<-
 get.sample(c(1-input$not.monitored.4m_adh_strontium.male,
              input$not.monitored.4m_adh_strontium.male))
rmultinorm$rmultinorm.not.monitored.4m_adh_ibandronate.male<-
 get.sample(c(1-input$not.monitored.4m_adh_ibandronate.male,
              input$not.monitored.4m_adh_ibandronate.male))
rmultinorm$rmultinorm.not.monitored.4m_adh_denosumab.male<-
 get.sample(c(1-input$not.monitored.4m_adh_denosumab.male,
              input$not.monitored.4m_adh_denosumab.male))
rmultinorm$rmultinorm.not.monitored.4m_adh_zoledronate.male<-
 get.sample(c(1-input$not.monitored.4m_adh_zoledronate.male,
              input$not.monitored.4m_adh_zoledronate.male))
rmultinorm$rmultinorm.not.monitored.4m_adh_teriparatide.male<-
 get.sample(c(1-input$not.monitored.4m_adh_teriparatide.male,
              input$not.monitored.4m_adh_teriparatide.male))
rmultinorm$rmultinorm.not.monitored.4m_adh_abaloparatide.male<-
 get.sample(c(1-input$not.monitored.4m_adh_abaloparatide.male,
              input$not.monitored.4m_adh_abaloparatide.male))
rmultinorm$rmultinorm.not.monitored.4m_adh_romo.male<-
 get.sample(c(1-input$not.monitored.4m_adh_romo.male,
              input$not.monitored.4m_adh_romo.male))


rmultinorm$rmultinorm.not.monitored.12m_adh_alendronate.male<-
 get.sample(c(1-input$not.monitored.12m_adh_alendronate.male,
              input$not.monitored.12m_adh_alendronate.male))
rmultinorm$rmultinorm.not.monitored.12m_adh_risedronate.male<-
 get.sample(c(1-input$not.monitored.12m_adh_risedronate.male,
              input$not.monitored.12m_adh_risedronate.male))
rmultinorm$rmultinorm.not.monitored.12m_adh_strontium.male<-
 get.sample(c(1-input$not.monitored.12m_adh_strontium.male,
              input$not.monitored.12m_adh_strontium.male))
rmultinorm$rmultinorm.not.monitored.12m_adh_ibandronate.male<-
 get.sample(c(1-input$not.monitored.12m_adh_ibandronate.male,
              input$not.monitored.12m_adh_ibandronate.male))
rmultinorm$rmultinorm.not.monitored.12m_adh_denosumab.male<-
 get.sample(c(1-input$not.monitored.12m_adh_denosumab.male,
              input$not.monitored.12m_adh_denosumab.male))
rmultinorm$rmultinorm.not.monitored.12m_adh_zoledronate.male<-
 get.sample(c(1-input$not.monitored.12m_adh_zoledronate.male,
              input$not.monitored.12m_adh_zoledronate.male))
rmultinorm$rmultinorm.not.monitored.12m_adh_teriparatide.male<-
 get.sample(c(1-input$not.monitored.12m_adh_teriparatide.male,
              input$not.monitored.12m_adh_teriparatide.male))
rmultinorm$rmultinorm.not.monitored.12m_adh_abaloparatide.male<-
 get.sample(c(1-input$not.monitored.12m_adh_abaloparatide.male,
              input$not.monitored.12m_adh_abaloparatide.male))



rmultinorm$rmultinorm.not.monitored.4m_adh_alendronate.female<-
 get.sample(c(1-input$not.monitored.4m_adh_alendronate.female,
              input$not.monitored.4m_adh_alendronate.female))
rmultinorm$rmultinorm.not.monitored.4m_adh_risedronate.female<-
 get.sample(c(1-input$not.monitored.4m_adh_risedronate.female,
              input$not.monitored.4m_adh_risedronate.female))
rmultinorm$rmultinorm.not.monitored.4m_adh_strontium.female<-
 get.sample(c(1-input$not.monitored.4m_adh_strontium.female,
              input$not.monitored.4m_adh_strontium.female))
rmultinorm$rmultinorm.not.monitored.4m_adh_ibandronate.female<-
 get.sample(c(1-input$not.monitored.4m_adh_ibandronate.female,
              input$not.monitored.4m_adh_ibandronate.female))
rmultinorm$rmultinorm.not.monitored.4m_adh_denosumab.female<-
 get.sample(c(1-input$not.monitored.4m_adh_denosumab.female,
              input$not.monitored.4m_adh_denosumab.female))
rmultinorm$rmultinorm.not.monitored.4m_adh_zoledronate.female<-
 get.sample(c(1-input$not.monitored.4m_adh_zoledronate.female,
              input$not.monitored.4m_adh_zoledronate.female))
rmultinorm$rmultinorm.not.monitored.4m_adh_teriparatide.female<-
 get.sample(c(1-input$not.monitored.4m_adh_teriparatide.female,
              input$not.monitored.4m_adh_teriparatide.female))
rmultinorm$rmultinorm.not.monitored.4m_adh_abaloparatide.female<-
 get.sample(c(1-input$not.monitored.4m_adh_abaloparatide.female,
              input$not.monitored.4m_adh_abaloparatide.female))
rmultinorm$rmultinorm.not.monitored.4m_adh_romo.female<-
 get.sample(c(1-input$not.monitored.4m_adh_romo.female,
              input$not.monitored.4m_adh_romo.female))


rmultinorm$rmultinorm.not.monitored.12m_adh_alendronate.female<-
 get.sample(c(1-input$not.monitored.12m_adh_alendronate.female,
              input$not.monitored.12m_adh_alendronate.female))
rmultinorm$rmultinorm.not.monitored.12m_adh_risedronate.female<-
 get.sample(c(1-input$not.monitored.12m_adh_risedronate.female,
              input$not.monitored.12m_adh_risedronate.female))
rmultinorm$rmultinorm.not.monitored.12m_adh_strontium.female<-
 get.sample(c(1-input$not.monitored.12m_adh_strontium.female,
              input$not.monitored.12m_adh_strontium.female))
rmultinorm$rmultinorm.not.monitored.12m_adh_ibandronate.female<-
 get.sample(c(1-input$not.monitored.12m_adh_ibandronate.female,
              input$not.monitored.12m_adh_ibandronate.female))
rmultinorm$rmultinorm.not.monitored.12m_adh_denosumab.female<-
 get.sample(c(1-input$not.monitored.12m_adh_denosumab.female,
              input$not.monitored.12m_adh_denosumab.female))
rmultinorm$rmultinorm.not.monitored.12m_adh_zoledronate.female<-
 get.sample(c(1-input$not.monitored.12m_adh_zoledronate.female,
              input$not.monitored.12m_adh_zoledronate.female))
rmultinorm$rmultinorm.not.monitored.12m_adh_teriparatide.female<-
 get.sample(c(1-input$not.monitored.12m_adh_teriparatide.female,
              input$not.monitored.12m_adh_teriparatide.female))
rmultinorm$rmultinorm.not.monitored.12m_adh_abaloparatide.female<-
 get.sample(c(1-input$not.monitored.12m_adh_abaloparatide.female,
              input$not.monitored.12m_adh_abaloparatide.female))









rmultinorm$rmultinorm.monitored.4m_adh_alendronate.male<-
 get.sample(c(1-input$monitored.4m_adh_alendronate.male,
              input$monitored.4m_adh_alendronate.male))
rmultinorm$rmultinorm.monitored.4m_adh_risedronate.male<-
 get.sample(c(1-input$monitored.4m_adh_risedronate.male,
              input$monitored.4m_adh_risedronate.male))
rmultinorm$rmultinorm.monitored.4m_adh_strontium.male<-
 get.sample(c(1-input$monitored.4m_adh_strontium.male,
              input$monitored.4m_adh_strontium.male))
rmultinorm$rmultinorm.monitored.4m_adh_ibandronate.male<-
 get.sample(c(1-input$monitored.4m_adh_ibandronate.male,
              input$monitored.4m_adh_ibandronate.male))
rmultinorm$rmultinorm.monitored.4m_adh_denosumab.male<-
 get.sample(c(1-input$monitored.4m_adh_denosumab.male,
              input$monitored.4m_adh_denosumab.male))
rmultinorm$rmultinorm.monitored.4m_adh_zoledronate.male<-
 get.sample(c(1-input$monitored.4m_adh_zoledronate.male,
              input$monitored.4m_adh_zoledronate.male))
rmultinorm$rmultinorm.monitored.4m_adh_teriparatide.male<-
 get.sample(c(1-input$monitored.4m_adh_teriparatide.male,
              input$monitored.4m_adh_teriparatide.male))
rmultinorm$rmultinorm.monitored.4m_adh_abaloparatide.male<-
 get.sample(c(1-input$monitored.4m_adh_abaloparatide.male,
              input$monitored.4m_adh_abaloparatide.male))
rmultinorm$rmultinorm.monitored.4m_adh_romo.male<-
 get.sample(c(1-input$monitored.4m_adh_romo.male,
              input$monitored.4m_adh_romo.male))


rmultinorm$rmultinorm.monitored.12m_adh_alendronate.male<-
 get.sample(c(1-input$monitored.12m_adh_alendronate.male,
              input$monitored.12m_adh_alendronate.male))
rmultinorm$rmultinorm.monitored.12m_adh_risedronate.male<-
 get.sample(c(1-input$monitored.12m_adh_risedronate.male,
              input$monitored.12m_adh_risedronate.male))
rmultinorm$rmultinorm.monitored.12m_adh_strontium.male<-
 get.sample(c(1-input$monitored.12m_adh_strontium.male,
              input$monitored.12m_adh_strontium.male))
rmultinorm$rmultinorm.monitored.12m_adh_ibandronate.male<-
 get.sample(c(1-input$monitored.12m_adh_ibandronate.male,
              input$monitored.12m_adh_ibandronate.male))
rmultinorm$rmultinorm.monitored.12m_adh_denosumab.male<-
 get.sample(c(1-input$monitored.12m_adh_denosumab.male,
              input$monitored.12m_adh_denosumab.male))
rmultinorm$rmultinorm.monitored.12m_adh_zoledronate.male<-
 get.sample(c(1-input$monitored.12m_adh_zoledronate.male,
              input$monitored.12m_adh_zoledronate.male))
rmultinorm$rmultinorm.monitored.12m_adh_teriparatide.male<-
 get.sample(c(1-input$monitored.12m_adh_teriparatide.male,
              input$monitored.12m_adh_teriparatide.male))
rmultinorm$rmultinorm.monitored.12m_adh_abaloparatide.male<-
 get.sample(c(1-input$monitored.12m_adh_abaloparatide.male,
              input$monitored.12m_adh_abaloparatide.male))



rmultinorm$rmultinorm.monitored.4m_adh_alendronate.female<-
 get.sample(c(1-input$monitored.4m_adh_alendronate.female,
              input$monitored.4m_adh_alendronate.female))
rmultinorm$rmultinorm.monitored.4m_adh_risedronate.female<-
 get.sample(c(1-input$monitored.4m_adh_risedronate.female,
              input$monitored.4m_adh_risedronate.female))
rmultinorm$rmultinorm.monitored.4m_adh_strontium.female<-
 get.sample(c(1-input$monitored.4m_adh_strontium.female,
              input$monitored.4m_adh_strontium.female))
rmultinorm$rmultinorm.monitored.4m_adh_ibandronate.female<-
 get.sample(c(1-input$monitored.4m_adh_ibandronate.female,
              input$monitored.4m_adh_ibandronate.female))
rmultinorm$rmultinorm.monitored.4m_adh_denosumab.female<-
 get.sample(c(1-input$monitored.4m_adh_denosumab.female,
              input$monitored.4m_adh_denosumab.female))
rmultinorm$rmultinorm.monitored.4m_adh_zoledronate.female<-
 get.sample(c(1-input$monitored.4m_adh_zoledronate.female,
              input$monitored.4m_adh_zoledronate.female))
rmultinorm$rmultinorm.monitored.4m_adh_teriparatide.female<-
 get.sample(c(1-input$monitored.4m_adh_teriparatide.female,
              input$monitored.4m_adh_teriparatide.female))
rmultinorm$rmultinorm.monitored.4m_adh_abaloparatide.female<-
 get.sample(c(1-input$monitored.4m_adh_abaloparatide.female,
              input$monitored.4m_adh_abaloparatide.female))
rmultinorm$rmultinorm.monitored.4m_adh_romo.female<-
 get.sample(c(1-input$monitored.4m_adh_romo.female,
              input$monitored.4m_adh_romo.female))


rmultinorm$rmultinorm.monitored.12m_adh_alendronate.female<-
 get.sample(c(1-input$monitored.12m_adh_alendronate.female,
              input$monitored.12m_adh_alendronate.female))
rmultinorm$rmultinorm.monitored.12m_adh_risedronate.female<-
 get.sample(c(1-input$monitored.12m_adh_risedronate.female,
              input$monitored.12m_adh_risedronate.female))
rmultinorm$rmultinorm.monitored.12m_adh_strontium.female<-
 get.sample(c(1-input$monitored.12m_adh_strontium.female,
              input$monitored.12m_adh_strontium.female))
rmultinorm$rmultinorm.monitored.12m_adh_ibandronate.female<-
 get.sample(c(1-input$monitored.12m_adh_ibandronate.female,
              input$monitored.12m_adh_ibandronate.female))
rmultinorm$rmultinorm.monitored.12m_adh_denosumab.female<-
 get.sample(c(1-input$monitored.12m_adh_denosumab.female,
              input$monitored.12m_adh_denosumab.female))
rmultinorm$rmultinorm.monitored.12m_adh_zoledronate.female<-
 get.sample(c(1-input$monitored.12m_adh_zoledronate.female,
              input$monitored.12m_adh_zoledronate.female))
rmultinorm$rmultinorm.monitored.12m_adh_teriparatide.female<-
 get.sample(c(1-input$monitored.12m_adh_teriparatide.female,
              input$monitored.12m_adh_teriparatide.female))
rmultinorm$rmultinorm.monitored.12m_adh_abaloparatide.female<-
 get.sample(c(1-input$monitored.12m_adh_abaloparatide.female,
              input$monitored.12m_adh_abaloparatide.female))

# primary adh  ------


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_alendronate.spine.no.fls.male,
              input$primary.adh_alendronate.spine.no.fls.male),
   probs.fls=c(1-input$primary.adh_alendronate.spine.fls.male,
              input$primary.adh_alendronate.spine.fls.male)
   )  
rmultinorm$rmultinorm.primary.adh_alendronate.spine.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_alendronate.spine.fls.male<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_risedronate.spine.no.fls.male,
              input$primary.adh_risedronate.spine.no.fls.male),
   probs.fls=c(1-input$primary.adh_risedronate.spine.fls.male,
              input$primary.adh_risedronate.spine.fls.male)
   )  
rmultinorm$rmultinorm.primary.adh_risedronate.spine.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_risedronate.spine.fls.male<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_strontium.spine.no.fls.male,
              input$primary.adh_strontium.spine.no.fls.male),
   probs.fls=c(1-input$primary.adh_strontium.spine.fls.male,
              input$primary.adh_strontium.spine.fls.male)
   )  
rmultinorm$rmultinorm.primary.adh_strontium.spine.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_strontium.spine.fls.male<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_ibandronate.spine.no.fls.male,
              input$primary.adh_ibandronate.spine.no.fls.male),
   probs.fls=c(1-input$primary.adh_ibandronate.spine.fls.male,
              input$primary.adh_ibandronate.spine.fls.male)
   )  
rmultinorm$rmultinorm.primary.adh_ibandronate.spine.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_ibandronate.spine.fls.male<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_denosumab.spine.no.fls.male,
              input$primary.adh_denosumab.spine.no.fls.male),
   probs.fls=c(1-input$primary.adh_denosumab.spine.fls.male,
              input$primary.adh_denosumab.spine.fls.male)
   )  
rmultinorm$rmultinorm.primary.adh_denosumab.spine.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_denosumab.spine.fls.male<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_zoledronate.spine.no.fls.male,
              input$primary.adh_zoledronate.spine.no.fls.male),
   probs.fls=c(1-input$primary.adh_zoledronate.spine.fls.male,
              input$primary.adh_zoledronate.spine.fls.male)
   )  
rmultinorm$rmultinorm.primary.adh_zoledronate.spine.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_zoledronate.spine.fls.male<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_teriparatide.spine.no.fls.male,
              input$primary.adh_teriparatide.spine.no.fls.male),
   probs.fls=c(1-input$primary.adh_teriparatide.spine.fls.male,
              input$primary.adh_teriparatide.spine.fls.male)
   )  
rmultinorm$rmultinorm.primary.adh_teriparatide.spine.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_teriparatide.spine.fls.male<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_abaloparatide.spine.no.fls.male,
              input$primary.adh_abaloparatide.spine.no.fls.male),
   probs.fls=c(1-input$primary.adh_abaloparatide.spine.fls.male,
              input$primary.adh_abaloparatide.spine.fls.male)
   )  
rmultinorm$rmultinorm.primary.adh_abaloparatide.spine.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_abaloparatide.spine.fls.male<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_romo.spine.no.fls.male,
              input$primary.adh_romo.spine.no.fls.male),
   probs.fls=c(1-input$primary.adh_romo.spine.fls.male,
              input$primary.adh_romo.spine.fls.male)
   )  
rmultinorm$rmultinorm.primary.adh_romo.spine.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_romo.spine.fls.male<-unlist(sample[2])



sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_alendronate.hip.no.fls.male,
              input$primary.adh_alendronate.hip.no.fls.male),
   probs.fls=c(1-input$primary.adh_alendronate.hip.fls.male,
              input$primary.adh_alendronate.hip.fls.male)
   )  
rmultinorm$rmultinorm.primary.adh_alendronate.hip.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_alendronate.hip.fls.male<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_risedronate.hip.no.fls.male,
              input$primary.adh_risedronate.hip.no.fls.male),
   probs.fls=c(1-input$primary.adh_risedronate.hip.fls.male,
              input$primary.adh_risedronate.hip.fls.male)
   )  
rmultinorm$rmultinorm.primary.adh_risedronate.hip.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_risedronate.hip.fls.male<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_strontium.hip.no.fls.male,
              input$primary.adh_strontium.hip.no.fls.male),
   probs.fls=c(1-input$primary.adh_strontium.hip.fls.male,
              input$primary.adh_strontium.hip.fls.male)
   )  
rmultinorm$rmultinorm.primary.adh_strontium.hip.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_strontium.hip.fls.male<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_ibandronate.hip.no.fls.male,
              input$primary.adh_ibandronate.hip.no.fls.male),
   probs.fls=c(1-input$primary.adh_ibandronate.hip.fls.male,
              input$primary.adh_ibandronate.hip.fls.male)
   )  
rmultinorm$rmultinorm.primary.adh_ibandronate.hip.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_ibandronate.hip.fls.male<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_denosumab.hip.no.fls.male,
              input$primary.adh_denosumab.hip.no.fls.male),
   probs.fls=c(1-input$primary.adh_denosumab.hip.fls.male,
              input$primary.adh_denosumab.hip.fls.male)
   )  
rmultinorm$rmultinorm.primary.adh_denosumab.hip.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_denosumab.hip.fls.male<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_zoledronate.hip.no.fls.male,
              input$primary.adh_zoledronate.hip.no.fls.male),
   probs.fls=c(1-input$primary.adh_zoledronate.hip.fls.male,
              input$primary.adh_zoledronate.hip.fls.male)
   )  
rmultinorm$rmultinorm.primary.adh_zoledronate.hip.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_zoledronate.hip.fls.male<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_teriparatide.hip.no.fls.male,
              input$primary.adh_teriparatide.hip.no.fls.male),
   probs.fls=c(1-input$primary.adh_teriparatide.hip.fls.male,
              input$primary.adh_teriparatide.hip.fls.male)
   )  
rmultinorm$rmultinorm.primary.adh_teriparatide.hip.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_teriparatide.hip.fls.male<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_abaloparatide.hip.no.fls.male,
              input$primary.adh_abaloparatide.hip.no.fls.male),
   probs.fls=c(1-input$primary.adh_abaloparatide.hip.fls.male,
              input$primary.adh_abaloparatide.hip.fls.male)
   )  
rmultinorm$rmultinorm.primary.adh_abaloparatide.hip.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_abaloparatide.hip.fls.male<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_romo.hip.no.fls.male,
              input$primary.adh_romo.hip.no.fls.male),
   probs.fls=c(1-input$primary.adh_romo.hip.fls.male,
              input$primary.adh_romo.hip.fls.male)
   )  
rmultinorm$rmultinorm.primary.adh_romo.hip.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_romo.hip.fls.male<-unlist(sample[2])





sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_alendronate.other.no.fls.male,
              input$primary.adh_alendronate.other.no.fls.male),
   probs.fls=c(1-input$primary.adh_alendronate.other.fls.male,
              input$primary.adh_alendronate.other.fls.male)
   )  
rmultinorm$rmultinorm.primary.adh_alendronate.other.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_alendronate.other.fls.male<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_risedronate.other.no.fls.male,
              input$primary.adh_risedronate.other.no.fls.male),
   probs.fls=c(1-input$primary.adh_risedronate.other.fls.male,
              input$primary.adh_risedronate.other.fls.male)
   )  
rmultinorm$rmultinorm.primary.adh_risedronate.other.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_risedronate.other.fls.male<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_strontium.other.no.fls.male,
              input$primary.adh_strontium.other.no.fls.male),
   probs.fls=c(1-input$primary.adh_strontium.other.fls.male,
              input$primary.adh_strontium.other.fls.male)
   )  
rmultinorm$rmultinorm.primary.adh_strontium.other.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_strontium.other.fls.male<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_ibandronate.other.no.fls.male,
              input$primary.adh_ibandronate.other.no.fls.male),
   probs.fls=c(1-input$primary.adh_ibandronate.other.fls.male,
              input$primary.adh_ibandronate.other.fls.male)
   )  
rmultinorm$rmultinorm.primary.adh_ibandronate.other.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_ibandronate.other.fls.male<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_denosumab.other.no.fls.male,
              input$primary.adh_denosumab.other.no.fls.male),
   probs.fls=c(1-input$primary.adh_denosumab.other.fls.male,
              input$primary.adh_denosumab.other.fls.male)
   )  
rmultinorm$rmultinorm.primary.adh_denosumab.other.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_denosumab.other.fls.male<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_zoledronate.other.no.fls.male,
              input$primary.adh_zoledronate.other.no.fls.male),
   probs.fls=c(1-input$primary.adh_zoledronate.other.fls.male,
              input$primary.adh_zoledronate.other.fls.male)
   )  
rmultinorm$rmultinorm.primary.adh_zoledronate.other.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_zoledronate.other.fls.male<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_teriparatide.other.no.fls.male,
              input$primary.adh_teriparatide.other.no.fls.male),
   probs.fls=c(1-input$primary.adh_teriparatide.other.fls.male,
              input$primary.adh_teriparatide.other.fls.male)
   )  
rmultinorm$rmultinorm.primary.adh_teriparatide.other.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_teriparatide.other.fls.male<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_abaloparatide.other.no.fls.male,
              input$primary.adh_abaloparatide.other.no.fls.male),
   probs.fls=c(1-input$primary.adh_abaloparatide.other.fls.male,
              input$primary.adh_abaloparatide.other.fls.male)
   )  
rmultinorm$rmultinorm.primary.adh_abaloparatide.other.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_abaloparatide.other.fls.male<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_romo.other.no.fls.male,
              input$primary.adh_romo.other.no.fls.male),
   probs.fls=c(1-input$primary.adh_romo.other.fls.male,
              input$primary.adh_romo.other.fls.male)
   )  
rmultinorm$rmultinorm.primary.adh_romo.other.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_romo.other.fls.male<-unlist(sample[2])










sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_alendronate.spine.no.fls.female,
              input$primary.adh_alendronate.spine.no.fls.female),
   probs.fls=c(1-input$primary.adh_alendronate.spine.fls.female,
              input$primary.adh_alendronate.spine.fls.female)
   )  
rmultinorm$rmultinorm.primary.adh_alendronate.spine.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_alendronate.spine.fls.female<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_risedronate.spine.no.fls.female,
              input$primary.adh_risedronate.spine.no.fls.female),
   probs.fls=c(1-input$primary.adh_risedronate.spine.fls.female,
              input$primary.adh_risedronate.spine.fls.female)
   )  
rmultinorm$rmultinorm.primary.adh_risedronate.spine.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_risedronate.spine.fls.female<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_strontium.spine.no.fls.female,
              input$primary.adh_strontium.spine.no.fls.female),
   probs.fls=c(1-input$primary.adh_strontium.spine.fls.female,
              input$primary.adh_strontium.spine.fls.female)
   )  
rmultinorm$rmultinorm.primary.adh_strontium.spine.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_strontium.spine.fls.female<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_ibandronate.spine.no.fls.female,
              input$primary.adh_ibandronate.spine.no.fls.female),
   probs.fls=c(1-input$primary.adh_ibandronate.spine.fls.female,
              input$primary.adh_ibandronate.spine.fls.female)
   )  
rmultinorm$rmultinorm.primary.adh_ibandronate.spine.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_ibandronate.spine.fls.female<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_denosumab.spine.no.fls.female,
              input$primary.adh_denosumab.spine.no.fls.female),
   probs.fls=c(1-input$primary.adh_denosumab.spine.fls.female,
              input$primary.adh_denosumab.spine.fls.female)
   )  
rmultinorm$rmultinorm.primary.adh_denosumab.spine.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_denosumab.spine.fls.female<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_zoledronate.spine.no.fls.female,
              input$primary.adh_zoledronate.spine.no.fls.female),
   probs.fls=c(1-input$primary.adh_zoledronate.spine.fls.female,
              input$primary.adh_zoledronate.spine.fls.female)
   )  
rmultinorm$rmultinorm.primary.adh_zoledronate.spine.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_zoledronate.spine.fls.female<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_teriparatide.spine.no.fls.female,
              input$primary.adh_teriparatide.spine.no.fls.female),
   probs.fls=c(1-input$primary.adh_teriparatide.spine.fls.female,
              input$primary.adh_teriparatide.spine.fls.female)
   )  
rmultinorm$rmultinorm.primary.adh_teriparatide.spine.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_teriparatide.spine.fls.female<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_abaloparatide.spine.no.fls.female,
              input$primary.adh_abaloparatide.spine.no.fls.female),
   probs.fls=c(1-input$primary.adh_abaloparatide.spine.fls.female,
              input$primary.adh_abaloparatide.spine.fls.female)
   )  
rmultinorm$rmultinorm.primary.adh_abaloparatide.spine.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_abaloparatide.spine.fls.female<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_romo.spine.no.fls.female,
              input$primary.adh_romo.spine.no.fls.female),
   probs.fls=c(1-input$primary.adh_romo.spine.fls.female,
              input$primary.adh_romo.spine.fls.female)
   )  
rmultinorm$rmultinorm.primary.adh_romo.spine.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_romo.spine.fls.female<-unlist(sample[2])



sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_alendronate.hip.no.fls.female,
              input$primary.adh_alendronate.hip.no.fls.female),
   probs.fls=c(1-input$primary.adh_alendronate.hip.fls.female,
              input$primary.adh_alendronate.hip.fls.female)
   )  
rmultinorm$rmultinorm.primary.adh_alendronate.hip.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_alendronate.hip.fls.female<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_risedronate.hip.no.fls.female,
              input$primary.adh_risedronate.hip.no.fls.female),
   probs.fls=c(1-input$primary.adh_risedronate.hip.fls.female,
              input$primary.adh_risedronate.hip.fls.female)
   )  
rmultinorm$rmultinorm.primary.adh_risedronate.hip.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_risedronate.hip.fls.female<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_strontium.hip.no.fls.female,
              input$primary.adh_strontium.hip.no.fls.female),
   probs.fls=c(1-input$primary.adh_strontium.hip.fls.female,
              input$primary.adh_strontium.hip.fls.female)
   )  
rmultinorm$rmultinorm.primary.adh_strontium.hip.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_strontium.hip.fls.female<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_ibandronate.hip.no.fls.female,
              input$primary.adh_ibandronate.hip.no.fls.female),
   probs.fls=c(1-input$primary.adh_ibandronate.hip.fls.female,
              input$primary.adh_ibandronate.hip.fls.female)
   )  
rmultinorm$rmultinorm.primary.adh_ibandronate.hip.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_ibandronate.hip.fls.female<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_denosumab.hip.no.fls.female,
              input$primary.adh_denosumab.hip.no.fls.female),
   probs.fls=c(1-input$primary.adh_denosumab.hip.fls.female,
              input$primary.adh_denosumab.hip.fls.female)
   )  
rmultinorm$rmultinorm.primary.adh_denosumab.hip.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_denosumab.hip.fls.female<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_zoledronate.hip.no.fls.female,
              input$primary.adh_zoledronate.hip.no.fls.female),
   probs.fls=c(1-input$primary.adh_zoledronate.hip.fls.female,
              input$primary.adh_zoledronate.hip.fls.female)
   )  
rmultinorm$rmultinorm.primary.adh_zoledronate.hip.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_zoledronate.hip.fls.female<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_teriparatide.hip.no.fls.female,
              input$primary.adh_teriparatide.hip.no.fls.female),
   probs.fls=c(1-input$primary.adh_teriparatide.hip.fls.female,
              input$primary.adh_teriparatide.hip.fls.female)
   )  
rmultinorm$rmultinorm.primary.adh_teriparatide.hip.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_teriparatide.hip.fls.female<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_abaloparatide.hip.no.fls.female,
              input$primary.adh_abaloparatide.hip.no.fls.female),
   probs.fls=c(1-input$primary.adh_abaloparatide.hip.fls.female,
              input$primary.adh_abaloparatide.hip.fls.female)
   )  
rmultinorm$rmultinorm.primary.adh_abaloparatide.hip.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_abaloparatide.hip.fls.female<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_romo.hip.no.fls.female,
              input$primary.adh_romo.hip.no.fls.female),
   probs.fls=c(1-input$primary.adh_romo.hip.fls.female,
              input$primary.adh_romo.hip.fls.female)
   )  
rmultinorm$rmultinorm.primary.adh_romo.hip.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_romo.hip.fls.female<-unlist(sample[2])





sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_alendronate.other.no.fls.female,
              input$primary.adh_alendronate.other.no.fls.female),
   probs.fls=c(1-input$primary.adh_alendronate.other.fls.female,
              input$primary.adh_alendronate.other.fls.female)
   )  
rmultinorm$rmultinorm.primary.adh_alendronate.other.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_alendronate.other.fls.female<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_risedronate.other.no.fls.female,
              input$primary.adh_risedronate.other.no.fls.female),
   probs.fls=c(1-input$primary.adh_risedronate.other.fls.female,
              input$primary.adh_risedronate.other.fls.female)
   )  
rmultinorm$rmultinorm.primary.adh_risedronate.other.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_risedronate.other.fls.female<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_strontium.other.no.fls.female,
              input$primary.adh_strontium.other.no.fls.female),
   probs.fls=c(1-input$primary.adh_strontium.other.fls.female,
              input$primary.adh_strontium.other.fls.female)
   )  
rmultinorm$rmultinorm.primary.adh_strontium.other.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_strontium.other.fls.female<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_ibandronate.other.no.fls.female,
              input$primary.adh_ibandronate.other.no.fls.female),
   probs.fls=c(1-input$primary.adh_ibandronate.other.fls.female,
              input$primary.adh_ibandronate.other.fls.female)
   )  
rmultinorm$rmultinorm.primary.adh_ibandronate.other.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_ibandronate.other.fls.female<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_denosumab.other.no.fls.female,
              input$primary.adh_denosumab.other.no.fls.female),
   probs.fls=c(1-input$primary.adh_denosumab.other.fls.female,
              input$primary.adh_denosumab.other.fls.female)
   )  
rmultinorm$rmultinorm.primary.adh_denosumab.other.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_denosumab.other.fls.female<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_zoledronate.other.no.fls.female,
              input$primary.adh_zoledronate.other.no.fls.female),
   probs.fls=c(1-input$primary.adh_zoledronate.other.fls.female,
              input$primary.adh_zoledronate.other.fls.female)
   )  
rmultinorm$rmultinorm.primary.adh_zoledronate.other.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_zoledronate.other.fls.female<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_teriparatide.other.no.fls.female,
              input$primary.adh_teriparatide.other.no.fls.female),
   probs.fls=c(1-input$primary.adh_teriparatide.other.fls.female,
              input$primary.adh_teriparatide.other.fls.female)
   )  
rmultinorm$rmultinorm.primary.adh_teriparatide.other.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_teriparatide.other.fls.female<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_abaloparatide.other.no.fls.female,
              input$primary.adh_abaloparatide.other.no.fls.female),
   probs.fls=c(1-input$primary.adh_abaloparatide.other.fls.female,
              input$primary.adh_abaloparatide.other.fls.female)
   )  
rmultinorm$rmultinorm.primary.adh_abaloparatide.other.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_abaloparatide.other.fls.female<-unlist(sample[2])


sample<- get.sample.no.fls_fls(
   probs.no.fls=c(1-input$primary.adh_romo.other.no.fls.female,
              input$primary.adh_romo.other.no.fls.female),
   probs.fls=c(1-input$primary.adh_romo.other.fls.female,
              input$primary.adh_romo.other.fls.female)
   )  
rmultinorm$rmultinorm.primary.adh_romo.other.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.primary.adh_romo.other.fls.female<-unlist(sample[2])






# monitored ------
rmultinorm$rmultinorm.monitored.4m_adh_alendronate.male<-
 get.sample(c(1-input$monitored.4m_adh_alendronate.male,
              input$monitored.4m_adh_alendronate.male))
rmultinorm$rmultinorm.monitored.4m_adh_risedronate.male<-
 get.sample(c(1-input$monitored.4m_adh_risedronate.male,
              input$monitored.4m_adh_risedronate.male))
rmultinorm$rmultinorm.monitored.4m_adh_strontium.male<-
 get.sample(c(1-input$monitored.4m_adh_strontium.male,
              input$monitored.4m_adh_strontium.male))
rmultinorm$rmultinorm.monitored.4m_adh_ibandronate.male<-
 get.sample(c(1-input$monitored.4m_adh_ibandronate.male,
              input$monitored.4m_adh_ibandronate.male))
rmultinorm$rmultinorm.monitored.4m_adh_denosumab.male<-
 get.sample(c(1-input$monitored.4m_adh_denosumab.male,
              input$monitored.4m_adh_denosumab.male))
rmultinorm$rmultinorm.monitored.4m_adh_zoledronate.male<-
 get.sample(c(1-input$monitored.4m_adh_zoledronate.male,
              input$monitored.4m_adh_zoledronate.male))
rmultinorm$rmultinorm.monitored.4m_adh_teriparatide.male<-
 get.sample(c(1-input$monitored.4m_adh_teriparatide.male,
              input$monitored.4m_adh_teriparatide.male))
rmultinorm$rmultinorm.monitored.4m_adh_abaloparatide.male<-
 get.sample(c(1-input$monitored.4m_adh_abaloparatide.male,
              input$monitored.4m_adh_abaloparatide.male))
rmultinorm$rmultinorm.monitored.4m_adh_romo.male<-
 get.sample(c(1-input$monitored.4m_adh_romo.male,
              input$monitored.4m_adh_romo.male))

rmultinorm$rmultinorm.monitored.12m_adh_alendronate.male<-
 get.sample(c(1-input$monitored.12m_adh_alendronate.male,
              input$monitored.12m_adh_alendronate.male))
rmultinorm$rmultinorm.monitored.12m_adh_risedronate.male<-
 get.sample(c(1-input$monitored.12m_adh_risedronate.male,
              input$monitored.12m_adh_risedronate.male))
rmultinorm$rmultinorm.monitored.12m_adh_strontium.male<-
 get.sample(c(1-input$monitored.12m_adh_strontium.male,
              input$monitored.12m_adh_strontium.male))
rmultinorm$rmultinorm.monitored.12m_adh_ibandronate.male<-
 get.sample(c(1-input$monitored.12m_adh_ibandronate.male,
              input$monitored.12m_adh_ibandronate.male))
rmultinorm$rmultinorm.monitored.12m_adh_denosumab.male<-
 get.sample(c(1-input$monitored.12m_adh_denosumab.male,
              input$monitored.12m_adh_denosumab.male))
rmultinorm$rmultinorm.monitored.12m_adh_zoledronate.male<-
 get.sample(c(1-input$monitored.12m_adh_zoledronate.male,
              input$monitored.12m_adh_zoledronate.male))
rmultinorm$rmultinorm.monitored.12m_adh_teriparatide.male<-
 get.sample(c(1-input$monitored.12m_adh_teriparatide.male,
              input$monitored.12m_adh_teriparatide.male))
rmultinorm$rmultinorm.monitored.12m_adh_abaloparatide.male<-
 get.sample(c(1-input$monitored.12m_adh_abaloparatide.male,
              input$monitored.12m_adh_abaloparatide.male))


rmultinorm$rmultinorm.monitored.4m_adh_alendronate.female<-
 get.sample(c(1-input$monitored.4m_adh_alendronate.female,
              input$monitored.4m_adh_alendronate.female))
rmultinorm$rmultinorm.monitored.4m_adh_risedronate.female<-
 get.sample(c(1-input$monitored.4m_adh_risedronate.female,
              input$monitored.4m_adh_risedronate.female))
rmultinorm$rmultinorm.monitored.4m_adh_strontium.female<-
 get.sample(c(1-input$monitored.4m_adh_strontium.female,
              input$monitored.4m_adh_strontium.female))
rmultinorm$rmultinorm.monitored.4m_adh_ibandronate.female<-
 get.sample(c(1-input$monitored.4m_adh_ibandronate.female,
              input$monitored.4m_adh_ibandronate.female))
rmultinorm$rmultinorm.monitored.4m_adh_denosumab.female<-
 get.sample(c(1-input$monitored.4m_adh_denosumab.female,
              input$monitored.4m_adh_denosumab.female))
rmultinorm$rmultinorm.monitored.4m_adh_zoledronate.female<-
 get.sample(c(1-input$monitored.4m_adh_zoledronate.female,
              input$monitored.4m_adh_zoledronate.female))
rmultinorm$rmultinorm.monitored.4m_adh_teriparatide.female<-
 get.sample(c(1-input$monitored.4m_adh_teriparatide.female,
              input$monitored.4m_adh_teriparatide.female))
rmultinorm$rmultinorm.monitored.4m_adh_abaloparatide.female<-
 get.sample(c(1-input$monitored.4m_adh_abaloparatide.female,
              input$monitored.4m_adh_abaloparatide.female))
rmultinorm$rmultinorm.monitored.4m_adh_romo.female<-
 get.sample(c(1-input$monitored.4m_adh_romo.female,
              input$monitored.4m_adh_romo.female))

rmultinorm$rmultinorm.monitored.12m_adh_alendronate.female<-
 get.sample(c(1-input$monitored.12m_adh_alendronate.female,
              input$monitored.12m_adh_alendronate.female))
rmultinorm$rmultinorm.monitored.12m_adh_risedronate.female<-
 get.sample(c(1-input$monitored.12m_adh_risedronate.female,
              input$monitored.12m_adh_risedronate.female))
rmultinorm$rmultinorm.monitored.12m_adh_strontium.female<-
 get.sample(c(1-input$monitored.12m_adh_strontium.female,
              input$monitored.12m_adh_strontium.female))
rmultinorm$rmultinorm.monitored.12m_adh_ibandronate.female<-
 get.sample(c(1-input$monitored.12m_adh_ibandronate.female,
              input$monitored.12m_adh_ibandronate.female))
rmultinorm$rmultinorm.monitored.12m_adh_denosumab.female<-
 get.sample(c(1-input$monitored.12m_adh_denosumab.female,
              input$monitored.12m_adh_denosumab.female))
rmultinorm$rmultinorm.monitored.12m_adh_zoledronate.female<-
 get.sample(c(1-input$monitored.12m_adh_zoledronate.female,
              input$monitored.12m_adh_zoledronate.female))
rmultinorm$rmultinorm.monitored.12m_adh_teriparatide.female<-
 get.sample(c(1-input$monitored.12m_adh_teriparatide.female,
              input$monitored.12m_adh_teriparatide.female))
rmultinorm$rmultinorm.monitored.12m_adh_abaloparatide.female<-
 get.sample(c(1-input$monitored.12m_adh_abaloparatide.female,
              input$monitored.12m_adh_abaloparatide.female))




rmultinorm$rmultinorm.not.monitored.4m_adh_alendronate.male<-
 get.sample(c(1-input$not.monitored.4m_adh_alendronate.male,
              input$not.monitored.4m_adh_alendronate.male))
rmultinorm$rmultinorm.not.monitored.4m_adh_risedronate.male<-
 get.sample(c(1-input$not.monitored.4m_adh_risedronate.male,
              input$not.monitored.4m_adh_risedronate.male))
rmultinorm$rmultinorm.not.monitored.4m_adh_strontium.male<-
 get.sample(c(1-input$not.monitored.4m_adh_strontium.male,
              input$not.monitored.4m_adh_strontium.male))
rmultinorm$rmultinorm.not.monitored.4m_adh_ibandronate.male<-
 get.sample(c(1-input$not.monitored.4m_adh_ibandronate.male,
              input$not.monitored.4m_adh_ibandronate.male))
rmultinorm$rmultinorm.not.monitored.4m_adh_denosumab.male<-
 get.sample(c(1-input$not.monitored.4m_adh_denosumab.male,
              input$not.monitored.4m_adh_denosumab.male))
rmultinorm$rmultinorm.not.monitored.4m_adh_zoledronate.male<-
 get.sample(c(1-input$not.monitored.4m_adh_zoledronate.male,
              input$not.monitored.4m_adh_zoledronate.male))
rmultinorm$rmultinorm.not.monitored.4m_adh_teriparatide.male<-
 get.sample(c(1-input$not.monitored.4m_adh_teriparatide.male,
              input$not.monitored.4m_adh_teriparatide.male))
rmultinorm$rmultinorm.not.monitored.4m_adh_abaloparatide.male<-
 get.sample(c(1-input$not.monitored.4m_adh_abaloparatide.male,
              input$not.monitored.4m_adh_abaloparatide.male))
rmultinorm$rmultinorm.not.monitored.4m_adh_romo.male<-
 get.sample(c(1-input$not.monitored.4m_adh_romo.male,
              input$not.monitored.4m_adh_romo.male))

rmultinorm$rmultinorm.not.monitored.12m_adh_alendronate.male<-
 get.sample(c(1-input$not.monitored.12m_adh_alendronate.male,
              input$not.monitored.12m_adh_alendronate.male))
rmultinorm$rmultinorm.not.monitored.12m_adh_risedronate.male<-
 get.sample(c(1-input$not.monitored.12m_adh_risedronate.male,
              input$not.monitored.12m_adh_risedronate.male))
rmultinorm$rmultinorm.not.monitored.12m_adh_strontium.male<-
 get.sample(c(1-input$not.monitored.12m_adh_strontium.male,
              input$not.monitored.12m_adh_strontium.male))
rmultinorm$rmultinorm.not.monitored.12m_adh_ibandronate.male<-
 get.sample(c(1-input$not.monitored.12m_adh_ibandronate.male,
              input$not.monitored.12m_adh_ibandronate.male))
rmultinorm$rmultinorm.not.monitored.12m_adh_denosumab.male<-
 get.sample(c(1-input$not.monitored.12m_adh_denosumab.male,
              input$not.monitored.12m_adh_denosumab.male))
rmultinorm$rmultinorm.not.monitored.12m_adh_zoledronate.male<-
 get.sample(c(1-input$not.monitored.12m_adh_zoledronate.male,
              input$not.monitored.12m_adh_zoledronate.male))
rmultinorm$rmultinorm.not.monitored.12m_adh_teriparatide.male<-
 get.sample(c(1-input$not.monitored.12m_adh_teriparatide.male,
              input$not.monitored.12m_adh_teriparatide.male))
rmultinorm$rmultinorm.not.monitored.12m_adh_abaloparatide.male<-
 get.sample(c(1-input$not.monitored.12m_adh_abaloparatide.male,
              input$not.monitored.12m_adh_abaloparatide.male))


rmultinorm$rmultinorm.not.monitored.4m_adh_alendronate.female<-
 get.sample(c(1-input$not.monitored.4m_adh_alendronate.female,
              input$not.monitored.4m_adh_alendronate.female))
rmultinorm$rmultinorm.not.monitored.4m_adh_risedronate.female<-
 get.sample(c(1-input$not.monitored.4m_adh_risedronate.female,
              input$not.monitored.4m_adh_risedronate.female))
rmultinorm$rmultinorm.not.monitored.4m_adh_strontium.female<-
 get.sample(c(1-input$not.monitored.4m_adh_strontium.female,
              input$not.monitored.4m_adh_strontium.female))
rmultinorm$rmultinorm.not.monitored.4m_adh_ibandronate.female<-
 get.sample(c(1-input$not.monitored.4m_adh_ibandronate.female,
              input$not.monitored.4m_adh_ibandronate.female))
rmultinorm$rmultinorm.not.monitored.4m_adh_denosumab.female<-
 get.sample(c(1-input$not.monitored.4m_adh_denosumab.female,
              input$not.monitored.4m_adh_denosumab.female))
rmultinorm$rmultinorm.not.monitored.4m_adh_zoledronate.female<-
 get.sample(c(1-input$not.monitored.4m_adh_zoledronate.female,
              input$not.monitored.4m_adh_zoledronate.female))
rmultinorm$rmultinorm.not.monitored.4m_adh_teriparatide.female<-
 get.sample(c(1-input$not.monitored.4m_adh_teriparatide.female,
              input$not.monitored.4m_adh_teriparatide.female))
rmultinorm$rmultinorm.not.monitored.4m_adh_abaloparatide.female<-
 get.sample(c(1-input$not.monitored.4m_adh_abaloparatide.female,
              input$not.monitored.4m_adh_abaloparatide.female))
rmultinorm$rmultinorm.not.monitored.4m_adh_romo.female<-
 get.sample(c(1-input$not.monitored.4m_adh_romo.female,
              input$not.monitored.4m_adh_romo.female))

rmultinorm$rmultinorm.not.monitored.12m_adh_alendronate.female<-
 get.sample(c(1-input$not.monitored.12m_adh_alendronate.female,
              input$not.monitored.12m_adh_alendronate.female))
rmultinorm$rmultinorm.not.monitored.12m_adh_risedronate.female<-
 get.sample(c(1-input$not.monitored.12m_adh_risedronate.female,
              input$not.monitored.12m_adh_risedronate.female))
rmultinorm$rmultinorm.not.monitored.12m_adh_strontium.female<-
 get.sample(c(1-input$not.monitored.12m_adh_strontium.female,
              input$not.monitored.12m_adh_strontium.female))
rmultinorm$rmultinorm.not.monitored.12m_adh_ibandronate.female<-
 get.sample(c(1-input$not.monitored.12m_adh_ibandronate.female,
              input$not.monitored.12m_adh_ibandronate.female))
rmultinorm$rmultinorm.not.monitored.12m_adh_denosumab.female<-
 get.sample(c(1-input$not.monitored.12m_adh_denosumab.female,
              input$not.monitored.12m_adh_denosumab.female))
rmultinorm$rmultinorm.not.monitored.12m_adh_zoledronate.female<-
 get.sample(c(1-input$not.monitored.12m_adh_zoledronate.female,
              input$not.monitored.12m_adh_zoledronate.female))
rmultinorm$rmultinorm.not.monitored.12m_adh_teriparatide.female<-
 get.sample(c(1-input$not.monitored.12m_adh_teriparatide.female,
              input$not.monitored.12m_adh_teriparatide.female))
rmultinorm$rmultinorm.not.monitored.12m_adh_abaloparatide.female<-
 get.sample(c(1-input$not.monitored.12m_adh_abaloparatide.female,
              input$not.monitored.12m_adh_abaloparatide.female))


# adh_annual ------
sample<- get.sample.no.fls_fls(
   probs.no.fls=c(input$adh_annual_decline_alendronate.no.fls.male,  # 1 stop adhering
              1-input$adh_annual_decline_alendronate.no.fls.male),# 2 continuing adhering
   probs.fls=c(input$adh_annual_decline_alendronate.fls.male, 
              1-input$adh_annual_decline_alendronate.fls.male)
   )  
rmultinorm$rmultinorm.adh_annual_decline_alendronate.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.adh_annual_decline_alendronate.fls.male<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(input$adh_annual_decline_risedronate.no.fls.male,  # 1 stop adhering
              1-input$adh_annual_decline_risedronate.no.fls.male),# 2 continuing adhering
   probs.fls=c(input$adh_annual_decline_risedronate.fls.male, 
              1-input$adh_annual_decline_risedronate.fls.male)
   )  
rmultinorm$rmultinorm.adh_annual_decline_risedronate.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.adh_annual_decline_risedronate.fls.male<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(input$adh_annual_decline_strontium.no.fls.male,  # 1 stop adhering
              1-input$adh_annual_decline_strontium.no.fls.male),# 2 continuing adhering
   probs.fls=c(input$adh_annual_decline_strontium.fls.male, 
              1-input$adh_annual_decline_strontium.fls.male)
   )  
rmultinorm$rmultinorm.adh_annual_decline_strontium.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.adh_annual_decline_strontium.fls.male<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(input$adh_annual_decline_ibandronate.no.fls.male,  # 1 stop adhering
              1-input$adh_annual_decline_ibandronate.no.fls.male),# 2 continuing adhering
   probs.fls=c(input$adh_annual_decline_ibandronate.fls.male, 
              1-input$adh_annual_decline_ibandronate.fls.male)
   )  
rmultinorm$rmultinorm.adh_annual_decline_ibandronate.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.adh_annual_decline_ibandronate.fls.male<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(input$adh_annual_decline_denosumab.no.fls.male,  # 1 stop adhering
              1-input$adh_annual_decline_denosumab.no.fls.male),# 2 continuing adhering
   probs.fls=c(input$adh_annual_decline_denosumab.fls.male, 
              1-input$adh_annual_decline_denosumab.fls.male)
   )  
rmultinorm$rmultinorm.adh_annual_decline_denosumab.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.adh_annual_decline_denosumab.fls.male<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(input$adh_annual_decline_zoledronate.no.fls.male,  # 1 stop adhering
              1-input$adh_annual_decline_zoledronate.no.fls.male),# 2 continuing adhering
   probs.fls=c(input$adh_annual_decline_zoledronate.fls.male, 
              1-input$adh_annual_decline_zoledronate.fls.male)
   )  
rmultinorm$rmultinorm.adh_annual_decline_zoledronate.no.fls.male<-unlist(sample[1])
rmultinorm$rmultinorm.adh_annual_decline_zoledronate.fls.male<-unlist(sample[2])






sample<- get.sample.no.fls_fls(
   probs.no.fls=c(input$adh_annual_decline_alendronate.no.fls.female,  # 1 stop adhering
              1-input$adh_annual_decline_alendronate.no.fls.female),# 2 continuing adhering
   probs.fls=c(input$adh_annual_decline_alendronate.fls.female, 
              1-input$adh_annual_decline_alendronate.fls.female)
   )  
rmultinorm$rmultinorm.adh_annual_decline_alendronate.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.adh_annual_decline_alendronate.fls.female<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(input$adh_annual_decline_risedronate.no.fls.female,  # 1 stop adhering
              1-input$adh_annual_decline_risedronate.no.fls.female),# 2 continuing adhering
   probs.fls=c(input$adh_annual_decline_risedronate.fls.female, 
              1-input$adh_annual_decline_risedronate.fls.female)
   )  
rmultinorm$rmultinorm.adh_annual_decline_risedronate.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.adh_annual_decline_risedronate.fls.female<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(input$adh_annual_decline_strontium.no.fls.female,  # 1 stop adhering
              1-input$adh_annual_decline_strontium.no.fls.female),# 2 continuing adhering
   probs.fls=c(input$adh_annual_decline_strontium.fls.female, 
              1-input$adh_annual_decline_strontium.fls.female)
   )  
rmultinorm$rmultinorm.adh_annual_decline_strontium.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.adh_annual_decline_strontium.fls.female<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(input$adh_annual_decline_ibandronate.no.fls.female,  # 1 stop adhering
              1-input$adh_annual_decline_ibandronate.no.fls.female),# 2 continuing adhering
   probs.fls=c(input$adh_annual_decline_ibandronate.fls.female, 
              1-input$adh_annual_decline_ibandronate.fls.female)
   )  
rmultinorm$rmultinorm.adh_annual_decline_ibandronate.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.adh_annual_decline_ibandronate.fls.female<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(input$adh_annual_decline_denosumab.no.fls.female,  # 1 stop adhering
              1-input$adh_annual_decline_denosumab.no.fls.female),# 2 continuing adhering
   probs.fls=c(input$adh_annual_decline_denosumab.fls.female, 
              1-input$adh_annual_decline_denosumab.fls.female)
   )  
rmultinorm$rmultinorm.adh_annual_decline_denosumab.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.adh_annual_decline_denosumab.fls.female<-unlist(sample[2])

sample<- get.sample.no.fls_fls(
   probs.no.fls=c(input$adh_annual_decline_zoledronate.no.fls.female,  # 1 stop adhering
              1-input$adh_annual_decline_zoledronate.no.fls.female),# 2 continuing adhering
   probs.fls=c(input$adh_annual_decline_zoledronate.fls.female, 
              1-input$adh_annual_decline_zoledronate.fls.female)
   )  
rmultinorm$rmultinorm.adh_annual_decline_zoledronate.no.fls.female<-unlist(sample[1])
rmultinorm$rmultinorm.adh_annual_decline_zoledronate.fls.female<-unlist(sample[2])


# for costing
#browser()
# 1 not hospitalised, 2) hopsitalised (for spine inpatient or outpatient)
rmultinorm$rmultinorm.fx.hosp.hip<-
 get.sample(c(input$prop.not.admitted.clinic.hip,
   input$prop.admitted.surgery.hip+
              input$prop.admitted.no.surgery.hip
              ))

rmultinorm$rmultinorm.fx.hosp.other<-
 get.sample(c(input$prop.not.admitted.clinic.other,
              input$prop.admitted.surgery.other+
              input$prop.admitted.no.surgery.other))

rmultinorm$rmultinorm.fx.hosp.spine<-
 get.sample(c(input$prop.hospital.community.spine,
              input$prop.hospital.spine))



#surgery- hip/other -if admitted
# 1 no surgery, 2 surgery
rmultinorm$rmultinorm.fx.hosp.surgery.hip<-
 get.sample(c(input$prop.admitted.no.surgery.hip,
              input$prop.admitted.surgery.hip))
rmultinorm$rmultinorm.fx.hosp.surgery.other<-
 get.sample(c(input$prop.admitted.no.surgery.other,
              input$prop.admitted.surgery.other))

#surgery- spine
# -if admitted
# 1 no surgery, 2 kyphoplasty, 3 vertebroplasty
rmultinorm$rmultinorm.fx.hosp.surgery.spine<-
 get.sample(c(input$prop.hospital.spine.no.intervention,
              input$prop.hospital.spine.kyphoplasty,
              input$prop.hospital.spine.vertebroplasty))
#if not admitted
rmultinorm$rmultinorm.fx.not.admitted.surgery.spine<-
 get.sample(c(input$prop.not.admitted.spine.no.intervention,
              input$prop.not.admitted.spine.kyphoplasty,
              input$prop.not.admitted.spine.vertebroplasty))

#browser()

# for location
# it will depend where they are, can't revert to



# any possible
# 1 temp rehap
# 2 home no support
# 3 home support
# 4 family home
# 5 long term care

rmultinorm$rmultinorm.discharged.hip.all<-
 get.sample(c(input$prop.discharged.temp.rehab.hip,
              input$prop.discharged.home.no.support.hip,
              input$prop.discharged.home.support.hip,
              input$prop.discharged.family.home.hip,
              input$prop.discharged.long.term.care.hip))


# from temp rehab
# 1 home no support
# 2 home support
# 3 family home
# 4 long term care
rmultinorm$rmultinorm.discharged.hip.temp.rehab<-
 get.sample(c(input$prop.rehab.to.home.no.support.hip,
              input$prop.rehab.to.home.support.hip,
              input$prop.rehab.to.family.home.hip,
              input$prop.rehab.to.long.term.care.hip))


# from home no support
#scale remaining
sum<-sum(input$prop.discharged.home.no.support.hip,
              input$prop.discharged.home.support.hip,
              input$prop.discharged.family.home.hip,
              input$prop.discharged.long.term.care.hip)
# 1 home no support
# 2 home support
# 3 family home
# 4 long term care
rmultinorm$rmultinorm.discharged.hip.from.home.no.support<-
get.sample(c(input$prop.discharged.home.no.support.hip/sum,
input$prop.discharged.home.support.hip/sum,
input$prop.discharged.family.home.hip/sum,
input$prop.discharged.long.term.care.hip/sum))


#from home support
sum<-sum(input$prop.discharged.home.support.hip,
              input$prop.discharged.family.home.hip,
              input$prop.discharged.long.term.care.hip)
# 1 home support
# 2 family home
# 3 long term care
rmultinorm$rmultinorm.discharged.hip.from.home.support<-
get.sample(c(input$prop.discharged.home.support.hip/sum,
input$prop.discharged.family.home.hip/sum,
input$prop.discharged.long.term.care.hip/sum))

#from family home
# 1 family home
# 2 long term care
sum<-sum(input$prop.discharged.family.home.hip,
              input$prop.discharged.long.term.care.hip)
rmultinorm$rmultinorm.discharged.hip.from.family.home<-
get.sample(c(
input$prop.discharged.family.home.hip/sum,
input$prop.discharged.long.term.care.hip/sum))



# any possible
# 1 temp rehap
# 2 home no support
# 3 home support
# 4 family home
# 5 long term care

rmultinorm$rmultinorm.discharged.spine.all<-
 get.sample(c(input$prop.discharged.temp.rehab.spine,
              input$prop.discharged.home.no.support.spine,
              input$prop.discharged.home.support.spine,
              input$prop.discharged.family.home.spine,
              input$prop.discharged.long.term.care.spine))


# from temp rehab
# 1 home no support
# 2 home support
# 3 family home
# 4 long term care
rmultinorm$rmultinorm.discharged.spine.temp.rehab<-
 get.sample(c(input$prop.rehab.to.home.no.support.spine,
              input$prop.rehab.to.home.support.spine,
              input$prop.rehab.to.family.home.spine,
              input$prop.rehab.to.long.term.care.spine))


# from home no support
#scale remaining
sum<-sum(input$prop.discharged.home.no.support.spine,
              input$prop.discharged.home.support.spine,
              input$prop.discharged.family.home.spine,
              input$prop.discharged.long.term.care.spine)
# 1 home no support
# 2 home support
# 3 family home
# 4 long term care
rmultinorm$rmultinorm.discharged.spine.from.home.no.support<-
get.sample(c(input$prop.discharged.home.no.support.spine/sum,
input$prop.discharged.home.support.spine/sum,
input$prop.discharged.family.home.spine/sum,
input$prop.discharged.long.term.care.spine/sum))


#from home support
sum<-sum(input$prop.discharged.home.support.spine,
              input$prop.discharged.family.home.spine,
              input$prop.discharged.long.term.care.spine)
# 1 home support
# 2 family home
# 3 long term care
if(input$prop.discharged.family.home.spine>0){
rmultinorm$rmultinorm.discharged.spine.from.home.support<-
get.sample(c(input$prop.discharged.home.support.spine/sum,
input$prop.discharged.family.home.spine/sum,
input$prop.discharged.long.term.care.spine/sum))} else {
  rmultinorm$rmultinorm.discharged.spine.from.home.support<-1
}

#from family home
# 1 family home
# 2 long term care
sum<-sum(input$prop.discharged.family.home.spine,
              input$prop.discharged.long.term.care.spine)
if(sum>0){
rmultinorm$rmultinorm.discharged.spine.from.family.home<-
get.sample(c(
input$prop.discharged.family.home.spine/sum,
input$prop.discharged.long.term.care.spine/sum))} else {
 rmultinorm$rmultinorm.discharged.spine.from.family.home<-0
}




# any possible
# 1 temp rehap
# 2 home no support
# 3 home support
# 4 family home
# 5 long term care

rmultinorm$rmultinorm.discharged.other.all<-
 get.sample(c(input$prop.discharged.temp.rehab.other,
              input$prop.discharged.home.no.support.other,
              input$prop.discharged.home.support.other,
              input$prop.discharged.family.home.other,
              input$prop.discharged.long.term.care.other))


# from temp rehab
# 1 home no support
# 2 home support
# 3 family home
# 4 long term care
rmultinorm$rmultinorm.discharged.other.temp.rehab<-
 get.sample(c(input$prop.rehab.to.home.no.support.other,
              input$prop.rehab.to.home.support.other,
              input$prop.rehab.to.family.home.other,
              input$prop.rehab.to.long.term.care.other))


# from home no support
#scale remaining
sum<-sum(input$prop.discharged.home.no.support.other,
              input$prop.discharged.home.support.other,
              input$prop.discharged.family.home.other,
              input$prop.discharged.long.term.care.other)
# 1 home no support
# 2 home support
# 3 family home
# 4 long term care
rmultinorm$rmultinorm.discharged.other.from.home.no.support<-
get.sample(c(input$prop.discharged.home.no.support.other/sum,
input$prop.discharged.home.support.other/sum,
input$prop.discharged.family.home.other/sum,
input$prop.discharged.long.term.care.other/sum))


#from home support
sum<-sum(input$prop.discharged.home.support.other,
              input$prop.discharged.family.home.other,
              input$prop.discharged.long.term.care.other)
# 1 home support
# 2 family home
# 3 long term care
if(sum>0){
rmultinorm$rmultinorm.discharged.other.from.home.support<-
get.sample(c(input$prop.discharged.home.support.other/sum,
input$prop.discharged.family.home.other/sum,
input$prop.discharged.long.term.care.other/sum))} else{
  rmultinorm$rmultinorm.discharged.other.from.home.support<-0
}

#from family home
# 1 family home
# 2 long term care
sum<-sum(input$prop.discharged.family.home.other,
              input$prop.discharged.long.term.care.other)
if(sum>0){
rmultinorm$rmultinorm.discharged.other.from.family.home<-
get.sample(c(
input$prop.discharged.family.home.other/sum,
input$prop.discharged.long.term.care.other/sum))} else{
  rmultinorm$rmultinorm.discharged.other.from.family.home<-0
}



#browser()
# lab test
#1 no
#2 yes
rmultinorm$rmultinorm.lab.test.no.fls.hip<-
 get.sample(c(1-input$prop.lab.test.no.fls.hip,
              input$prop.lab.test.no.fls.hip))
rmultinorm$rmultinorm.lab.test.no.fls.spine<-
 get.sample(c(1-input$prop.lab.test.no.fls.spine,
              input$prop.lab.test.no.fls.spine))
rmultinorm$rmultinorm.lab.test.no.fls.other<-
 get.sample(c(1-input$prop.lab.test.no.fls.other,
              input$prop.lab.test.no.fls.other))

rmultinorm$rmultinorm.lab.test.fls.hip<-
 get.sample(c(1-input$prop.lab.test.fls.hip,
              input$prop.lab.test.fls.hip))
rmultinorm$rmultinorm.lab.test.fls.spine<-
 get.sample(c(1-input$prop.lab.test.fls.spine,
              input$prop.lab.test.fls.spine))
rmultinorm$rmultinorm.lab.test.fls.other<-
 get.sample(c(1-input$prop.lab.test.fls.other,
              input$prop.lab.test.fls.other))

#dxa
rmultinorm$rmultinorm.dxa.no.fls.hip<-
 get.sample(c(1-input$prop.dxa.no.fls.hip,
              input$prop.dxa.no.fls.hip))
rmultinorm$rmultinorm.dxa.no.fls.spine<-
 get.sample(c(1-input$prop.dxa.no.fls.spine,
              input$prop.dxa.no.fls.spine))
rmultinorm$rmultinorm.dxa.no.fls.other<-
 get.sample(c(1-input$prop.dxa.no.fls.other,
              input$prop.dxa.no.fls.other))

rmultinorm$rmultinorm.dxa.fls.hip<-
 get.sample(c(1-input$prop.dxa.fls.hip,
              input$prop.dxa.fls.hip))
rmultinorm$rmultinorm.dxa.fls.spine<-
 get.sample(c(1-input$prop.dxa.fls.spine,
              input$prop.dxa.fls.spine))
rmultinorm$rmultinorm.dxa.fls.other<-
 get.sample(c(1-input$prop.dxa.fls.other,
              input$prop.dxa.fls.other))




# trans ----
for(i in 1:61){
   rmultinorm$rmultinorm.trans[[i]]<-set.sampe.trans()
}

rmultinorm
}
rmultinorm<-list()
for (i in 1:61) {
  rmultinorm[[i]]<-get.rmultinorm1(input=user_inputs, n_microsimulation, microsim_pop=microsim_pop)
}
rmultinorm<-bind_rows(rmultinorm[[i]])
