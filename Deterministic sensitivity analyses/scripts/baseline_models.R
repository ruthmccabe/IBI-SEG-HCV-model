### four nations baseline models 

source("Model/scripts/deterministic_model.R")

england <- task4_deterministic_short_nation(nation="England",
                                            deferral_reduction = 0.67,
                                            deferral_effect_year = 1985,
                                            donor_non_clearance = 0.74,
                                            contamination_constant_prop = 0.25,
                                            units_per_donation_scenario = "NBTSS",
                                            trans_survival_years=10,
                                            hepc_survival_hazard = 1.53,
                                            trans_survival_hazard_scenario = "age",
                                            survivor_non_clearance = 0.82)


ni <- task4_deterministic_short_nation(nation="Northern Ireland",
                                            deferral_reduction = 0.67,
                                            deferral_effect_year = 1985,
                                            donor_non_clearance = 0.74,
                                            contamination_constant_prop = 0.25,
                                            units_per_donation_scenario = "NBTSS",
                                            trans_survival_years=10,
                                            hepc_survival_hazard = 1.53,
                                            trans_survival_hazard_scenario = "age",
                                            survivor_non_clearance = 0.82)


wales <- task4_deterministic_short_nation(nation="Wales",
                                            deferral_reduction = 0.67,
                                            deferral_effect_year = 1985,
                                            donor_non_clearance = 0.74,
                                            contamination_constant_prop = 0.25,
                                            units_per_donation_scenario = "NBTSS",
                                            trans_survival_years=10,
                                            hepc_survival_hazard = 1.53,
                                            trans_survival_hazard_scenario = "age",
                                            survivor_non_clearance = 0.82)

scotland <- task4_deterministic_short_nation(nation="Scotland",
                                            deferral_reduction = 0.67,
                                            deferral_effect_year = 1984,
                                            donor_non_clearance = 0.74,
                                            contamination_constant_prop = 0.25,
                                            units_per_donation_scenario = "NBTSS",
                                            trans_survival_years=10,
                                            hepc_survival_hazard = 1.53,
                                            trans_survival_hazard_scenario = "age",
                                            survivor_non_clearance = 0.82)

