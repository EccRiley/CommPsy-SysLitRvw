library(dplyr); library(Riley); library(lavaan); library(semPlot)

# Variable Descriptions For Reference
v1 <- "Pre-Trial Referral"
v2 <- "Post-Conviction Referral & Added Sanctions"
v3 <- "Program Duration"
v4 <- "Additional Alcohol and Substance Abuse Services"
v5 <- "Victim Services"
v6 <- "CTS-2"
v7 <- "Inventories of Non-Physical Abuse"

m.gondolf99 <- 'Intensity =~ v1 + v2 + v3
                Services =~ v4 + v5
                Comprehensiveness =~ Intensity + Services
                Recidivism =~ v6 + v7
                Recidivism ~ b*Intervention +
                    a*(Intervention)(Comprehensiveness)
                ab := a*b
                total := ab + c' %>% lavaanify()

semPaths(m.gondolf99)
