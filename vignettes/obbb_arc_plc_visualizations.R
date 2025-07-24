data <- readRDS("./vignettes/obbb.rds") %>%
  select(fips,state_name,county_name,crop,program_year,plc_payment,
         arc_payment, plc_payment_obbb, arc_payment_obbb )

