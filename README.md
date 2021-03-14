# Prostate cancer buffy coat project

## Scripts

**Tidying.R** = Takes raw CSV and tidies it up, and creates nested dataframes. Produces nestdb.RDS, nestdb_g1.RDS (Start at GGG1), nestdb_gx.RDS (Start at GGG >1). *Needs to be run first otherwise required .RDS files will not be generated for other scripts. Alternatively this can just be sourced.*

**BLDescr.R** = Computes baseline charasteristics of patient cohort.

**TreatDresc.R** = Computes charasteristics noted over active surveillance and following start of treatment.

**Surv.R** = Generates Kaplan-Myer surivival curves

**ExportData.R** = Prepares tidied nested dataframe for export in multiple formats.

## Other

**/data** = CSV input for Tidying.R script go in here

**/output** = Graphs and data for export does here.

**/old** = Old archived scripts. 
