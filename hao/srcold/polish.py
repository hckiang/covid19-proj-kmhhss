import covid19poland as PL
import sys
PL.covid_death_cases().to_csv(sys.argv[1])
