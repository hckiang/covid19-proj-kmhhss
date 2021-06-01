# covid-death

`src` and `noweb` is some work in progress and `srcold` contains
previous stuff for Sweden. The reason why we need a completely
rewritten code is that the age groups of all the countries
are different, and there are missing data in both COVID-19
and EuroStat data sets.

In particular, Switzerland's COVID-19 data may contain errors;
and Germany's EuroStat data set lacks per-age male/female population
size (without this, we cannot compare the male and female ratios
of the national level to the regional excess death).

Source codes are documented in the noweb pdf, and everytime I update
this git repo I will push both the pdf and source code so you may
not need to use the `Makefile`, which only extract code from the
literate program.

*Currently I am still cleaning up the age group stuff so the code
in `src` may not be runnable*.