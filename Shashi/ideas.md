# Objectives
1. a. Find similar regions/age-groups/sexes (categories) that consistently show similar normalised excess-mortality (EM) trends (esp. high EM)
1. b. Over what periods of time do these categories show abovementioned trends
2. Visualise EM time-series without clutter (most important)
3. Repeat 1 and 2 for other metrics e.g. normalisedcovid mortality

# Sub-problems for Objective 1
1. Run analysis at Region level
1. a. Do separate analysis for M/F sexes
2. Run analysis at Region x Age-Group level
2. a. Do separate analysis for M/F sexes
~~3. Run analysis at Region x Sex level~~
~~4. Run analysis at Region x Age-Group x Sex level~~


# Data Pre-processing
1. Based on the sub-problem to be solved, exclude or merge categories with small population - low priority
2. What is small population? Can visualisation help? - low priority; perhaps we can review after data is visualised

# Alignment Problem/Getting Similarity Scores whilst adjusting for lags:
1. Use DTW with modifications (see [link](https://tslearn.readthedocs.io/en/stable/user_guide/dtw.html#id1)) -- use plain vanilla DTW for now
2. LCSS -- low priority
3. ...
4. Which method is better? Equally weight all scores? -- low priority

# Cluster Categories
1. Use same procedures (except manual Czekanowski Diagram read??) as in Shashi RP; review Yin's work

# Visualisation
1. Cluster mean (median) +/- 1 SD (25th/75h percentiles) + outliers (box-plot style?) for time-series graphics
2. Interactive Tool - low priority

# Comment
- Involve Martin
