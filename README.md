# Carnegie_Hall_Internship

The scripts folder contains different R-Scripts to read in the data and run different analysis,  specifically

1. Load_Data_Complete.Rmd
→ contains a detailed explanation on how we read in the data into R, which cleaning procedures we followed and how to connect it to a database
→ the final output is a dataframe containing the xyz-signals for the relevant window

2. Visualization_xyz.R
→ Loads the windowed IMU dataset from the DuckDB database (table acc_data_in_window_db) and produces quick diagnostic visualizations of the raw x/y/z acceleration signals.
→ The script samples a small number of participants via ECG_ParticipantID to keep plotting fast, materializes the subset as a temporary/persistent table for reuse, and then creates:
- Histograms of x/y/z per participant to inspect value ranges and potential sensor artifacts,
- Magnitude-over-time plots (√(x²+y²+z²)) to compare overall activity dynamics across participants,
- 3D trajectories (x vs. y vs. z) for a single participant and for a parent–child dyad (identified via ParticipantID and ParticipantType in masterdata_db) to visually assess movement structure and similarity.

3. Activity_level.R
→ Computes a simple activity metric from the windowed IMU signals by quantifying frame-to-frame changes in raw x/y/z acceleration within each ECG_ParticipantID.
→ Concretely, the script derives a per-sample movement measure as the Euclidean norm of successive differences (Δx, Δy, Δz), then aggregates this to participant-level summaries (e.g., mean/median movement and number of samples).
→ These participant-level activity summaries are joined with masterdata_db to attach Condition_Physio and ParticipantType, filtered to children where needed, and used for basic between-condition comparisons (e.g., Concert vs. Playtime) and accompanying visual checks.

4. Creating Dataset for Person-Person Synchrony.R
5. Person-Person Synchrony Ideas.R
6. T tests.R
   
