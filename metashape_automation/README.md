# Metashape - An automated workflow for processing drone imagery in Metashape

## NB. The code in this branch will work with the updated python API in MetaShape >1.8.5 and V2.0.2 (see file names for version)
These updates also ensure RTK image data are handled properly by Metashape with full location accuracy


**1. Interactive: Collate & Prepare Datasets**
- Find/start entry in the Image-Based Modelling Processing Log (use the ‘scripted log (for V2)’). Keep this log up to date as new parameters are confirmed and steps are completed.
- Create project directory (E.g. ‘Sev_SEG_20180531’, note updated date code format).
- Within the project directory, add an ‘Input_Data’ subdirectory, containing:
	- ‘photos’ subdirectory.
	- correctly formatted marker coordinate file (refer to example below!)
	- Other relevant datasets (e.g. ground-based photographs, biomass harvest data sheets, elemental composition datasheets, etc.)
- Determine the EPSG code of the desired coordinate reference system, and for the supplied marker coordinates (e.g. ‘WGS84 UTM 7N / EPSG:32607”, or “NAD83 UTM 7N / EPSG:26907”. Enter this into the processing log.
- Set up ‘input_file.csv’ parameters: mapping all file paths, specifying quality settings and desired output files and resolutions.

**2. Script 1: Camera Alignment**

- Run ‘MetaShp_Part1_SPC.py’ *(If running on ISCA note the Moab number for tracking purposes).*

**3. Interactive**
- If run on ISCA, note processing time, peak memory use and number of images (send information to Andy).
- Review Console output for error and/or warning messages (If on ISCA, ‘e’ & ‘o’ files.
Look at the number/% of points excluded by the reprojection filter.
- Review the ‘XXX_project_settings.csv’ file, to assess the proportion of aligned images, and the number of tie points excluded by the reprojection error filter.
- Consider whether it is necessary to review image quality manually (e.g. water, etc.).
- Review plausibility of sparse point cloud, and remove obvious outliers.
- Review plausibility of camera positions (Show Cameras), check for no large gaps in coverage . 
- Implement ten placements of all available markers. Place markers on the first five images (those where camera position is closest to the marker), and then another five images (ideally photographs displaying multiple markers).
- Deselect markers for independent accuracy assessment.

**4. Script 2: Dense Point Cloud**
- Run ‘MetaShp_Part2_DPC_MS.py’ *(If running on ISCA note the Moab number for tracking purposes).*

**5. Interactive**
- If running on ISCA, note processing time, peak memory use, dense cloud quality setting and number of images (send information to Andy).
- Review console output for errors.
- Review plausibility of dense point cloud (& remove obvious outliers).

**6. Script 3: Export**
- Run ‘MetaShp_Part3_Exp.py’ *(If running on ISCA note the Moab number for tracking purposes).*

**7. Interactive**
- If run on ISCA, note processing time, peak memory use, whether texture was generated and number of images (send information to Andy).
- Review console output for errors.
- Review processing report:
	-Inspect all graphics
	-Inspect marker error values
	-Inspect mean reprojection errors
- Check all required files were generated:
	-	.laz
	- Orthomosaic
- Download data.
