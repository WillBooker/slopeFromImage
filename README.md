# slopeFromImage #

This repository uses a three step process from which the real space slope values for water and bed surfaces,
and the depth of pools can be derived present in a side viewing camera used on the LFP at UBC Ponderosa flume lab.
This is for both posterity and versioning, as well as future use of the flume for undergraduate and other graduate
work.
This model is intended for use with .tif files, but should be able to be used with other file formats supported by EBImage.
This approach would require the specification of file extension and subsequent alteration of function code. 

This repository uses R and several packages within it: raster and sp, dplyr, randomForest, EBImage, caret, rgeos and their dependencies.
The end result is achieved through the classification of provided images, segregated by an inclusion/exclusion vector, of the flume
using a randomForest model provided either in person or developed by the user.
The script saves these classified images as matrices (.txt files) into the directory where the images are stored.
Next, the water and bed surfaces are located and their real slopes/positions derived from pixel values corrected into
real space through supplied or created rasters of x and y coordinates.
These values are saved as pointsList and regList for water/bed surface.
Finally, the difference between the two surfaces are calculated, assumed as equatable to the water depth.
The poolstat.rds file stores the list of mean, max, min and sd of pool depths for each image.
