# hts2sts

## Description

Convert Household Travel Survey (trips and individuals) into state sequence format

## License

GNU AFFERO GENERAL PUBLIC LICENSE v3

## User guide

### Input tables

- `table of trips` with with the following fields (the order matters, the name does not):

1. Individual ID (join key with the table of individuals)
2. Trip origin (code or name)
3. Trip destination (code or name)
4. Departure hour (integer from 4 to 28, i.e. 4AM the next day)
5. Arrival hour (integer from 4 to 28, i.e. 4AM the next day)
6. Departure minute (integer from 0 to 59)
7. Arrival minute (integer from 0 to 59)
8. Purpose at origin (code or name)
9. Purpose at destination (code or name)
10. Transport mode (code or name)


- `table of individuals` with with the following fields (the order matters, the name does not):

1. Individual ID (join key with the table of trips)
2. Weights

**IMPORTANT: correspondance between both tables is required, i.e. no individual without trip and no trip without individual**


### Output table

- a character matrix corresponding to `TraMineR` SPS format. This table can be used directly with the `seqdef()` function.
