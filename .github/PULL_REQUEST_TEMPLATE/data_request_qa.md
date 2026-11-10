# Data request QA checklist

- [ ] Check code runs without errors
- [ ] Check StEIS search uses correct dates and does not apply any location filters
- [ ] Check filters that are applied are correct:
  - [ ] That the dates match the dates required in the DevOps request
  - [ ] That the categorical filters match the information in the DevOps request. Check that the codes are correct  (see the codes dataframe (NRLS) or ResponseReference dataframe (LFPSE)).
  - [ ] That the NRLS country filter has been applied if the country is set to England. (LFPSE is only England so this is not required) 

- [ ] Check that the text filters make sense and follow the requirements in the data request.
  - [ ] These can be quite error prone. One thing to check is that the new word string is correct ( \\b)- it is easy to miss the b off which stops the code working properly. 

- [ ] Check that the other parameters are correct, including sampling. 
- [ ] If the sample is very large, confirm from the data request that the requester is prepared for this. 
- [ ] Check that any new code makes sense and does not introduce errors. The pull request should highlight the changes that have been made 
- [ ] Check that the output file looks correct 
 
 Add comments to the pull request, and either approve, comment or request changes to indicate the next steps required. 

 
