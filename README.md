This is the Github repository for data and replication code for my paper, ["Who Pays for Reparations?,"](https://manhattan.institute/article/who-pays-for-reparations-the-immigration-challenge-in-the-reparations-debate?utm_source=external&utm_medium=github) released by the Manhattan Institute on October 19, 2023.

Code for constructing the death estimates is in the Death Estimates/ folder. 

Code for constructing the birth estimates is in the Birth Estimates/ folder. 

Code for simulating population growth post-1860 absent immigration is in Population Estimates/. 

Charts are outputted to img/.

The birth estimates are dependent on the death estimates, and population estimates are dependent on both. Therefore, the scripts must be executed in order, as in Construct and Simulate.R.

Code for reconstructing the appendix is in Household Income Estimates/. To execute properly, it requires an IPUMS ACS 2021 extract including the variable ANCESTR1, HHINCOME, and HHWT. I do not include such a file because it would be much too large, but readers can download one from www.ipums.org
