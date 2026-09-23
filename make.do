// the 'make.do' file is automatically created by 'github' package.
// execute the code below to generate the package installation files.
// DO NOT FORGET to update the version of the package, if changed!
// for more information visit http://github.com/haghish/github

make mlim, replace toc pkg  version(0.1)                                     ///
     license("MIT")                                                          ///
     author("E. F. Haghish")                                                 ///
     affiliation("Department of Psychological Sciences, University of Bergen") ///
     email("haghish@uib.no")                                                 ///
     url("https://github.com/haghish/mlim")                                  ///
     title("Single and Multiple Imputation with Automated Machine Learning") ///
     description("")                                                         ///
     install("mlim.do;mlim.do;mlim.sthlp;mlim.sthlp;mlim.do")                ///
     iancillary("")                                                         

// Generate the MarkDoc documentation
markdoc "mlim.ado", mini export(sthlp) replace
