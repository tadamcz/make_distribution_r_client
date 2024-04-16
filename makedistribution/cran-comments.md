## R CMD check results
0 errors | 1 warnings | 2 notes

The warning is:

```
* checking for code which exercises the package ... WARNING
No examples, no tests, no vignettes
```

I have included an example in `inst/examples/example_usage.R`, but it seems this is not a standard location. Including examples inline using `@examples` and `roxygen2` would be quite verbose and unnecessary as each example would repeat most of the arguments indentically. 

If you can provide guidance on the idiomatic way to do this in the R ecosystem I'm happy to change it.