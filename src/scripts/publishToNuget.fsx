#load "build.fsx"
open Build

cleanPackFolder ()
pack "../src/TheBlunt/TheBlunt.fsproj"
publish ()
