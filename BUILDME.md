# Building AppleRed

## Building AppleRed.jar in/from the Red folder

### Using `scalac` via `make`

The command

        make

compiles (using `scalac`) the entire source into Red.jar, then makes
from it an `AppleRed.jar` that can be run from the java top-level
anywhere, on any operating system, because it has embedded in it
all the libraries it requires.

You will need to have jars corresponding to the libraries `regexkit,`
`scala-swing,` and a recent `scala-library` present: this can be
achieved by making symbolic links to them.

### Using `IntelliJ`

After an `IntelliJ` project build, an artefact named `AppleRed` will
be found in `./out/artifacts/AppleRed/` as `AppleRed.jar`; it should
be functionally equivalent to the `make`-built `AppleRed.jar`, and
can be used in subsequent steps.

## Distribution and Installation

When you are happy with the program, you can take steps to package
it for distribution or installation for your own purposes.  There
are detailed instructions for doing so in the folder `RedApp` (see
`RedApp/README.md` or `RedApp/README.pdf`). The OS/X distribution
instructions probably need following to the letter if you want to
build something that behaves like an OS/X app; but the Linux ones
can be obeyed less strictly.


## Working on new features

The files `Makefile`, and `Makefile-Scala` specify various other make
targets, some of which will be of use while working on new features
with `IntelliJ`.

        ./ired [args]

runs the editor using the compilation classpath and the current
`IntelliJ`-compiled class files.

If you don't use `IntelliJ` in your development it's not a
problem, because:

        ./sred [args]

runs the editor using the compilation classpath and `Red.jar`

You can test a new variant you have compiled with either of the above
two commands.

You specify the logging of classes/modules by providing additional
arguments of the form

        -l<package>.<class>

which logs at or below the most detailed level; or

        -l<package>.<class>=<level>

which logs at or below the specified level.


\newpage

For example:

        ./ired -lRed.EditSession=FINER exampletexts/L03.tex

generates this log as the first few mouse clicks are made


      INFO@Logging.Default     :
    **********
    AppleRed starting at 2022-09-05-182803
    **********
      INFO@Logging.Default  : Red.EditSession=FINER
     FINER@Red.EditSession  : theSession.setMark(254)
     FINER@Red.EditSession  : selectUntil 254 -> 254
     FINER@Red.EditSession  : setCursorAndMark(12, 35) with selection=Span(254,254,false)
     FINER@Red.EditSession  : theSession.setMark(240)
     FINER@Red.EditSession  : selectUntil 240 -> 240
     FINER@Red.EditSession  : setCursorAndMark(12, 21) with selection=Span(240,240,false)
     FINER@Red.EditSession  : theSession.setMark(246)
     FINER@Red.EditSession  : selectUntil 235 -> 246

\newpage

