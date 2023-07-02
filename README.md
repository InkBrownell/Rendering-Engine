# Haskell Rendering Engine
This program is a toy renderer, which creates PNG images from OBJ model files.
If you have non-OBJ models you wish to view, use a converter, like this one,
to convert them to OBJ:

http://www.greentoken.de/onlineconv/

# Compiling


Basic build:
`ghc renderer.hs`

Optimized build:
`ghc -O2 -threaded renderer.hs`

Debug build:
`ghc -rtsopts renderer.hs`

# Running
The program must be executed from the command line, and requires 6 arguments:

1. Input model file (in .obj format)
2. Output picture location
3. Model scale factor
4. Camera's distance from origin
5. Camera's azimuth angle (in degrees)
6. Camera's inclination angle (in degrees)

For example, here are some valid commands:

`./renderer testmodel.obj testpicture.png 1.0 3.0 45.0 25.0`

`./renderer testmodel.obj testpicture.png 0.3 25 10 -35`

On Windows, substitute "renderer.exe" for "./renderer", e.g.:

`renderer.exe in.obj out.png 1 1 0 0`
