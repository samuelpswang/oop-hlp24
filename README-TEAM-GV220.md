The team-phase work is in [TestDrawBlockD3] (src/Renderer/TestDrawBlockD3.fs). I created several helper functions and test circuits:

1. Added a better circuit generation DSL - which takes a list of components and a list of connections to generate circuits that have at least user-specified threshold distance between components. Also used Gen<> to apply small random position changes
to each component.
2. The circuit generation DSL includes another helper function that only takes a list of components and connections are randomly generated (by shuffling components and connecting them pairwise). This implementation offers more non-deterministic testing.
These helper functions will allow the sheetWireLabelSymbol creator to quickly and easily generate tests by providing a simpler, more intuitive DSL.
3. Created a random set of "should work/easy" tests, specifically the example test provided in sheetWireLabelSymbol (Figure C1, C2). Also used Gen<> to apply small random position changes, random rotations and flips to components.
4. Created a random set of "likely to fail/hard" tests. These tests have 2-3 overlapping wires, multiple wires from the same output port to different target ports. Components in these tests were also a user-specified threshold distance apart, meaning
sheetWireLabelSymbol could be applied. 
These tests are useful in evaluating how well sheetWireLabelSymbol performs across a range of tests with varying difficulties. All these tests also have user-specified threshold distances between components, this allows the sheetWireLabelSymbol 
creator to evaluate the function's performance across different thresholds. The user can also specify a low threshold distance for the tests, and evaluate whether their function detects that threshold distances between components is too low and therefore
doesn't try to beautify them.
5. Added flip and rotate helper functions to SheetBeautifyHelpers as it offers more possible tests.
6. Created a metrics counter that when given a circuit schematic counts: the number of symbol overlaps, number of wires intersecting symbols, number of wire bends. "Should-never-happen" cases i.e. number of symbol overlaps > 0 and number of wire intersecting symbols > 0
are made into an assertion case. 
7. Another helper function: collectMetricsOfTests leverages the metrics counter by determining metrics of a sheet before and after applying sheetWireLabelSymbol (id is used as stub function).
These helper functions for metrics allows the sheetWireLabelSymbol creator to numerically evaluate the function's performance.