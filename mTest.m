BeginPackage["mTest`"];
mTestLicenseText =
"MIT License

Copyright (c) 2018 George Dadunashvili

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the \"Software\"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
";
mTest::usage =
    "Copyright (c) 2017 George Dadunashvili

This package is licenced under MIT software license. For the complete license
text type: licenseText. For more information about the package type:
mTestDocs.";

(* TODO write an introduction to the package *)

mTestDocs = "not done yet!";
colorPrintExpr::usage = " ";
colorPrint::usage = "red failed or green passed for a boolean statement";
printFailed::usage = "Given a list of True's & False's, print the position of
 false statements";
dotPrint::usage = "print dot for True and F for False.";
allTrueTest::usage = "If all test results are true print all passed else print
which failed.";
formatedTest::usage = "ToDo."; (* ToDo *)
br::usage = "ToDo."; (* ToDo *)

Begin["Private`"]

cl  = "\033[0m"; (*clear*)
b = "\033[1m"; (*bold*)
red = "\033[31m";
green = "\033[32m";

orange = "\033[33m";
blue = "\033[34m";
purple = "\033[35m";

SetOptions[ $Output, FormatType -> OutputForm ];

colorPrintExpr =
    Function[{expr, test},
      Print[If[test, green<>#, red<>#]&@(b<>ToString[expr]<>cl)]];

colorPrint =
    Function[{test},
      Print[If[test, b<>green<>"passed"<>cl, b<>red<>"failed"<>cl]]];

Module[{failedTests, nAll, nFailed},
printFailed =
  Function[{testList},
    failedTests = Flatten@Position[testList, False];
    nAll = Length[testList];
    nFailed= Length[failedTests];
    Print[ToString[nFailed]<>" test(s) out of "<>
        ToString[nAll]<>red<>" failed"<>cl];
    If[nFailed == 0, None,
    If[nFailed == 1, Print["failed test: ", failedTests[[1]]],
      Print["failed test(s): ", failedTests]]];
  ];
];

dotPrint = Function[{testList},
   Block[{fStr = testList /. {True -> b<>green<>".", False -> red<>"F"}},
    Print[StringJoin[fStr]<>cl]]];

allTrueTest =
  Function[{allTests},
   Block[{allPassed = AllTrue[TrueQ]@allTests,
          allFailed = AllTrue[Not@TrueQ[#]&]@allTests},
     If[allPassed, Print[b<>green<>"all tests passed"<>cl],
       If[allFailed, Print[b<>red<>"all tests failed"<>cl], printFailed[allTests]]
     ]
   ]];

br = StringJoin@Table[ToString[#1], #2]&;

formatedTest[testName_String, functionResultPairList_] :=
    Module[{header, lh},
      header =
          "Performing "<> b <> testName <> cl <>" test ";
          lh = Length[Characters@header] - 2Length[Characters@b];

      Print[br["~", lh]];
      Print[br["~", lh]];
      Print[header];
      Print[br["=", Round@(2/3 * lh)]];

      Do[
        Print[b<>ToString@pair[[1]]<>cl];
        allTrueTest[pair[[2]]];
        Print[br["-", Round@(2/3 * lh)]],
        {pair, functionResultPairList}
      ];
    ];

End[];
EndPackage[];