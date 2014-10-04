
Mathematica2rst
==============================

convert Mathematica notebook to reStructureText format

Quick Install
--------------------------

Copy Export.m and Converter.m to ~/.Mathematica/SystemFiles/Formats/RST

Example
--------------

   SetDirectory[NotebookDirectory[]];
   notebook=Import["README.nb"];
   Export["README.rst",notebook,"RST"];
