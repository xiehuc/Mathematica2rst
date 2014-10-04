(* ::Package:: *)

Begin["Codex`Convert`NotebookDump`"]

ClearAll[ConvertCell,ConvertText,ConvertForm]
$ConvertFileName="";
$ConvertImageType="png";
$ImageCount=1;

ConvertCell[n_Notebook]:=StringJoin[ToString/@Flatten[ConvertCell@@#&/@n[[1]]]];
ConvertCell[t_,"Title",__]:=ConvertTitle[t,"=="];
ConvertCell[t_,"Section",__]:=ConvertTitle[t,"--"];
ConvertCell[t_,"Chapter",__]:=ConvertTitle[t,"--"];
ConvertCell[t_,"Subsection",__]:=ConvertTitle[t,"~~"];
ConvertCell[t_,"SubChapter",__]:=ConvertTitle[t,"~~"];
ConvertCell[t_,"Input",__]:="::\n\n"<>"   "<>ConvertText[t];
ConvertCell[t_String,"Text",__]:=ToString[t]<>"\n";
ConvertCell[t_TextData,type_,__]:=ConvertText@@#&/@t[[1]]<>"\n";
ConvertCell[t_,"Equation",__]:=ConvertText[t]<>"\n";
ConvertCell[l_List,___]:=ConvertCell@@#&/@l;
ConvertCell[cg_CellGroupData]:=ConvertCellGroup@@cg;
ConvertCell[c_BoxData,___]:=ConvertText[c];
ConvertCell[t_String,type_,__]:=ConvertText[t]<>"\n";

ConvertTitle[t_,sub_]:="\n"<>t<>"\n"<>StringJoin[ConstantArray[sub,StringLength[t]]]<>"\n\n";

ConvertCellGroup[l_List]:=ConvertCell[l];
ConvertCellGroup[l_List,Open]:=ConvertCell[l];
ConvertCellGroup[l_List,o_List]:=ConvertCell[l[[o]]];

Options[ConvertText]={FontColor->Black};
ConvertText["\[IndentingNewLine]"]:="\n"<>"   ";
ConvertText[t_String,OptionsPattern[]]:=t;
ConvertText[c_BoxData,___]:=ConvertText@@c;
ConvertText[c_List,___]:=ConvertText/@c;
ConvertText[c_RowBox]:=ConvertText@@c(*ConvertText@@#&/@c[[1]]*)
ConvertText[g_GraphicsBox,___]:=ConvertImage[RawBoxes[g],GenerateFileName[$ConvertFileName]]
ConvertText[g_Graph,___]:="";
ConvertText[c_FormBox]:=ConvertForm@@c;


ConvertForm[c_,TraditionalForm]:="$"<>ConvertForm[c]<>"$";
ConvertForm[s_String]:="\\text{"<>s<>"}"/;AnyTrue[ToCharacterCode[s],#>10000&&#<60000&](*\:5305\:542b\:6c49\:5b57*)
ConvertForm[s_String]:=s/;AllTrue[ToCharacterCode[s],#<256&](*\:5168\:90e8\:662fASCII\:7801*)
ConvertForm[s_String]:=ToString[TeXForm[s]]<>" "(*\:6709\:7279\:6b8a\:7b26\:53f7,\:5982\cdots*)
ConvertForm[r_RowBox]:=StringJoin[ConvertForm[#]&/@r[[1]]];

(*ConvertForm[c_RowBox]:=ToString[TeXForm[RawBoxes[c]]]*)
ConvertForm[c_]:=ToString[TeXForm[RawBoxes[c]]]

GenerateFileName[filename_]:=FileBaseName[filename]<>"."<>ToString[$ImageCount++]<>"."<>$ConvertImageType

ConvertImage[g_,file_]:="\n.. image:: "<>Export[file,g,$ConvertImageType]<>"\n\n";

ConvertEvaluation[t_]:=CreateDocument[t,Visible->False]//NotebookEvaluate//TeXForm//ToString

Options[myExportRST] = {"ImageFormat"->"png"};
myExportRST[filename_, rules_, opts : OptionsPattern[]] := 
Module[
  {},
  $ImageCount=1;
  $ConvertFileName=filename;
  stream = OpenWrite[filename,CharacterEncoding->"UTF8"];
  WriteString[stream,ConvertCell[rules]];
  Close[stream];
]

ExportRST[filename_, rules_, opts___] := myExportRST[filename, rules, FilterRules[Flatten@{opts}, Options[myExportRST]]]

End[]






