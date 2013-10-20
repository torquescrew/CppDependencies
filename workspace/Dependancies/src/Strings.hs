module Strings where


test2 = "class DataPaneBase { public : const ContextMenuInfoBase * GetContextMenuInfo () const { return mContextMenuInfo . get () ; } ContextMenuInfoBase * GetContextMenuInfoRef () { return mContextMenuInfo . get () ; } void SetContextMenuInfo ( ContextMenuInfoBase * info ) { mContextMenuInfo . reset ( info ) ; }"



allEntities :: String
allEntities = "/Users/tobysuggate/Documents/build_tool/AllEntities.cpp"


source :: String
source = "bool operator()(const IChartRecordDrawer *p1,const IChartRecordDrawer *p2)\n   {\n   long drawLayer1;\n   p1->GetLayer(&drawLayer1);\n   long drawLayer2;\n   p2->GetLayer(&drawLayer2);\n   return drawLayer1<drawLayer2;\n   }"

chartDrawer :: String
chartDrawer = "/Users/tobysuggate/Desktop/LCWM4/LabChartEssentials/LabChart/ChartDraw/ChartDrawer.h"

reactor :: String
reactor = "/Users/tobysuggate/Documents/build_tool/Reactor.h"
