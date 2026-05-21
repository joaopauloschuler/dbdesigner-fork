
// Fix for PaletteToolsForm.FormCloseQuery - defensive check
// Replace line 216:
// Original:   TMainForm(Application.MainForm).ToolsMI.Checked:=False;
// New:        if Assigned(Application.MainForm) and Assigned(TMainForm(Application.MainForm).FindComponent('ToolsMI')) then
//               TMainForm(Application.MainForm).ToolsMI.Checked:=False;
