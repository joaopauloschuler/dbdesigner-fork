//==============================================================================
// Phase 37: Test PaletteNav, PaletteModel paint box clicks (tab-switching)
// SKIP OptionsImg on all three palettes — opens popup menu that blocks.
//==============================================================================
procedure Phase37_TestPaletteImagesAndPaintBoxes(AMainForm: TForm; var PassCount, FailCount, SkipCount: Integer);
var
  I: Integer;
  PaletteForm: TForm;
  Component: TComponent;
  Entry: TTestEntry;
begin
  Log('--- Phase 37: Testing palette TImage/TPaintBox clicks ---');
  Log('');

  // ---- PaletteNavForm: NavigatorPBox, InfoPBox ----
  PaletteForm := nil;
  for I := 0 to Screen.FormCount - 1 do
    if CompareText(Screen.Forms[I].ClassName, 'TPaletteNavForm') = 0 then
    begin
      PaletteForm := Screen.Forms[I];
      Break;
    end;

  if PaletteForm <> nil then
  begin
    Log('  Found PaletteNavForm.');

    // NavigatorPBox (TPaintBox) — tab-switching, safe
    Component := PaletteForm.FindComponent('NavigatorPBox');
    if (Component <> nil) and (Component is TPaintBox) and Assigned(TPaintBox(Component).OnClick) then
    begin
      Log('  [TRYING] clickable paintbox: ' + PaletteForm.Name + '.NavigatorPBox');
      Entry.ComponentName := PaletteForm.Name + '.NavigatorPBox';
      Entry.ComponentClass := Component.ClassName;
      Entry.ErrorMessage := '';
      Entry.StackTrace := '';
      try
        TPaintBox(Component).OnClick(Component);
        Application.ProcessMessages;
        Sleep(50);
        Application.ProcessMessages;
        Entry.Result := trPass;
        LogTestEntry(Entry);
        Inc(PassCount);
      except
        on E: Exception do
        begin
          Entry.Result := trFail;
          Entry.ErrorMessage := E.ClassName + ': ' + E.Message;
          Entry.StackTrace := GetExceptionStackTrace;
          LogTestEntry(Entry);
          Inc(FailCount);
        end;
      end;
      Application.ProcessMessages;
    end
    else
    begin
      Log('  [SKIP] PaletteNavForm.NavigatorPBox not found or has no OnClick handler.');
      Inc(SkipCount);
    end;

    // InfoPBox (TPaintBox) — tab-switching, safe
    Component := PaletteForm.FindComponent('InfoPBox');
    if (Component <> nil) and (Component is TPaintBox) and Assigned(TPaintBox(Component).OnClick) then
    begin
      Log('  [TRYING] clickable paintbox: ' + PaletteForm.Name + '.InfoPBox');
      Entry.ComponentName := PaletteForm.Name + '.InfoPBox';
      Entry.ComponentClass := Component.ClassName;
      Entry.ErrorMessage := '';
      Entry.StackTrace := '';
      try
        TPaintBox(Component).OnClick(Component);
        Application.ProcessMessages;
        Sleep(50);
        Application.ProcessMessages;
        Entry.Result := trPass;
        LogTestEntry(Entry);
        Inc(PassCount);
      except
        on E: Exception do
        begin
          Entry.Result := trFail;
          Entry.ErrorMessage := E.ClassName + ': ' + E.Message;
          Entry.StackTrace := GetExceptionStackTrace;
          LogTestEntry(Entry);
          Inc(FailCount);
        end;
      end;
      Application.ProcessMessages;
    end
    else
    begin
      Log('  [SKIP] PaletteNavForm.InfoPBox not found or has no OnClick handler.');
      Inc(SkipCount);
    end;

    // OptionsImg opens popup menu — skip
    Log('  [SKIP] PaletteNavForm.OptionsImg opens popup menu (skipped).');
    Inc(SkipCount);
  end
  else
  begin
    Log('  [SKIP] PaletteNavForm not found.');
    Inc(SkipCount, 2);
    Inc(SkipCount);
  end;

  // ---- PaletteModelForm: TablesPBox, ModelPBox ----
  PaletteForm := nil;
  for I := 0 to Screen.FormCount - 1 do
    if CompareText(Screen.Forms[I].ClassName, 'TPaletteModelForm') = 0 then
    begin
      PaletteForm := Screen.Forms[I];
      Break;
    end;

  if PaletteForm <> nil then
  begin
    Log('  Found PaletteModelForm.');

    // TablesPBox (TPaintBox)
    Component := PaletteForm.FindComponent('TablesPBox');
    if (Component <> nil) and (Component is TPaintBox) and Assigned(TPaintBox(Component).OnClick) then
    begin
      Log('  [TRYING] clickable paintbox: ' + PaletteForm.Name + '.TablesPBox');
      Entry.ComponentName := PaletteForm.Name + '.TablesPBox';
      Entry.ComponentClass := Component.ClassName;
      Entry.ErrorMessage := '';
      Entry.StackTrace := '';
      try
        TPaintBox(Component).OnClick(Component);
        Application.ProcessMessages;
        Sleep(50);
        Application.ProcessMessages;
        Entry.Result := trPass;
        LogTestEntry(Entry);
        Inc(PassCount);
      except
        on E: Exception do
        begin
          Entry.Result := trFail;
          Entry.ErrorMessage := E.ClassName + ': ' + E.Message;
          Entry.StackTrace := GetExceptionStackTrace;
          LogTestEntry(Entry);
          Inc(FailCount);
        end;
      end;
      Application.ProcessMessages;
    end
    else
    begin
      Log('  [SKIP] PaletteModelForm.TablesPBox not found or has no OnClick handler.');
      Inc(SkipCount);
    end;

    // ModelPBox (TPaintBox) — has 'exit;' in first line, safe to click
    Component := PaletteForm.FindComponent('ModelPBox');
    if (Component <> nil) and (Component is TPaintBox) and Assigned(TPaintBox(Component).OnClick) then
    begin
      Log('  [TRYING] clickable paintbox: ' + PaletteForm.Name + '.ModelPBox');
      Entry.ComponentName := PaletteForm.Name + '.ModelPBox';
      Entry.ComponentClass := Component.ClassName;
      Entry.ErrorMessage := '';
      Entry.StackTrace := '';
      try
        TPaintBox(Component).OnClick(Component);
        Application.ProcessMessages;
        Sleep(50);
        Application.ProcessMessages;
        Entry.Result := trPass;
        LogTestEntry(Entry);
        Inc(PassCount);
      except
        on E: Exception do
        begin
          Entry.Result := trFail;
          Entry.ErrorMessage := E.ClassName + ': ' + E.Message;
          Entry.StackTrace := GetExceptionStackTrace;
          LogTestEntry(Entry);
          Inc(FailCount);
        end;
      end;
      Application.ProcessMessages;
    end
    else
    begin
      Log('  [SKIP] PaletteModelForm.ModelPBox not found or has no OnClick handler.');
      Inc(SkipCount);
    end;

    // OptionsImg opens popup menu — skip
    Log('  [SKIP] PaletteModelForm.OptionsImg opens popup menu (skipped).');
    Inc(SkipCount);
  end
  else
  begin
    Log('  [SKIP] PaletteModelForm not found.');
    Inc(SkipCount, 2);
    Inc(SkipCount);
  end;

  // ---- PaletteDataTypesForm: (OptionsImg opens popup, skip) ----
  PaletteForm := nil;
  for I := 0 to Screen.FormCount - 1 do
    if CompareText(Screen.Forms[I].ClassName, 'TPaletteDataTypesForm') = 0 then
    begin
      PaletteForm := Screen.Forms[I];
      Break;
    end;

  if PaletteForm <> nil then
  begin
    Log('  Found PaletteDataTypesForm.');
    Log('  [SKIP] PaletteDataTypesForm.OptionsImg opens popup menu (skipped).');
    Inc(SkipCount);
  end
  else
  begin
    Log('  [SKIP] PaletteDataTypesForm not found.');
    Inc(SkipCount);
  end;

  Log('');
end;
