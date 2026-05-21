
{====================================================================
 Phase 32: Test PaletteToolsForm clickable images (safe TImage OnClick)
 Safe images: Designimg, QueryImg, CreatesImg
 Unsafe/skip: SyncImg (db sync), RevImg (reverse engineering), HeaderImg
 ====================================================================}
procedure Phase32_TestPaletteToolsImages(AMainForm: TForm; var PassCount, FailCount, SkipCount: Integer);
var
  I: Integer;
  PaletteForm: TForm;
  Component: TComponent;
  Entry: TTestEntry;
  ImgName: string;
  SafeImages: array[0..2] of string = ('Designimg', 'QueryImg', 'CreatesImg');
begin
  Log('--- Phase 32: Testing PaletteToolsForm clickable images ---');
  Log('');

  PaletteForm := nil;
  for I := 0 to Screen.FormCount - 1 do
    if CompareText(Screen.Forms[I].ClassName, 'TPaletteToolsForm') = 0 then
    begin
      PaletteForm := Screen.Forms[I];
      Break;
    end;

  if PaletteForm = nil then
  begin
    Log('[SKIP] PaletteToolsForm not found.');
    Log('');
    Exit;
  end;

  Log('  Found PaletteToolsForm.');

  for I := Low(SafeImages) to High(SafeImages) do
  begin
    ImgName := SafeImages[I];
    Component := PaletteForm.FindComponent(ImgName);
    if (Component <> nil) and (Component is TImage) and Assigned(TImage(Component).OnClick) then
    begin
      Log('  [TRYING] clickable image: ' + PaletteForm.Name + '.' + ImgName);
      Entry.ComponentName := PaletteForm.Name + '.' + ImgName;
      Entry.ComponentClass := Component.ClassName;
      Entry.ErrorMessage := '';
      Entry.StackTrace := '';
      try
        if (CompareText(ImgName, 'CreatesImg') = 0) then
          ScheduleModalClose(800);
        TImage(Component).OnClick(Component);
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
      Log('  [SKIP] ' + PaletteForm.Name + '.' + ImgName + ' not found or has no OnClick handler.');
      Inc(SkipCount);
    end;
  end;

  Log('');
end;
