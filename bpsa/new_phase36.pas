{ Phase 36: Test PaletteNav and PaletteModel buttons }
procedure Phase36_TestPaletteNavAndModelButtons(AMainForm: TForm; var PassCount, FailCount, SkipCount: Integer);
var
  I: Integer;
  PaletteNavForm: TPaletteNavForm;
  PaletteModelForm: TPaletteModelFrom;
  Btn: TControl;
  TestedCount: Integer;
begin
  Log('--- Phase 36: Testing PaletteNav and PaletteModel buttons ---');
  TestedCount := 0;

  // Test PaletteNavForm
  PaletteNavForm := nil;
  for I := 0 to Screen.FormCount - 1 do
    if Screen.Forms[I] is TPaletteNavForm then
    begin
      PaletteNavForm := TPaletteNavForm(Screen.Forms[I]);
      Break;
    end;

  if PaletteNavForm <> nil then
  begin
    PaletteNavForm.Show;
    Application.ProcessMessages;
    Sleep(200);
    Application.ProcessMessages;

    for I := 0 to PaletteNavForm.ComponentCount - 1 do
    begin
      if PaletteNavForm.Components[I] is TSpeedButton then
      begin
        Btn := TSpeedButton(PaletteNavForm.Components[I]);
        if Assigned(Btn.OnClick) and not IsUnsafe(Btn.Name, 'PaletteNavForm') then
        begin
          Log('  [Nav] Click: ' + Btn.Name);
          try
            ScheduleModalClose(800);
            Btn.OnClick(Btn);
            Application.ProcessMessages;
            Sleep(100);
            Application.ProcessMessages;
            Log('    [PASS] ' + Btn.Name);
            Inc(PassCount);
          except
            on E: Exception do
            begin
              Log('    [FAIL] ' + Btn.Name + ': ' + E.ClassName + ' - ' + E.Message);
              Inc(FailCount);
            end;
          end;
          Inc(TestedCount);
        end;
      end;
    end;
  end
  else
  begin
    Log('  [SKIP] PaletteNavForm not found.');
    Inc(SkipCount);
  end;

  // Test PaletteModelForm
  PaletteModelForm := nil;
  for I := 0 to Screen.FormCount - 1 do
    if Screen.Forms[I] is TPaletteModelFrom then
    begin
      PaletteModelForm := TPaletteModelFrom(Screen.Forms[I]);
      Break;
    end;

  if PaletteModelForm <> nil then
  begin
    PaletteModelForm.Show;
    Application.ProcessMessages;
    Sleep(200);
    Application.ProcessMessages;

    for I := 0 to PaletteModelForm.ComponentCount - 1 do
    begin
      if PaletteModelForm.Components[I] is TSpeedButton then
      begin
        Btn := TSpeedButton(PaletteModelForm.Components[I]);
        // Skip AddBtn - already tested in Phase 7b, opens modal DBConnSelectForm
        if (CompareText(Btn.Name, 'AddBtn') = 0) then
          Continue;
        if Assigned(Btn.OnClick) and not IsUnsafe(Btn.Name, 'PaletteModelForm') then
        begin
          Log('  [Model] Click: ' + Btn.Name);
          try
            ScheduleModalClose(800);
            Btn.OnClick(Btn);
            Application.ProcessMessages;
            Sleep(100);
            Application.ProcessMessages;
            Log('    [PASS] ' + Btn.Name);
            Inc(PassCount);
          except
            on E: Exception do
            begin
              Log('    [FAIL] ' + Btn.Name + ': ' + E.ClassName + ' - ' + E.Message);
              Inc(FailCount);
            end;
          end;
          Inc(TestedCount);
        end;
      end;
    end;
  end
  else
  begin
    Log('  [SKIP] PaletteModelForm not found.');
    Inc(SkipCount);
  end;

  // Test SnapToGridBtn on MainForm
  Log('  [Main] Testing SnapToGridBtn...');
  for I := 0 to AMainForm.ComponentCount - 1 do
    if CompareText(AMainForm.Components[I].Name, 'SnapToGridBtn') = 0 then
    begin
      if AMainForm.Components[I] is TSpeedButton then
      begin
        Btn := TSpeedButton(AMainForm.Components[I]);
        if Assigned(Btn.OnClick) then
        begin
          try
            Btn.OnClick(Btn);
            Application.ProcessMessages;
            Sleep(100);
            Log('    [PASS] SnapToGridBtn');
            Inc(PassCount);
          except
            on E: Exception do
            begin
              Log('    [FAIL] SnapToGridBtn: ' + E.ClassName + ' - ' + E.Message);
              Inc(FailCount);
            end;
          end;
          Inc(TestedCount);
        end
        else
        begin
          Log('    [SKIP] SnapToGridBtn has no OnClick handler');
          Inc(SkipCount);
        end;
      end;
      Break;
    end;

  Log(Format('  Tested %d buttons across palette forms.', [TestedCount]));
  Log('');
end;