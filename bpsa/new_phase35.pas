{ Phase 35: Test PaletteTools buttons }
procedure Phase35_TestPaletteToolsButtons(AMainForm: TForm; var PassCount, FailCount, SkipCount: Integer);
var
  I: Integer;
  PaletteTools: TPaletteToolsForm;
  Btn: TSpeedButton;
  TestedCount: Integer;
begin
  Log('--- Phase 35: Testing PaletteTools buttons ---');
  PaletteTools := nil;
  for I := 0 to Screen.FormCount - 1 do
    if Screen.Forms[I] is TPaletteToolsForm then
    begin
      PaletteTools := TPaletteToolsForm(Screen.Forms[I]);
      Break;
    end;

  if PaletteTools = nil then
  begin
    Log('  [SKIP] PaletteToolsForm not found.');
    Inc(SkipCount);
    Log('');
    Exit;
  end;

  PaletteTools.Show;
  Application.ProcessMessages;
  Sleep(200);
  Application.ProcessMessages;

  TestedCount := 0;
  for I := 0 to PaletteTools.ComponentCount - 1 do
  begin
    if PaletteTools.Components[I] is TSpeedButton then
    begin
      Btn := TSpeedButton(PaletteTools.Components[I]);
      if Assigned(Btn.OnClick) and not IsUnsafe(Btn.Name, 'PaletteToolsForm') then
      begin
        Log('  Click: ' + Btn.Name);
        try
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
      end
    end;
  end;

  Log(Format('  Tested %d buttons on PaletteToolsForm.', [TestedCount]));
  Log('');
end;