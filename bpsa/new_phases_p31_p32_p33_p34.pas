
//==============================================================================
// Phase 31: Test PaletteDataTypesForm clickable paint boxes (TPaintBox OnClick)
// These are safe components that just bring panels to front.
//==============================================================================
procedure Phase31_TestPaletteDataTypesImages(AMainForm: TForm; var PassCount, FailCount, SkipCount: Integer);
var
  I: Integer;
  PaletteForm: TForm;
  Component: TComponent;
  Entry: TTestEntry;
begin
  Log('--- Phase 31: Testing PaletteDataTypesForm clickable paint boxes ---');
  Log('');

  PaletteForm := nil;
  for I := 0 to Screen.FormCount - 1 do
    if CompareText(Screen.Forms[I].ClassName, 'TPaletteDataTypesForm') = 0 then
    begin
      PaletteForm := Screen.Forms[I];
      Break;
    end;

  if PaletteForm = nil then
  begin
    Log('[SKIP] PaletteDataTypesForm not found.');
    Log('');
    Exit;
  end;

  Log('  Found PaletteDataTypesForm.');

  // Test CommonDatatypesPBox (TPaintBox) - brings panel to front, safe
  Component := PaletteForm.FindComponent('CommonDatatypesPBox');
  if (Component <> nil) and (Component is TPaintBox) and Assigned(TPaintBox(Component).OnClick) then
  begin
    Log('  [TRYING] clickable paintbox: ' + PaletteForm.Name + '.CommonDatatypesPBox');
    Entry.ComponentName := PaletteForm.Name + '.CommonDatatypesPBox';
    Entry.ComponentClass := Component.ClassName;
    Entry.ErrorMessage := '';
    Entry.StackTrace := '';
    try
      TPaintBox(Component).OnClick(Component);
      Application.ProcessMessages;
      Sleep(100);
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
    Log('  [SKIP] CommonDatatypesPBox not found or has no OnClick handler.');
    Inc(SkipCount);
  end;

  // Test AllDatatypesPBox (TPaintBox) - brings panel to front, safe
  Component := PaletteForm.FindComponent('AllDatatypesPBox');
  if (Component <> nil) and (Component is TPaintBox) and Assigned(TPaintBox(Component).OnClick) then
  begin
    Log('  [TRYING] clickable paintbox: ' + PaletteForm.Name + '.AllDatatypesPBox');
    Entry.ComponentName := PaletteForm.Name + '.AllDatatypesPBox';
    Entry.ComponentClass := Component.ClassName;
    Entry.ErrorMessage := '';
    Entry.StackTrace := '';
    try
      TPaintBox(Component).OnClick(Component);
      Application.ProcessMessages;
      Sleep(100);
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
    Log('  [SKIP] AllDatatypesPBox not found or has no OnClick handler.');
    Inc(SkipCount);
  end;

  Log('');
end;

//==============================================================================
// Phase 32: Test PaletteToolsForm clickable images (safe TImage OnClick)
// Safe images: Designimg, QueryImg, CreatesImg, HeaderImg
// Unsafe/skip: SyncImg (db sync), RevImg (reverse engineering)
//==============================================================================
procedure Phase32_TestPaletteToolsImages(AMainForm: TForm; var PassCount, FailCount, SkipCount: Integer);
var
  I: Integer;
  PaletteForm: TForm;
  Component: TComponent;
  Entry: TTestEntry;
  ImgName: string;
  SafeImages: array[0..0] of string = ('Designimg');  // QueryImg, CreatesImg, HeaderImg skipped - unsafe
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

//==============================================================================
// Phase 33: Test MainDM.DMMain string utility functions
//==============================================================================
procedure Phase33_TestMainDMUtilities(AMainForm: TForm; var PassCount, FailCount, SkipCount: Integer);
var
  S: string;
  SL: TStringList;
  Count: Integer;
begin
  Log('--- Phase 33: Testing MainDM utility functions ---');
  Log('');

  if DMMain = nil then
  begin
    Log('[SKIP] DMMain global is nil.');
    Log('');
    Exit;
  end;

  // Test ReplaceText
  try
    S := DMMain.ReplaceText('hello world', 'world', 'there');
    if S = 'hello there' then
    begin
      Log('[PASS] ReplaceText: "hello world" -> "' + S + '"');
      Inc(PassCount);
    end
    else
    begin
      Log('[FAIL] ReplaceText expected "hello there" but got "' + S + '"');
      Inc(FailCount);
    end;
  except
    on E: Exception do
    begin
      Log('[FAIL] ReplaceText exception: ' + E.ClassName + ': ' + E.Message);
      Inc(FailCount);
    end;
  end;

  // Test ReplaceString
  try
    S := DMMain.ReplaceString('abc123def123', '123', 'XYZ');
    if S = 'abcXYZdefXYZ' then
    begin
      Log('[PASS] ReplaceString: "' + S + '"');
      Inc(PassCount);
    end
    else
    begin
      Log('[FAIL] ReplaceString expected "abcXYZdefXYZ" but got "' + S + '"');
      Inc(FailCount);
    end;
  except
    on E: Exception do
    begin
      Log('[FAIL] ReplaceString exception: ' + E.ClassName + ': ' + E.Message);
      Inc(FailCount);
    end;
  end;

  // Test GetSubStringCountInString
  try
    Count := DMMain.GetSubStringCountInString('one, two, three, four, five', ', ');
    if Count = 4 then
    begin
      Log('[PASS] GetSubStringCountInString: found ' + IntToStr(Count) + ' occurrences.');
      Inc(PassCount);
    end
    else
    begin
      Log('[FAIL] GetSubStringCountInString expected 4 but got ' + IntToStr(Count));
      Inc(FailCount);
    end;
  except
    on E: Exception do
    begin
      Log('[FAIL] GetSubStringCountInString exception: ' + E.ClassName + ': ' + E.Message);
      Inc(FailCount);
    end;
  end;

  // Test GetColumnCountFromSepString
  try
    Count := DMMain.GetColumnCountFromSepString('a,b,c,d', ',', '"');
    if Count = 4 then
    begin
      Log('[PASS] GetColumnCountFromSepString: ' + IntToStr(Count) + ' columns.');
      Inc(PassCount);
    end
    else
    begin
      Log('[FAIL] GetColumnCountFromSepString expected 4 but got ' + IntToStr(Count));
      Inc(FailCount);
    end;
  except
    on E: Exception do
    begin
      Log('[FAIL] GetColumnCountFromSepString exception: ' + E.ClassName + ': ' + E.Message);
      Inc(FailCount);
    end;
  end;

  // Test GetColumnFromSepString
  try
    S := DMMain.GetColumnFromSepString('apple,banana,cherry', 1, ',', '"');
    if S = 'banana' then
    begin
      Log('[PASS] GetColumnFromSepString: column 2 = "' + S + '"');
      Inc(PassCount);
    end
    else
    begin
      Log('[FAIL] GetColumnFromSepString expected "banana" but got "' + S + '"');
      Inc(FailCount);
    end;
  except
    on E: Exception do
    begin
      Log('[FAIL] GetColumnFromSepString exception: ' + E.ClassName + ': ' + E.Message);
      Inc(FailCount);
    end;
  end;

  // Test FixLength
  try
    S := DMMain.FixLength('abc', 10, True, ' ');
    if Length(S) = 10 then
    begin
      Log('[PASS] FixLength: length = ' + IntToStr(Length(S)));
      Inc(PassCount);
    end
    else
    begin
      Log('[FAIL] FixLength expected length 10 but got ' + IntToStr(Length(S)));
      Inc(FailCount);
    end;
  except
    on E: Exception do
    begin
      Log('[FAIL] FixLength exception: ' + E.ClassName + ': ' + E.Message);
      Inc(FailCount);
    end;
  end;

  Log('');
end;

//==============================================================================
// Phase 34: (reserved for future use)
// Skips gracefully (placeholder)
//==============================================================================
procedure Phase34_ReservedForFuture(AMainForm: TForm; var PassCount, FailCount, SkipCount: Integer);
begin
  Log('--- Phase 34: (reserved for future use) ---');
  Log('  [SKIP] Phase 34 is a placeholder.');
  Inc(SkipCount);
  Log('');
end;
