unit PanelBitmap;
{$mode delphi}
{
  CLX's TPanel had a Bitmap property that was tiled over the panel background.
  LCL's TPanel has no such property, so this class helper provides one.  The
  bitmap is kept per panel in a hash list together with a small painter object
  that hooks the panel's OnPaint (TCustomPanel.Paint calls OnPaint after the
  bevels and caption) and tiles the bitmap over the client area, like CLX did.
  Used by the Options dialog ("Header Preview") and the query drop targets.
}
interface

uses Classes, SysUtils, Graphics, ExtCtrls, Contnrs;

type
  TPanelHelper = class helper for TPanel
  private
    function GetBitmap: TBitmap;
    procedure SetBitmap(Value: TBitmap);
  public
    property Bitmap: TBitmap read GetBitmap write SetBitmap;
  end;

implementation

type
  //Owned by the panel, so it is freed (and unregistered) with it
  TPanelBitmapPainter = class(TComponent)
  private
    FPanel: TPanel;
    FBitmap: TBitmap;
    FPrevOnPaint: TNotifyEvent;
    procedure PanelPaint(Sender: TObject);
  public
    constructor Create(APanel: TPanel); reintroduce;
    destructor Destroy; override;
    property Bitmap: TBitmap read FBitmap;
  end;

var
  PanelBitmaps: TFPHashList;

function PanelKey(Panel: TPanel): string;
begin
  Result := IntToStr(PtrUInt(Panel));
end;

{ TPanelBitmapPainter }

constructor TPanelBitmapPainter.Create(APanel: TPanel);
begin
  inherited Create(APanel);
  FPanel := APanel;
  FBitmap := TBitmap.Create;
  FPrevOnPaint := APanel.OnPaint;
  APanel.OnPaint := PanelPaint;
end;

destructor TPanelBitmapPainter.Destroy;
var
  idx: Integer;
begin
  if PanelBitmaps <> nil then
  begin
    idx := PanelBitmaps.FindIndexOf(PanelKey(FPanel));
    if idx >= 0 then
      PanelBitmaps.Delete(idx);
  end;
  FBitmap.Free;
  inherited Destroy;
end;

procedure TPanelBitmapPainter.PanelPaint(Sender: TObject);
var
  x, y: Integer;
  R: TRect;
begin
  if (FBitmap.Width > 0) and (FBitmap.Height > 0) then
  begin
    R := FPanel.ClientRect;
    y := R.Top;
    while y < R.Bottom do
    begin
      x := R.Left;
      while x < R.Right do
      begin
        FPanel.Canvas.Draw(x, y, FBitmap);
        Inc(x, FBitmap.Width);
      end;
      Inc(y, FBitmap.Height);
    end;
  end;
  if Assigned(FPrevOnPaint) then
    FPrevOnPaint(Sender);
end;

function GetPainter(Panel: TPanel; CreateIfMissing: Boolean): TPanelBitmapPainter;
var
  idx: Integer;
begin
  Result := nil;
  if PanelBitmaps = nil then
    PanelBitmaps := TFPHashList.Create;
  idx := PanelBitmaps.FindIndexOf(PanelKey(Panel));
  if idx >= 0 then
    Result := TPanelBitmapPainter(PanelBitmaps.Items[idx])
  else if CreateIfMissing then
  begin
    Result := TPanelBitmapPainter.Create(Panel);
    PanelBitmaps.Add(PanelKey(Panel), Result);
  end;
end;

function TPanelHelper.GetBitmap: TBitmap;
begin
  Result := GetPainter(Self, True).Bitmap;
end;

procedure TPanelHelper.SetBitmap(Value: TBitmap);
var
  Painter: TPanelBitmapPainter;
begin
  if Value = nil then
  begin
    Painter := GetPainter(Self, False);
    if Painter <> nil then
    begin
      Painter.Bitmap.FreeImage;
      Painter.Bitmap.SetSize(0, 0);
    end;
  end
  else
    GetPainter(Self, True).Bitmap.Assign(Value);
  Invalidate;
end;

initialization
  PanelBitmaps := nil;

finalization
  //Painters are freed by their owning panels
  FreeAndNil(PanelBitmaps);

end.
