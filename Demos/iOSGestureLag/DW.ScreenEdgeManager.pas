unit DW.ScreenEdgeManager;

interface

type
  TScreenEdge = (Top, Left, Bottom, Right);

  TScreenEdges = set of TScreenEdge;

  TScreenEdgeManager = record
  private
    class var FPreferredScreenEdges: TScreenEdges;
  public
    class procedure SetPreferredScreenEdges(const AEdges: TScreenEdges); static;
    class property PreferredScreenEdges: TScreenEdges read FPreferredScreenEdges;
  end;

implementation

{$IF Defined(IOS)}
uses
  System.SysUtils,
  Macapi.ObjectiveC, Macapi.Helpers,
  iOSapi.UIKit;

type
  UIViewControllerEx = interface(UIViewController)
    ['{D8A72AEC-9EE6-4046-B989-7F747524265A}']
    procedure setNeedsUpdateOfScreenEdgesDeferringSystemGestures; cdecl;
  end;
  TUIViewControllerEx = class(TOCGenericImport<UIViewControllerClass, UIViewControllerEx>)  end;

function SharedApplication: UIApplication;
begin
  Result := TUIApplication.Wrap(TUIApplication.OCClass.sharedApplication);
end;
{$ENDIF}

{ TScreenEdgeManager }

class procedure TScreenEdgeManager.SetPreferredScreenEdges(const AEdges: TScreenEdges);
begin
  FPreferredScreenEdges := AEdges;
  {$IF Defined(IOS)}
  if TOSVersion.Check(11) then
    TUIViewControllerEx.Wrap(NSObjectToID(SharedApplication.keyWindow.rootViewController)).setNeedsUpdateOfScreenEdgesDeferringSystemGestures;
  {$ENDIF}
end;

end.
