# macOS Menu Bar Demo

## Description

Demonstrates the use of `TPlatformMenu` and associated classes in `Core\DW.Menus.Mac.pas`.

It is the recommended to solution for menus in the macOS status bar, and an *alternative* to that described in the [macOSStatusBar](../macOSStatusBar/) demo.

The demo is a partial replication of the menus used in [Mosco](https://github.com/DelphiWorlds/Mosco), and even includes the images used in the menu.

## Supported Delphi versions

Delphi 12, Delphi 11.x. It _may_ also work in Delphi 10.4.2, and perhaps earlier.

## Working with TPlatformMenu and TPlatformStatusItem

A menu for the status bar for macOS consists of two parts:

* A menu (`TPlatformMenu`) that contains the menu items and their submenu items
* A status bar item (`TPlatformStatusItem`)

In the demo, each of these is created in the `CreateMenu` method:

```delphi
  FMenu := TPlatformMenu.Create;
  FStatusItem := TPlatformStatusItem.Create(FMenu.Menu);
  FStatusItem.SetImage(GetMenuImage(0));
```

The `GetMenuImage` method is a convenience method for retrieving an image from the `ImageList`.

Then menu items are added to the menu using the `CreateItem` method, e.g.:

```delphi
  FStartStopItem := FMenu.CreateItem('Start', GetMenuImage(1), StartStopItemExecuteHandler);
  FOptionsItem := FMenu.CreateItem('Options', GetMenuImage(3), OptionsItemExecuteHandler);
```

The third parameter is a handler for when the item is clicked.

Separator items are added using `CreateSeparator`:

```delphi
  FMenu.CreateSeparator;
```

Menu items and their subitems are free'd by their parent, so it is only necessary to free the instances of `TPlatformMenu` and `TPlatformStatusItem`, as per the demo:

```delphi
destructor TForm1.Destroy;
begin
  FStatusItem.Free;
  FMenu.Free;
  inherited;
end;
```

The caption and image of each item can be changed in code, as per the `StartStopItemExecuteHandler` method in the demo:

```delphi
procedure TForm1.StartStopItemExecuteHandler(Sender: TObject);
begin
  FIsActive := not FIsActive;
  if FIsActive then
  begin
    FStartStopItem.Title := 'Stop';
    FStartStopItem.SetImage(GetMenuImage(2));
  end
  else
  begin
    FStartStopItem.Title := 'Start';
    FStartStopItem.SetImage(GetMenuImage(1));
  end;
end;
```

## Hiding the form and the dock icon

If you use a form to contain the menus, you may want to prevent the form from actually showing. This can be achieved by overriding the `CanShow` method of the form, e.g:

```delphi
function TMainView.CanShow: Boolean;
begin
  Result := False;
end;
```

Most applications that have a menu item in the status bar of macOS also hide the application icon from the Dock. In the demo, this is achieved by the code at the end of the unit, namely:

```delphi
function SharedApplication: NSApplication;
begin
  Result := TNSApplication.Wrap(TNSApplication.OCClass.sharedApplication);
end;

initialization
  // Hides the application from the "Dock"
  //   https://stackoverflow.com/a/9220857/3164070
  SharedApplication.setActivationPolicy(NSApplicationActivationPolicyAccessory);
```

## Working with menu subitems

Use the `CreateSubItem` method of `TPlatformMenuItem` to create subitems, and `CreateSeparator` to create separator items, like in the UpdatePAServers menu method in the demo:

```delphi
procedure TForm1.UpdatePAServersMenu;
var
  I: Integer;
begin
  FPAServersItem.Clear;
  for I := 22 to 23 do
    FPAServersItem.CreateSubItem('PAServer ' + I.ToString, PAServersItemClickHandler);
  FPAServersSepItem := FPAServersItem.CreateSeparator;
  FPAServersConfigItem := FPAServersItem.CreateSubItem('Configure', PAServersConfigItemClickHandler);
  // In Mosco, the value FPAServersSepItem.Visible would be set depending on whether there were any PAServer subitems created
end;
```

Note that this is just for demonstration of how to create the items. In your own projects, you would need to add code for the handlers of the subitems.
