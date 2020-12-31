# Delphi Worlds Coding Standards

## Prologue

These standards are a living document, i.e. they may change over time. There is also a little flexibility given - use the indicators as a guide, i.e.

* "should" indicates a recommendation
* "shall" indicates a strong recommendation
* "must", or a sentence starting with "No" indicates a hard and fast rule

## Naming

**Note:** Where naming rules are used, the case of the naming must match as per examples given.

### Namespacing

Unit files are divided into logical namespaces. _Framework_ code (shared amongst projects) are to use the `DW` prefix. Framework units should be named using the Embarcadero naming style. 

For _platform api_ units:

`<prefix>.<platform>api.<functionality>.pas`, e.g.:  `DW.iOSapi.Photos.pas`

For _platform agnostic_ units: `<prefix>.<functionality>.pas`, e.g.: `DW.SuperWidget.pas`

For _platform specific_ units that consume a platform api: `<prefix>.<functionality>.<platform>.pas`, e.g.: `DW.SuperWidget.iOS.pas`

_Project specific_ units are to use the codename for that project: `<projectprefix>.<unitname>.pas`, e.g.: `MM.Types.pas`

Form and frame units shall use the format `<prefix>.View.<viewname>`, e.g: `SDX.View.Main.pas`

### Component Naming

Components shall have a meaningful name, and be suffixed with the component type, minus the `T` and vendor prefix, e.g: a `TLayout` might be named `UpperLayout`, a `TEdit` might be named `UsernameEdit`, a TFDTable might be named `ListsTable`.

### Type naming

All distinct types shall be named starting with a capital `T`

### Event handler method naming

Event handler methods that are assigned at runtime shall be named logically, pertaining to the event they are handling (with the word “On” removed, if it has one), and be suffixed with the word: Handler, e.g. if a handler is assigned in code to the `OnClick` event of a button called `CancelButton`, the handler should be named: `CancelButtonClickHandler`

### Variable naming

Field variables (i.e. in classes) are to be prefixed with a capital `F`

Local variables are to be prefixed with a capital `L`, with the exception of _Integer_ loop variables, which should be named `I` for the first, `J` for the second, and so on.

## Types

Types shall be declared on a line immediately following the previous line, except in the case of classes and records, which shall be separated by one blank line

### Classes

Visibility specifiers must be on a new line, in order of the visibility, i.e. the order is:

* strict private
* private
* strict protected
* protected
* public
* published

For each visibility, field variables must appear first, in _alphabetical order_, with the exception of event reference variables, which follow the other field variables, and the event reference variables shall also be in alphabetical order themselves.

Methods must follow the field variables, also in _alphabetical order_.

**Exceptions:**

Constructors and destructors must appear _first_ in any visibility section, in this order:

* class constructors
* class destructors
* constructors
* destructors

Methods that are implementation of interfaces: these must always appear in their own public section, preceded by a braced comment that indicates the interface being implemented, e.g.:

```
  TDWCameraDeviceStateCallbackDelegate = class(TJavaLocal, JDWCameraDeviceStateCallbackDelegate)
  private
    FPlatformCamera: TPlatformCamera;
  public
    { JDWCameraDeviceStateCallbackDelegate }
    procedure Disconnected(camera: JCameraDevice); cdecl;
    procedure Error(camera: JCameraDevice; error: Integer); cdecl;
    procedure Opened(camera: JCameraDevice); cdecl;
  public
    constructor Create(const APlatformCamera: TPlatformCamera);
  end;
```

Properties must follow the methods, also in alphabetical order, with the exception of event properties, which must follow the other properties, however the event properties must also be in alphabetical order

The only exception to the above rules is for IDE generated classes: forms, frames and datamodules, where the auto-generated published component and event handler references appear first.

## Interfaces

Interfaces must be declared similarly to classes, with method declarations first, in alphabetical order, then properties, also in alphabetical order

## Code Structures

### with

Under **no** circumstances is `with` to be used. **No discussions will be entered into**.

### Uses clauses

Units that are used by another, must appear in the **section of lowest scope**, i.e. if a unit imports symbols from another unit that are required only the implementation section, the used unit **must appear in the implementation section** of the unit using it. This is to **avoid creating greater dependency than is required**, and avoids having to resolve circular references.

### Finally Blocks

Code inside a `finally` block is to execute resource deallocation or restoring of state only, e.g. calling `Free`, `DisposeOf`, releasing of handles, etc.

### Checking For Assignment

Use the Assigned method for event handlers **only**. All other checks for assignment shall be compared against `nil`.

## Global Variables

Global variables must be avoided, unless there is a very good reason to use them. Globals created by the IDE such as frmMain etc are to be ignored. Instead, consider the scope of use, and perhaps access via a class variable

## Formatting

### Margin

The margin is set at 150 characters. Whole expressions and individual parameter declarations including their type, going beyond the margin, shall be moved to the next line, and be indented by 2 spaces. Unit names in `uses` clauses going beyond the margin shall also be moved to the next line, at the _same_ indentation as the line before.

### Case

Keywords (represented by bold in the Delphi IDE) must be all lowercase

Constants shall start with a lowercase `c`.

**All** other identifiers must start with a capital

Identifiers must use the same case throughout the code, as they were declared

### Indentation

* All declaration _section_ headers (`uses`, `type`, `const`, `procedure` etc) must start in the first column
* All declarations within a declaration section must be indented by 2 spaces
* Code blocks (eg inside a try..finally, within a begin..end, code that is executed by an if or for statement) must be indented 2 spaces

### Spacing

Blank lines:

* Always consider as being before a declaration (aside from unit name at beginning)
* No blank lines within a declaration (i.e. `record`, `class`, `method`)
* No multiple blank lines

Spaces within lines:

* Only one space must separate identifiers, operators etc, i.e. **no aligning of declarations using multiple spaces**
* Operators must be separated by one space, eg:

```
LHeight := (Screen.Height / 2) + (Height / 2);
```

* No spaces before or after parentheses, except for before a constant array declaration

### Statements

All statements, where possible, must be terminated by a semicolon, even where the compiler will allow no semicolon.

### Code Blocks

The `begin` in a `begin..end` code block must start on a new line.

### Structured Statements

#### if/while statements:

Where the conditional code of an `if`/`while` statement spans more than one line, even if it is only one statement, the conditional code is to be enclosed in a `begin..end` block. The `begin` is to be aligned to the `if`. An `if` following the `else` in another `if` statement is to appear immediately after the `else`. e.g.

```
if SomeCondition then 
begin 
  DoSomething; 
  DoSomeMore; 
end 
else if SomeOtherCondition then 
  DoSomethingElse;
```

#### repeat..until statements:

The code inside a `repeat..until` statement must be indented 2 spaces

#### case statements:

* Each case condition must start on a new line, and be indented 2 spaces in relation to the `case` keyword
* Code blocks for the `case` condition that span more than one line are to be enclosed in a `begin..end` block, which is aligned to the condition, e.g:

```
case Key of 
  vkUp: 
  begin 
    DoSomething; 
    DoSomethingElse;
  end; 
  vkDown: 
    DoAnotherThing; 
else 
  DoSomethingCompletelyDifferent; 
end;
```

## Documentation and Comments

### XML Documentation

Where the intention of a method may be unclear, XML Documentation may be used to clarify the intention. The documentation must immediately precede the method declaration, and must contain at least the summary tag.

### Comments

Comments **must** appear on their own lines (i.e. not on the same line as code), and must be used for clarifying what the code is intended for. Comments must use the double slash style, i.e. //

Code must not be commented out completely unless it is part of work in progress, i.e. any code intended for promotion that is commented out **must** be removed, because the code would already exist in the source control history.