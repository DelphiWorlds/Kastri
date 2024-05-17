# "Scroll No Click" Demo

## Description

This demo shows one *possible* means of preventing clicks and/or gain of focus when a control is inside of a scrollbox.

## How It Works

As per the code in `MainFrm.pas`, it declares "interposer" classes (instead of using actual descendants that need to be installed) that override the mouse events, and determines whether the mouse has moved more than a certain distance, and if so, prevents calling the `Click` event. This means that when scrolling through the controls, clicks and gain of focus will occur only if the "mouse" (could be via touch) has not moved much.

The `TMouseInfo` structure is used for convenience, encapsulating the properties required for the behaviour to be implemented.

## Supported Delphi versions

The demo should compile and work for Delphi 12, Delphi 11.x, and possibly earlier
