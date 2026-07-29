# Age Status demo

## Description

This demo project illustrates the use of the **Age Status** feature from the [Kastri](https://github.com/DelphiWorlds/Kastri) library. Age Status provides a cross-platform abstraction (currently Android-only) over Google Play’s **Age Signals API** (beta).

The name “Age Status” was chosen deliberately: while the current implementation targets Google’s Age Signals API, the design anticipates future support for additional age-verification systems (for example Apple’s Declared Age Range API on iOS).

**Important note**  
The Play Age Signals API is a rapidly evolving beta. Google has already introduced significant changes (two-function architecture in 0.0.4, deprecation of `userStatus`, new significant-change fields, etc.). Consequently the Kastri implementation is expected to change further—and soon. Treat the current code as a working snapshot rather than a finished, stable API.

## Supported Delphi versions

Delphi 13.x

## Demo overview

The demo is a simple FireMonkey form with two buttons:

- **Check Age Signals** – performs a live request against the device’s Google account (requires a suitable test environment / region where Age Signals are available).
- **“Fake” Check** – uses Google’s `FakeAgeSignalsManager` to simulate various responses without needing a real age-signal backend. Several example scenarios are provided in the code (corresponding to the official test cases on the Android Developers site).

Results are written to a memo so you can inspect age range, significant-change status, approval date, and any error conditions.

## Using Age Status in your own project

1. **Add the Kastri units**  
   Ensure the search path includes the Kastri folders that contain:
   - `DW.AgeStatus.pas`
   - `DW.AgeStatus.Android.pas`
   - Supporting units (`DW.PlayTasksEvents.Android`, `DW.Androidapi.JNI.PlayServices.AgeSignals`, etc.)

2. **Add the Age Signals JAR**  
   In the Delphi IDE:
   - Open the Project Manager.
   - Expand the **Target Platforms → Android 32-bit** node.
   - Right-click **Libraries** → **Add…**.
   - Select `age-signals-0.0.4.jar` from the `ThirdParty\Android` folder in your copy of the Kastri repo.

3. **Basic usage**

```delphi
uses
  DW.AgeStatus;

// Live check
AgeStatus.Check(
  procedure(const AStatusResult: TAgeStatusResult)
  begin
    case AStatusResult.Kind of
      TAgeStatusResultKind.Success:
        // Use AStatusResult.AgeLower, AgeUpper,
        // SignificantChangeStatus, SignificantChangeApprovalDate
      else
        // Handle AccessFailed, CheckStatusFailed,
        // StatusNotShared, StatusUnspecified, StatusVerificationRequired
    end;
  end);

// Fake / test check (Android only)
AgeStatus.FakeCheck(
  TFakeAgeStatusDetails.Create(
    TAgeSignalsStatus.Shared,
    TAgeRangeSource.TierB,   // parent/guardian managed
    13, 15,                  // age range
    TSignificantChangeStatus.Approved,
    EncodeDate(2026, 5, 1)
  ),
  YourResultHandler);
```

The result record `TAgeStatusResult` surfaces:

- `Kind` – overall outcome  
- `AgeLower` / `AgeUpper`  
- `SignificantChangeStatus` (`None`, `Unspecified`, `Approved`, `Pending`, `Declined`)  
- `SignificantChangeApprovalDate`  
- `StatusMessage` (for failure cases)

### Fake-check scenarios

The demo contains several ready-made examples that mirror the official Google test cases (see the comments in `Unit1.pas` for the corresponding Android documentation links). Change the call inside `FakeCheckButtonClick` to try different scenarios.
