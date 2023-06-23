unit DW.Androidapi.JNI.Hardware;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2023 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.Os, Androidapi.JNI.Java.Security,
  // DW
  DW.Androidapi.JNI.Security;
  
type
  JFingerprintManager = interface;
  JFingerprintManager_AuthenticationCallback = interface;
  JFingerprintManager_AuthenticationResult = interface;
  JFingerprintManager_CryptoObject = interface;
  JSensor = interface;
  JSensorEvent = interface;
  JSensorEventListener = interface;
  JSensorListener = interface;
  JSensorManager = interface;
  JTriggerEvent = interface;
  JTriggerEventListener = interface;

  JFingerprintManagerClass = interface(JObjectClass)
    ['{508B5B4C-CE33-418A-8878-CE03A1088B75}']
    {class} function _GetFINGERPRINT_ACQUIRED_GOOD: Integer; cdecl;
    {class} function _GetFINGERPRINT_ACQUIRED_IMAGER_DIRTY: Integer; cdecl;
    {class} function _GetFINGERPRINT_ACQUIRED_INSUFFICIENT: Integer; cdecl;
    {class} function _GetFINGERPRINT_ACQUIRED_PARTIAL: Integer; cdecl;
    {class} function _GetFINGERPRINT_ACQUIRED_TOO_FAST: Integer; cdecl;
    {class} function _GetFINGERPRINT_ACQUIRED_TOO_SLOW: Integer; cdecl;
    {class} function _GetFINGERPRINT_ERROR_CANCELED: Integer; cdecl;
    {class} function _GetFINGERPRINT_ERROR_HW_UNAVAILABLE: Integer; cdecl;
    {class} function _GetFINGERPRINT_ERROR_LOCKOUT: Integer; cdecl;
    {class} function _GetFINGERPRINT_ERROR_NO_SPACE: Integer; cdecl;
    {class} function _GetFINGERPRINT_ERROR_TIMEOUT: Integer; cdecl;
    {class} function _GetFINGERPRINT_ERROR_UNABLE_TO_PROCESS: Integer; cdecl;
    {class} property FINGERPRINT_ACQUIRED_GOOD: Integer read _GetFINGERPRINT_ACQUIRED_GOOD;
    {class} property FINGERPRINT_ACQUIRED_IMAGER_DIRTY: Integer read _GetFINGERPRINT_ACQUIRED_IMAGER_DIRTY;
    {class} property FINGERPRINT_ACQUIRED_INSUFFICIENT: Integer read _GetFINGERPRINT_ACQUIRED_INSUFFICIENT;
    {class} property FINGERPRINT_ACQUIRED_PARTIAL: Integer read _GetFINGERPRINT_ACQUIRED_PARTIAL;
    {class} property FINGERPRINT_ACQUIRED_TOO_FAST: Integer read _GetFINGERPRINT_ACQUIRED_TOO_FAST;
    {class} property FINGERPRINT_ACQUIRED_TOO_SLOW: Integer read _GetFINGERPRINT_ACQUIRED_TOO_SLOW;
    {class} property FINGERPRINT_ERROR_CANCELED: Integer read _GetFINGERPRINT_ERROR_CANCELED;
    {class} property FINGERPRINT_ERROR_HW_UNAVAILABLE: Integer read _GetFINGERPRINT_ERROR_HW_UNAVAILABLE;
    {class} property FINGERPRINT_ERROR_LOCKOUT: Integer read _GetFINGERPRINT_ERROR_LOCKOUT;
    {class} property FINGERPRINT_ERROR_NO_SPACE: Integer read _GetFINGERPRINT_ERROR_NO_SPACE;
    {class} property FINGERPRINT_ERROR_TIMEOUT: Integer read _GetFINGERPRINT_ERROR_TIMEOUT;
    {class} property FINGERPRINT_ERROR_UNABLE_TO_PROCESS: Integer read _GetFINGERPRINT_ERROR_UNABLE_TO_PROCESS;
  end;

  [JavaSignature('android/hardware/fingerprint/FingerprintManager')]
  JFingerprintManager = interface(JObject)
    ['{51C72153-F6E5-4424-A121-C1855387749F}']
    procedure authenticate(crypto: JFingerprintManager_CryptoObject; cancel: JCancellationSignal; flags: Integer;
      callback: JFingerprintManager_AuthenticationCallback; handler: JHandler); cdecl;
    function hasEnrolledFingerprints: Boolean; cdecl;
    function isHardwareDetected: Boolean; cdecl;
  end;
  TJFingerprintManager = class(TJavaGenericImport<JFingerprintManagerClass, JFingerprintManager>) end;

  JFingerprintManager_AuthenticationCallbackClass = interface(IJavaClass)
    ['{FE4132F8-A677-4036-BEB1-CE865EB6025B}']
    {class} function init: JFingerprintManager_AuthenticationCallback; cdecl;
    {class} procedure onAuthenticationFailed; cdecl;
    {class} procedure onAuthenticationHelp(helpCode: Integer; helpString: JCharSequence); cdecl;
    {class} procedure onAuthenticationSucceeded(result: JFingerprintManager_AuthenticationResult); cdecl;
  end;

  [JavaSignature('android/hardware/fingerprint/FingerprintManager$AuthenticationCallback')]
  JFingerprintManager_AuthenticationCallback = interface(IJavaInstance)
    ['{CDB400F0-B2A3-4C3B-A468-AF028465B11A}']
    procedure onAuthenticationError(errCode: Integer; errString: JCharSequence); cdecl;
    procedure onAuthenticationFailed; cdecl;
    procedure onAuthenticationHelp(helpCode: Integer; helpString: JCharSequence); cdecl;
    procedure onAuthenticationSucceeded(result: JFingerprintManager_AuthenticationResult); cdecl;
  end;
  TJFingerprintManager_AuthenticationCallback = class(TJavaGenericImport<JFingerprintManager_AuthenticationCallbackClass,
    JFingerprintManager_AuthenticationCallback>) end;

  JFingerprintManager_AuthenticationResultClass = interface(JObjectClass)
    ['{83524DD2-9DED-47C0-A7A5-7EC91419781E}']
  end;

  [JavaSignature('android/hardware/fingerprint/FingerprintManager$AuthenticationResult')]
  JFingerprintManager_AuthenticationResult = interface(JObject)
    ['{641F725C-6D2B-4306-A7FF-191CB20F6B63}']
    function getCryptoObject: JFingerprintManager_CryptoObject; cdecl;
  end;
  TJFingerprintManager_AuthenticationResult = class(TJavaGenericImport<JFingerprintManager_AuthenticationResultClass,
    JFingerprintManager_AuthenticationResult>) end;

  JFingerprintManager_CryptoObjectClass = interface(JObjectClass)
    ['{4B4CF256-9483-4D76-A965-ED1BCF6FE123}']
    {class} function init(signature: Jsecurity_Signature): JFingerprintManager_CryptoObject; cdecl; overload;
    {class} function init(cipher: JCipher): JFingerprintManager_CryptoObject; cdecl; overload;
    {class} function init(mac: JMac): JFingerprintManager_CryptoObject; cdecl; overload;
    {class} function getCipher: JCipher; cdecl;
    {class} function getMac: JMac; cdecl;
  end;

  [JavaSignature('android/hardware/fingerprint/FingerprintManager$CryptoObject')]
  JFingerprintManager_CryptoObject = interface(JObject)
    ['{216277CF-9BA8-4C16-B42E-144731624A74}']
    function getSignature: Jsecurity_Signature; cdecl;
  end;
  TJFingerprintManager_CryptoObject = class(TJavaGenericImport<JFingerprintManager_CryptoObjectClass, JFingerprintManager_CryptoObject>) end;

  JSensorClass = interface(JObjectClass)
    ['{9025B1D0-DA48-4D2C-9D19-2E1F23FB7B8E}']
    {class} function _GetREPORTING_MODE_CONTINUOUS: Integer; cdecl;
    {class} function _GetREPORTING_MODE_ONE_SHOT: Integer; cdecl;
    {class} function _GetREPORTING_MODE_ON_CHANGE: Integer; cdecl;
    {class} function _GetREPORTING_MODE_SPECIAL_TRIGGER: Integer; cdecl;
    {class} function _GetSTRING_TYPE_ACCELEROMETER: JString; cdecl;
    {class} function _GetSTRING_TYPE_AMBIENT_TEMPERATURE: JString; cdecl;
    {class} function _GetSTRING_TYPE_GAME_ROTATION_VECTOR: JString; cdecl;
    {class} function _GetSTRING_TYPE_GEOMAGNETIC_ROTATION_VECTOR: JString; cdecl;
    {class} function _GetSTRING_TYPE_GRAVITY: JString; cdecl;
    {class} function _GetSTRING_TYPE_GYROSCOPE: JString; cdecl;
    {class} function _GetSTRING_TYPE_GYROSCOPE_UNCALIBRATED: JString; cdecl;
    {class} function _GetSTRING_TYPE_HEART_RATE: JString; cdecl;
    {class} function _GetSTRING_TYPE_LIGHT: JString; cdecl;
    {class} function _GetSTRING_TYPE_LINEAR_ACCELERATION: JString; cdecl;
    {class} function _GetSTRING_TYPE_MAGNETIC_FIELD: JString; cdecl;
    {class} function _GetSTRING_TYPE_MAGNETIC_FIELD_UNCALIBRATED: JString; cdecl;
    {class} function _GetSTRING_TYPE_ORIENTATION: JString; cdecl;
    {class} function _GetSTRING_TYPE_PRESSURE: JString; cdecl;
    {class} function _GetSTRING_TYPE_PROXIMITY: JString; cdecl;
    {class} function _GetSTRING_TYPE_RELATIVE_HUMIDITY: JString; cdecl;
    {class} function _GetSTRING_TYPE_ROTATION_VECTOR: JString; cdecl;
    {class} function _GetSTRING_TYPE_SIGNIFICANT_MOTION: JString; cdecl;
    {class} function _GetSTRING_TYPE_STEP_COUNTER: JString; cdecl;
    {class} function _GetSTRING_TYPE_STEP_DETECTOR: JString; cdecl;
    {class} function _GetSTRING_TYPE_TEMPERATURE: JString; cdecl;
    {class} function _GetTYPE_ACCELEROMETER: Integer; cdecl;
    {class} function _GetTYPE_ALL: Integer; cdecl;
    {class} function _GetTYPE_AMBIENT_TEMPERATURE: Integer; cdecl;
    {class} function _GetTYPE_GAME_ROTATION_VECTOR: Integer; cdecl;
    {class} function _GetTYPE_GEOMAGNETIC_ROTATION_VECTOR: Integer; cdecl;
    {class} function _GetTYPE_GRAVITY: Integer; cdecl;
    {class} function _GetTYPE_GYROSCOPE: Integer; cdecl;
    {class} function _GetTYPE_GYROSCOPE_UNCALIBRATED: Integer; cdecl;
    {class} function _GetTYPE_HEART_RATE: Integer; cdecl;
    {class} function _GetTYPE_LIGHT: Integer; cdecl;
    {class} function _GetTYPE_LINEAR_ACCELERATION: Integer; cdecl;
    {class} function _GetTYPE_MAGNETIC_FIELD: Integer; cdecl;
    {class} function _GetTYPE_MAGNETIC_FIELD_UNCALIBRATED: Integer; cdecl;
    {class} function _GetTYPE_ORIENTATION: Integer; cdecl;
    {class} function _GetTYPE_PRESSURE: Integer; cdecl;
    {class} function _GetTYPE_PROXIMITY: Integer; cdecl;
    {class} function _GetTYPE_RELATIVE_HUMIDITY: Integer; cdecl;
    {class} function _GetTYPE_ROTATION_VECTOR: Integer; cdecl;
    {class} function _GetTYPE_SIGNIFICANT_MOTION: Integer; cdecl;
    {class} function _GetTYPE_STEP_COUNTER: Integer; cdecl;
    {class} function _GetTYPE_STEP_DETECTOR: Integer; cdecl;
    {class} function _GetTYPE_TEMPERATURE: Integer; cdecl;
    {class} property REPORTING_MODE_CONTINUOUS: Integer read _GetREPORTING_MODE_CONTINUOUS;
    {class} property REPORTING_MODE_ONE_SHOT: Integer read _GetREPORTING_MODE_ONE_SHOT;
    {class} property REPORTING_MODE_ON_CHANGE: Integer read _GetREPORTING_MODE_ON_CHANGE;
    {class} property REPORTING_MODE_SPECIAL_TRIGGER: Integer read _GetREPORTING_MODE_SPECIAL_TRIGGER;
    {class} property STRING_TYPE_ACCELEROMETER: JString read _GetSTRING_TYPE_ACCELEROMETER;
    {class} property STRING_TYPE_AMBIENT_TEMPERATURE: JString read _GetSTRING_TYPE_AMBIENT_TEMPERATURE;
    {class} property STRING_TYPE_GAME_ROTATION_VECTOR: JString read _GetSTRING_TYPE_GAME_ROTATION_VECTOR;
    {class} property STRING_TYPE_GEOMAGNETIC_ROTATION_VECTOR: JString read _GetSTRING_TYPE_GEOMAGNETIC_ROTATION_VECTOR;
    {class} property STRING_TYPE_GRAVITY: JString read _GetSTRING_TYPE_GRAVITY;
    {class} property STRING_TYPE_GYROSCOPE: JString read _GetSTRING_TYPE_GYROSCOPE;
    {class} property STRING_TYPE_GYROSCOPE_UNCALIBRATED: JString read _GetSTRING_TYPE_GYROSCOPE_UNCALIBRATED;
    {class} property STRING_TYPE_HEART_RATE: JString read _GetSTRING_TYPE_HEART_RATE;
    {class} property STRING_TYPE_LIGHT: JString read _GetSTRING_TYPE_LIGHT;
    {class} property STRING_TYPE_LINEAR_ACCELERATION: JString read _GetSTRING_TYPE_LINEAR_ACCELERATION;
    {class} property STRING_TYPE_MAGNETIC_FIELD: JString read _GetSTRING_TYPE_MAGNETIC_FIELD;
    {class} property STRING_TYPE_MAGNETIC_FIELD_UNCALIBRATED: JString read _GetSTRING_TYPE_MAGNETIC_FIELD_UNCALIBRATED;
    {class} property STRING_TYPE_ORIENTATION: JString read _GetSTRING_TYPE_ORIENTATION;
    {class} property STRING_TYPE_PRESSURE: JString read _GetSTRING_TYPE_PRESSURE;
    {class} property STRING_TYPE_PROXIMITY: JString read _GetSTRING_TYPE_PROXIMITY;
    {class} property STRING_TYPE_RELATIVE_HUMIDITY: JString read _GetSTRING_TYPE_RELATIVE_HUMIDITY;
    {class} property STRING_TYPE_ROTATION_VECTOR: JString read _GetSTRING_TYPE_ROTATION_VECTOR;
    {class} property STRING_TYPE_SIGNIFICANT_MOTION: JString read _GetSTRING_TYPE_SIGNIFICANT_MOTION;
    {class} property STRING_TYPE_STEP_COUNTER: JString read _GetSTRING_TYPE_STEP_COUNTER;
    {class} property STRING_TYPE_STEP_DETECTOR: JString read _GetSTRING_TYPE_STEP_DETECTOR;
    {class} property STRING_TYPE_TEMPERATURE: JString read _GetSTRING_TYPE_TEMPERATURE;
    {class} property TYPE_ACCELEROMETER: Integer read _GetTYPE_ACCELEROMETER;
    {class} property TYPE_ALL: Integer read _GetTYPE_ALL;
    {class} property TYPE_AMBIENT_TEMPERATURE: Integer read _GetTYPE_AMBIENT_TEMPERATURE;
    {class} property TYPE_GAME_ROTATION_VECTOR: Integer read _GetTYPE_GAME_ROTATION_VECTOR;
    {class} property TYPE_GEOMAGNETIC_ROTATION_VECTOR: Integer read _GetTYPE_GEOMAGNETIC_ROTATION_VECTOR;
    {class} property TYPE_GRAVITY: Integer read _GetTYPE_GRAVITY;
    {class} property TYPE_GYROSCOPE: Integer read _GetTYPE_GYROSCOPE;
    {class} property TYPE_GYROSCOPE_UNCALIBRATED: Integer read _GetTYPE_GYROSCOPE_UNCALIBRATED;
    {class} property TYPE_HEART_RATE: Integer read _GetTYPE_HEART_RATE;
    {class} property TYPE_LIGHT: Integer read _GetTYPE_LIGHT;
    {class} property TYPE_LINEAR_ACCELERATION: Integer read _GetTYPE_LINEAR_ACCELERATION;
    {class} property TYPE_MAGNETIC_FIELD: Integer read _GetTYPE_MAGNETIC_FIELD;
    {class} property TYPE_MAGNETIC_FIELD_UNCALIBRATED: Integer read _GetTYPE_MAGNETIC_FIELD_UNCALIBRATED;
    {class} property TYPE_ORIENTATION: Integer read _GetTYPE_ORIENTATION;
    {class} property TYPE_PRESSURE: Integer read _GetTYPE_PRESSURE;
    {class} property TYPE_PROXIMITY: Integer read _GetTYPE_PROXIMITY;
    {class} property TYPE_RELATIVE_HUMIDITY: Integer read _GetTYPE_RELATIVE_HUMIDITY;
    {class} property TYPE_ROTATION_VECTOR: Integer read _GetTYPE_ROTATION_VECTOR;
    {class} property TYPE_SIGNIFICANT_MOTION: Integer read _GetTYPE_SIGNIFICANT_MOTION;
    {class} property TYPE_STEP_COUNTER: Integer read _GetTYPE_STEP_COUNTER;
    {class} property TYPE_STEP_DETECTOR: Integer read _GetTYPE_STEP_DETECTOR;
    {class} property TYPE_TEMPERATURE: Integer read _GetTYPE_TEMPERATURE;
  end;

  [JavaSignature('android/hardware/Sensor')]
  JSensor = interface(JObject)
    ['{6A4E470B-F097-434E-B27D-6C771C44F318}']
    function getFifoMaxEventCount: Integer; cdecl;
    function getFifoReservedEventCount: Integer; cdecl;
    function getMaxDelay: Integer; cdecl;
    function getMaximumRange: Single; cdecl;
    function getMinDelay: Integer; cdecl;
    function getName: JString; cdecl;
    function getPower: Single; cdecl;
    function getReportingMode: Integer; cdecl;
    function getResolution: Single; cdecl;
    function getStringType: JString; cdecl;
    function getType: Integer; cdecl;
    function getVendor: JString; cdecl;
    function getVersion: Integer; cdecl;
    function isWakeUpSensor: Boolean; cdecl;
    function toString: JString; cdecl;
  end;
  TJSensor = class(TJavaGenericImport<JSensorClass, JSensor>) end;

  JSensorEventClass = interface(JObjectClass)
    ['{F24352F6-3196-48D5-8512-A337867AB54C}']
  end;

  [JavaSignature('android/hardware/SensorEvent')]
  JSensorEvent = interface(JObject)
    ['{583C4821-4D8A-407E-8797-BA1D2853CC36}']
    function _Getaccuracy: Integer; cdecl;
    function _Getsensor: JSensor; cdecl;
    function _Gettimestamp: Int64; cdecl;
    function _Getvalues: TJavaArray<Single>; cdecl;
    procedure _Setaccuracy(Value: Integer); cdecl;
    procedure _Settimestamp(Value: Int64); cdecl;
    property accuracy: Integer read _Getaccuracy write _Setaccuracy;
    property sensor: JSensor read _Getsensor;
    property timestamp: Int64 read _Gettimestamp write _Settimestamp;
    property values: TJavaArray<Single> read _Getvalues;
  end;
  TJSensorEvent = class(TJavaGenericImport<JSensorEventClass, JSensorEvent>) end;

  JSensorEventListenerClass = interface(IJavaClass)
    ['{6943522B-147C-4C46-895F-562843FA365D}']
  end;

  [JavaSignature('android/hardware/SensorEventListener')]
  JSensorEventListener = interface(IJavaInstance)
    ['{EFEC7A15-54EC-416D-938E-B225C7A24F9A}']
    procedure onAccuracyChanged(sensor: JSensor; accuracy: Integer); cdecl;
    procedure onSensorChanged(event: JSensorEvent); cdecl;
  end;
  TJSensorEventListener = class(TJavaGenericImport<JSensorEventListenerClass, JSensorEventListener>) end;

  JSensorListenerClass = interface(IJavaClass)
    ['{50DAACBA-22C2-4D0B-8221-70F4E322D45B}']
  end;

  [JavaSignature('android/hardware/SensorListener')]
  JSensorListener = interface(IJavaInstance)
    ['{B456AFE9-D0C1-41EC-BD0C-50DBB07CA41E}']
    procedure onAccuracyChanged(sensor: Integer; accuracy: Integer); cdecl;
    procedure onSensorChanged(sensor: Integer; values: TJavaArray<Single>); cdecl;
  end;
  TJSensorListener = class(TJavaGenericImport<JSensorListenerClass, JSensorListener>) end;

  JSensorManagerClass = interface(JObjectClass)
    ['{D74F76E9-975D-4B6C-84EA-24C8F16CA81E}']
    {class} function _GetAXIS_MINUS_X: Integer; cdecl;
    {class} function _GetAXIS_MINUS_Y: Integer; cdecl;
    {class} function _GetAXIS_MINUS_Z: Integer; cdecl;
    {class} function _GetAXIS_X: Integer; cdecl;
    {class} function _GetAXIS_Y: Integer; cdecl;
    {class} function _GetAXIS_Z: Integer; cdecl;
    {class} function _GetDATA_X: Integer; cdecl;
    {class} function _GetDATA_Y: Integer; cdecl;
    {class} function _GetDATA_Z: Integer; cdecl;
    {class} function _GetGRAVITY_DEATH_STAR_I: Single; cdecl;
    {class} function _GetGRAVITY_EARTH: Single; cdecl;
    {class} function _GetGRAVITY_JUPITER: Single; cdecl;
    {class} function _GetGRAVITY_MARS: Single; cdecl;
    {class} function _GetGRAVITY_MERCURY: Single; cdecl;
    {class} function _GetGRAVITY_MOON: Single; cdecl;
    {class} function _GetGRAVITY_NEPTUNE: Single; cdecl;
    {class} function _GetGRAVITY_PLUTO: Single; cdecl;
    {class} function _GetGRAVITY_SATURN: Single; cdecl;
    {class} function _GetGRAVITY_SUN: Single; cdecl;
    {class} function _GetGRAVITY_THE_ISLAND: Single; cdecl;
    {class} function _GetGRAVITY_URANUS: Single; cdecl;
    {class} function _GetGRAVITY_VENUS: Single; cdecl;
    {class} function _GetLIGHT_CLOUDY: Single; cdecl;
    {class} function _GetLIGHT_FULLMOON: Single; cdecl;
    {class} function _GetLIGHT_NO_MOON: Single; cdecl;
    {class} function _GetLIGHT_OVERCAST: Single; cdecl;
    {class} function _GetLIGHT_SHADE: Single; cdecl;
    {class} function _GetLIGHT_SUNLIGHT: Single; cdecl;
    {class} function _GetLIGHT_SUNLIGHT_MAX: Single; cdecl;
    {class} function _GetLIGHT_SUNRISE: Single; cdecl;
    {class} function _GetMAGNETIC_FIELD_EARTH_MAX: Single; cdecl;
    {class} function _GetMAGNETIC_FIELD_EARTH_MIN: Single; cdecl;
    {class} function _GetPRESSURE_STANDARD_ATMOSPHERE: Single; cdecl;
    {class} function _GetRAW_DATA_INDEX: Integer; cdecl;
    {class} function _GetRAW_DATA_X: Integer; cdecl;
    {class} function _GetRAW_DATA_Y: Integer; cdecl;
    {class} function _GetRAW_DATA_Z: Integer; cdecl;
    {class} function _GetSENSOR_ACCELEROMETER: Integer; cdecl;
    {class} function _GetSENSOR_ALL: Integer; cdecl;
    {class} function _GetSENSOR_DELAY_FASTEST: Integer; cdecl;
    {class} function _GetSENSOR_DELAY_GAME: Integer; cdecl;
    {class} function _GetSENSOR_DELAY_NORMAL: Integer; cdecl;
    {class} function _GetSENSOR_DELAY_UI: Integer; cdecl;
    {class} function _GetSENSOR_LIGHT: Integer; cdecl;
    {class} function _GetSENSOR_MAGNETIC_FIELD: Integer; cdecl;
    {class} function _GetSENSOR_MAX: Integer; cdecl;
    {class} function _GetSENSOR_MIN: Integer; cdecl;
    {class} function _GetSENSOR_ORIENTATION: Integer; cdecl;
    {class} function _GetSENSOR_ORIENTATION_RAW: Integer; cdecl;
    {class} function _GetSENSOR_PROXIMITY: Integer; cdecl;
    {class} function _GetSENSOR_STATUS_ACCURACY_HIGH: Integer; cdecl;
    {class} function _GetSENSOR_STATUS_ACCURACY_LOW: Integer; cdecl;
    {class} function _GetSENSOR_STATUS_ACCURACY_MEDIUM: Integer; cdecl;
    {class} function _GetSENSOR_STATUS_NO_CONTACT: Integer; cdecl;
    {class} function _GetSENSOR_STATUS_UNRELIABLE: Integer; cdecl;
    {class} function _GetSENSOR_TEMPERATURE: Integer; cdecl;
    {class} function _GetSENSOR_TRICORDER: Integer; cdecl;
    {class} function _GetSTANDARD_GRAVITY: Single; cdecl;
    {class} property AXIS_MINUS_X: Integer read _GetAXIS_MINUS_X;
    {class} property AXIS_MINUS_Y: Integer read _GetAXIS_MINUS_Y;
    {class} property AXIS_MINUS_Z: Integer read _GetAXIS_MINUS_Z;
    {class} property AXIS_X: Integer read _GetAXIS_X;
    {class} property AXIS_Y: Integer read _GetAXIS_Y;
    {class} property AXIS_Z: Integer read _GetAXIS_Z;
    {class} property DATA_X: Integer read _GetDATA_X;
    {class} property DATA_Y: Integer read _GetDATA_Y;
    {class} property DATA_Z: Integer read _GetDATA_Z;
    {class} property GRAVITY_DEATH_STAR_I: Single read _GetGRAVITY_DEATH_STAR_I;
    {class} property GRAVITY_EARTH: Single read _GetGRAVITY_EARTH;
    {class} property GRAVITY_JUPITER: Single read _GetGRAVITY_JUPITER;
    {class} property GRAVITY_MARS: Single read _GetGRAVITY_MARS;
    {class} property GRAVITY_MERCURY: Single read _GetGRAVITY_MERCURY;
    {class} property GRAVITY_MOON: Single read _GetGRAVITY_MOON;
    {class} property GRAVITY_NEPTUNE: Single read _GetGRAVITY_NEPTUNE;
    {class} property GRAVITY_PLUTO: Single read _GetGRAVITY_PLUTO;
    {class} property GRAVITY_SATURN: Single read _GetGRAVITY_SATURN;
    {class} property GRAVITY_SUN: Single read _GetGRAVITY_SUN;
    {class} property GRAVITY_THE_ISLAND: Single read _GetGRAVITY_THE_ISLAND;
    {class} property GRAVITY_URANUS: Single read _GetGRAVITY_URANUS;
    {class} property GRAVITY_VENUS: Single read _GetGRAVITY_VENUS;
    {class} property LIGHT_CLOUDY: Single read _GetLIGHT_CLOUDY;
    {class} property LIGHT_FULLMOON: Single read _GetLIGHT_FULLMOON;
    {class} property LIGHT_NO_MOON: Single read _GetLIGHT_NO_MOON;
    {class} property LIGHT_OVERCAST: Single read _GetLIGHT_OVERCAST;
    {class} property LIGHT_SHADE: Single read _GetLIGHT_SHADE;
    {class} property LIGHT_SUNLIGHT: Single read _GetLIGHT_SUNLIGHT;
    {class} property LIGHT_SUNLIGHT_MAX: Single read _GetLIGHT_SUNLIGHT_MAX;
    {class} property LIGHT_SUNRISE: Single read _GetLIGHT_SUNRISE;
    {class} property MAGNETIC_FIELD_EARTH_MAX: Single read _GetMAGNETIC_FIELD_EARTH_MAX;
    {class} property MAGNETIC_FIELD_EARTH_MIN: Single read _GetMAGNETIC_FIELD_EARTH_MIN;
    {class} property PRESSURE_STANDARD_ATMOSPHERE: Single read _GetPRESSURE_STANDARD_ATMOSPHERE;
    {class} property RAW_DATA_INDEX: Integer read _GetRAW_DATA_INDEX;
    {class} property RAW_DATA_X: Integer read _GetRAW_DATA_X;
    {class} property RAW_DATA_Y: Integer read _GetRAW_DATA_Y;
    {class} property RAW_DATA_Z: Integer read _GetRAW_DATA_Z;
    {class} property SENSOR_ACCELEROMETER: Integer read _GetSENSOR_ACCELEROMETER;
    {class} property SENSOR_ALL: Integer read _GetSENSOR_ALL;
    {class} property SENSOR_DELAY_FASTEST: Integer read _GetSENSOR_DELAY_FASTEST;
    {class} property SENSOR_DELAY_GAME: Integer read _GetSENSOR_DELAY_GAME;
    {class} property SENSOR_DELAY_NORMAL: Integer read _GetSENSOR_DELAY_NORMAL;
    {class} property SENSOR_DELAY_UI: Integer read _GetSENSOR_DELAY_UI;
    {class} property SENSOR_LIGHT: Integer read _GetSENSOR_LIGHT;
    {class} property SENSOR_MAGNETIC_FIELD: Integer read _GetSENSOR_MAGNETIC_FIELD;
    {class} property SENSOR_MAX: Integer read _GetSENSOR_MAX;
    {class} property SENSOR_MIN: Integer read _GetSENSOR_MIN;
    {class} property SENSOR_ORIENTATION: Integer read _GetSENSOR_ORIENTATION;
    {class} property SENSOR_ORIENTATION_RAW: Integer read _GetSENSOR_ORIENTATION_RAW;
    {class} property SENSOR_PROXIMITY: Integer read _GetSENSOR_PROXIMITY;
    {class} property SENSOR_STATUS_ACCURACY_HIGH: Integer read _GetSENSOR_STATUS_ACCURACY_HIGH;
    {class} property SENSOR_STATUS_ACCURACY_LOW: Integer read _GetSENSOR_STATUS_ACCURACY_LOW;
    {class} property SENSOR_STATUS_ACCURACY_MEDIUM: Integer read _GetSENSOR_STATUS_ACCURACY_MEDIUM;
    {class} property SENSOR_STATUS_NO_CONTACT: Integer read _GetSENSOR_STATUS_NO_CONTACT;
    {class} property SENSOR_STATUS_UNRELIABLE: Integer read _GetSENSOR_STATUS_UNRELIABLE;
    {class} property SENSOR_TEMPERATURE: Integer read _GetSENSOR_TEMPERATURE;
    {class} property SENSOR_TRICORDER: Integer read _GetSENSOR_TRICORDER;
    {class} property STANDARD_GRAVITY: Single read _GetSTANDARD_GRAVITY;
  end;

  [JavaSignature('android/hardware/SensorManager')]
  JSensorManager = interface(JObject)
    ['{BDC77D59-3B37-44EC-9386-F75117670E80}']
    function cancelTriggerSensor(listener: JTriggerEventListener; sensor: JSensor): Boolean; cdecl;
    function flush(listener: JSensorEventListener): Boolean; cdecl;
    function getAltitude(p0: Single; p: Single): Single; cdecl;
    procedure getAngleChange(angleChange: TJavaArray<Single>; R: TJavaArray<Single>; prevR: TJavaArray<Single>); cdecl;
    function getDefaultSensor(&type: Integer): JSensor; cdecl; overload;
    function getDefaultSensor(&type: Integer; wakeUp: Boolean): JSensor; cdecl; overload;
    function getInclination(I: TJavaArray<Single>): Single; cdecl;
    function getOrientation(R: TJavaArray<Single>; values: TJavaArray<Single>): TJavaArray<Single>; cdecl;
    procedure getQuaternionFromVector(Q: TJavaArray<Single>; rv: TJavaArray<Single>); cdecl;
    function getRotationMatrix(R: TJavaArray<Single>; I: TJavaArray<Single>; gravity: TJavaArray<Single>; geomagnetic: TJavaArray<Single>): Boolean; cdecl;
    procedure getRotationMatrixFromVector(R: TJavaArray<Single>; rotationVector: TJavaArray<Single>); cdecl;
    function getSensorList(&type: Integer): JList; cdecl;
    function getSensors: Integer; cdecl;
    function registerListener(listener: JSensorListener; sensors: Integer): Boolean; cdecl; overload;
    function registerListener(listener: JSensorListener; sensors: Integer; rate: Integer): Boolean; cdecl; overload;
    function registerListener(listener: JSensorEventListener; sensor: JSensor; samplingPeriodUs: Integer; maxReportLatencyUs: Integer; handler: JHandler): Boolean; cdecl; overload;
    function registerListener(listener: JSensorEventListener; sensor: JSensor; samplingPeriodUs: Integer): Boolean; cdecl; overload;
    function registerListener(listener: JSensorEventListener; sensor: JSensor; samplingPeriodUs: Integer; maxReportLatencyUs: Integer): Boolean; cdecl; overload;
    function registerListener(listener: JSensorEventListener; sensor: JSensor; samplingPeriodUs: Integer; handler: JHandler): Boolean; cdecl; overload;
    function remapCoordinateSystem(inR: TJavaArray<Single>; X: Integer; Y: Integer; outR: TJavaArray<Single>): Boolean; cdecl;
    function requestTriggerSensor(listener: JTriggerEventListener; sensor: JSensor): Boolean; cdecl;
    procedure unregisterListener(listener: JSensorEventListener); cdecl; overload;
    procedure unregisterListener(listener: JSensorListener); cdecl; overload;
    procedure unregisterListener(listener: JSensorListener; sensors: Integer); cdecl; overload;
    procedure unregisterListener(listener: JSensorEventListener; sensor: JSensor); cdecl; overload;
  end;
  TJSensorManager = class(TJavaGenericImport<JSensorManagerClass, JSensorManager>) end;

  JTriggerEventClass = interface(JObjectClass)
    ['{F78AF156-EFBE-4746-879C-D6CC66EF2331}']
  end;

  [JavaSignature('android/hardware/TriggerEvent')]
  JTriggerEvent = interface(JObject)
    ['{E7D0476D-E806-4F94-B91B-C686E54AF563}']
    function _Getsensor: JSensor; cdecl;
    function _Gettimestamp: Int64; cdecl;
    function _Getvalues: TJavaArray<Single>; cdecl;
    procedure _Setsensor(Value: JSensor); cdecl;
    property sensor: JSensor read _Getsensor write _Setsensor;
    property timestamp: Int64 read _Gettimestamp;
    property values: TJavaArray<Single> read _Getvalues;
  end;
  TJTriggerEvent = class(TJavaGenericImport<JTriggerEventClass, JTriggerEvent>) end;

  JTriggerEventListenerClass = interface(JObjectClass)
    ['{AD91A8F2-F9F5-4B53-991C-C82B2CC5D103}']
    {class} function init: JTriggerEventListener; cdecl;
  end;

  [JavaSignature('android/hardware/TriggerEventListener')]
  JTriggerEventListener = interface(JObject)
    ['{6D2A9C5C-7EDC-4B5D-84AC-E866F466A281}']
    procedure onTrigger(event: JTriggerEvent); cdecl;
  end;
  TJTriggerEventListener = class(TJavaGenericImport<JTriggerEventListenerClass, JTriggerEventListener>) end;

implementation

end.

