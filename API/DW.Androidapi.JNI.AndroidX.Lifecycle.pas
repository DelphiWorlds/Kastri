unit DW.Androidapi.JNI.AndroidX.Lifecycle;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2026 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

uses
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText,
  // DW
  DW.Androidapi.JNI.Concurrent;

type
  JLiveData = interface;
  JMutableLiveData = interface;
  JLifecycle = interface;
  JLifecycleObserver = interface;
  JLifecycleOwner = interface;
  JLifecycle_State = interface;
  JProcessLifecycleOwner = interface;
  JViewModel = interface;
  JViewModelProvider_Factory = interface;
  JViewModelStore = interface;

  JLiveDataClass = interface(JObjectClass)
    ['{57738F7E-F789-4911-90D1-9C8E14720CC0}']
    {class} function init: JLiveData; cdecl;
  end;

  [JavaSignature('androidx/lifecycle/LiveData')]
  JLiveData = interface(JObject)
    ['{520539FF-2147-4B43-8287-23E12551B616}']
    function getValue: JObject; cdecl;
    function hasActiveObservers: Boolean; cdecl;
    function hasObservers: Boolean; cdecl;
    procedure observe(owner: JLifecycleOwner; observer: JLifecycleObserver); cdecl;
    procedure observeForever(observer: JLifecycleObserver); cdecl;
    procedure removeObserver(observer: JLifecycleObserver); cdecl;
    procedure removeObservers(owner: JLifecycleOwner); cdecl;
  end;
  TJLiveData = class(TJavaGenericImport<JLiveDataClass, JLiveData>) end;

  JMutableLiveDataClass = interface(JLiveDataClass)
    ['{F28C102C-51C2-4048-8FCC-9AB9EA939938}']
    {class} function init: JMutableLiveData; cdecl;
  end;

  [JavaSignature('androidx/lifecycle/MutableLiveData')]
  JMutableLiveData = interface(JLiveData)
    ['{597E7685-0F91-4384-A153-818DD5436B5D}']
    procedure postValue(object_: JObject); cdecl;
    procedure setValue(object_: JObject); cdecl;
  end;
  TJMutableLiveData = class(TJavaGenericImport<JMutableLiveDataClass, JMutableLiveData>) end;

  JLifecycleClass = interface(JObjectClass)
    ['{F2F1F7E5-DA76-4C08-8D90-8A4C787987D4}']
    {class} function init: JLifecycle; cdecl;
  end;

  [JavaSignature('androidx/lifecycle/Lifecycle')]
  JLifecycle = interface(JObject)
    ['{B72796CC-2153-4453-ACA7-294F41101518}']
    procedure addObserver(observer: JLifecycleObserver); cdecl;
    function getCurrentState: JLifecycle_State; cdecl;
    procedure removeObserver(observer: JLifecycleObserver); cdecl;
  end;
  TJLifecycle = class(TJavaGenericImport<JLifecycleClass, JLifecycle>) end;

  JLifecycleObserverClass = interface(IJavaClass)
    ['{4AF7ACEE-58A0-4FED-BE07-21162B9AA120}']
  end;

  [JavaSignature('androidx/lifecycle/LifecycleObserver')]
  JLifecycleObserver = interface(IJavaInstance)
    ['{A78847B9-5FDD-4E82-86A0-4D026F433099}']
  end;
  TJlifecycle_Observer = class(TJavaGenericImport<JLifecycleObserverClass, JLifecycleObserver>) end;

  JLifecycle_StateClass = interface(JEnumClass)
    ['{3996AB34-573B-4CA4-B854-BD6A56E0D513}']
    {class} function _GetCREATED: JLifecycle_State; cdecl;
    {class} function _GetDESTROYED: JLifecycle_State; cdecl;
    {class} function _GetINITIALIZED: JLifecycle_State; cdecl;
    {class} function _GetRESUMED: JLifecycle_State; cdecl;
    {class} function _GetSTARTED: JLifecycle_State; cdecl;
    {class} function valueOf(name: JString): JLifecycle_State; cdecl;
    {class} function values: TJavaObjectArray<JLifecycle_State>; cdecl;
    {class} property CREATED: JLifecycle_State read _GetCREATED;
    {class} property DESTROYED: JLifecycle_State read _GetDESTROYED;
    {class} property INITIALIZED: JLifecycle_State read _GetINITIALIZED;
    {class} property RESUMED: JLifecycle_State read _GetRESUMED;
    {class} property STARTED: JLifecycle_State read _GetSTARTED;
  end;

  [JavaSignature('androidx/lifecycle/Lifecycle$State')]
  JLifecycle_State = interface(JEnum)
    ['{2A370F27-CD68-4479-A219-D25D05FAB2FC}']
    function isAtLeast(state: JLifecycle_State): Boolean; cdecl;
  end;
  TJLifecycle_State = class(TJavaGenericImport<JLifecycle_StateClass, JLifecycle_State>) end;

  JLifecycleOwnerClass = interface(IJavaClass)
    ['{4A72E007-665C-4E9A-BF6A-3997CC4C55FB}']
  end;

  [JavaSignature('androidx/lifecycle/LifecycleOwner')]
  JLifecycleOwner = interface(IJavaInstance)
    ['{4ECDBBFC-2F20-471F-912E-BE96CA23E96E}']
    function getLifecycle: JLifecycle; cdecl;
  end;
  TJLifecycleOwner = class(TJavaGenericImport<JLifecycleOwnerClass, JLifecycleOwner>) end;

  JProcessLifecycleOwnerClass = interface(JLifecycleOwnerClass)
    ['{E1AC8BAC-3A0B-4346-AFC9-D309E99E6143}']
    {class} function _GetTIMEOUT_MS: Int64; cdecl;
    {class} function &get: JLifecycleOwner; cdecl;
    {class} property TIMEOUT_MS: Int64 read _GetTIMEOUT_MS;
  end;

  [JavaSignature('androidx/lifecycle/ProcessLifecycleOwner')]
  JProcessLifecycleOwner = interface(JLifecycleOwner)
    ['{7C7BDCCA-49A9-41BD-8AC7-ED239696EFD6}']
    function getLifecycle: JLifecycle; cdecl;
  end;
  TJProcessLifecycleOwner = class(TJavaGenericImport<JProcessLifecycleOwnerClass, JProcessLifecycleOwner>) end;

  JViewModelClass = interface(JObjectClass)
    ['{0B92D4F3-3E51-4AA6-A38D-FF1DE9FC9F35}']
    {class} function init: JViewModel; cdecl;
  end;

  [JavaSignature('androidx/lifecycle/ViewModel')]
  JViewModel = interface(JObject)
    ['{B4E441BA-736B-4A24-9BFF-21FF43DBC845}']
  end;
  TJViewModel = class(TJavaGenericImport<JViewModelClass, JViewModel>) end;

  JViewModelProvider_FactoryClass = interface(IJavaClass)
    ['{095B2E6D-B41C-4E02-9915-A6311A340DAF}']
  end;

  [JavaSignature('androidx/lifecycle/ViewModelProvider$Factory')]
  JViewModelProvider_Factory = interface(IJavaInstance)
    ['{6FBA0B54-3A11-41E2-8A30-C2E031A52B45}']
    function create(modelClass: Jlang_Class): JViewModel; cdecl;
  end;
  TJViewModelProvider_Factory = class(TJavaGenericImport<JViewModelProvider_FactoryClass, JViewModelProvider_Factory>) end;

  JViewModelStoreClass = interface(JObjectClass)
    ['{8A380CDC-F548-4D42-BD22-A8CB9A60CBFE}']
    {class} function init: JViewModelStore; cdecl;
  end;

  [JavaSignature('androidx/lifecycle/ViewModelStore')]
  JViewModelStore = interface(JObject)
    ['{649C82B8-57DB-4847-AA1C-217E51908113}']
    procedure clear; cdecl;
  end;
  TJViewModelStore = class(TJavaGenericImport<JViewModelStoreClass, JViewModelStore>) end;

implementation

end.
