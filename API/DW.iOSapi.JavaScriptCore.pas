unit DW.iOSapi.JavaScriptCore;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2025 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

uses
  Macapi.ObjectiveC, Macapi.CoreFoundation,
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.CoreGraphics;

const
  kJSTypeUndefined = 0;
  kJSTypeNull = 1;
  kJSTypeBoolean = 2;
  kJSTypeNumber = 3;
  kJSTypeString = 4;
  kJSTypeObject = 5;
  kJSTypeSymbol = 6;
  kJSTypedArrayTypeInt8Array = 0;
  kJSTypedArrayTypeInt16Array = 1;
  kJSTypedArrayTypeInt32Array = 2;
  kJSTypedArrayTypeUint8Array = 3;
  kJSTypedArrayTypeUint8ClampedArray = 4;
  kJSTypedArrayTypeUint16Array = 5;
  kJSTypedArrayTypeUint32Array = 6;
  kJSTypedArrayTypeFloat32Array = 7;
  kJSTypedArrayTypeFloat64Array = 8;
  kJSTypedArrayTypeArrayBuffer = 9;
  kJSTypedArrayTypeNone = 10;
  kJSPropertyAttributeNone = 0;
  kJSPropertyAttributeReadOnly = 2;
  kJSPropertyAttributeDontEnum = 4;
  kJSPropertyAttributeDontDelete = 8;
  kJSClassAttributeNone = 0;
  kJSClassAttributeNoAutomaticPrototype = 2;

type
  JSContext = interface;
  JSValue = interface;
  JSManagedValue = interface;
  JSVirtualMachine = interface;
  JSExport = interface;

  POpaqueJSContextGroup = Pointer;
  PPOpaqueJSContextGroup = ^POpaqueJSContextGroup;
  POpaqueJSContext = Pointer;
  PPOpaqueJSContext = ^POpaqueJSContext;
  POpaqueJSString = Pointer;
  PPOpaqueJSString = ^POpaqueJSString;
  POpaqueJSClass = Pointer;
  PPOpaqueJSClass = ^POpaqueJSClass;
  POpaqueJSPropertyNameArray = Pointer;
  PPOpaqueJSPropertyNameArray = ^POpaqueJSPropertyNameArray;
  POpaqueJSPropertyNameAccumulator = Pointer;
  PPOpaqueJSPropertyNameAccumulator = ^POpaqueJSPropertyNameAccumulator;
  POpaqueJSValue = Pointer;
  PPOpaqueJSValue = ^POpaqueJSValue;
  PJSStaticValue = ^JSStaticValue;
  PJSStaticFunction = ^JSStaticFunction;
  PJSClassDefinition = ^JSClassDefinition;
  JSType = UInt32;
  JSTypedArrayType = UInt32;

  JSContextGroupRef = Pointer;
  PJSContextGroupRef = ^JSContextGroupRef;
  JSContextRef = Pointer;
  PJSContextRef = ^JSContextRef;
  JSGlobalContextRef = Pointer;
  PJSGlobalContextRef = ^JSGlobalContextRef;
  JSStringRef = Pointer;
  PJSStringRef = ^JSStringRef;
  JSClassRef = Pointer;
  PJSClassRef = ^JSClassRef;
  JSPropertyNameArrayRef = Pointer;
  PJSPropertyNameArrayRef = ^JSPropertyNameArrayRef;
  JSPropertyNameAccumulatorRef = Pointer;
  PJSPropertyNameAccumulatorRef = ^JSPropertyNameAccumulatorRef;

  JSTypedArrayBytesDeallocator = procedure(bytes: Pointer; deallocatorContext: Pointer); cdecl;
  JSValueRef = Pointer;
  PJSValueRef = ^JSValueRef;
  JSObjectRef = Pointer;
  PJSObjectRef = ^JSObjectRef;
  JSPropertyAttributes = Cardinal;
  JSClassAttributes = Cardinal;

  JSObjectInitializeCallback = procedure(ctx: JSContextRef; &object: JSObjectRef); cdecl;

  JSObjectFinalizeCallback = procedure(&object: JSObjectRef); cdecl;

  JSObjectHasPropertyCallback = function: Integer; cdecl;

  JSObjectGetPropertyCallback = function(ctx: JSContextRef; &object: JSObjectRef; propertyName: JSStringRef;
    exception: PJSValueRef): JSValueRef; cdecl;

  JSObjectSetPropertyCallback = function: Integer; cdecl;

  JSObjectDeletePropertyCallback = function: Integer; cdecl;

  JSObjectGetPropertyNamesCallback = procedure(ctx: JSContextRef; &object: JSObjectRef; propertyNames: JSPropertyNameAccumulatorRef); cdecl;

  JSObjectCallAsFunctionCallback = function(ctx: JSContextRef; &function: JSObjectRef; thisObject: JSObjectRef; argumentCount: NativeUInt;
    arguments: PJSValueRef; exception: PJSValueRef): JSValueRef; cdecl;

  JSObjectCallAsConstructorCallback = function(ctx: JSContextRef; &constructor: JSObjectRef; argumentCount: NativeUInt; arguments: PJSValueRef;
    exception: PJSValueRef): JSObjectRef; cdecl;

  JSObjectHasInstanceCallback = function: Integer; cdecl;

  JSObjectConvertToTypeCallback = function(ctx: JSContextRef; &object: JSObjectRef; &type: JSType; exception: PJSValueRef): JSValueRef; cdecl;

  JSStaticValue = record
    name: PAnsiChar;
    getProperty: JSObjectGetPropertyCallback;
    setProperty: JSObjectSetPropertyCallback;
    attributes: JSPropertyAttributes;
  end;

  JSStaticFunction = record
    name: PAnsiChar;
    callAsFunction: JSObjectCallAsFunctionCallback;
    attributes: JSPropertyAttributes;
  end;

  JSClassDefinition = record
    version: Integer;
    attributes: JSClassAttributes;
    className: PAnsiChar;
    parentClass: JSClassRef;
    staticValues: PJSStaticValue;
    staticFunctions: PJSStaticFunction;
    initialize: JSObjectInitializeCallback;
    finalize: JSObjectFinalizeCallback;
    hasProperty: JSObjectHasPropertyCallback;
    getProperty: JSObjectGetPropertyCallback;
    setProperty: JSObjectSetPropertyCallback;
    deleteProperty: JSObjectDeletePropertyCallback;
    getPropertyNames: JSObjectGetPropertyNamesCallback;
    callAsFunction: JSObjectCallAsFunctionCallback;
    callAsConstructor: JSObjectCallAsConstructorCallback;
    hasInstance: JSObjectHasInstanceCallback;
    convertToType: JSObjectConvertToTypeCallback;
  end;

  JSChar = Word;
  PJSChar = ^JSChar;
  JSValueProperty = NSString;
  TJSContextBlockMethod1 = procedure(param1: JSContext; param2: JSValue) of object;
  TJSContextBlockMethod2 = procedure of object;
  TJSValueBlockMethod1 = procedure(resolve: JSValue; reject: JSValue) of object;

  JSContextClass = interface(NSObjectClass)
    ['{691FD580-14D3-4E89-B904-60958F301D44}']
    {class} function contextWithJSGlobalContextRef(jsGlobalContextRef: JSGlobalContextRef): JSContext; cdecl;
    {class} function currentArguments: NSArray; cdecl;
    {class} function currentCallee: JSValue; cdecl;
    {class} function currentContext: JSContext; cdecl;
    {class} function currentThis: JSValue; cdecl;
  end;

  JSContext = interface(NSObject)
    ['{3635EBDC-5609-41FA-B867-626497BCFAE5}']
    function evaluateScript(script: NSString; withSourceURL: NSURL): JSValue; overload; cdecl;
    function evaluateScript(script: NSString): JSValue; overload; cdecl;
    function exception: JSValue; cdecl;
    function exceptionHandler: TJSContextBlockMethod1; cdecl;
    function globalObject: JSValue; cdecl;
    function initWithVirtualMachine(virtualMachine: JSVirtualMachine): Pointer; cdecl;
    function JSGlobalContextRef: JSGlobalContextRef; cdecl;
    function name: NSString; cdecl;
    function objectForKeyedSubscript(key: Pointer): JSValue; cdecl;
    procedure setException(exception: JSValue); cdecl;
    procedure setExceptionHandler(exceptionHandler: TJSContextBlockMethod2); cdecl;
    procedure setName(name: NSString); cdecl;
    procedure setObject(&object: Pointer; forKeyedSubscript: NSObject); cdecl;
    function virtualMachine: JSVirtualMachine; cdecl;
  end;
  TJSContext = class(TOCGenericImport<JSContextClass, JSContext>) end;

  JSValueClass = interface(NSObjectClass)
    ['{3F79EE8A-2B07-4D49-9E6E-8D5A6A6E1FD0}']
    {class} function valueWithBool(value: Boolean; inContext: JSContext): JSValue; cdecl;
    {class} function valueWithDouble(value: Double; inContext: JSContext): JSValue; cdecl;
    {class} function valueWithInt32(value: Int32; inContext: JSContext): JSValue; cdecl;
    {class} function valueWithJSValueRef(value: JSValueRef; inContext: JSContext): JSValue; cdecl;
    {class} function valueWithNewArrayInContext(context: JSContext): JSValue; cdecl;
    {class} function valueWithNewErrorFromMessage(message: NSString; inContext: JSContext): JSValue; cdecl;
    {class} function valueWithNewObjectInContext(context: JSContext): JSValue; cdecl;
    {class} function valueWithNewPromiseInContext(context: JSContext; fromExecutor: TJSValueBlockMethod1): JSValue; cdecl;
    {class} function valueWithNewPromiseRejectedWithReason(reason: Pointer; inContext: JSContext): JSValue; cdecl;
    {class} function valueWithNewPromiseResolvedWithResult(result: Pointer; inContext: JSContext): JSValue; cdecl;
    {class} function valueWithNewRegularExpressionFromPattern(pattern: NSString; flags: NSString; inContext: JSContext): JSValue; cdecl;
    {class} function valueWithNewSymbolFromDescription(description: NSString; inContext: JSContext): JSValue; cdecl;
    {class} function valueWithNullInContext(context: JSContext): JSValue; cdecl;
    {class} function valueWithObject(value: Pointer; inContext: JSContext): JSValue; cdecl;
    {class} function valueWithPoint(point: CGPoint; inContext: JSContext): JSValue; cdecl;
    {class} function valueWithRange(range: NSRange; inContext: JSContext): JSValue; cdecl;
    {class} function valueWithRect(rect: CGRect; inContext: JSContext): JSValue; cdecl;
    {class} function valueWithSize(size: CGSize; inContext: JSContext): JSValue; cdecl;
    {class} function valueWithUInt32(value: UInt32; inContext: JSContext): JSValue; cdecl;
    {class} function valueWithUndefinedInContext(context: JSContext): JSValue; cdecl;
  end;

  JSValue = interface(NSObject)
    ['{4BC0145D-84BA-4125-AEA8-A88AD205A590}']
    function callWithArguments(arguments: NSArray): JSValue; cdecl;
    function constructWithArguments(arguments: NSArray): JSValue; cdecl;
    function context: JSContext; cdecl;
    procedure defineProperty(&property: JSValueProperty; descriptor: Pointer); cdecl;
    function deleteProperty(&property: JSValueProperty): Boolean; cdecl;
    function hasProperty(&property: JSValueProperty): Boolean; cdecl;
    function invokeMethod(method: NSString; withArguments: NSArray): JSValue; cdecl;
    function isArray: Boolean; cdecl;
    function isBoolean: Boolean; cdecl;
    function isDate: Boolean; cdecl;
    function isEqualToObject(value: Pointer): Boolean; cdecl;
    function isEqualWithTypeCoercionToObject(value: Pointer): Boolean; cdecl;
    function isInstanceOf(value: Pointer): Boolean; cdecl;
    function isNull: Boolean; cdecl;
    function isNumber: Boolean; cdecl;
    function isObject: Boolean; cdecl;
    function isString: Boolean; cdecl;
    function isSymbol: Boolean; cdecl;
    function isUndefined: Boolean; cdecl;
    function JSValueRef: JSValueRef; cdecl;
    function objectAtIndexedSubscript(index: NSUInteger): JSValue; cdecl;
    function objectForKeyedSubscript(key: Pointer): JSValue; cdecl;
    procedure setObject(&object: Pointer; forKeyedSubscript: Pointer); overload; cdecl;
    procedure setObject(&object: Pointer; atIndexedSubscript: NSUInteger); overload; cdecl;
    procedure setValue(value: Pointer; atIndex: NSUInteger); overload; cdecl;
    procedure setValue(value: Pointer; forProperty: JSValueProperty); overload; cdecl;
    function toArray: NSArray; cdecl;
    function toBool: Boolean; cdecl;
    function toDate: NSDate; cdecl;
    function toDictionary: NSDictionary; cdecl;
    function toDouble: Double; cdecl;
    function toInt32: Int32; cdecl;
    function toNumber: NSNumber; cdecl;
    function toObject: Pointer; cdecl;
    function toObjectOfClass(expectedClass: Pointer): Pointer; cdecl;
    function toPoint: CGPoint; cdecl;
    function toRange: NSRange; cdecl;
    function toRect: CGRect; cdecl;
    function toSize: CGSize; cdecl;
    function toString: NSString; cdecl;
    function toUInt32: UInt32; cdecl;
    function valueAtIndex(index: NSUInteger): JSValue; cdecl;
    function valueForProperty(&property: JSValueProperty): JSValue; cdecl;
  end;
  TJSValue = class(TOCGenericImport<JSValueClass, JSValue>) end;

  JSManagedValueClass = interface(NSObjectClass)
    ['{7BFF2469-40D7-4298-A4B6-ACF815E4419A}']
    {class} function managedValueWithValue(value: JSValue): JSManagedValue; overload; cdecl;
    {class} function managedValueWithValue(value: JSValue; andOwner: Pointer): JSManagedValue; overload; cdecl;
  end;

  JSManagedValue = interface(NSObject)
    ['{16F29E16-B21A-448F-8A29-51CED9B28214}']
    function initWithValue(value: JSValue): Pointer; cdecl;
    function value: JSValue; cdecl;
  end;
  TJSManagedValue = class(TOCGenericImport<JSManagedValueClass, JSManagedValue>) end;

  JSVirtualMachineClass = interface(NSObjectClass)
    ['{DE6CF6EC-7463-44DA-833C-A41D31544F56}']
  end;

  JSVirtualMachine = interface(NSObject)
    ['{7047EDE5-15DE-4437-B3CB-6E121CFEBDC8}']
    procedure addManagedReference(&object: Pointer; withOwner: Pointer); cdecl;
    procedure removeManagedReference(&object: Pointer; withOwner: Pointer); cdecl;
  end;
  TJSVirtualMachine = class(TOCGenericImport<JSVirtualMachineClass, JSVirtualMachine>) end;

  JSExport = interface(IObjectiveC)
    ['{5CE85F54-380D-460D-B60B-79C0A656977E}']
  end;

function JSPropertyDescriptorWritableKey: NSString;
function JSPropertyDescriptorEnumerableKey: NSString;
function JSPropertyDescriptorConfigurableKey: NSString;
function JSPropertyDescriptorValueKey: NSString;
function JSPropertyDescriptorGetKey: NSString;
function JSPropertyDescriptorSetKey: NSString;

const
  libJavaScriptCore = '/System/Library/Frameworks/JavaScriptCore.framework/JavaScriptCore';

function JSEvaluateScript(ctx: JSContextRef; script: JSStringRef; thisObject: JSObjectRef; sourceURL: JSStringRef; startingLineNumber: Integer;
  exception: PJSValueRef): JSValueRef; cdecl;
  external libJavaScriptCore name _PU + 'JSEvaluateScript';

function JSCheckScriptSyntax(): Integer; cdecl;
  external libJavaScriptCore name _PU + 'JSCheckScriptSyntax';

procedure JSGarbageCollect(ctx: JSContextRef); cdecl;
  external libJavaScriptCore name _PU + 'JSGarbageCollect';

function JSValueGetType(ctx: JSContextRef; value: JSValueRef): JSType; cdecl;
  external libJavaScriptCore name _PU + 'JSValueGetType';

function JSValueIsUndefined(): Integer; cdecl;
  external libJavaScriptCore name _PU + 'JSValueIsUndefined';

function JSValueIsNull(): Integer; cdecl;
  external libJavaScriptCore name _PU + 'JSValueIsNull';

function JSValueIsBoolean(): Integer; cdecl;
  external libJavaScriptCore name _PU + 'JSValueIsBoolean';

function JSValueIsNumber(): Integer; cdecl;
  external libJavaScriptCore name _PU + 'JSValueIsNumber';

function JSValueIsString(): Integer; cdecl;
  external libJavaScriptCore name _PU + 'JSValueIsString';

function JSValueIsSymbol(): Integer; cdecl;
  external libJavaScriptCore name _PU + 'JSValueIsSymbol';

function JSValueIsObject(): Integer; cdecl;
  external libJavaScriptCore name _PU + 'JSValueIsObject';

function JSValueIsObjectOfClass(): Integer; cdecl;
  external libJavaScriptCore name _PU + 'JSValueIsObjectOfClass';

function JSValueIsArray(): Integer; cdecl;
  external libJavaScriptCore name _PU + 'JSValueIsArray';

function JSValueIsDate(): Integer; cdecl;
  external libJavaScriptCore name _PU + 'JSValueIsDate';

function JSValueGetTypedArrayType(ctx: JSContextRef; value: JSValueRef; exception: PJSValueRef): JSTypedArrayType; cdecl;
  external libJavaScriptCore name _PU + 'JSValueGetTypedArrayType';

function JSValueIsEqual(): Integer; cdecl;
  external libJavaScriptCore name _PU + 'JSValueIsEqual';

function JSValueIsStrictEqual(): Integer; cdecl;
  external libJavaScriptCore name _PU + 'JSValueIsStrictEqual';

function JSValueIsInstanceOfConstructor(): Integer; cdecl;
  external libJavaScriptCore name _PU + 'JSValueIsInstanceOfConstructor';

function JSValueMakeUndefined(ctx: JSContextRef): JSValueRef; cdecl;
  external libJavaScriptCore name _PU + 'JSValueMakeUndefined';

function JSValueMakeNull(ctx: JSContextRef): JSValueRef; cdecl;
  external libJavaScriptCore name _PU + 'JSValueMakeNull';

function JSValueMakeBoolean(ctx: JSContextRef; boolean: Boolean): JSValueRef; cdecl;
  external libJavaScriptCore name _PU + 'JSValueMakeBoolean';

function JSValueMakeNumber(ctx: JSContextRef; number: Double): JSValueRef; cdecl;
  external libJavaScriptCore name _PU + 'JSValueMakeNumber';

function JSValueMakeString(ctx: JSContextRef; &string: JSStringRef): JSValueRef; cdecl;
  external libJavaScriptCore name _PU + 'JSValueMakeString';

function JSValueMakeSymbol(ctx: JSContextRef; description: JSStringRef): JSValueRef; cdecl;
  external libJavaScriptCore name _PU + 'JSValueMakeSymbol';

function JSValueMakeFromJSONString(ctx: JSContextRef; &string: JSStringRef): JSValueRef; cdecl;
  external libJavaScriptCore name _PU + 'JSValueMakeFromJSONString';

function JSValueCreateJSONString(ctx: JSContextRef; value: JSValueRef; indent: Cardinal; exception: PJSValueRef): JSStringRef; cdecl;
  external libJavaScriptCore name _PU + 'JSValueCreateJSONString';

function JSValueToBoolean(): Integer; cdecl;
  external libJavaScriptCore name _PU + 'JSValueToBoolean';

function JSValueToNumber(ctx: JSContextRef; value: JSValueRef; exception: PJSValueRef): Double; cdecl;
  external libJavaScriptCore name _PU + 'JSValueToNumber';

function JSValueToStringCopy(ctx: JSContextRef; value: JSValueRef; exception: PJSValueRef): JSStringRef; cdecl;
  external libJavaScriptCore name _PU + 'JSValueToStringCopy';

function JSValueToObject(ctx: JSContextRef; value: JSValueRef; exception: PJSValueRef): JSObjectRef; cdecl;
  external libJavaScriptCore name _PU + 'JSValueToObject';

procedure JSValueProtect(ctx: JSContextRef; value: JSValueRef); cdecl;
  external libJavaScriptCore name _PU + 'JSValueProtect';

procedure JSValueUnprotect(ctx: JSContextRef; value: JSValueRef); cdecl;
  external libJavaScriptCore name _PU + 'JSValueUnprotect';

function JSClassCreate(definition: PJSClassDefinition): JSClassRef; cdecl;
  external libJavaScriptCore name _PU + 'JSClassCreate';

function JSClassRetain(jsClass: JSClassRef): JSClassRef; cdecl;
  external libJavaScriptCore name _PU + 'JSClassRetain';

procedure JSClassRelease(jsClass: JSClassRef); cdecl;
  external libJavaScriptCore name _PU + 'JSClassRelease';

function JSObjectMake(ctx: JSContextRef; jsClass: JSClassRef; data: Pointer): JSObjectRef; cdecl;
  external libJavaScriptCore name _PU + 'JSObjectMake';

function JSObjectMakeFunctionWithCallback(ctx: JSContextRef; name: JSStringRef; callAsFunction: JSObjectCallAsFunctionCallback): JSObjectRef; cdecl;
  external libJavaScriptCore name _PU + 'JSObjectMakeFunctionWithCallback';

function JSObjectMakeConstructor(ctx: JSContextRef; jsClass: JSClassRef; callAsConstructor: JSObjectCallAsConstructorCallback): JSObjectRef; cdecl;
  external libJavaScriptCore name _PU + 'JSObjectMakeConstructor';

function JSObjectMakeArray(ctx: JSContextRef; argumentCount: NativeUInt; arguments: PJSValueRef; exception: PJSValueRef): JSObjectRef; cdecl;
  external libJavaScriptCore name _PU + 'JSObjectMakeArray';

function JSObjectMakeDate(ctx: JSContextRef; argumentCount: NativeUInt; arguments: PJSValueRef; exception: PJSValueRef): JSObjectRef; cdecl;
  external libJavaScriptCore name _PU + 'JSObjectMakeDate';

function JSObjectMakeError(ctx: JSContextRef; argumentCount: NativeUInt; arguments: PJSValueRef; exception: PJSValueRef): JSObjectRef; cdecl;
  external libJavaScriptCore name _PU + 'JSObjectMakeError';

function JSObjectMakeRegExp(ctx: JSContextRef; argumentCount: NativeUInt; arguments: PJSValueRef; exception: PJSValueRef): JSObjectRef; cdecl;
  external libJavaScriptCore name _PU + 'JSObjectMakeRegExp';

function JSObjectMakeDeferredPromise(ctx: JSContextRef; resolve: PJSObjectRef; reject: PJSObjectRef; exception: PJSValueRef): JSObjectRef; cdecl;
  external libJavaScriptCore name _PU + 'JSObjectMakeDeferredPromise';

function JSObjectMakeFunction(ctx: JSContextRef; name: JSStringRef; parameterCount: Cardinal; parameterNames: PJSStringRef; body: JSStringRef;
  sourceURL: JSStringRef; startingLineNumber: Integer; exception: PJSValueRef): JSObjectRef; cdecl;
  external libJavaScriptCore name _PU + 'JSObjectMakeFunction';

function JSObjectGetPrototype(ctx: JSContextRef; &object: JSObjectRef): JSValueRef; cdecl;
  external libJavaScriptCore name _PU + 'JSObjectGetPrototype';

procedure JSObjectSetPrototype(ctx: JSContextRef; &object: JSObjectRef; value: JSValueRef); cdecl;
  external libJavaScriptCore name _PU + 'JSObjectSetPrototype';

function JSObjectHasProperty(): Integer; cdecl;
  external libJavaScriptCore name _PU + 'JSObjectHasProperty';

function JSObjectGetProperty(ctx: JSContextRef; &object: JSObjectRef; propertyName: JSStringRef; exception: PJSValueRef): JSValueRef; cdecl;
  external libJavaScriptCore name _PU + 'JSObjectGetProperty';

procedure JSObjectSetProperty(ctx: JSContextRef; &object: JSObjectRef; propertyName: JSStringRef; value: JSValueRef;
  attributes: JSPropertyAttributes; exception: PJSValueRef); cdecl;
  external libJavaScriptCore name _PU + 'JSObjectSetProperty';

function JSObjectDeleteProperty(): Integer; cdecl;
  external libJavaScriptCore name _PU + 'JSObjectDeleteProperty';

function JSObjectHasPropertyForKey(): Integer; cdecl;
  external libJavaScriptCore name _PU + 'JSObjectHasPropertyForKey';

function JSObjectGetPropertyForKey(ctx: JSContextRef; &object: JSObjectRef; propertyKey: JSValueRef; exception: PJSValueRef): JSValueRef; cdecl;
  external libJavaScriptCore name _PU + 'JSObjectGetPropertyForKey';

procedure JSObjectSetPropertyForKey(ctx: JSContextRef; &object: JSObjectRef; propertyKey: JSValueRef; value: JSValueRef;
  attributes: JSPropertyAttributes; exception: PJSValueRef); cdecl;
  external libJavaScriptCore name _PU + 'JSObjectSetPropertyForKey';

function JSObjectDeletePropertyForKey(): Integer; cdecl;
  external libJavaScriptCore name _PU + 'JSObjectDeletePropertyForKey';

function JSObjectGetPropertyAtIndex(ctx: JSContextRef; &object: JSObjectRef; propertyIndex: Cardinal; exception: PJSValueRef): JSValueRef; cdecl;
  external libJavaScriptCore name _PU + 'JSObjectGetPropertyAtIndex';

procedure JSObjectSetPropertyAtIndex(ctx: JSContextRef; &object: JSObjectRef; propertyIndex: Cardinal; value: JSValueRef;
  exception: PJSValueRef); cdecl;
  external libJavaScriptCore name _PU + 'JSObjectSetPropertyAtIndex';

function JSObjectGetPrivate(&object: JSObjectRef): Pointer; cdecl;
  external libJavaScriptCore name _PU + 'JSObjectGetPrivate';

function JSObjectSetPrivate(): Integer; cdecl;
  external libJavaScriptCore name _PU + 'JSObjectSetPrivate';

function JSObjectIsFunction(): Integer; cdecl;
  external libJavaScriptCore name _PU + 'JSObjectIsFunction';

function JSObjectCallAsFunction(ctx: JSContextRef; &object: JSObjectRef; thisObject: JSObjectRef; argumentCount: NativeUInt; arguments: PJSValueRef;
  exception: PJSValueRef): JSValueRef; cdecl;
  external libJavaScriptCore name _PU + 'JSObjectCallAsFunction';

function JSObjectIsConstructor(): Integer; cdecl;
  external libJavaScriptCore name _PU + 'JSObjectIsConstructor';

function JSObjectCallAsConstructor(ctx: JSContextRef; &object: JSObjectRef; argumentCount: NativeUInt; arguments: PJSValueRef;
  exception: PJSValueRef): JSObjectRef; cdecl;
  external libJavaScriptCore name _PU + 'JSObjectCallAsConstructor';

function JSObjectCopyPropertyNames(ctx: JSContextRef; &object: JSObjectRef): JSPropertyNameArrayRef; cdecl;
  external libJavaScriptCore name _PU + 'JSObjectCopyPropertyNames';

function JSPropertyNameArrayRetain(&array: JSPropertyNameArrayRef): JSPropertyNameArrayRef; cdecl;
  external libJavaScriptCore name _PU + 'JSPropertyNameArrayRetain';

procedure JSPropertyNameArrayRelease(&array: JSPropertyNameArrayRef); cdecl;
  external libJavaScriptCore name _PU + 'JSPropertyNameArrayRelease';

function JSPropertyNameArrayGetCount(&array: JSPropertyNameArrayRef): NativeUInt; cdecl;
  external libJavaScriptCore name _PU + 'JSPropertyNameArrayGetCount';

function JSPropertyNameArrayGetNameAtIndex(&array: JSPropertyNameArrayRef; index: NativeUInt): JSStringRef; cdecl;
  external libJavaScriptCore name _PU + 'JSPropertyNameArrayGetNameAtIndex';

procedure JSPropertyNameAccumulatorAddName(accumulator: JSPropertyNameAccumulatorRef; propertyName: JSStringRef); cdecl;
  external libJavaScriptCore name _PU + 'JSPropertyNameAccumulatorAddName';

function JSContextGroupCreate: JSContextGroupRef; cdecl;
  external libJavaScriptCore name _PU + 'JSContextGroupCreate';

function JSContextGroupRetain(group: JSContextGroupRef): JSContextGroupRef; cdecl;
  external libJavaScriptCore name _PU + 'JSContextGroupRetain';

procedure JSContextGroupRelease(group: JSContextGroupRef); cdecl;
  external libJavaScriptCore name _PU + 'JSContextGroupRelease';

function JSGlobalContextCreate(globalObjectClass: JSClassRef): JSGlobalContextRef; cdecl;
  external libJavaScriptCore name _PU + 'JSGlobalContextCreate';

function JSGlobalContextCreateInGroup(group: JSContextGroupRef; globalObjectClass: JSClassRef): JSGlobalContextRef; cdecl;
  external libJavaScriptCore name _PU + 'JSGlobalContextCreateInGroup';

function JSGlobalContextRetain(ctx: JSGlobalContextRef): JSGlobalContextRef; cdecl;
  external libJavaScriptCore name _PU + 'JSGlobalContextRetain';

procedure JSGlobalContextRelease(ctx: JSGlobalContextRef); cdecl;
  external libJavaScriptCore name _PU + 'JSGlobalContextRelease';

function JSContextGetGlobalObject(ctx: JSContextRef): JSObjectRef; cdecl;
  external libJavaScriptCore name _PU + 'JSContextGetGlobalObject';

function JSContextGetGroup(ctx: JSContextRef): JSContextGroupRef; cdecl;
  external libJavaScriptCore name _PU + 'JSContextGetGroup';

function JSContextGetGlobalContext(ctx: JSContextRef): JSGlobalContextRef; cdecl;
  external libJavaScriptCore name _PU + 'JSContextGetGlobalContext';

function JSGlobalContextCopyName(ctx: JSGlobalContextRef): JSStringRef; cdecl;
  external libJavaScriptCore name _PU + 'JSGlobalContextCopyName';

procedure JSGlobalContextSetName(ctx: JSGlobalContextRef; name: JSStringRef); cdecl;
  external libJavaScriptCore name _PU + 'JSGlobalContextSetName';

function JSStringCreateWithCharacters(chars: PJSChar; numChars: NativeUInt): JSStringRef; cdecl;
  external libJavaScriptCore name _PU + 'JSStringCreateWithCharacters';

function JSStringCreateWithUTF8CString(&string: PAnsiChar): JSStringRef; cdecl;
  external libJavaScriptCore name _PU + 'JSStringCreateWithUTF8CString';

function JSStringRetain(&string: JSStringRef): JSStringRef; cdecl;
  external libJavaScriptCore name _PU + 'JSStringRetain';

procedure JSStringRelease(&string: JSStringRef); cdecl;
  external libJavaScriptCore name _PU + 'JSStringRelease';

function JSStringGetLength(&string: JSStringRef): NativeUInt; cdecl;
  external libJavaScriptCore name _PU + 'JSStringGetLength';

function JSStringGetCharactersPtr(&string: JSStringRef): PJSChar; cdecl;
  external libJavaScriptCore name _PU + 'JSStringGetCharactersPtr';

function JSStringGetMaximumUTF8CStringSize(&string: JSStringRef): NativeUInt; cdecl;
  external libJavaScriptCore name _PU + 'JSStringGetMaximumUTF8CStringSize';

function JSStringGetUTF8CString(&string: JSStringRef; buffer: PAnsiChar; bufferSize: NativeUInt): NativeUInt; cdecl;
  external libJavaScriptCore name _PU + 'JSStringGetUTF8CString';

function JSStringIsEqual(): Integer; cdecl;
  external libJavaScriptCore name _PU + 'JSStringIsEqual';

function JSStringIsEqualToUTF8CString(): Integer; cdecl;
  external libJavaScriptCore name _PU + 'JSStringIsEqualToUTF8CString';

function JSObjectMakeTypedArray(ctx: JSContextRef; arrayType: JSTypedArrayType; length: NativeUInt; exception: PJSValueRef): JSObjectRef; cdecl;
  external libJavaScriptCore name _PU + 'JSObjectMakeTypedArray';

function JSObjectMakeTypedArrayWithBytesNoCopy(ctx: JSContextRef; arrayType: JSTypedArrayType; bytes: Pointer; byteLength: NativeUInt;
  bytesDeallocator: JSTypedArrayBytesDeallocator; deallocatorContext: Pointer; exception: PJSValueRef): JSObjectRef; cdecl;
  external libJavaScriptCore name _PU + 'JSObjectMakeTypedArrayWithBytesNoCopy';

function JSObjectMakeTypedArrayWithArrayBuffer(ctx: JSContextRef; arrayType: JSTypedArrayType; buffer: JSObjectRef;
  exception: PJSValueRef): JSObjectRef; cdecl;
  external libJavaScriptCore name _PU + 'JSObjectMakeTypedArrayWithArrayBuffer';

function JSObjectMakeTypedArrayWithArrayBufferAndOffset(ctx: JSContextRef; arrayType: JSTypedArrayType; buffer: JSObjectRef; byteOffset: NativeUInt;
  length: NativeUInt; exception: PJSValueRef): JSObjectRef; cdecl;
  external libJavaScriptCore name _PU + 'JSObjectMakeTypedArrayWithArrayBufferAndOffset';

function JSObjectGetTypedArrayBytesPtr(ctx: JSContextRef; &object: JSObjectRef; exception: PJSValueRef): Pointer; cdecl;
  external libJavaScriptCore name _PU + 'JSObjectGetTypedArrayBytesPtr';

function JSObjectGetTypedArrayLength(ctx: JSContextRef; &object: JSObjectRef; exception: PJSValueRef): NativeUInt; cdecl;
  external libJavaScriptCore name _PU + 'JSObjectGetTypedArrayLength';

function JSObjectGetTypedArrayByteLength(ctx: JSContextRef; &object: JSObjectRef; exception: PJSValueRef): NativeUInt; cdecl;
  external libJavaScriptCore name _PU + 'JSObjectGetTypedArrayByteLength';

function JSObjectGetTypedArrayByteOffset(ctx: JSContextRef; &object: JSObjectRef; exception: PJSValueRef): NativeUInt; cdecl;
  external libJavaScriptCore name _PU + 'JSObjectGetTypedArrayByteOffset';

function JSObjectGetTypedArrayBuffer(ctx: JSContextRef; &object: JSObjectRef; exception: PJSValueRef): JSObjectRef; cdecl;
  external libJavaScriptCore name _PU + 'JSObjectGetTypedArrayBuffer';

function JSObjectMakeArrayBufferWithBytesNoCopy(ctx: JSContextRef; bytes: Pointer; byteLength: NativeUInt;
  bytesDeallocator: JSTypedArrayBytesDeallocator; deallocatorContext: Pointer; exception: PJSValueRef): JSObjectRef; cdecl;
  external libJavaScriptCore name _PU + 'JSObjectMakeArrayBufferWithBytesNoCopy';

function JSObjectGetArrayBufferBytesPtr(ctx: JSContextRef; &object: JSObjectRef; exception: PJSValueRef): Pointer; cdecl;
  external libJavaScriptCore name _PU + 'JSObjectGetArrayBufferBytesPtr';

function JSObjectGetArrayBufferByteLength(ctx: JSContextRef; &object: JSObjectRef; exception: PJSValueRef): NativeUInt; cdecl;
  external libJavaScriptCore name _PU + 'JSObjectGetArrayBufferByteLength';

function JSStringCreateWithCFString(&string: CFStringRef): JSStringRef; cdecl;
  external libJavaScriptCore name _PU + 'JSStringCreateWithCFString';

function JSStringCopyCFString(alloc: CFAllocatorRef; &string: JSStringRef): CFStringRef; cdecl;
  external libJavaScriptCore name _PU + 'JSStringCopyCFString';

implementation

{$IF Defined(IOS) and not Defined(CPUARM)}
uses
  Posix.Dlfcn;

var
  JavaScriptCoreModule: THandle;
{$ENDIF}

function JSPropertyDescriptorWritableKey: NSString;
begin
  Result := CocoaNSStringConst(libJavaScriptCore, 'JSPropertyDescriptorWritableKey');
end;

function JSPropertyDescriptorEnumerableKey: NSString;
begin
  Result := CocoaNSStringConst(libJavaScriptCore, 'JSPropertyDescriptorEnumerableKey');
end;

function JSPropertyDescriptorConfigurableKey: NSString;
begin
  Result := CocoaNSStringConst(libJavaScriptCore, 'JSPropertyDescriptorConfigurableKey');
end;

function JSPropertyDescriptorValueKey: NSString;
begin
  Result := CocoaNSStringConst(libJavaScriptCore, 'JSPropertyDescriptorValueKey');
end;

function JSPropertyDescriptorGetKey: NSString;
begin
  Result := CocoaNSStringConst(libJavaScriptCore, 'JSPropertyDescriptorGetKey');
end;

function JSPropertyDescriptorSetKey: NSString;
begin
  Result := CocoaNSStringConst(libJavaScriptCore, 'JSPropertyDescriptorSetKey');
end;

{$IF Defined(IOS) and not Defined(CPUARM)}
initialization
  JavaScriptCoreModule := dlopen(MarshaledAString(libJavaScriptCore), RTLD_LAZY);

finalization
  dlclose(JavaScriptCoreModule)
{$ENDIF}

end.