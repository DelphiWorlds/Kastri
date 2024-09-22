#import <Foundation/Foundation.h>

#import "MLKRemoteModel.h"

@class MLKRemoteModelSource;

NS_ASSUME_NONNULL_BEGIN

/** A custom model that is stored remotely on the server and downloaded to the device. */
NS_SWIFT_NAME(CustomRemoteModel)
@interface MLKCustomRemoteModel : MLKRemoteModel

/**
 * Creates a new instance with the given remote model source.
 *
 * @param remoteModelSource The source of the custom remote model.
 * @return A new `CustomRemoteModel` instance.
 */
- (instancetype)initWithRemoteModelSource:(MLKRemoteModelSource *)remoteModelSource;

/** Unavailable. */
- (instancetype)init NS_UNAVAILABLE;

@end

NS_ASSUME_NONNULL_END
