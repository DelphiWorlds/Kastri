#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

/** A model stored locally on the device. */
NS_SWIFT_NAME(LocalModel)
@interface MLKLocalModel : NSObject

/** An absolute path to a model file stored locally on the device. */
@property(nonatomic, copy, readonly) NSString *path;

/**
 * An absolute path to a model manifest file stored locally on the device. `nil` if the model does
 * not have a manifest.
 */
@property(nonatomic, copy, readonly, nullable) NSString *manifestPath;

/**
 * Creates a new instance with the given model file path.
 *
 * @param path Absolute path to a model file stored locally on the device.
 * @return A new `LocalModel` instance.
 */
- (instancetype)initWithPath:(NSString *)path;

/**
 * Creates a new instance with the given manifest file path of an AutoML Vision Edge model.
 *
 * @param manifestPath Absolute path to an AutoML Vision Edge model manifest stored locally on the
 *     device.
 * @return A new `LocalModel` instance. `nil` if the model manifest at the given `manifestPath` is
 *     either missing or invalid.
 */
- (nullable instancetype)initWithManifestPath:(NSString *)manifestPath;

/** Unavailable. */
- (instancetype)init NS_UNAVAILABLE;

@end

NS_ASSUME_NONNULL_END
