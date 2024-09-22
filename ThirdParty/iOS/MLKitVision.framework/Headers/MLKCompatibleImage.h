#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

/**
 * A protocol for indicating images that are compatible with ML Kit.
 *
 * Don’t implement this protocol yourself; instead, use `MLImage` to create an
 * `MLKitCompatibleImage`.
 */
NS_SWIFT_NAME(MLKitCompatibleImage)
@protocol MLKCompatibleImage <NSObject>
@end

NS_ASSUME_NONNULL_END
