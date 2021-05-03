#import <CoreGraphics/CoreGraphics.h>
#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

/** A point in an image. The point's coordinates have the same scale as the original image. */
NS_SWIFT_NAME(VisionPoint)
@interface MLKVisionPoint : NSObject

/** The x-coordinate of the point. */
@property(nonatomic, readonly) CGFloat x;

/** The y-coordinate of the point. */
@property(nonatomic, readonly) CGFloat y;

/** Unavailable. */
- (instancetype)init NS_UNAVAILABLE;

@end

NS_ASSUME_NONNULL_END
