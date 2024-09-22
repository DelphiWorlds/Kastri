#import <Foundation/Foundation.h>

#import <MLImage/GMLImage.h>


#import "MLKCompatibleImage.h"

NS_ASSUME_NONNULL_BEGIN

/** A category for indicating that an `MLImage` is an `MLKitCompatibleImage`. */
@interface GMLImage (MLKitCompatible) <MLKCompatibleImage>
@end

NS_ASSUME_NONNULL_END
