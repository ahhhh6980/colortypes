# colortypes
[![crates.io](https://img.shields.io/crates/v/colortypes.svg)](https://crates.io/crates/colortypes)
[![Documentation](https://docs.rs/colortypes/badge.svg)](https://docs.rs/colortypes)

## A type safe color conversion library

This crate provides many methods for converting between color types.

Everything is implemented abstractly, and is easily extensible by the user.

When converting you call the `from_color` method of the colortype struct on your color!

## Implemented conversions:

| From | To | Method |
| ---------- | ---------- | ------------- |
| RGB | XYZ | Directly |
| RGB | HSV | Directly |
| RGB | HSL | Directly |
| RGB | LCH | Through XYZ |
| RGB | LAB | Through XYZ |
| LAB | LCH | Directly |
| RGB | YcBcR | Directly |

## Example usage

```rust
use colortypes::{Color, FromColorType, Rgba, Xyza, SRGB};

fn main() {
    // You can directly call Rgba this way (defaults to D65)
    let color = Rgba::new::<{ SRGB }>([0.0, 0.0, 0.0, 1.0]);
    // Or can call Color and specify Rgba this way
    let color = Color::<Rgba, { SRGB }>::new([0.0, 0.0, 0.0, 1.0]);

    // To convert to Xyza
    let new_color = Xyza::from_color(color);
}et new_color = Xyza::from_color(color);
}
```

## Images

This crate also provides a type for dealing with images, it's still work in progress
```rust
use colortypes::{Align, Image, Rgba, Xyya, SRGB};

fn main() {
    let mut img = Image::<Rgba, { SRGB }>::new((6000, 4000));

    img.put_pixel((0, 0), Rgba::new([0.7, 0.3, 0.2, 1.0]));

    let new_img = img.convert::<Xyya>();
    let new_img_b = img.convert::<Xyya>();
    let mut new_img = ((new_img + new_img_b) / 2.0).convert::<Rgba>();

    new_img = new_img.crop_align((Align::Front, Align::Front), (256, 256));

    let my_col = new_img.get_pixel((0, 0));
}
```
