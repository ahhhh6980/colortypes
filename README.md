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
use colortypes::prelude::*;

fn main() {
    // You can directly call Rgba this way (defaults to D65)
    let color = Rgba::new::<D65>([0.0, 0.0, 0.0, 1.0]);
    // Or can call Color and specify Rgba this way
    let color = Color::<Rgb, D65>::new([0.0, 0.0, 0.0, 1.0]);

    // To convert to Xyza
    let new_color = Xyz::from_color(color);

    // To adapt to a a new white point
    let new_color = Xyz::adapt_chroma::<D50>(color);

    // To clamp the values between a range
    let new_color = color.clamp(0.0,1.0);

    // To clamp the values within the color space
    let new_color = color.clamp_to_gamut();
}
```

## Colors also implement as methods the following f64 methods directly:

### sqrt, cbrt, cos, cosh, acos, acosh, sin, sinh, asin, asinh, tan, tanh, atan, atanh,abs, floor, ceil, round, exp, exp2, exp_m1, ln, ln_1p, log2, log10, recip, signum, powf, atan2, log, max, min, hypot, div_euclid, rem_euclid, mul_add, clamp

```rs
let new_color = my_color.mul_add_color(my_other_color, my_third_color);
```

# Complex Usage

```rust
pub const MYCOLORSPACE: ColorGamut = ColorGamut {
    // Here you construct the primaries in the xyY space
    // Col3(x, y, Y)
    primaries_xyy: [
        Col3(0.7350, 0.2740, 0.1670),
        Col3(0.2650, 0.7170, 0.0090),
        Col3(0.176204, 0.812985, 0.010811),
    ],
    // Here you reference the transfer functions
    transfer_fn: srgb_companding,
    transfer_fn_inv: srgb_inv_companding,
    // This is the conversion matrix intended for use from
    // XYZ to your color space
    conversion: Mat3(
        Col3(0.4887180, 0.3106803, 0.2006017),
        Col3(0.1762044, 0.8129847, 0.0108109),
        Col3(0.0000000, 0.0102048, 0.9897952),
    ),
    // Here you not the reference white point used
    white: E,
};
```

```rust
// With this you can define your own custom color space
impl_colorspace! {
    MyColorSpace<MY_COLOR_GAMUT &MY_REFERENCE_WHITE>
        [0.0..1.0, 0.0..1.0, 0.0..1.0],
}
// You'll notice
// [0.0..1.0, 0.0..1.0, 0.0..1.0]
// These are the ranges for each channel

// Heres CIELCH for example
impl_colorspace! {
    CIELch<CIELAB&E>
        [0.0..100.0, 0.0..133.0, 0.0..360.0],   
}

// Then you can implement your conversion method
impl_conversion!(|color: MyColorSpace| -> Rgb {
    // Code to convert
    ...
    // Construct your Color
    Color::new([new_ch.0, new_ch.1, new_ch.2, color.3])
});
```

# Images

This crate also provides a type for dealing with images, it's still work in progress
```rust
use colortypes::{Align, Image, Rgba, Xyya, SRGB};

fn main() {
    // Images can be constructed in two ways:
    let mut img = Image::<Rgba, D65>::new((6000, 4000));
    let mut img = Image::<Rgba, D65>::new_with((6000, 4000), Rgba::new([0.0, 0.0, 0.0, 1.0]));

    // You can place pixels and return pixels
    img.put_pixel((0, 0), Rgba::new([0.7, 0.3, 0.2, 1.0])).unwrap();
    img.get_pixel((0, 0)).unwrap();

    let new_img = img.convert::<Xyya>();
    let new_img_b = img.convert::<Xyya>();
    let mut new_img = ((new_img + new_img_b) / 2.0).convert::<Rgba>();

    new_img = new_img.crop_align((Align::Front, Align::Front), (256, 256));

    let my_col = new_img.get_pixel((0, 0));
}
```

## There are also various iteration methods for Image

```rs
 // There are various iteration methods

    pixels()
    Iterator<&Color>

    pixels_mut()
    Iterator<&mut Color>

    pixels_zip(&Image)
    Iterator<(&Color, &Color)>

    pixels_mut_zip(&Image)
    Iterator<(&mut Color, &Color)>

    pixels_zip_mut(&mut Image)
    Iterator<(&Color, &mut Color)>

    pixels_mut_zip_mut(&mut Image)
    Iterator<(&mut Color, &mut Color)>

    for_each_pixel(FnMut(&Color))
    Iterator<&Color>

    for_each_pixel_mut(FnMut(&Color))
    Iterator<&mut Color>

    map_pixels(FnMut(&Color) -> Color)
    Iterator<Color>

    map_f()
    Iterator<f64>

    map_pixels_with(&image)
    Iterator<Color>

    map_pixels_to::<ColorType>()
    Iterator<Color>

    map_pixels_to_with::<ColorType>(&Image)
    Iterator<(Color, &Color)>
```