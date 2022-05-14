use colortypes::colors::Rgb;
use colortypes::types::Color;
// use colortypes::{FromColorType, Xyza, SRGB};
use colortypes::prelude::*;

#[allow(unused)]
#[test]
fn init_methods_identical() {
    let ch = [0.0, 0.0, 0.0, 1.0];
    // You can directly call Rgba this way, and default to D65
    // let color1 = Rgba::new(ch);
    // You can directly call Rgba this way and specify white
    // let color2 = Rgba::new_w::<D65>(ch);
    // You can directly call Rgba this way and specify gamut
    // let color3 = Rgba::new_g::<ADOBE_RGB>(ch);
    // You can directly call Rgba this way and specify gamut AND white
    let color4 = Rgb::new::<D65>(ch);
    // Or can call Color and specify Rgba this way
    let color5 = Color::<Rgb, D65>::new(ch);

    // let color = Xyza::from_color(color1);

    // assert_eq!(color1, color2);
    // assert_eq!(color2, color3);
    // assert_eq!(color1, color3);
}
