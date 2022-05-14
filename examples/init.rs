//! An Example of initializing a Color

use colortypes::{prelude::*, CIELab, CIELch, Hsv, Ycbcr, Yxy, D50};
extern crate colortypes;

#[allow(unused)]
fn main() {
    // You can directly call Rgba this way (defaults to D65)
    let color = Rgb::new::<D65>([0.0, 0.0, 0.0, 1.0]);
    // Or can call Color and specify Rgba this way
    let color = Color::<Rgb, D65>::new([0.33, 1.0, 0.5, 1.0]);

    let new_color = color.mul_add_color(color, color);

    // let test_color =

    println! {"RGB: {}", color};
    let color2 = CIELab::from_color(color);
    println!("->   LAB {}", color2);
    let color2 = Xyz::from_color(color);
    println!("->   XYZ {}", color2);
    let color2 = Yxy::from_color(color);
    println!("->   Yxy {}", color2);
    let color2 = CIELch::from_color(color);
    println!("->   LCH {}", color2);
    let color2 = Hsv::from_color(color);
    println!("->   HSV {}", color2);
    // let color2 = Hsl::from_color(color);
    // println!("->   LCH {}", color2);
    let color2 = Ycbcr::from_color(color);
    println!("->   YcBcR {}", color2);

    let adapted_d50 = color.adapt_chroma::<D50>();
    println!("\n{}\n   Adapted From &D65 -> &D50\n{}", color, adapted_d50);
    println!(
        "   Adapted Back From &D50 -> &D65\n{}\n",
        adapted_d50.adapt_chroma::<D65>()
    );

    // println!("{:?}", (*color2.gamut()).primaries_xyy())
}
