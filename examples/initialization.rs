//! An Example of initializing a Color

use colortypes::{Color, Rgba, SRGB};
extern crate colortypes;

#[allow(unused)]
fn main() {
    // You can directly call Rgba this way (defaults to D65)
    let color = Rgba::new::<{ SRGB }>([0.0, 0.0, 0.0, 1.0]);
    // Or can call Color and specify Rgba this way
    let color = Color::<Rgba, { SRGB }>::new([0.0, 0.0, 0.0, 1.0]);
}
