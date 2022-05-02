//! An Example of initializing a Color

use colortypes::{Color, RefWhite::*, Rgba};
extern crate colortypes;

#[allow(unused)]
fn main() {
    // You can directly call Rgba this way (defaults to D65)
    let color = Rgba::new([0.0, 0.0, 0.0, 1.0]);
    // You can directly call Rgba this way and specify white reference
    let color = Rgba::new_w::<{ D65 }>([0.0, 0.0, 0.0, 1.0]);
    // Or can call Color and specify Rgba this way
    let color = Color::<Rgba, { D65 }>::new([0.0, 0.0, 0.0, 1.0]);
}
