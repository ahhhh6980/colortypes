use colortypes::colors::Rgba;
use colortypes::types::Color;
use colortypes::SRGB;

#[allow(unused)]
#[test]
fn init_methods_identical() {
    // You can directly call Rgba this way (defaults to D65)
    let ch = [0.0, 0.0, 0.0, 1.0];
    let color1 = Rgba::new(ch);
    // You can directly call Rgba this way and specify white reference
    let color2 = Rgba::new::<{ SRGB }>(ch);
    // Or can call Color and specify Rgba this way
    let color2 = Color::<Rgba, { SRGB }>::new(ch);
    assert_eq!(color1, color2);
    assert_eq!(color2, color1);
}
