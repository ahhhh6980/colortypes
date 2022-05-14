use colortypes::{Ycbcr, CIELAB, D65, REC709};
// TO-DO: https://doc.rust-lang.org/book/ch11-03-test-organization.html
use colortypes::colors::{CIELab, CIELch, Hsv, Rgb, Srgb, Xyz};
use colortypes::types::FromColorType;
/*
ADOBE_RGB
*/
extern crate rand;
use rand::Rng;
macro_rules! test_conversion_and_back {
    ($from:ident<$white:ident, $gamut:ident>, $to:ident<$gamut_to:ident>) => {
        paste::item! {
            #[test]
            #[allow(non_snake_case)]
            fn [< convert_ $from _ $to _ $from >] () {
                let mut rng = rand::thread_rng();
                let color = <$from>::new::<$white>([
                    rng.gen_range(0.0..=1.0),
                    rng.gen_range(0.0..=1.0),
                    rng.gen_range(0.0..=1.0),
                    0.0,
                ]);
                let new_col1 = <$to>::from_color(color);
                let new_col2 = <$from>::from_color(new_col1);

                let b = color.within_u16_sqrd_precision_of(new_col2);

                assert!(b[0]);
                assert!(b[1]);
                assert!(b[2]);
                assert!(b[3]);
            }
            #[test]
            #[allow(non_snake_case)]
            fn [< convert_ $to _ $from _ $to >] () {
                let mut rng = rand::thread_rng();
                let color = <$to>::new::<$white>([
                    rng.gen_range(0.0..=1.0),
                    rng.gen_range(0.0..=1.0),
                    rng.gen_range(0.0..=1.0),
                    0.0,
                ]);
                let new_col1 = <$from>::from_color(color);
                let new_col2 = <$to>::from_color(new_col1);

                let b = color.within_u16_sqrd_precision_of(new_col2);

                assert!(b[0]);
                assert!(b[1]);
                assert!(b[2]);
                assert!(b[3]);
            }
        }
    };
}

test_conversion_and_back!(Rgb<D65, SRGB>, Xyz<SRGB>);
test_conversion_and_back!(Rgb<D65, SRGB>, Srgb<SRGB>);
test_conversion_and_back!(Rgb<D65, SRGB>, CIELab<CIELAB>);
test_conversion_and_back!(Rgb<D65, SRGB>, CIELch<CIELAB>);
// Theres something weird about my Hsva conversion, and this may not be accurate
test_conversion_and_back!(Rgb<D65, SRGB>, Hsv<SRGB>);
test_conversion_and_back!(Rgb<D65, SRGB>, Ycbcr<SRGB>);
test_conversion_and_back!(CIELab<D65, CIELAB>, CIELch<CIELAB>);
test_conversion_and_back!(CIELab<D65, CIELAB>, Xyz<SRGB>);
