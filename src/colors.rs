use crate::{D50, D65, E};

use super::types::{Col3, Color, ColorGamut, ColorType, FromColorType, Mat3, White};
// pub type RgbaD65 = Color<Rgb, D65, SRGB>;

/*
TO-DO:
JzAzBz
https://observablehq.com/@jrus/jzazbz
OKLab
https://bottosson.github.io/posts/oklab/
*/
// impl FromColorType<Lab> for LCH(ab)

/// CIELAB Gamut
///
#[allow(unused)]
pub const CIELAB: ColorGamut = ColorGamut {
    primaries_xyy: [
        Col3(0.7350, 0.2740, 0.1670),
        Col3(0.2650, 0.7170, 0.0090),
        Col3(0.176204, 0.812985, 0.010811),
    ],
    transfer_fn: srgb_companding,
    transfer_fn_inv: srgb_inv_companding,
    conversion: Mat3(
        Col3(0.4887180, 0.3106803, 0.2006017),
        Col3(0.1762044, 0.8129847, 0.0108109),
        Col3(0.0000000, 0.0102048, 0.9897952),
    ),
    white: E,
};

/// The Wide RGB Color Gamut
///
/// wip
#[allow(dead_code)]
pub const WIDE_GAMUT_RGB: ColorGamut = ColorGamut {
    primaries_xyy: [
        Col3(0.735, 0.115, 0.157),
        Col3(0.265, 0.826, 0.018),
        Col3(0.258187, 0.724938, 0.016875),
    ],
    transfer_fn_inv: srgb_inv_companding,
    transfer_fn: srgb_companding,
    conversion: Mat3(
        Col3(0.7161046, 0.1009296, 0.1471858),
        Col3(0.2581874, 0.7249378, 0.0168748),
        Col3(0.0000000, 0.0517813, 0.7734287),
    ),
    white: D50,
};

/// The Rec. 709 / sRGB Gamut
pub const REC709: ColorGamut = ColorGamut {
    primaries_xyy: [
        Col3(0.64, 0.3, 0.15),
        Col3(0.33, 0.6, 0.06),
        Col3(0.212656, 0.715158, 0.072186),
    ],
    transfer_fn: srgb_companding,
    transfer_fn_inv: srgb_inv_companding,
    conversion: Mat3(
        Col3(0.4124564, 0.3575761, 0.1804375),
        Col3(0.2126729, 0.7151522, 0.0721750),
        Col3(0.0193339, 0.1191920, 0.9503041),
    ),
    white: D65,
};

/// The Adobe RGB Color Gamut
///
/// wip
pub const ADOBE_RGB: ColorGamut = ColorGamut {
    primaries_xyy: [
        Col3(0.64, 0.21, 0.15),
        Col3(0.33, 0.71, 0.06),
        Col3(0.297361, 0.627355, 0.075285),
    ],
    transfer_fn: |x| x.powf((2.2f64).recip()),
    transfer_fn_inv: |x| x.powf(2.2),
    conversion: {
        Mat3(
            Col3(0.5767309, 0.1855540, 0.1881852),
            Col3(0.2973769, 0.6273491, 0.075274),
            Col3(0.0270343, 0.0706872, 0.9911085),
        )
    },
    white: D65,
};

/// Implement the construction of a new color type
#[macro_export]
macro_rules! impl_colorspace {
    {$($ctype:ident<$gamut:ident&$white:ident>
        [$r0:expr,
        $r1:expr,
        $r2:expr]
    ),+} => {
        $(
            #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
            pub struct $ctype;
            impl ColorType for $ctype {
                fn gamut() -> &'static ColorGamut {
                    &$gamut
                }
                fn white() -> &'static  White {
                    &$white
                }
                fn range() -> [std::ops::Range<f64>;3] {
                    [
                        $r0,
                        $r1,
                        $r2
                    ]
                }
            }
            impl $ctype {
                // /// Initialize color with a custom white point
                pub fn new< const WHITE: White>(ch: [f64;4]) -> Color<$ctype, WHITE> {
                    Color::new(ch)
                }

            }

        )+
    };
}

pub(crate) use impl_colorspace;

// pub trait FromWhiteType<const WHITE: White>: WhiteType {
//     fn from_white<const FWHITE: White, SPACE: ColorType>(
//         _: Color<SPACE, FWHITE>,
//     ) -> Color<SPACE, WHITE>;
// }

// #[macro_export]
// macro_rules! impl_white_conversion {
//     ($($from:ident : $to:ident),*) => {
//         $(
//             impl FromWhiteType<$from> for $to {
//                 fn from_white<const SW: White, const DW: White, SPACE: ColorType>(
//                     color: Color<SPACE, SW>,
//                 ) {
//                     DW::adapt_chroma_from_xyz(color)
//                 }
//             }
//         )*
//     };
// }
// pub(crate) use impl_white_conversion;

/// Implement the conversion from one type to another
#[macro_export]
macro_rules! impl_conversion {
    (|$color_name:ident : $from:ident| -> $to:ident $method:block) => {
        impl FromColorType<$from> for $to {
            fn from_color<const WHITE: White>(
                $color_name: Color<$from, WHITE>,
            ) -> Color<$to, WHITE> {
                // let $color_name = match FROMWHITE {
                //     WHITE => $color_name,
                //     _ => $color_name,
                // };
                $method
            }
        }
    };
}
pub(crate) use impl_conversion;

// impl_white_conversion!(D65: D50, D50: D65, D65: E, D50: E, E: D50, E: D65);

impl_colorspace! {
    Rgb<ADOBE_RGB&D65>
        [0.0..1.0, 0.0..1.0, 0.0..1.0],

    Xyz<REC709&D65>
        [-200.0..200.0, -200.0..200.0, -200.0..200.0],

    Yxy<REC709&D65>
        [0.0..1.0, 0.0..1.0, 0.0..1.0],

    Srgb<REC709&D65>
        [0.0..1.0, 0.0..1.0, 0.0..1.0],

    CIELab<CIELAB&E>
        [0.0..100.0, 0.0..100.0, 0.0..100.0],

    CIELch<CIELAB&E>
        [0.0..100.0, 0.0..133.0, 0.0..360.0],

    Hsv<REC709&D65>
        [0.0..360.0, 0.0..1.0, 0.0..1.0],

    Hsl<REC709&D65>
        [0.0..360.0, 0.0..1.0, 0.0..1.0],

    Ycbcr<REC709&D65>
        [0.0..1.0,   0.0..1.0, 0.0..1.0]
}

impl_conversion!(|color: Rgb| -> Srgb {
    let f = srgb_inv_companding;
    let ch = [f(color.0), f(color.1), f(color.2)];
    Color::new([ch[0], ch[1], ch[2], color.3])
});
impl_conversion!(|color: Srgb| -> Rgb {
    let f = srgb_companding;
    let ch = [f(color.0), f(color.1), f(color.2)];
    Color::new([ch[0], ch[1], ch[2], color.3])
});

fn ycbcr_mat(kr: f64, kg: f64, kb: f64) -> Mat3 {
    Mat3(
        Col3(kr, kg, kb),
        Col3(-0.5 * (kr / (1.0 - kb)), -0.5 * (kg / (1.0 - kb)), 0.5),
        Col3(0.5, -0.5 * (kg / (1.0 - kr)), -0.5 * (kb / (1.0 - kr))),
    )
}

fn ycbcr_mat_inv(kr: f64, kg: f64, kb: f64) -> Mat3 {
    Mat3(
        Col3(1.0, 0.0, 2.0 - 2.0 * kr),
        Col3(
            1.0,
            -(kb / kg) * (2.0 - 2.0 * kb),
            -(kr / kg) * (2.0 - 2.0 * kr),
        ),
        Col3(1.0, 2.0 - 2.0 * kb, 0.0),
    )
}
impl_conversion!(|color: Rgb| -> Ycbcr {
    let f = srgb_inv_companding;
    let col = ycbcr_mat(0.2126, 0.0722, 0.7152) * Col3(f(color.0), f(color.1), f(color.2));
    Color::new([col.0, col.1, col.2, color.3])
});
impl_conversion!(|color: Ycbcr| -> Rgb {
    let f = srgb_companding;
    let col = ycbcr_mat_inv(0.2126, 0.0722, 0.7152) * Col3(color.0, color.1, color.2);
    Color::new([f(col.0), f(col.1), f(col.2), color.3])
});
#[rustfmt::skip]
impl_conversion!(|color: Hsl| -> Rgb {
    let h = (color.0 * 360.0) % 360.0;

    let c = (1.0 - (2.0 * color.2).abs()) * color.1;
    let h_a = h / 60.0;
    let x = c * (1.0 - ((h_a % 2.0) - 1.0));

    // <https://en.wikipedia.org/wiki/HSL_and_HSV#HSL_to_RGB>
    let [r,g,b] = {
    if (0.0..1.0f64).contains(&h_a) {      [c, x, 0.0] }
    else if (1.0..2.0f64).contains(&h_a) { [x, c, 0.0] }
    else if (2.0..3.0f64).contains(&h_a) { [0.0, c, x] }
    else if (3.0..4.0f64).contains(&h_a) { [0.0, x, c] }
    else if (4.0..5.0f64).contains(&h_a) { [x, 0.0, c] }
    else if (5.0..6.0f64).contains(&h_a) { [c, 0.0, x] }
    else { [c, x, 0.0] } };

    let m = color.2 - (c * 0.5);

    Color::new([r + m, g + m, b + m, color.3])
});

impl_conversion!(|color: Rgb| -> Hsv {
    let v = color.0.max(color.1).max(color.2);
    let min = color.0.min(color.1).min(color.2);
    let c = v - min;
    // let l = v - (c / 2.0);
    let mut h = 0.0;
    let (r, g, b) = (color.0, color.1, color.2);
    if c != 0.0 {
        h = 60.0;
        if v == r {
            h *= 0.0 + ((g - b) / c);
        }
        if v == g {
            h *= 2.0 + ((b - r) / c);
        }
        if v == b {
            h *= 4.0 + ((r - g) / c);
        }
    }
    let s = if v != 0.0 { c / v } else { 0.0f64 };
    Color::new([h, s, v, color.3])
});

impl_conversion!(|color: Hsv| -> Rgb {
    let Color(_, _, _, _, _) = color;
    fn f(h: f64, s: f64, v: f64, n: f64) -> f64 {
        let k = (n + (h / 60.0)) % 6.0;
        v - (v * s * (0.0f64).max((k).min((4.0 - k).min(1.0))))
    }
    Color::new([
        f(color.0, color.1, color.2, 5.0),
        f(color.0, color.1, color.2, 3.0),
        f(color.0, color.1, color.2, 1.0),
        color.3,
    ])
});

impl_conversion!(|color: Rgb| -> Xyz {
    let f = REC709.transfer_fn_inv;
    let new_ch = REC709.conversion * Col3(f(color.0), f(color.1), f(color.2));
    Xyz::new([new_ch.0, new_ch.1, new_ch.2, color.3])
    // match WHITE {
    //     D65 => Xyz::new([new_ch.0, new_ch.1, new_ch.2, color.3]),
    //     _ => {
    //         let m = Mat3(
    //             Col3(0.8951, 0.2664, -0.1614),
    //             Col3(-0.7502, 1.7135, 0.0367),
    //             Col3(0.0389, -0.0685, 1.0296),
    //         );
    //         let mi = Mat3(
    //             Col3(0.9869929, -0.1470543, 0.1599627),
    //             Col3(0.4323053, 0.5183603, 0.0492912),
    //             Col3(-0.0085287, 0.0400428, 0.9684867),
    //         );
    //         let Col3(ps, ys, bs) = m * {
    //             let xyy = D65.tristimulus();
    //             let [x, y, z] = [
    //                 (xyy.0 * xyy.2) * xyy.1.recip(),
    //                 xyy.2,
    //                 ((1.0 - xyy.0 - xyy.1) * xyy.2) * xyy.1.recip(),
    //             ];
    //             Col3(x, y, z)
    //         };
    //         let Col3(pd, yd, bd) = m * {
    //             let xyy = WHITE.tristimulus();
    //             let [x, y, z] = [
    //                 (xyy.0 * xyy.2) * xyy.1.recip(),
    //                 xyy.2,
    //                 ((1.0 - xyy.0 - xyy.1) * xyy.2) * xyy.1.recip(),
    //             ];
    //             Col3(x, y, z)
    //         };
    //         let adapt_mat =
    //             (mi * Mat3(
    //                 Col3(pd / ps, 0.0, 0.0),
    //                 Col3(0.0, yd / ys, 0.0),
    //                 Col3(0.0, 0.0, bd / bs),
    //             )) * m;
    //         let Col3(x, y, z) = adapt_mat * new_ch;
    //         Xyz::new([x, y, z, color.3])
    //     }
    // }
});
impl_conversion!(|color: Xyz| -> Rgb {
    let f = REC709.transfer_fn;
    let new_ch = REC709.conversion.inverse() * Col3(color.0, color.1, color.2);
    Color::new([f(new_ch.0), f(new_ch.1), f(new_ch.2), color.3])
});
impl_conversion!(|color: Srgb| -> Xyz {
    let new_ch = REC709.conversion * Col3(color.0, color.1, color.2);
    Color::new([new_ch.0, new_ch.1, new_ch.2, color.3])
});
impl_conversion!(|color: Xyz| -> Srgb {
    let new_ch = REC709.conversion.inverse() * Col3(color.0, color.1, color.2);
    Color::new([new_ch.0, new_ch.1, new_ch.2, color.3])
});
impl_conversion!(|color: Xyz| -> Yxy {
    let s = color.0 + color.1 + color.2;
    Color::new([color.1, color.0 * s.recip(), color.1 * s.recip(), color.3])
});
impl_conversion!(|color: Yxy| -> Xyz {
    Color::new([
        (color.1 * color.0) * color.2.recip(),
        color.0,
        ((1.0 - color.1 - color.2) * color.0) * color.2.recip(),
        color.3,
    ])
});
impl_conversion!(|color: Yxy| -> Rgb { Rgb::from_color(Xyz::from_color(color)) });
impl_conversion!(|color: Rgb| -> Yxy { Yxy::from_color(Xyz::from_color(color)) });
impl_conversion!(|color: Xyz| -> CIELab {
    fn f(v: f64) -> f64 {
        if v > 0.008856 {
            v.cbrt()
        } else {
            (((24389.0 * 27.0f64.recip()) * v) + 16.0) * 116.0f64.recip()
        }
    }
    let (wx, wy, wz) = {
        let xyy = WHITE.tristimulus();
        (
            (xyy.0 * xyy.2) * xyy.1.recip(),
            xyy.2,
            ((1.0 - xyy.0 - xyy.1) * xyy.2) * xyy.1.recip(),
        )
    };

    let (fx, fy, fz) = (
        f(color.0 * wx.recip()),
        f(color.1 * wy.recip()),
        f(color.2 * wz.recip()),
    );

    Color::new([
        (116.0 * fy) - 16.0,
        500.0 * (fx - fy),
        200.0 * (fy - fz),
        color.3,
    ])
});
impl_conversion!(|color: CIELab| -> Xyz {
    fn f(v: f64) -> f64 {
        if v.powi(3) > 0.008856 {
            v.powi(3)
        } else {
            ((116.0 * v) - 16.0) * (27.0 * 24389.0f64.recip())
        }
    }

    let fy = (color.0 + 16.0) * 116.0f64.recip();
    let fz = fy - (color.2 * 0.005);
    let fx = (color.1 * 0.002) + fy;

    let y = if color.0 > 0.008856 * (24389.0 * 27.0f64.recip()) {
        ((color.0 + 16.0) * 116.0f64.recip()).powi(3)
    } else {
        color.0 * (27.0 * 24389.0f64.recip())
    };

    let (wx, wy, wz) = {
        let xyy = WHITE.tristimulus();
        (
            (xyy.0 * xyy.2) * xyy.1.recip(),
            xyy.2,
            ((1.0 - xyy.0 - xyy.1) * xyy.2) * xyy.1.recip(),
        )
    };

    Color::<Xyz, WHITE>::new([wx * f(fx), wy * (y), wz * f(fz), color.3])
});
impl_conversion!(|color: CIELab| -> Rgb { Rgb::from_color(Xyz::from_color(color)) });
impl_conversion!(|color: Rgb| -> CIELab { CIELab::from_color(Xyz::from_color(color)) });
impl_conversion!(|color: CIELab| -> CIELch {
    let h = color.2.atan2(color.1).to_degrees();
    Color::new([
        color.0,
        ((color.1 * color.1) + (color.2 * color.2)).sqrt(),
        if h >= 0.0 { h } else { h + 360.0 },
        color.3,
    ])
});
impl_conversion!(|color: CIELch| -> CIELab {
    Color::new([
        color.0,
        color.1 * color.2.to_radians().cos(),
        color.1 * color.2.to_radians().sin(),
        color.3,
    ])
});
impl_conversion!(|color: CIELch| -> Rgb {
    Rgb::from_color(Xyz::from_color(CIELab::from_color(color)))
});
impl_conversion!(|color: Rgb| -> CIELch {
    CIELch::from_color(CIELab::from_color(Xyz::from_color(color)))
});

/// linear (r,g,b) to non-linear (R,G,B)
fn srgb_companding(v: f64) -> f64 {
    if v <= 0.0031308 {
        12.92 * v
    } else {
        (1.055 * v.powf((2.4f64).recip())) - 0.055
    }
}

/// non-linear (R,G,B) to linear (r,g,b)
fn srgb_inv_companding(v: f64) -> f64 {
    if v <= 0.04045 {
        v * 12.92f64.recip()
    } else {
        ((v + 0.055) * 1.055f64.recip()).powf(2.4)
    }
}

/// linear (r,g,b) to non-linear (R,G,B)
#[allow(dead_code)]
fn l_companding(v: f64) -> f64 {
    if v <= 0.008856 {
        v * 90.33
    } else {
        (1.16 * v.cbrt()) - 0.16
    }
}

/// non-linear (R,G,B) to linear (r,g,b)
#[allow(dead_code)]
fn l_inv_companding(x: f64) -> f64 {
    if x <= 0.08 {
        0.110706 * x
    } else {
        ((x + 0.16) * (1.16f64).recip()).powi(3)
    }
}
