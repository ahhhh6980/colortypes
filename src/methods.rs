use super::types::{Col3, Color, ColorGamut, ColorType, FromColorType, Gamut, Mat3, RefWhite};

/*
TO-DO:
JzAzBz
https://observablehq.com/@jrus/jzazbz
OKLab
https://bottosson.github.io/posts/oklab/
*/
// impl FromColorType<Lab> for LCH(ab)

/// Implement the construction of a new color type
#[macro_export]
macro_rules! impl_colorspace {
    {$($struct:ident),+} => {
        $(
            #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
            pub struct $struct;
            impl ColorType for $struct {}
            impl $struct {
                #[allow(dead_code)]
                pub fn new(ch: [f64;4]) -> Color<$struct, {RefWhite::D65}> {
                    Color::<$struct, {RefWhite::D65}>::new(ch)
                }
                #[allow(dead_code)]
                pub fn new_w<const WHITE: RefWhite>(ch: [f64;4]) -> Color<$struct, WHITE> {
                    Color::<$struct, WHITE>::new(ch)
                }
            }
        )+
    };
}
pub(crate) use impl_colorspace;
/// Implement the conversion from one type to another
#[macro_export]
macro_rules! impl_conversion {
    ($from:ident, $to:ident, |$color_name:ident| $method:block) => {
        impl FromColorType<$from> for $to {
            fn from_color<const WHITE: RefWhite>(
                $color_name: Color<$from, WHITE>,
            ) -> Color<$to, WHITE> {
                Color::new($method)
            }
        }
    };
}
pub(crate) use impl_conversion;

impl_colorspace! {Rgba, Xyza, Xyya, Srgba, CIELaba, CIELcha, Hsva, Hsla, Ycbcr}

impl_conversion!(Rgba, Srgba, |color| {
    let f = srgb_inv_companding;
    let ch = [f(color.0), f(color.1), f(color.2)];
    [ch[0], ch[1], ch[2], color.3]
});
impl_conversion!(Srgba, Rgba, |color| {
    let f = srgb_companding;
    let ch = [f(color.0), f(color.1), f(color.2)];
    [ch[0], ch[1], ch[2], color.3]
});
// impl_conversion!(Ycbcr, Rgba, |color| {
//     let col = Mat3(
//         Col3(0.256789062, 0.504128906, 0.09790625),
//         Col3(-0.148222656, -0.290992187, 0.439214844),
//         Col3(0.439214844, -0.367789063, -0.071425781),
//     )
//     .inverse()
//         * Col3(color.0 - 0.0625, color.1 + 0.5, color.2 - 0.5);
//     [col.0, col.1, col.2, color.3]
// });
// impl_conversion!(Rgba, Ycbcr, |color| {
//     let col = Mat3(
//         Col3(0.256789062, 0.504128906, 0.09790625),
//         Col3(-0.148222656, -0.290992187, 0.439214844),
//         Col3(0.439214844, -0.367789063, -0.071425781),
//     ) * Col3(color.0, color.1, color.2);
//     [0.0625 + col.0, 0.5 - col.1, 0.5 + col.2, color.3]
// });
#[rustfmt::skip]
impl_conversion!(Hsla, Rgba, |color| {
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

    [r + m, g + m, b + m, color.0]
});

impl_conversion!(Rgba, Hsva, |color| {
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
    [h / 360.0, s, v, color.3]
});

impl_conversion!(Hsva, Rgba, |color| {
    let Color(_, _, _, _, _) = color;
    fn f(h: f64, s: f64, v: f64, n: f64) -> f64 {
        let k = (n + (h / 60.0)) % 6.0;
        v - (v * s * (0.0f64).max((k).min((4.0 - k).min(1.0))))
    }
    [
        f(color.0 * 360.0, color.1, color.2, 5.0),
        f(color.0 * 360.0, color.1, color.2, 3.0),
        f(color.0 * 360.0, color.1, color.2, 1.0),
        color.3,
    ]
});

impl_conversion!(Rgba, Xyza, |color| {
    let f = srgb_inv_companding;
    let new_ch = system_matrix(WHITE, SRGB) * Col3(f(color.0), f(color.1), f(color.2));
    [new_ch.0, new_ch.1, new_ch.2, color.3]
});
impl_conversion!(Srgba, Xyza, |color| {
    let new_ch = system_matrix(WHITE, SRGB) * Col3(color.0, color.1, color.2);
    [new_ch.0, new_ch.1, new_ch.2, color.3]
});
impl_conversion!(Xyza, Srgba, |color| {
    let new_ch = system_matrix(WHITE, SRGB).inverse() * Col3(color.0, color.1, color.2);
    [new_ch.0, new_ch.1, new_ch.2, color.3]
});
impl_conversion!(Xyza, Rgba, |color| {
    let f = srgb_companding;
    let new_ch = system_matrix(WHITE, SRGB).inverse() * Col3(color.0, color.1, color.2);
    [f(new_ch.0), f(new_ch.1), f(new_ch.2), color.3]
});
impl_conversion!(Xyza, Xyya, |color| {
    let s = color.0 + color.1 + color.2;
    [color.0 * s.recip(), color.1 * s.recip(), color.2, color.3]
});
impl_conversion!(Xyya, Xyza, |color| {
    [
        (color.0 * color.2) * color.1.recip(),
        color.2,
        ((1.0 - color.0 - color.1) * color.2) * color.1.recip(),
        color.3,
    ]
});
impl_conversion!(Xyya, Rgba, |color| {
    Xyya::from_color(Xyza::from_color(color)).to_arr().0
});
impl_conversion!(Rgba, Xyya, |color| {
    Rgba::from_color(Xyza::from_color(color)).to_arr().0
});
impl_conversion!(Xyza, CIELaba, |color| {
    fn f(v: f64) -> f64 {
        if v > 0.008856 {
            v.cbrt()
        } else {
            (((24389.0 * 27.0f64.recip()) * v) + 16.0) * 116.0f64.recip()
        }
    }
    let Col3(wx, wy, wz) = white_tristim(WHITE);
    let (fx, fy, fz) = (
        f(color.0 * wx.recip()),
        f(color.1 * wy.recip()),
        f(color.2 * wz.recip()),
    );
    [
        (116.0 * fy) - 16.0,
        500.0 * (fx - fy),
        200.0 * (fy - fz),
        color.3,
    ]
});
impl_conversion!(CIELaba, Xyza, |color| {
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
    let Col3(wx, wy, wz) = white_tristim(WHITE);
    [wx * f(fx), wy * (y), wz * f(fz), color.3]
});
impl_conversion!(CIELaba, Rgba, |color| {
    Rgba::from_color(Xyza::from_color(color)).to_arr().0
});
impl_conversion!(Rgba, CIELaba, |color| {
    CIELaba::from_color(Xyza::from_color(color)).to_arr().0
});
impl_conversion!(CIELaba, CIELcha, |color| {
    let h = color.2.atan2(color.1).to_degrees();
    [
        color.0,
        ((color.1 * color.1) + (color.2 * color.2)).sqrt(),
        if h >= 0.0 { h } else { h + 360.0 },
        color.3,
    ]
});
impl_conversion!(CIELcha, CIELaba, |color| {
    [
        color.0,
        color.1 * color.2.to_radians().cos(),
        color.1 * color.2.to_radians().sin(),
        color.3,
    ]
});
impl_conversion!(CIELcha, Rgba, |color| {
    Rgba::from_color(Xyza::from_color(CIELaba::from_color(color)))
        .to_arr()
        .0
});
impl_conversion!(Rgba, CIELcha, |color| {
    CIELcha::from_color(CIELaba::from_color(Xyza::from_color(color)))
        .to_arr()
        .0
});

/// The CIE color space
#[allow(dead_code)]
pub static CIE_RGB: ColorGamut<{ RefWhite::E }> = ColorGamut {
    gamma: 2.2,
    primaries_xyy: [
        Col3(0.7350, 0.2740, 0.1670),
        Col3(0.2650, 0.7170, 0.0090),
        Col3(0.176204, 0.812985, 0.010811),
    ],
    transfer_inv: srgb_inv_companding,
    transfer: srgb_companding,
    conversion: Mat3(
        Col3(0.4887180, 0.3106803, 0.2006017),
        Col3(0.1762044, 0.8129847, 0.0108109),
        Col3(0.0000000, 0.0102048, 0.9897952),
    ),
};

/// The Wide Gamut RGB color space
#[allow(dead_code)]
pub static WIDE_GAMUT_RGB: ColorGamut<{ RefWhite::D50 }> = ColorGamut {
    gamma: 2.2,
    primaries_xyy: [
        Col3(0.735, 0.115, 0.157),
        Col3(0.265, 0.826, 0.018),
        Col3(0.258187, 0.724938, 0.016875),
    ],
    transfer_inv: srgb_inv_companding,
    transfer: srgb_companding,
    conversion: Mat3(
        Col3(0.7161046, 0.1009296, 0.1471858),
        Col3(0.2581874, 0.7249378, 0.0168748),
        Col3(0.0000000, 0.0517813, 0.7734287),
    ),
};

/// The SRGB color space
#[allow(dead_code)]
pub static SRGB: ColorGamut<{ RefWhite::D65 }> = ColorGamut {
    gamma: 2.2,
    primaries_xyy: [
        Col3(0.64, 0.3, 0.15),
        Col3(0.33, 0.6, 0.06),
        Col3(0.212656, 0.715158, 0.072186),
    ],
    transfer_inv: srgb_inv_companding,
    transfer: srgb_companding,
    conversion: Mat3(
        Col3(0.4124564, 0.3575761, 0.1804375),
        Col3(0.2126729, 0.7151522, 0.0721750),
        Col3(0.0193339, 0.1191920, 0.9503041),
    ),
};

/// The ADOBE RGB color space
#[allow(dead_code)]
pub static ADOBE_RGB: ColorGamut<{ RefWhite::D65 }> = ColorGamut {
    gamma: 2.2,
    primaries_xyy: [
        Col3(0.64, 0.21, 0.15),
        Col3(0.33, 0.71, 0.06),
        Col3(0.297361, 0.627355, 0.075285),
    ],
    transfer_inv: |x| x.powf(2.2),
    transfer: |x| x.powf((2.2f64).recip()),
    conversion: Mat3(
        Col3(0.5767309, 0.1855540, 0.1881852),
        Col3(0.2973769, 0.6273491, 0.075274),
        Col3(0.0270343, 0.0706872, 0.9911085),
    ),
};

// /// The rec2020 color space
// #[allow(dead_code)]
// pub static REC709: ColorGamut<{ RefWhite::D65 }> = ColorGamut {
//     gamma: 2.2,
//     primaries_xyy: [
//         Col3(0.64, 0.3, 0.15),
//         Col3(0.33, 0.6, 0.06),
//         Col3(1.0, 1.0, 1.0),
//     ],
//     transfer_inv: |x| x.powf(2.2),
//     transfer: |x| x.powf(1.0 * 2.2f64.recip()),
// };

/// Return the reference whitepoint
impl<const WHITE: RefWhite> Gamut for ColorGamut<WHITE> {
    fn white(&self) -> RefWhite {
        WHITE
    }
}

/// Specifications grabbed from here:
/// <http://www.russellcottrell.com/photo/matrixCalculator.htm>
pub fn white_tristim(t: RefWhite) -> Col3 {
    match t {
        RefWhite::D65 => Col3(0.95047, 1.0, 1.08883),
        RefWhite::D50 => Col3(0.96422, 1.0, 0.82521),
        RefWhite::Custom(t) => {
            let white = t_to_xy(t as f64);
            Col3(
                white[0] * white[1].recip(),
                1.0,
                (1.0 - white[0] - white[1]) * white[1].recip(),
            )
        }
        _ => Col3(0.95047, 1.0, 1.08883),
    }
}

/// linear (r,g,b) to non-linear (R,G,B)
pub fn srgb_companding(v: f64) -> f64 {
    if v <= 0.0031308 {
        12.92 * v
    } else {
        (1.055 * v.powf((2.4f64).recip())) - 0.055
    }
}

/// non-linear (R,G,B) to linear (r,g,b)
pub fn srgb_inv_companding(v: f64) -> f64 {
    if v <= 0.04045 {
        v * 12.92f64.recip()
    } else {
        ((v + 0.055) * 1.055f64.recip()).powf(2.4)
    }
}

/// linear (r,g,b) to non-linear (R,G,B)
#[allow(dead_code)]
pub fn l_companding(v: f64) -> f64 {
    if v <= 0.008856 {
        v * 90.33
    } else {
        (1.16 * v.cbrt()) - 0.16
    }
}

/// non-linear (R,G,B) to linear (r,g,b)
#[allow(dead_code)]
pub fn l_inv_companding(x: f64) -> f64 {
    if x <= 0.08 {
        0.110706 * x
    } else {
        ((x + 0.16) * (1.16f64).recip()).powi(3)
    }
}

/// Convert a color temperature into xy chromaticity coordinates
/// <http://www.brucelindbloom.com/index.html>
pub fn t_to_xy(t: f64) -> [f64; 2] {
    let x = if (4000f64..=7000f64).contains(&t) {
        (-4.6070 * (10f64).powi(9)).mul_add(
            t.powi(3).recip(),
            (2.9678 * (10f64).powi(6)).mul_add(
                (t * t).recip(),
                (0.09911 * (10f64).powi(3)).mul_add(t.recip(), 0.244063),
            ),
        )
    } else {
        (-2.0064 * (10f64).powi(9)).mul_add(
            t.powi(3).recip(),
            (1.9018 * (10f64).powi(6)).mul_add(
                (t * t).recip(),
                (0.24748 * (10f64).powi(3)).mul_add(t.recip(), 0.237040),
            ),
        )
    };
    let y = (-3.0f64).mul_add(x * x, (2.87f64).mul_add(x, -0.275));
    [x, y]
}

// #[allow(dead_code)]
// pub enum ChromaMatrix {
//     Xyz,
//     Bradford,
//     VonKries,
// }
// <http://www.brucelindbloom.com/index.html?Eqn_ChromAdapt.html>
// #[allow(dead_code, unused_variables)]
// pub fn adapt_chroma(mat: Mat3, response: ChromaMatrix, from: RefWhite, to: RefWhite) -> Mat3 {
//     let m = match response {
//         _ => Mat3::identity(),
//     };
//     mat
// }

/// Compute the sRGB matrix for RGB to XYZ conversion
/// Y is the brightness of the whitepoint
///
/// sources:
/// <https://mina86.com/2019/srgb-xyz-matrix/>
/// <http://www.brucelindbloom.com/index.html?WorkingSpaceInfo.html#Specifications>
pub fn system_matrix<const WHITE: RefWhite>(t: RefWhite, system: ColorGamut<WHITE>) -> Mat3 {
    // Color specifications tristimulus]
    let ColorGamut {
        gamma: _,
        primaries_xyy: s,
        transfer: _,
        transfer_inv: _,
        conversion: mat,
    } = system;
    match t {
        RefWhite::Custom(t) => {
            // Reference whitepoint tristimulus
            let ref_white = {
                let white = t_to_xy(t as f64);
                Col3(
                    white[0] * white[1].recip(),
                    1.0,
                    (1.0 - white[0] - white[1]) * white[1].recip(),
                )
            };
            let m = Mat3(
                s[0].div(s[1]),
                Col3(1.0, 1.0, 1.0),
                ((s[0]).over(|x| 1.0 - x) - s[1]).div(s[1]),
            );
            // Our computed RGB -> XYZ matrix :D
            m.over_columns(m.inverse() * ref_white, |a, b| a.mult(b))
        }
        _ => mat,
    }
}
