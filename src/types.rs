use std::{fmt, marker::PhantomData};

use crate::{colors::CIELaba, CIELcha, Xyza, SRGB};

pub trait ColorType: Copy {}

/// Method for safely converting between ColorType structs
pub trait FromColorType<SPACE>: ColorType
where
    SPACE: ColorType + Clone + Copy,
{
    fn from_color<const GAMUT: ColorGamut>(_: Color<SPACE, GAMUT>) -> Color<Self, GAMUT>
    where
        Self: std::marker::Sized;
}

/// An abstract color type that has operator overloading
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub struct Color<SPACE: ColorType + Clone + Copy, const GAMUT: ColorGamut>(
    pub f64,
    pub f64,
    pub f64,
    pub f64,
    pub std::marker::PhantomData<SPACE>,
);

macro_rules! impl_float_ops_color {
    (
        ($($name_1_arg:ident),*),($($name_2_arg:ident),*),($($name_3_arg:ident),*)
    ) => {
        paste::item! {
        $(
        pub fn [<$name_1_arg:snake>] (&self) -> Color<SPACE, GAMUT> {
             apply_ops_color!(($name_1_arg),(self))
        }
        )*
        $(
        pub fn [<$name_2_arg:snake _color>] (&self, rhs: Color<SPACE, GAMUT>) -> Color<SPACE, GAMUT> {
            apply_ops_color!(($name_2_arg),(self, rhs))
        }
        pub fn [<$name_2_arg:snake>] (&self, rhs: f64) -> Color<SPACE, GAMUT> {
            Color::<SPACE, GAMUT>::new(
                [
                    f64::[<$name_2_arg>](self.0, rhs),
                    f64::[<$name_2_arg>](self.1, rhs),
                    f64::[<$name_2_arg>](self.2, rhs),
                    f64::[<$name_2_arg>](self.3, rhs),
                ]
            )
        }
        )*
        $(
        pub fn [<$name_3_arg:snake _color>] (&self, rhs: Color<SPACE, GAMUT>, rhs2: Color<SPACE,GAMUT>) -> Color<SPACE, GAMUT> {
            apply_ops_color!(($name_3_arg),(self, rhs, rhs2))
        }
        )*
    }
    };
}
macro_rules! apply_ops_color {
    (($name_arg:ident),($($name:ident),*) ) => {

        paste::item! {
            Color::<SPACE, GAMUT>::new(
                [
                    f64::[<$name_arg>]($($name.0,)*),
                    f64::[<$name_arg>]($($name.1,)*),
                    f64::[<$name_arg>]($($name.2,)*),
                    f64::[<$name_arg>]($($name.3,)*),
                ]
            )
        }

    };
}

impl<SPACE: ColorType + Clone + Copy, const GAMUT: ColorGamut> fmt::Display
    for Color<SPACE, GAMUT>
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "[{:.4}, {:.4}, {:.4}, {:.4}]",
            self.0, self.1, self.2, self.3
        )
    }
}

impl<SPACE: ColorType + Clone + Copy, const GAMUT: ColorGamut> Default for Color<SPACE, GAMUT> {
    fn default() -> Color<SPACE, GAMUT> {
        Color::new([0.0, 0.0, 0.0, 1.0])
    }
}

#[allow(dead_code)]
impl<SPACE: ColorType + Clone + Copy, const GAMUT: ColorGamut> Color<SPACE, GAMUT> {
    /// Return the reference white point
    #[inline]
    pub fn white(&self) -> White {
        GAMUT.white()
    }

    /// Return the tristimulus values of the white point
    #[inline]
    pub fn white_tristimulus(&self) -> Col3 {
        GAMUT.white().tristimulus()
    }

    /// Return the gamut const
    #[inline]
    pub fn gamut(&self) -> ColorGamut {
        GAMUT
    }

    /// Construct a new color
    pub fn new(ch: [f64; 4]) -> Color<SPACE, GAMUT> {
        Color::<SPACE, GAMUT>(ch[0], ch[1], ch[2], ch[3], PhantomData)
    }

    /// Color to (\[f64;4\], RefWhite, ColorSpace)
    pub fn to_arr(self) -> ([f64; 4], White) {
        ([self.0, self.1, self.2, self.3], self.white())
    }

    /// Color to (\[u8;4\], RefWhite, ColorSpace)
    pub fn to_arr8(self) -> ([u8; 4], White) {
        (
            [
                (self.0 * u8::MAX as f64).round() as u8,
                (self.1 * u8::MAX as f64).round() as u8,
                (self.2 * u8::MAX as f64).round() as u8,
                (self.3 * u8::MAX as f64).round() as u8,
            ],
            self.white(),
        )
    }

    /// Color to (\[u16;4\], RefWhite, ColorSpace)
    pub fn to_arr16(self) -> ([u16; 4], White) {
        (
            [
                (self.0 * u16::MAX as f64).round() as u16,
                (self.1 * u16::MAX as f64).round() as u16,
                (self.2 * u16::MAX as f64).round() as u16,
                (self.3 * u16::MAX as f64).round() as u16,
            ],
            self.white(),
        )
    }

    /// Adapt to a new white point
    ///
    /// not well tested
    ///
    /// <http://www.brucelindbloom.com/index.html?Eqn_ChromAdapt.html>
    #[allow(dead_code, unused_variables)]
    pub fn adapt_chroma(self, mat: Mat3, to: White) -> Color<SPACE, GAMUT>
    where
        Xyza: FromColorType<SPACE>,
        SPACE: FromColorType<Xyza>,
    {
        let xyz_color = Xyza::from_color(self);
        let m = Mat3(
            Col3(0.8951, 0.2664, -0.1614),
            Col3(-0.7502, 1.7135, 0.0367),
            Col3(0.0389, -0.0685, 1.0296),
        );
        let mi = Mat3(
            Col3(0.9869929, -0.1470543, 0.1599627),
            Col3(0.4323053, 0.5183603, 0.0492912),
            Col3(-0.0085287, 0.0400428, 0.9684867),
        );
        let xyy_white = SRGB.white().tristimulus().to_arr();
        let w_s = [
            (xyy_white[0] * xyy_white[2]) * xyy_white[1].recip(),
            xyy_white[2],
            ((1.0 - xyy_white[0] - xyy_white[1]) * xyy_white[2]) * xyy_white[1].recip(),
        ];
        let xyy_white = to.tristimulus().to_arr();
        let w_d = [
            (xyy_white[0] * xyy_white[2]) * xyy_white[1].recip(),
            xyy_white[2],
            ((1.0 - xyy_white[0] - xyy_white[1]) * xyy_white[2]) * xyy_white[1].recip(),
        ];
        let Col3(ps, ys, bs) = m * Col3(w_s[0], w_s[1], w_s[2]);
        let Col3(pd, yd, bd) = m * Col3(w_d[0], w_d[1], w_d[2]);
        let adapt_mat =
            (mi * Mat3(
                Col3(pd / ps, 0.0, 0.0),
                Col3(0.0, yd / ys, 0.0),
                Col3(0.0, 0.0, bd / bs),
            )) * m;
        let Col3(x, y, z) = adapt_mat * Col3(xyz_color.0, xyz_color.1, xyz_color.2);
        SPACE::from_color(Xyza::new::<{ GAMUT }>([x, y, z, self.3]))
    }

    // Color difference for acceptability
    pub fn delta_e_a(self, color: Color<SPACE, GAMUT>) -> f64
    where
        CIELaba: FromColorType<SPACE>,
    {
        self.delta_e(color, (2.0, 1.0))
    }

    // Color difference for perceptibility
    pub fn delta_e_p(self, color: Color<SPACE, GAMUT>) -> f64
    where
        CIELaba: FromColorType<SPACE>,
    {
        self.delta_e(color, (1.0, 1.0))
    }

    /// Color difference
    pub fn delta_e(self, color: Color<SPACE, GAMUT>, (l, c): (f64, f64)) -> f64
    where
        CIELaba: FromColorType<SPACE>,
    {
        // let a = // let a = CIELaba::from_color(self);
        let a = CIELaba::from_color(self);
        let b = CIELaba::from_color(color);

        let (c1, c2) = (
            (a.1 * a.1 + a.2 * a.2).sqrt(),
            (b.1 * b.1 + b.2 * b.2).sqrt(),
        );

        let delta_c = c1 - c2;

        let delta = a - b;

        #[rustfmt::skip]
        let delta_h = delta.1.mul_add(
            delta.1, delta.2.mul_add(
                delta.2, delta_c * delta_c))
            .sqrt();

        let s_l = if a.0 < 16.0 {
            0.511
        } else {
            (0.040975 * a.0) / (1.0 + (0.01765 * a.0))
        };

        let s_c = (0.0638 * c1) / (1.0 + (0.0131 * c1));

        let h = a.2.atan2(a.1);
        let h1 = if h >= 0.0 {
            h
        } else {
            h + 360.0f64.to_radians()
        };

        let f = (c1.powi(4) / (c1.powi(4) + 1900.0)).sqrt();

        let t = if (164f64.to_radians()..345f64.to_radians()).contains(&h1) {
            0.56 + (0.2 * (h1 + 168f64.to_radians()).cos()).abs()
        } else {
            0.36 + (0.4 * (h1 + 35f64.to_radians()).cos()).abs()
        };

        let s_h = s_c * (f * t + 1.0 - f);

        ((delta.0 / (l * s_l)).powi(2) + (delta.1 / (c * s_c)).powi(2) + (delta_h / s_h).powi(2))
            .sqrt()
    }

    /// Checks if two colors are within good precision of each other
    ///
    /// This is NOT a distance function, do not use this to compare colors
    pub fn within_u16_sqrd_precision_of(&self, rhs: Color<SPACE, GAMUT>) -> [bool; 4] {
        [
            (self.0 - rhs.0).abs() < (u16::MAX as f64).powi(2).recip(),
            (self.1 - rhs.1).abs() < (u16::MAX as f64).powi(2).recip(),
            (self.2 - rhs.2).abs() < (u16::MAX as f64).powi(2).recip(),
            (self.3 - rhs.3).abs() < (u16::MAX as f64).powi(2).recip(),
        ]
    }

    impl_float_ops_color!(
        (
            sqrt, cbrt, cos, cosh, acos, acosh, sin, sinh, asin, asinh, tan, tanh, atan, atanh, ln,
            abs, ceil, exp, exp2, exp_m1, floor, ln_1p, log2, log10, round, recip, signum
        ),
        (powf, atan2, log, max, min, hypot),
        (mul_add)
    );
}

impl<SPACE: ColorType + Clone + Copy, const GAMUT: ColorGamut> std::ops::Add<Color<SPACE, GAMUT>>
    for Color<SPACE, GAMUT>
{
    type Output = Color<SPACE, GAMUT>;
    fn add(self, rhs: Color<SPACE, GAMUT>) -> Color<SPACE, GAMUT> {
        Self::new([
            self.0 + rhs.0,
            self.1 + rhs.1,
            self.2 + rhs.2,
            self.3 + rhs.3,
        ])
    }
}

impl<SPACE: ColorType + Clone + Copy, const GAMUT: ColorGamut>
    std::ops::AddAssign<Color<SPACE, GAMUT>> for Color<SPACE, GAMUT>
{
    fn add_assign(&mut self, rhs: Color<SPACE, GAMUT>) {
        self.0 += rhs.0;
        self.1 += rhs.1;
        self.2 += rhs.2;
        self.3 += rhs.3;
    }
}

impl<SPACE: ColorType + Clone + Copy, const GAMUT: ColorGamut> std::ops::Sub<Color<SPACE, GAMUT>>
    for Color<SPACE, GAMUT>
{
    type Output = Color<SPACE, GAMUT>;
    fn sub(self, rhs: Color<SPACE, GAMUT>) -> Color<SPACE, GAMUT> {
        Self::new([
            self.0 - rhs.0,
            self.1 - rhs.1,
            self.2 - rhs.2,
            self.3 - rhs.3,
        ])
    }
}

impl<SPACE: ColorType + Clone + Copy, const GAMUT: ColorGamut>
    std::ops::SubAssign<Color<SPACE, GAMUT>> for Color<SPACE, GAMUT>
{
    fn sub_assign(&mut self, rhs: Color<SPACE, GAMUT>) {
        self.0 -= rhs.0;
        self.1 -= rhs.1;
        self.2 -= rhs.2;
        self.3 -= rhs.3;
    }
}

impl<SPACE: ColorType + Clone + Copy, const GAMUT: ColorGamut> std::ops::Mul<Color<SPACE, GAMUT>>
    for Color<SPACE, GAMUT>
{
    type Output = Color<SPACE, GAMUT>;
    fn mul(self, rhs: Color<SPACE, GAMUT>) -> Color<SPACE, GAMUT> {
        Self::new([
            self.0 * rhs.0,
            self.1 * rhs.1,
            self.2 * rhs.2,
            self.3 * rhs.3,
        ])
    }
}

impl<SPACE: ColorType + Clone + Copy, const GAMUT: ColorGamut>
    std::ops::MulAssign<Color<SPACE, GAMUT>> for Color<SPACE, GAMUT>
{
    fn mul_assign(&mut self, rhs: Color<SPACE, GAMUT>) {
        self.0 *= rhs.0;
        self.1 *= rhs.1;
        self.2 *= rhs.2;
        self.3 *= rhs.3;
    }
}

impl<SPACE: ColorType + Clone + Copy, const GAMUT: ColorGamut> std::ops::Add<f64>
    for Color<SPACE, GAMUT>
{
    type Output = Color<SPACE, GAMUT>;
    fn add(self, rhs: f64) -> Color<SPACE, GAMUT> {
        Self::new([self.0 + rhs, self.1 + rhs, self.2 + rhs, self.3 + rhs])
    }
}

impl<SPACE: ColorType + Clone + Copy, const GAMUT: ColorGamut> std::ops::Sub<f64>
    for Color<SPACE, GAMUT>
{
    type Output = Color<SPACE, GAMUT>;
    fn sub(self, rhs: f64) -> Color<SPACE, GAMUT> {
        Self::new([self.0 - rhs, self.1 - rhs, self.2 - rhs, self.3 - rhs])
    }
}

impl<SPACE: ColorType + Clone + Copy, const GAMUT: ColorGamut> std::ops::Div<Color<SPACE, GAMUT>>
    for Color<SPACE, GAMUT>
{
    type Output = Color<SPACE, GAMUT>;
    fn div(self, rhs: Color<SPACE, GAMUT>) -> Color<SPACE, GAMUT> {
        Self::new([
            self.0 / rhs.0,
            self.1 / rhs.1,
            self.2 / rhs.2,
            self.3 / rhs.3,
        ])
    }
}

impl<SPACE: ColorType + Clone + Copy, const GAMUT: ColorGamut>
    std::ops::DivAssign<Color<SPACE, GAMUT>> for Color<SPACE, GAMUT>
{
    fn div_assign(&mut self, rhs: Color<SPACE, GAMUT>) {
        self.0 /= rhs.0;
        self.1 /= rhs.1;
        self.2 /= rhs.2;
        self.3 /= rhs.3;
    }
}

impl<SPACE: ColorType + Clone + Copy, const GAMUT: ColorGamut> std::ops::Mul<f64>
    for Color<SPACE, GAMUT>
{
    type Output = Color<SPACE, GAMUT>;
    fn mul(self, rhs: f64) -> Color<SPACE, GAMUT> {
        Self::new([self.0 * rhs, self.1 * rhs, self.2 * rhs, self.3 * rhs])
    }
}

impl<SPACE: ColorType + Clone + Copy, const GAMUT: ColorGamut> std::ops::MulAssign<f64>
    for Color<SPACE, GAMUT>
{
    // type Output = Color<SPACE, GAMUT>;
    fn mul_assign(&mut self, rhs: f64) {
        self.0 *= rhs;
        self.1 *= rhs;
        self.2 *= rhs;
        self.3 *= rhs;
    }
}

impl<SPACE: ColorType + Clone + Copy, const GAMUT: ColorGamut> std::ops::Mul<Color<SPACE, GAMUT>>
    for f64
{
    type Output = Color<SPACE, GAMUT>;
    fn mul(self, rhs: Color<SPACE, GAMUT>) -> Color<SPACE, GAMUT> {
        Color::new([self * rhs.0, self * rhs.1, self * rhs.2, self * rhs.3])
    }
}

impl<SPACE: ColorType + Clone + Copy, const GAMUT: ColorGamut> std::ops::Div<f64>
    for Color<SPACE, GAMUT>
{
    type Output = Color<SPACE, GAMUT>;
    fn div(self, rhs: f64) -> Color<SPACE, GAMUT> {
        Self::new([self.0 / rhs, self.1 / rhs, self.2 / rhs, self.3 / rhs])
    }
}

impl<SPACE: ColorType + Clone + Copy, const GAMUT: ColorGamut> std::ops::DivAssign<f64>
    for Color<SPACE, GAMUT>
{
    // type Output = Color<SPACE, GAMUT>;
    fn div_assign(&mut self, rhs: f64) {
        self.0 /= rhs;
        self.1 /= rhs;
        self.2 /= rhs;
        self.3 /= rhs;
    }
}

impl<SPACE: ColorType + Clone + Copy, const GAMUT: ColorGamut> std::ops::Div<Color<SPACE, GAMUT>>
    for f64
{
    type Output = Color<SPACE, GAMUT>;
    fn div(self, rhs: Color<SPACE, GAMUT>) -> Color<SPACE, GAMUT> {
        Color::new([self / rhs.0, self / rhs.1, self / rhs.2, self / rhs.3])
    }
}

///

#[allow(dead_code)]
/// The reference white point of the color. Default: D65

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Hash)]
pub struct White {
    tristimulus: fn() -> Col3,
}

impl White {
    /// Return the tristimulus (xyY) value
    #[inline]
    pub fn tristimulus(&self) -> Col3 {
        (self.tristimulus)()
    }
}

/// Standard White Point D65
#[allow(unused)]
pub const D65: White = White {
    tristimulus: || Col3(0.95047, 1.0, 1.08883),
};

/// Standard White Point D50
#[allow(unused)]
pub const D50: White = White {
    tristimulus: || Col3(0.96422, 1.0, 0.82521),
};

/// Standard White Point E
#[allow(unused)]
pub const E: White = White {
    tristimulus: || {
        let white = t_to_xy(5455.0);
        Col3(
            white[0] * white[1].recip(),
            1.0,
            (1.0 - white[0] - white[1]) * white[1].recip(),
        )
    },
};

/// Convert a color temperature into xy chromaticity coordinates
/// <http://www.brucelindbloom.com/index.html>
fn t_to_xy(t: f64) -> [f64; 2] {
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

/// ColorGamut holds information needed for color processing
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd)]
pub struct ColorGamut {
    pub primaries_xyy: fn() -> [Col3; 3],
    pub transfer_fn: fn(v: f64) -> f64,
    pub transfer_fn_inv: fn(v: f64) -> f64,
    pub conversion: fn() -> Mat3,
    pub white: fn() -> White,
}

impl ColorGamut {
    /// Return the primaries of the gamut in xyy
    #[inline]
    pub fn primaries_xyy(&self) -> [Col3; 3] {
        (self.primaries_xyy)()
    }

    /// Return the transfer function from linear to gamma
    #[inline]
    pub fn transfer_fn(&self, v: f64) -> f64 {
        (self.transfer_fn)(v)
    }

    /// Return the transfer function from gamma to linear
    #[inline]
    pub fn transfer_fn_inv(&self, v: f64) -> f64 {
        (self.transfer_fn_inv)(v)
    }

    /// Return the white point object
    #[inline]
    pub fn white(&self) -> White {
        (self.white)()
    }

    /// Return the conversion matrix
    pub fn conversion_matrix(&self) -> Mat3 {
        (self.conversion)()
    }

    /// Compute the sRGB matrix for RGB to XYZ conversion
    /// Y is the brightness of the whitepoint
    ///
    /// sources:
    /// <https://mina86.com/2019/srgb-xyz-matrix/>
    /// <http://www.brucelindbloom.com/index.html?WorkingSpaceInfo.html#Specifications>
    pub fn custom_system_matrix(&self, t: f64) -> Mat3 {
        let s = self.primaries_xyy();

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
}

/// 3x3 matrix with op overloading
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub struct Mat3(pub Col3, pub Col3, pub Col3);
/// 3x1 column vector with op overloading
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub struct Col3(pub f64, pub f64, pub f64);

#[rustfmt::skip]
impl fmt::Display for Mat3 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}\n{}\n{}\n{}\n{}",
            String::from("┌                             ┐"),
            self.0, self.1, self.2,
            String::from("└                             ┘")
        )
    }
}

impl fmt::Display for Col3 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "│{: >+9.4} {: ^+9.4} {: <+9.4}│", self.0, self.1, self.2)
    }
}

impl Col3 {
    /// Create a new Col3
    pub fn new(c: [f64; 3]) -> Self {
        Col3(c[0], c[1], c[2])
    }
    /// Multiply a Col3 element-wise with another
    pub fn mult(&self, rhs: Col3) -> Col3 {
        Col3(self.0 * rhs.0, self.1 * rhs.1, self.2 * rhs.2)
    }
    /// Divide a Col3 element-wise with another
    pub fn div(&self, rhs: Col3) -> Col3 {
        Col3(
            self.0 * rhs.0.recip(),
            self.1 * rhs.1.recip(),
            self.2 * rhs.2.recip(),
        )
    }
    /// Apply an operation over the elements
    pub fn over(&self, f: fn(x: f64) -> f64) -> Col3 {
        Col3(f(self.0), f(self.1), f(self.2))
    }
    /// Return an array
    pub fn to_arr(self) -> [f64; 3] {
        [self.0, self.1, self.2]
    }
}

#[allow(dead_code)]
impl Mat3 {
    /// Construct a new Mat3
    pub fn new(m: [[f64; 3]; 3]) -> Self {
        Mat3(
            Col3(m[0][0], m[0][1], m[0][2]),
            Col3(m[1][0], m[1][1], m[1][2]),
            Col3(m[2][0], m[2][1], m[2][2]),
        )
    }
    /// COnvert the mat3 into an array
    pub fn to_arr(self) -> [[f64; 3]; 3] {
        [
            [self.0 .0, self.0 .1, self.0 .2],
            [self.1 .0, self.1 .1, self.1 .2],
            [self.2 .0, self.2 .1, self.2 .2],
        ]
    }
    /// Return matrix of 0's
    pub fn zeros() -> Mat3 {
        Mat3(Col3(0., 0., 0.), Col3(0., 0., 0.), Col3(0., 0., 0.))
    }
    /// Returns the identity matrix
    pub fn identity() -> Mat3 {
        Mat3(Col3(1., 0., 0.), Col3(0., 1., 0.), Col3(0., 0., 1.))
    }
    /// Grab a row
    pub fn row(&self, n: usize) -> &Col3 {
        [&self.0, &self.1, &self.2][n % 3]
    }
    /// Assign a row
    pub fn row_assign(&mut self, n: usize, new_row: Col3) {
        *[&mut self.0, &mut self.1, &mut self.2][n % 3] = new_row;
    }
    /// Inverse of matrix using cross products and the triple product
    /// <https://en.wikipedia.org/wiki/Invertible_matrix#Inversion_of_3_%C3%97_3_matrices>
    pub fn inverse(&self) -> Mat3 {
        let det: f64 = (self.0 * (self.1 ^ self.2)).recip();
        Mat3(
            (self.1 ^ self.2) * det,
            (self.2 ^ self.0) * det,
            (self.0 ^ self.1) * det,
        )
        .transpose()
    }
    /// Transpose matrix
    pub fn transpose(&self) -> Mat3 {
        Mat3(
            Col3(self.0 .0, self.1 .0, self.2 .0),
            Col3(self.0 .1, self.1 .1, self.2 .1),
            Col3(self.0 .2, self.1 .2, self.2 .2),
        )
    }
    /// Apply an operation over all the columns
    pub fn over_columns(&self, rhs: Col3, f: fn(a: Col3, b: Col3) -> Col3) -> Mat3 {
        Mat3(f(self.0, rhs), f(self.1, rhs), f(self.2, rhs))
    }
}

impl std::ops::Mul<Mat3> for Mat3 {
    type Output = Mat3;
    fn mul(self, rhs: Mat3) -> Self::Output {
        Mat3(
            Col3(
                self.0 * Col3(rhs.0 .0, rhs.1 .0, rhs.2 .0),
                self.0 * Col3(rhs.0 .1, rhs.1 .1, rhs.2 .1),
                self.0 * Col3(rhs.0 .2, rhs.1 .2, rhs.2 .2),
            ),
            Col3(
                self.1 * Col3(rhs.0 .0, rhs.1 .0, rhs.2 .0),
                self.1 * Col3(rhs.0 .1, rhs.1 .1, rhs.2 .1),
                self.1 * Col3(rhs.0 .2, rhs.1 .2, rhs.2 .2),
            ),
            Col3(
                self.2 * Col3(rhs.0 .0, rhs.1 .0, rhs.2 .0),
                self.2 * Col3(rhs.0 .1, rhs.1 .1, rhs.2 .1),
                self.2 * Col3(rhs.0 .2, rhs.1 .2, rhs.2 .2),
            ),
        )
    }
}

impl std::ops::Mul<f64> for Mat3 {
    type Output = Mat3;
    fn mul(self, rhs: f64) -> Self::Output {
        Mat3(
            Col3(self.0 .0 * rhs, self.0 .1 * rhs, self.0 .2 * rhs),
            Col3(self.1 .0 * rhs, self.1 .1 * rhs, self.1 .2 * rhs),
            Col3(self.2 .0 * rhs, self.2 .1 * rhs, self.2 .2 * rhs),
        )
    }
}

impl std::ops::Div<f64> for Mat3 {
    type Output = Mat3;
    fn div(self, rhs: f64) -> Self::Output {
        Mat3(
            Col3(self.0 .0 / rhs, self.0 .1 / rhs, self.0 .2 / rhs),
            Col3(self.1 .0 / rhs, self.1 .1 / rhs, self.1 .2 / rhs),
            Col3(self.2 .0 / rhs, self.2 .1 / rhs, self.2 .2 / rhs),
        )
    }
}

impl std::ops::Mul<Col3> for Mat3 {
    type Output = Col3;
    fn mul(self, rhs: Col3) -> Self::Output {
        Col3(
            rhs.0
                .mul_add(self.0 .0, rhs.1.mul_add(self.0 .1, rhs.2 * self.0 .2)),
            rhs.0
                .mul_add(self.1 .0, rhs.1.mul_add(self.1 .1, rhs.2 * self.1 .2)),
            rhs.0
                .mul_add(self.2 .0, rhs.1.mul_add(self.2 .1, rhs.2 * self.2 .2)),
        )
    }
}

/// Cross Product
impl std::ops::BitXor<Col3> for Col3 {
    type Output = Col3;
    #[rustfmt::skip]
    fn bitxor(self, rhs: Col3) -> Self::Output {
        Col3(
            self.1.mul_add(rhs.2, - (self.2 * rhs.1)),
          -(self.0.mul_add(rhs.2, - (self.2 * rhs.0))) ,
            self.0.mul_add(rhs.1, - (self.1 * rhs.0)),
        )
    }
}

/// Dot Product
impl std::ops::Mul<Col3> for Col3 {
    type Output = f64;
    fn mul(self, rhs: Col3) -> Self::Output {
        self.0.mul_add(rhs.0, self.1.mul_add(rhs.1, self.2 * rhs.2))
    }
}

impl std::ops::Add<Col3> for Col3 {
    type Output = Col3;
    fn add(self, rhs: Col3) -> Self::Output {
        Col3(self.0 + rhs.0, self.1 + rhs.1, self.2 + rhs.2)
    }
}

impl std::ops::Sub<Col3> for Col3 {
    type Output = Col3;
    fn sub(self, rhs: Col3) -> Self::Output {
        Col3(self.0 - rhs.0, self.1 - rhs.1, self.2 - rhs.2)
    }
}

impl std::ops::Mul<f64> for Col3 {
    type Output = Col3;
    fn mul(self, rhs: f64) -> Self::Output {
        Col3(self.0 * rhs, self.1 * rhs, self.2 * rhs)
    }
}

impl std::ops::Div<f64> for Col3 {
    type Output = Col3;
    fn div(self, rhs: f64) -> Self::Output {
        Col3(
            self.0 * rhs.recip(),
            self.1 * rhs.recip(),
            self.2 * rhs.recip(),
        )
    }
}

impl std::ops::Mul<Col3> for f64 {
    type Output = Col3;
    fn mul(self, rhs: Col3) -> Self::Output {
        Col3(self * rhs.0, self * rhs.1, self * rhs.2)
    }
}

impl std::ops::Div<Col3> for f64 {
    type Output = Col3;
    fn div(self, rhs: Col3) -> Self::Output {
        Col3(
            self * rhs.0.recip(),
            self * rhs.1.recip(),
            self * rhs.2.recip(),
        )
    }
}

/// Image may change during development
///
/// An image that can do math and has operator overloading
#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Image<SPACE: ColorType + Clone + Copy, const GAMUT: ColorGamut> {
    pub data: Vec<Color<SPACE, GAMUT>>,
    pub size: (usize, usize),
}

#[allow(dead_code)]
impl<SPACE: ColorType + Clone + Copy, const GAMUT: ColorGamut> Image<SPACE, GAMUT> {
    /// Return an empty Image
    fn default() -> Image<SPACE, GAMUT> {
        Image {
            data: Vec::new(),
            size: (0, 0),
        }
    }
    /// Construct a new image with size
    pub fn new((width, height): (usize, usize)) -> Image<SPACE, GAMUT> {
        Image {
            data: vec![Color::new([0.0, 0.0, 0.0, 1.0]); width * height],
            size: (width, height),
        }
    }
    /// Construct a new image with a specified color
    pub fn new_with(
        (width, height): (usize, usize),
        fill: Color<SPACE, GAMUT>,
    ) -> Image<SPACE, GAMUT> {
        Image {
            data: vec![fill; width * height],
            size: (width, height),
        }
    }
    /// Construct an image from a vector of colors
    pub fn from_vec(size: (usize, usize), data: Vec<Color<SPACE, GAMUT>>) -> Image<SPACE, GAMUT> {
        Image { data, size }
    }
    /// Return the width of the image
    pub fn width(&self) -> usize {
        self.size.0
    }
    /// Return the height of the image
    pub fn height(&self) -> usize {
        self.size.1
    }
    /// Get the pixels by reference
    pub fn pixels(&self) -> &Vec<Color<SPACE, GAMUT>> {
        &self.data
    }
    /// Get the pixels as mutable
    pub fn pixels_mut(&mut self) -> &mut Vec<Color<SPACE, GAMUT>> {
        &mut self.data
    }
    /// Return a specific channel of all colors as a Vec
    pub fn channel(&self, n: usize) -> Vec<f64> {
        match n {
            0 => self.pixels().clone().iter().map(|x| x.0).collect(),
            1 => self.pixels().clone().iter().map(|x| x.1).collect(),
            2 => self.pixels().clone().iter().map(|x| x.2).collect(),
            3 => self.pixels().clone().iter().map(|x| x.3).collect(),
            _ => self.pixels().clone().iter().map(|x| x.0).collect(),
        }
    }
    /// Return a vec of f64
    pub fn to_vec(&self) -> Vec<f64> {
        let mut vec: Vec<f64> = Vec::new();
        for col in self.data.iter() {
            vec.extend(col.to_arr().0);
        }
        vec
    }
    /// Put a pixel at (x,y)
    pub fn put_pixel(&mut self, (x, y): (usize, usize), pixel: Color<SPACE, GAMUT>) {
        let w = self.width();
        self.pixels_mut()[x + (y * w)] = pixel;
    }
    /// Get the pixel at (x,y)
    pub fn get_pixel(&self, (x, y): (usize, usize)) -> Color<SPACE, GAMUT> {
        let w = self.width();
        self.pixels()[x + (y * w)]
    }
    /// Convert all colors in the image
    #[rustfmt::skip]
    pub fn convert<NEWSPACE: ColorType + FromColorType<SPACE> + Clone + Copy>(&self) -> Image<NEWSPACE, GAMUT> {
        Image {
            data: self.pixels().iter().map(
                |x| NEWSPACE::from_color(*x)
            ).collect::<Vec<Color<NEWSPACE, GAMUT>>>(),
            size: self.size,
        }
    }
    /// Return a region of the Image
    pub fn crop(&self, offset: (usize, usize), size: (usize, usize)) -> Image<SPACE, GAMUT> {
        let w = self.size.0;
        let (x_range, y_range) = (
            offset.0..(size.0 + offset.0),
            (offset.1 * w)..((offset.1 * w) + (w * size.1)),
        );
        let mut new_data = vec![Color::<SPACE, GAMUT>::new([0.0, 0.0, 0.0, 0.0]); 0];
        for row in self.pixels()[y_range].chunks_exact(w) {
            let new = row.to_vec()[x_range.clone()].to_vec();
            new_data.extend(new);
        }
        Image {
            data: new_data,
            size,
        }
    }
    /// Return a cropped region aligned
    pub fn crop_align(&self, mode: (Align, Align), size: (usize, usize)) -> Image<SPACE, GAMUT> {
        let offset = (
            match mode.0 {
                Align::Center => (self.size.0 / 2) - (size.0 / 2),
                Align::Front => 0,
                Align::Back => self.size.0 - size.0,
            },
            match mode.1 {
                Align::Center => (self.size.1 / 2) - (size.1 / 2),
                Align::Front => 0,
                Align::Back => self.size.1 - size.1,
            },
        );
        //
        self.crop(offset, size)
    }
    // Return the min and max colors
    pub fn range(&self) -> (Color<SPACE, GAMUT>, Color<SPACE, GAMUT>) {
        let (mut min, mut max) = ([100000.0; 4], [-100000.0; 4]);
        for pixel in self.pixels().iter() {
            let ch = pixel.to_arr().0;
            for i in 0..4 {
                if ch[i] < min[i] {
                    min[i] = ch[i];
                }
                if ch[i] > max[i] {
                    max[i] = ch[i];
                }
            }
        }
        (
            Color::<SPACE, GAMUT>::new([min[0], min[1], min[2], min[3]]),
            Color::<SPACE, GAMUT>::new([max[0], max[1], max[2], max[3]]),
        )
    }
    // Return the min and max colors in a given color space
    pub fn range_in_space<NEWSPACE: ColorType + FromColorType<SPACE> + Clone + Copy>(
        &self,
    ) -> (Color<NEWSPACE, GAMUT>, Color<NEWSPACE, GAMUT>) {
        let (mut min, mut max) = ([100000.0; 4], [-100000.0; 4]);
        for pixel in self.pixels().iter() {
            let ch = NEWSPACE::from_color(*pixel).to_arr().0;
            for i in 0..4 {
                if ch[i] < min[i] {
                    min[i] = ch[i];
                }
                if ch[i] > max[i] {
                    max[i] = ch[i];
                }
            }
        }
        (
            Color::<NEWSPACE, GAMUT>::new([min[0], min[1], min[2], min[3]]),
            Color::<NEWSPACE, GAMUT>::new([max[0], max[1], max[2], max[3]]),
        )
    }
    /// The mean of all colors
    pub fn mean(&self) -> Color<SPACE, GAMUT> {
        let mut avg = Color::<SPACE, GAMUT>::new([0.0, 0.0, 0.0, 0.0]);
        let mut i = 0;
        for color in self.pixels().iter() {
            i += 1;
            avg += *color;
        }
        avg / i as f64
    }
    /// The mean of all colors in a given color space
    pub fn mean_in_space<NEWSPACE: ColorType + FromColorType<SPACE> + Clone + Copy>(
        &self,
    ) -> Color<NEWSPACE, GAMUT> {
        let mut avg = Color::<NEWSPACE, GAMUT>::new([0.0, 0.0, 0.0, 0.0]);
        let mut i = 0;
        for color in self.pixels().iter() {
            i += 1;
            avg += NEWSPACE::from_color(*color);
        }
        avg / i as f64
    }
    /// Return the variance of color
    pub fn variance(&self) -> Color<SPACE, GAMUT> {
        let mean = self.mean();

        let mut v = Color::<SPACE, GAMUT>::new([0.0, 0.0, 0.0, 0.0]);
        let mut i = 0;
        for color in self.pixels().iter() {
            i += 1;
            let diff = *color - mean;
            v += diff * diff;
        }

        v / i as f64
    }
    /// Return the variance of color in a given color space
    pub fn variance_in_space<NEWSPACE: ColorType + FromColorType<SPACE> + Clone + Copy>(
        &self,
    ) -> Color<NEWSPACE, GAMUT> {
        let mean = self.mean_in_space::<NEWSPACE>();

        let mut v = Color::<NEWSPACE, GAMUT>::new([0.0, 0.0, 0.0, 0.0]);
        let mut i = 0;
        for color in self.pixels().iter() {
            i += 1;
            let diff = NEWSPACE::from_color(*color) - mean;
            v += diff * diff;
        }

        v / i as f64
    }
    /// Return the covariance of color with another Image
    pub fn covariance(&self, image: &Image<SPACE, GAMUT>) -> Color<SPACE, GAMUT> {
        let mean1 = self.mean();
        let mean2 = image.mean();

        let mut v = Color::<SPACE, GAMUT>::new([0.0, 0.0, 0.0, 0.0]);
        let mut i = 0;
        for (color1, color2) in self.pixels().iter().zip(image.pixels().iter()) {
            i += 1;
            v += (*color1 - mean1) * (*color2 - mean2);
        }

        v / i as f64
    }

    /// Return the covariance of color with another Image in a given color space
    pub fn covariance_in_space<NEWSPACE: ColorType + FromColorType<SPACE> + Clone + Copy>(
        &self,
        image: &Image<SPACE, GAMUT>,
    ) -> Color<NEWSPACE, GAMUT> {
        let mean1 = self.mean_in_space::<NEWSPACE>();
        let mean2 = image.mean_in_space::<NEWSPACE>();

        let mut v = Color::<NEWSPACE, GAMUT>::new([0.0, 0.0, 0.0, 0.0]);
        let mut i = 0;
        for (color1, color2) in self.pixels().iter().zip(image.pixels().iter()) {
            i += 1;
            v += (NEWSPACE::from_color(*color1) - mean1) * (NEWSPACE::from_color(*color2) - mean2);
        }

        v / i as f64
    }

    // Structural similarity index with another image
    pub fn ssim(&self, image: Image<SPACE, GAMUT>) -> f64
    where
        CIELcha: FromColorType<SPACE>,
    {
        let meanx = self.mean_in_space::<CIELcha>().0;
        let meany = image.mean_in_space::<CIELcha>().0;

        let variancex = self.variance_in_space::<CIELcha>().0;
        let variancey = image.variance_in_space::<CIELcha>().0;

        let covariance = self.covariance_in_space::<CIELcha>(&image).0;

        let (c1, c2) = (
            (0.01 * (2f64.powi(16) - 1.0)).powi(2),
            (0.03 * (2f64.powi(16) - 1.0)).powi(2),
        );

        (((2.0 * meanx * meany) + c1) * ((covariance + c2) * 2.0))
            / (((meanx * meanx) + (meany * meany) + c1) * (variancex + variancey + c2))
    }

    // Structural similarity index with another image, in a specific color space
    pub fn ssim_in_space<NEWSPACE: ColorType>(
        &self,
        image: Image<SPACE, GAMUT>,
    ) -> Color<NEWSPACE, GAMUT>
    where
        NEWSPACE: FromColorType<SPACE>,
    {
        let meanx = self.mean_in_space::<NEWSPACE>();
        let meany = image.mean_in_space::<NEWSPACE>();

        let variancex = self.variance_in_space::<NEWSPACE>();
        let variancey = image.variance_in_space::<NEWSPACE>();

        let covariance = self.covariance_in_space::<NEWSPACE>(&image);

        let (c1, c2) = (
            (0.01 * (2f64.powi(16) - 1.0)).powi(2),
            (0.03 * (2f64.powi(16) - 1.0)).powi(2),
        );

        (((2.0 * meanx * meany) + c1) * ((covariance + c2) * 2.0))
            / (((meanx * meanx) + (meany * meany) + c1) * (variancex + variancey + c2))
    }

    // pub fn luminance_range(&self) -> (f64, f64) {}
    // pub fn stdev() -> Color<SPACE, GAMUT> {}
    // pub fn luminance() -> f64 {
    //     0.0
    // }
}

impl<SPACE: ColorType + Clone + Copy, const GAMUT: ColorGamut> std::ops::Add<Image<SPACE, GAMUT>>
    for Image<SPACE, GAMUT>
{
    type Output = Image<SPACE, GAMUT>;
    fn add(self, rhs: Image<SPACE, GAMUT>) -> Self::Output {
        let mut new_img = Image {
            data: self.pixels().clone(),
            size: self.size,
        };
        for (pixel, rhs_px) in new_img.pixels_mut().iter_mut().zip(rhs.pixels().iter()) {
            *pixel += *rhs_px;
        }
        new_img
    }
}

impl<SPACE: ColorType + Clone + Copy, const GAMUT: ColorGamut> std::ops::Sub<Image<SPACE, GAMUT>>
    for Image<SPACE, GAMUT>
{
    type Output = Image<SPACE, GAMUT>;
    fn sub(self, rhs: Image<SPACE, GAMUT>) -> Self::Output {
        let mut new_img = Image {
            data: self.pixels().clone(),
            size: self.size,
        };
        for (pixel, rhs_px) in new_img.pixels_mut().iter_mut().zip(rhs.pixels().iter()) {
            *pixel -= *rhs_px;
        }
        new_img
    }
}

impl<SPACE: ColorType + Clone + Copy, const GAMUT: ColorGamut> std::ops::Mul<Image<SPACE, GAMUT>>
    for Image<SPACE, GAMUT>
{
    type Output = Image<SPACE, GAMUT>;
    fn mul(self, rhs: Image<SPACE, GAMUT>) -> Self::Output {
        let mut new_img = Image {
            data: self.pixels().clone(),
            size: self.size,
        };
        for (pixel, rhs_px) in new_img.pixels_mut().iter_mut().zip(rhs.pixels().iter()) {
            *pixel *= *rhs_px;
        }
        new_img
    }
}

impl<SPACE: ColorType + Clone + Copy, const GAMUT: ColorGamut> std::ops::Div<Image<SPACE, GAMUT>>
    for Image<SPACE, GAMUT>
{
    type Output = Image<SPACE, GAMUT>;
    fn div(self, rhs: Image<SPACE, GAMUT>) -> Self::Output {
        let mut new_img = Image {
            data: self.pixels().clone(),
            size: self.size,
        };
        for (pixel, rhs_px) in new_img.pixels_mut().iter_mut().zip(rhs.pixels().iter()) {
            *pixel /= *rhs_px;
        }
        new_img
    }
}

///
///

impl<SPACE: ColorType + Clone + Copy, const GAMUT: ColorGamut> std::ops::Add<Color<SPACE, GAMUT>>
    for Image<SPACE, GAMUT>
{
    type Output = Image<SPACE, GAMUT>;
    fn add(self, rhs: Color<SPACE, GAMUT>) -> Self::Output {
        let mut new_img = Image {
            data: self.pixels().clone(),
            size: self.size,
        };
        for pixel in new_img.pixels_mut().iter_mut() {
            *pixel += rhs;
        }
        new_img
    }
}

impl<SPACE: ColorType + Clone + Copy, const GAMUT: ColorGamut> std::ops::Sub<Color<SPACE, GAMUT>>
    for Image<SPACE, GAMUT>
{
    type Output = Image<SPACE, GAMUT>;
    fn sub(self, rhs: Color<SPACE, GAMUT>) -> Self::Output {
        let mut new_img = Image {
            data: self.pixels().clone(),
            size: self.size,
        };
        for pixel in new_img.pixels_mut().iter_mut() {
            *pixel -= rhs;
        }
        new_img
    }
}

impl<SPACE: ColorType + Clone + Copy, const GAMUT: ColorGamut> std::ops::Mul<Color<SPACE, GAMUT>>
    for Image<SPACE, GAMUT>
{
    type Output = Image<SPACE, GAMUT>;
    fn mul(self, rhs: Color<SPACE, GAMUT>) -> Self::Output {
        let mut new_img = Image {
            data: self.pixels().clone(),
            size: self.size,
        };
        for pixel in new_img.pixels_mut().iter_mut() {
            *pixel *= rhs;
        }
        new_img
    }
}

impl<SPACE: ColorType + Clone + Copy, const GAMUT: ColorGamut> std::ops::Div<Color<SPACE, GAMUT>>
    for Image<SPACE, GAMUT>
{
    type Output = Image<SPACE, GAMUT>;
    fn div(self, rhs: Color<SPACE, GAMUT>) -> Self::Output {
        let mut new_img = Image {
            data: self.pixels().clone(),
            size: self.size,
        };
        for pixel in new_img.pixels_mut().iter_mut() {
            *pixel /= rhs;
        }
        new_img
    }
}

///
///

impl<SPACE: ColorType + Clone + Copy, const GAMUT: ColorGamut> std::ops::Mul<f64>
    for Image<SPACE, GAMUT>
{
    type Output = Image<SPACE, GAMUT>;
    fn mul(self, rhs: f64) -> Self::Output {
        let mut new_img = Image {
            data: self.pixels().clone(),
            size: self.size,
        };
        for pixel in new_img.pixels_mut().iter_mut() {
            *pixel *= rhs;
        }
        new_img
    }
}

impl<SPACE: ColorType + Clone + Copy, const GAMUT: ColorGamut> std::ops::Div<f64>
    for Image<SPACE, GAMUT>
{
    type Output = Image<SPACE, GAMUT>;
    fn div(self, rhs: f64) -> Self::Output {
        let mut new_img = Image {
            data: self.pixels().clone(),
            size: self.size,
        };
        for pixel in new_img.pixels_mut().iter_mut() {
            *pixel /= rhs;
        }
        new_img
    }
}

impl<SPACE: ColorType + Clone + Copy, const GAMUT: ColorGamut> std::ops::Div<Image<SPACE, GAMUT>>
    for f64
{
    type Output = Image<SPACE, GAMUT>;
    fn div(self, rhs: Image<SPACE, GAMUT>) -> Self::Output {
        let mut new_img = Image {
            data: rhs.pixels().clone(),
            size: rhs.size,
        };
        for pixel in new_img.pixels_mut().iter_mut() {
            *pixel *= 1.0 / self;
        }
        new_img
    }
}

impl<SPACE: ColorType + Clone + Copy, const GAMUT: ColorGamut> std::ops::Mul<Image<SPACE, GAMUT>>
    for f64
{
    type Output = Image<SPACE, GAMUT>;
    fn mul(self, rhs: Image<SPACE, GAMUT>) -> Self::Output {
        let mut new_img = Image {
            data: rhs.pixels().clone(),
            size: rhs.size,
        };
        for pixel in new_img.pixels_mut().iter_mut() {
            *pixel *= self;
        }
        new_img
    }
}

//

/// Align is likely to change wording during development
///
/// The alignment of cropping
///
/// Front = left/top
///
/// Back = right/bottom
#[allow(dead_code)]
#[derive(Clone, Copy)]
pub enum Align {
    Center,
    Front,
    Back,
}
