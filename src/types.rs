use std::{fmt, marker::PhantomData, num::FpCategory};

use crate::{colors::CIELab, Xyz, Yxy};

// #[derive(Clone, Debug, PartialEq, Eq, PartialOrd)]
// pub struct Intent {
//     method: |x: Color| -> Self,
// }

// const PERCEPTUAL: Intent = Intent;

pub trait WhiteType:  'static + Copy + Send + Sync {

}
#[const_trait]
pub trait ColorType: 'static + Copy + Send + Sync {
    fn gamut() -> &'static ColorGamut;
    fn white() -> &'static White;
    fn range() -> [std::ops::Range<f64>;3];
}

/// Method for safely converting between ColorType structs
pub trait FromColorType<SPACE>: ColorType
where
    SPACE: 'static + ColorType + Clone + Copy + Send + Sync,
{
    fn from_color<const WHITE: White>(
        _: Color<SPACE, WHITE>,
    ) -> Color<Self, WHITE>
    where
        Self: ColorType + std::marker::Sized + Clone + Copy + Send + Sync;
}

// pub trait FromWhiteType<const DW: White>: WhiteType
// {
//     fn from_white<const SW: White, SPACE: ColorType>(
//         _: Color<SPACE, SW>,
//     ) -> Color<SPACE, DW>;
// }

// pub trait FromColorGamut: ColorType {
//     fn from_color<SPACE: ColorType, const WHITE: White>(
//         _: Color<SPACE, WHITE>,
//     ) -> Color<Self, WHITE>
//     where
//         Self: ColorType + std::marker::Sized + Clone + Copy + Send + Sync;
// }

// pub trait FromWhitePoint<const WHITE: White>: ColorType {
//     fn from_color<SPACE: ColorType>(
//         _: Color<SPACE, WHITE>,
//     ) -> Color<Self, WHITE>
//     where
//         Self: ColorType + std::marker::Sized + Clone + Copy + Send + Sync;
// }

/// An abstract color type that has operator overloading
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub struct Color<
    SPACE: 'static + ColorType + Clone + Copy + Send + Sync,
    const WHITE: White
>(
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
        /// [<$name_1_arg>]
        pub fn [<$name_1_arg:snake>] (&self) -> Color<SPACE,  WHITE,> {
             apply_ops_color!(($name_1_arg),(self))
        }
        )*
        $(
        /// [<$name_2_arg>]
        pub fn [<$name_2_arg:snake _color>] (&self, rhs: Color<SPACE, WHITE>) -> Color<SPACE, WHITE> {
            apply_ops_color!(($name_2_arg),(self, rhs))
        }
        /// [<$name_2_arg>]
        pub fn [<$name_2_arg:snake>] (&self, rhs: f64) -> Color<SPACE,  WHITE,> {
            Color::<SPACE, WHITE>::new(
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
        /// [<$name_3_arg>]
        pub fn [<$name_3_arg:snake _color>] (&self, rhs: Color<SPACE,  WHITE,>, rhs2: Color<SPACE, WHITE>) -> Color<SPACE,  WHITE,> {
            apply_ops_color!(($name_3_arg),(self, rhs, rhs2))
        }
        pub fn [<$name_3_arg:snake _colf>] (&self, rhs: Color<SPACE,  WHITE,>, rhs2: f64) -> Color<SPACE,  WHITE,> {
            Color::<SPACE, WHITE>::new(
                [
                    f64::[<$name_3_arg>](self.0, rhs.0, rhs2),
                    f64::[<$name_3_arg>](self.1, rhs.1, rhs2),
                    f64::[<$name_3_arg>](self.2, rhs.2, rhs2),
                    f64::[<$name_3_arg>](self.3, rhs.3, rhs2),
                ]
            )
        }
        pub fn [<$name_3_arg:snake _fcol>] (&self, rhs: f64, rhs2: Color<SPACE,  WHITE,>) -> Color<SPACE,  WHITE,> {
            Color::<SPACE, WHITE>::new(
                [
                    f64::[<$name_3_arg>](self.0, rhs, rhs2.0),
                    f64::[<$name_3_arg>](self.1, rhs, rhs2.1),
                    f64::[<$name_3_arg>](self.2, rhs, rhs2.2),
                    f64::[<$name_3_arg>](self.3, rhs, rhs2.3),
                ]
            )
        }
        pub fn [<$name_3_arg:snake>] (&self, rhs: f64, rhs2: f64) -> Color<SPACE,  WHITE,> {
            Color::<SPACE, WHITE>::new(
                [
                    f64::[<$name_3_arg>](self.0, rhs, rhs2),
                    f64::[<$name_3_arg>](self.1, rhs, rhs2),
                    f64::[<$name_3_arg>](self.2, rhs, rhs2),
                    f64::[<$name_3_arg>](self.3, rhs, rhs2),
                ]
            )
        }
        )*
    }
    };
}

macro_rules! apply_ops_color {

    (($name_arg:ident),($($name:ident),*) ) => {
        paste::item! {
            Color::<SPACE, WHITE>::new(
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

impl<SPACE: ColorType + Clone + Copy, const WHITE: White> fmt::Display
    for Color<SPACE, WHITE>
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "[{:.4}, {:.4}, {:.4}, {:.4}]",
            self.0, self.1, self.2, self.3
        )
    }
}

impl<SPACE: ColorType + Clone + Copy, const WHITE: White> Default
    for Color<SPACE, WHITE>
{
    fn default() -> Color<SPACE, WHITE> {
        Color::new([0.0, 0.0, 0.0, 1.0])
    }
}

#[allow(dead_code)]
impl<
        SPACE: ColorType + Clone + Copy + Send + Sync,
        const WHITE: White,
    > Color<SPACE, WHITE>
{
    /// Return the reference white point
    #[inline]
    pub fn white(&self) -> White {
        WHITE
    }

    /// Return the tristimulus values of the white point
    #[inline]
    pub fn white_tristimulus(&self) -> Col3 {
        WHITE.tristimulus()
    }

    /// Return the gamut const
    #[inline]
    pub fn gamut(&self) -> &ColorGamut {
        SPACE::gamut()
    }


    /// Construct a new color
    pub fn new(ch: [f64; 4]) -> Self {
        Color::<SPACE, WHITE>(ch[0], ch[1], ch[2], ch[3], PhantomData)
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

    pub fn blend_linear(self, color: Self, f: f64) -> Self {
        (self * f) + (color * (1.0 - f))
    }

    /// Color difference for acceptability
    pub fn delta_e_a(self, color: Color<SPACE, WHITE>) -> f64
    where
        CIELab: FromColorType<SPACE>,
    {
        self.delta_e(color, (2.0, 1.0))
    }

    /// Color difference for perceptibility
    pub fn delta_e_p(self, color: Color<SPACE, WHITE>) -> f64
    where
        CIELab: FromColorType<SPACE>,
    {
        self.delta_e(color, (1.0, 1.0))
    }

    /// Color difference
    pub fn delta_e(self, color: Color<SPACE, WHITE>, (l, c): (f64, f64)) -> f64
    where
        CIELab: FromColorType<SPACE>,
    {
        // let a = // let a = CIELaba::from_color(self);
        let a = CIELab::from_color(self);
        let b = CIELab::from_color(color);

        let (c1, c2) = (
            (a.1 * a.1 + a.2 * a.2).sqrt(),
            (b.1 * b.1 + b.2 * b.2).sqrt(),
        );

        let delta_c = c1 - c2;

        let delta = a - b;

        #[rustfmt::skip]
        let delta_h = (delta.1 *
            delta.1) + (delta.2 * 
                delta.2) + (delta_c * delta_c)
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
    pub fn within_u16_sqrd_precision_of(&self, rhs: Color<SPACE, WHITE>) -> [bool; 4] {
        [
            (self.0 - rhs.0).abs() < (u16::MAX as f64).powi(2).recip(),
            (self.1 - rhs.1).abs() < (u16::MAX as f64).powi(2).recip(),
            (self.2 - rhs.2).abs() < (u16::MAX as f64).powi(2).recip(),
            (self.3 - rhs.3).abs() < (u16::MAX as f64).powi(2).recip(),
        ]
    }

    pub fn clamp_to_gamut(&self) -> Self {
        let ranges = SPACE::range();
        Color::new(
            [
                self.0.clamp(ranges[0].start, ranges[0].end),
                self.1.clamp(ranges[1].start, ranges[1].end),
                self.2.clamp(ranges[2].start, ranges[2].end),
                self.3,
            ]
        )
    }

    pub fn is_nan(&self) -> [bool;4] {
        [
            self.0.is_nan(),
            self.1.is_nan(),
            self.2.is_nan(),
            self.3.is_nan(),
        ]
    }
    pub fn is_infinite(&self) -> [bool;4] {
        [
            self.0.is_infinite(),
            self.1.is_infinite(),
            self.2.is_infinite(),
            self.3.is_infinite(),
        ]
    }
    pub fn is_finite(&self) -> [bool;4] {
        [
            self.0.is_finite(),
            self.1.is_finite(),
            self.2.is_finite(),
            self.3.is_finite(),
        ]
    }
    pub fn is_subnormal(&self) -> [bool;4] {
        [
            self.0.is_subnormal(),
            self.1.is_subnormal(),
            self.2.is_subnormal(),
            self.3.is_subnormal(),
        ]
    }
    pub fn is_normal(&self) -> [bool;4] {
        [
            self.0.is_normal(),
            self.1.is_normal(),
            self.2.is_normal(),
            self.3.is_normal(),
        ]
    }
    pub fn classify(&self) -> [FpCategory;4] {
        [
            self.0.classify(),
            self.1.classify(),
            self.2.classify(),
            self.3.classify(),
        ]
    }
    pub fn powi(&self, rhs: i32) -> Self {
        Self::new([
            self.0.powi(rhs),
            self.1.powi(rhs),
            self.2.powi(rhs),
            self.3.powi(rhs),
        ])
    }

    impl_float_ops_color!(
        (
            sqrt, cbrt, cos, cosh, acos, acosh, sin, sinh, asin, asinh, tan, tanh, atan, atanh,
            abs, floor, ceil, round, exp, exp2, exp_m1, ln, ln_1p, log2, log10, recip, signum
        ),
        (powf, atan2, log, max, min, hypot, div_euclid, rem_euclid),
        (mul_add, clamp)
    );
}

impl<SPACE: ColorType + Clone + Copy, const WHITE: White> std::ops::Neg
    for Color<SPACE, WHITE>
{
    type Output = Color<SPACE, WHITE>;
    fn neg(self) -> Self::Output {
        Self::new([-self.0, -self.1, -self.2, self.3])
    }
}

impl<SPACE: ColorType + Clone + Copy, const WHITE: White>
    std::ops::Add<Color<SPACE, WHITE>> for Color<SPACE, WHITE>
{
    type Output = Color<SPACE, WHITE>;
    fn add(self, rhs: Color<SPACE, WHITE>) -> Color<SPACE, WHITE> {
        Self::new([
            self.0 + rhs.0,
            self.1 + rhs.1,
            self.2 + rhs.2,
            self.3 + rhs.3,
        ])
    }
}

impl<SPACE: ColorType + Clone + Copy, const WHITE: White>
    std::ops::AddAssign<Color<SPACE, WHITE>> for Color<SPACE, WHITE>
{
    fn add_assign(&mut self, rhs: Color<SPACE, WHITE>) {
        self.0 += rhs.0;
        self.1 += rhs.1;
        self.2 += rhs.2;
        self.3 += rhs.3;
    }
}

impl<SPACE: ColorType + Clone + Copy, const WHITE: White>
    std::ops::Sub<Color<SPACE, WHITE>> for Color<SPACE, WHITE>
{
    type Output = Color<SPACE, WHITE>;
    fn sub(self, rhs: Color<SPACE, WHITE>) -> Color<SPACE, WHITE> {
        Self::new([
            self.0 - rhs.0,
            self.1 - rhs.1,
            self.2 - rhs.2,
            self.3 - rhs.3,
        ])
    }
}

impl<SPACE: ColorType + Clone + Copy, const WHITE: White>
    std::ops::SubAssign<Color<SPACE, WHITE>> for Color<SPACE, WHITE>
{
    fn sub_assign(&mut self, rhs: Color<SPACE, WHITE>) {
        self.0 -= rhs.0;
        self.1 -= rhs.1;
        self.2 -= rhs.2;
        self.3 -= rhs.3;
    }
}

impl<SPACE: ColorType + Clone + Copy, const WHITE: White>
    std::ops::Mul<Color<SPACE, WHITE>> for Color<SPACE, WHITE>
{
    type Output = Color<SPACE, WHITE>;
    fn mul(self, rhs: Color<SPACE, WHITE>) -> Color<SPACE, WHITE> {
        Self::new([
            self.0 * rhs.0,
            self.1 * rhs.1,
            self.2 * rhs.2,
            self.3 * rhs.3,
        ])
    }
}

impl<SPACE: ColorType + Clone + Copy, const WHITE: White>
    std::ops::MulAssign<Color<SPACE, WHITE>> for Color<SPACE, WHITE>
{
    fn mul_assign(&mut self, rhs: Color<SPACE, WHITE>) {
        self.0 *= rhs.0;
        self.1 *= rhs.1;
        self.2 *= rhs.2;
        self.3 *= rhs.3;
    }
}

impl<SPACE: ColorType + Clone + Copy, const WHITE: White>
    std::ops::Add<f64> for Color<SPACE, WHITE>
{
    type Output = Color<SPACE, WHITE>;
    fn add(self, rhs: f64) -> Color<SPACE, WHITE> {
        Self::new([self.0 + rhs, self.1 + rhs, self.2 + rhs, self.3 + rhs])
    }
}

impl<SPACE: ColorType + Clone + Copy, const WHITE: White>
    std::ops::Sub<f64> for Color<SPACE, WHITE>
{
    type Output = Color<SPACE, WHITE>;
    fn sub(self, rhs: f64) -> Color<SPACE, WHITE> {
        Self::new([self.0 - rhs, self.1 - rhs, self.2 - rhs, self.3 - rhs])
    }
}

impl<SPACE: ColorType + Clone + Copy, const WHITE: White>
    std::ops::Div<Color<SPACE, WHITE>> for Color<SPACE, WHITE>
{
    type Output = Color<SPACE, WHITE>;
    fn div(self, rhs: Color<SPACE, WHITE>) -> Color<SPACE, WHITE> {
        Self::new([
            self.0 / rhs.0,
            self.1 / rhs.1,
            self.2 / rhs.2,
            self.3 / rhs.3,
        ])
    }
}

impl<SPACE: ColorType + Clone + Copy, const WHITE: White>
    std::ops::DivAssign<Color<SPACE, WHITE>> for Color<SPACE, WHITE>
{
    fn div_assign(&mut self, rhs: Color<SPACE, WHITE>) {
        self.0 /= rhs.0;
        self.1 /= rhs.1;
        self.2 /= rhs.2;
        self.3 /= rhs.3;
    }
}

impl<SPACE: ColorType + Clone + Copy, const WHITE: White>
    std::ops::Mul<f64> for Color<SPACE, WHITE>
{
    type Output = Color<SPACE, WHITE>;
    fn mul(self, rhs: f64) -> Color<SPACE, WHITE> {
        Self::new([self.0 * rhs, self.1 * rhs, self.2 * rhs, self.3 * rhs])
    }
}

impl<SPACE: ColorType + Clone + Copy, const WHITE: White>
    std::ops::MulAssign<f64> for Color<SPACE, WHITE>
{
    fn mul_assign(&mut self, rhs: f64) {
        self.0 *= rhs;
        self.1 *= rhs;
        self.2 *= rhs;
        self.3 *= rhs;
    }
}

impl<SPACE: ColorType + Clone + Copy, const WHITE: White>
    std::ops::Mul<Color<SPACE, WHITE>> for f64
{
    type Output = Color<SPACE, WHITE>;
    fn mul(self, rhs: Color<SPACE, WHITE>) -> Color<SPACE, WHITE> {
        Color::new([self * rhs.0, self * rhs.1, self * rhs.2, self * rhs.3])
    }
}

impl<SPACE: ColorType + Clone + Copy, const WHITE: White>
    std::ops::Div<f64> for Color<SPACE, WHITE>
{
    type Output = Color<SPACE, WHITE>;
    fn div(self, rhs: f64) -> Color<SPACE, WHITE> {
        Self::new([self.0 / rhs, self.1 / rhs, self.2 / rhs, self.3 / rhs])
    }
}

impl<SPACE: ColorType + Clone + Copy, const WHITE: White>
    std::ops::DivAssign<f64> for Color<SPACE, WHITE>
{
    fn div_assign(&mut self, rhs: f64) {
        self.0 /= rhs;
        self.1 /= rhs;
        self.2 /= rhs;
        self.3 /= rhs;
    }
}

impl<SPACE: ColorType + Clone + Copy, const WHITE: White>
    std::ops::Div<Color<SPACE, WHITE>> for f64
{
    type Output = Color<SPACE, WHITE>;
    fn div(self, rhs: Color<SPACE, WHITE>) -> Color<SPACE, WHITE> {
        Color::new([self / rhs.0, self / rhs.1, self / rhs.2, self / rhs.3])
    }
}

#[allow(dead_code)]
/// The reference white point of the color. Default: D65

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Hash)]
pub struct White {
    pub t1: u64,
    pub t2: u64,
    pub t3: u64,
}

impl White {
    pub fn new(tri: [f64;3]) -> Self {
        White {
            t1: tri[0].to_bits(),
            t2: tri[1].to_bits(),
            t3: tri[2].to_bits(),
        }
    }
    /// Return the tristimulus (xyY) value
    #[inline(always)]
    pub fn tristimulus(&self) -> Col3 {
        Col3::new([f64::from_bits(self.t1),f64::from_bits(self.t2),f64::from_bits(self.t3)])
    }

    /// Adapt to a new white point
    ///
    /// not well tested
    ///
    /// <http://www.brucelindbloom.com/index.html?Eqn_ChromAdapt.html>
    /*
    where
        Xyz: FromColorType<SPACE> + ~const ColorType,
        SPACE: FromColorType<Xyz> + ~const ColorType,
    {
        let xyz_color = Xyz::from_color(color);
    */
    #[allow(dead_code, unused_variables)]
    pub fn adapt_chroma_from_xyz<const SW: White, const DW: White>(self, color: Color<Xyz, SW>) -> Color<Xyz, DW>
    {
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
        let Col3(ps, ys, bs) = m * {
            let xyy = SW.tristimulus();
            let [x,y,z] = [
                (xyy.0 * xyy.2) * xyy.1.recip(),
                xyy.2,
                ((1.0 - xyy.0 - xyy.1) * xyy.2) * xyy.1.recip(),
            ];
            Col3(x,y,z)
        };
        let Col3(pd, yd, bd) = m * {
            let xyy = DW.tristimulus();
            let [x,y,z] = [
                (xyy.0 * xyy.2) * xyy.1.recip(),
                xyy.2,
                ((1.0 - xyy.0 - xyy.1) * xyy.2) * xyy.1.recip(),
            ];
            Col3(x,y,z)
        };
        let adapt_mat =
            (mi * Mat3(
                Col3(pd / ps, 0.0, 0.0),
                Col3(0.0, yd / ys, 0.0),
                Col3(0.0, 0.0, bd / bs),
            )) * m;
        let Col3(x, y, z) = adapt_mat * Col3(color.0, color.1, color.2);
        Xyz::new([x, y, z, color.3])
    }
    pub fn adapt_chroma<SPACE: ColorType, const SW: White, const DW: White>(self, color: Color<SPACE, SW>) -> Color<SPACE, DW>
    where
        Xyz: FromColorType<SPACE> + ~const ColorType,
        SPACE: FromColorType<Xyz> + ~const ColorType,
    {
        SPACE::from_color(self.adapt_chroma_from_xyz::<SW,DW>(Xyz::from_color(color)))
    }
}

/// Standard White Point D65
#[allow(unused)]
pub const D65: White = White {
    t1: 0.312727f64.to_bits(),
    t2: 0.329023f64.to_bits(),
    t3: 1.0f64.to_bits(),
};
/// Standard White Point D50
#[allow(unused)]
pub const D50: White = White {
    t1: 0.34567f64.to_bits(),
    t2: 0.35850f64.to_bits(),
    t3: 1.0f64.to_bits(),
};

/// Standard White Point E
#[allow(unused)]
pub const E: White = White {
    t1: (1.0f64 / 3.0).to_bits(),
    t2: (1.0f64 / 3.0).to_bits(),
    t3: 1.0f64.to_bits(),
};

/// Convert a color temperature into xy chromaticity coordinates
///
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
pub struct ColorGamut {
    pub primaries_xyy: [Col3; 3],
    pub transfer_fn: fn(v: f64) -> f64,
    pub transfer_fn_inv: fn(v: f64) -> f64,
    pub conversion: Mat3,
    pub white: White,
}

impl ColorGamut {
    /// Return the primaries of the gamut in xyy
    #[inline]
    pub fn primaries_xyy(&self) -> &[Col3; 3] {
        &self.primaries_xyy
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
        self.white
    }

    /// Return the conversion matrix
    pub fn conversion_matrix(&self) -> Mat3 {
        self.conversion
    }

    /// Compute the sRGB matrix for RGB to XYZ conversion
    /// Y is the brightness of the whitepoint
    ///
    /// sources:
    /// <https://mina86.com/2019/srgb-xyz-matrix/>
    ///
    /// <http://www.brucelindbloom.com/index.html?WorkingSpaceInfo.html#Specifications>
    #[inline(always)]
    pub fn custom_system_matrix_exact(&self, t: f64) -> Mat3 {
        let s = self.primaries_xyy();

        // Reference whitepoint tristimulus
        let ref_white = {
            let white = t_to_xy(t);
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
    #[inline(always)]
    pub fn custom_system_matrix<const SW: White, const DW: White>(&self) -> Mat3 {
        let s = *self.primaries_xyy();
        let s = [
            DW.adapt_chroma::<_, SW, DW>(Yxy::new::<SW>([s[0].2, s[0].0, s[0].1, 1.0])),
            DW.adapt_chroma::<_, SW, DW>(Yxy::new::<SW>([s[1].2, s[1].0, s[1].1, 1.0])),
            DW.adapt_chroma::<_, SW, DW>(Yxy::new::<SW>([s[2].2, s[2].0, s[2].1, 1.0]))
            ];
        let s = [
            Col3(s[0].1,s[0].2,s[0].0),
            Col3(s[1].1,s[1].2,s[1].0),
            Col3(s[2].1,s[2].2,s[2].0),
        ];
        // Reference whitepoint tristimulus
        let ref_white = {
            let Col3(x,y,_) = DW.tristimulus();
            Col3(
                x * y.recip(),
                1.0,
                (1.0 - x - y) * y.recip(),
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

    /// Convert the mat3 into an array
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
    ///
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
            (rhs.0 * self.0 .0) + (rhs.1 * self.0 .1) + (rhs.2 * self.0 .2),
            (rhs.0 * self.1 .0) + (rhs.1 * self.1 .1) + (rhs.2 * self.1 .2),
            (rhs.0 * self.2 .0) + (rhs.1 * self.2 .1) + (rhs.2 * self.2 .2),
        )
    }
}

/// Cross Product
impl std::ops::BitXor<Col3> for Col3 {
    type Output = Col3;
    #[rustfmt::skip]
    fn bitxor(self, rhs: Col3) -> Self::Output {
        Col3(
            (self.1 *rhs.2) - (self.2 * rhs.1),
          -((self.0 *rhs.2) - (self.2 * rhs.0)) ,
            (self.0 *rhs.1) - (self.1 * rhs.0),
        )
    }
}

/// Dot Product
impl std::ops::Mul<Col3> for Col3 {
    type Output = f64;
    fn mul(self, rhs: Col3) -> Self::Output {
        (self.0 * rhs.0) + (self.1 * rhs.1) + (self.2 * rhs.2)
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
pub struct Image<
    SPACE: 'static + ColorType + Clone + Copy + Send + Sync,
    const WHITE: White,
> {
    pub data: Vec<Color<SPACE, WHITE>>,
    pub size: (usize, usize),
}

#[allow(dead_code)]
impl<
        SPACE: ColorType + Clone + Copy + Send + Sync,
        const WHITE: White,
    > Image<SPACE, WHITE>
{
    /// Return an empty Image
    fn default() -> Image<SPACE, WHITE> {
        Image {
            data: Vec::new(),
            size: (0, 0),
        }
    }

    /// Construct a new image with size
    pub fn new((width, height): (usize, usize)) -> Image<SPACE, WHITE> {
        Image {
            data: vec![Color::new([0.0, 0.0, 0.0, 1.0]); width * height],
            size: (width, height),
        }
    }

    /// Construct a new image with a specified color
    pub fn new_with(
        (width, height): (usize, usize),
        fill: Color<SPACE, WHITE>,
    ) -> Image<SPACE, WHITE> {
        Image {
            data: vec![fill; width * height],
            size: (width, height),
        }
    }

    /// Construct an image from a vector of colors
    pub fn from_vec(
        size: (usize, usize),
        data: Vec<Color<SPACE, WHITE>>,
    ) -> Image<SPACE, WHITE> {
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
    pub fn pixels(&self) -> impl Iterator<Item = &Color<SPACE, WHITE>> {
        self.data.iter()
    }

    /// Get the pixels as mutable
    pub fn pixels_mut(&mut self) -> impl Iterator<Item = &mut Color<SPACE, WHITE>> {
        self.data.iter_mut()
    }

    /// iterate (&self, &image)
    pub fn pixels_zip<'a>(
        &'a self,
        image: &'a Image<SPACE, WHITE>,
    ) -> impl Iterator<Item = (&Color<SPACE, WHITE>, &Color<SPACE, WHITE>)> + 'a {
        self.pixels().zip(image.pixels())
    }

    /// iterate (&mut self, &image)
    pub fn pixels_mut_zip<'a>(
        &'a mut self,
        image: &'a Image<SPACE, WHITE>,
    ) -> impl Iterator<Item = (&mut Color<SPACE, WHITE>, &Color<SPACE, WHITE>)> + 'a
    {
        self.pixels_mut().zip(image.pixels())
    }

    /// iterate (&self, &mut image)
    pub fn pixels_zip_mut<'a>(
        &'a self,
        image: &'a mut Image<SPACE, WHITE>,
    ) -> impl Iterator<Item = (&Color<SPACE, WHITE>, &mut Color<SPACE, WHITE>)> + 'a
    {
        self.pixels().zip(image.pixels_mut())
    }

    /// iterate (&mut self, &mut image)
    pub fn pixels_mut_zip_mut<'a>(
        &'a mut self,
        image: &'a mut Image<SPACE, WHITE>,
    ) -> impl Iterator<
        Item = (
            &mut Color<SPACE, WHITE>,
            &mut Color<SPACE, WHITE>,
        ),
    > + 'a {
        self.pixels_mut().zip(image.pixels_mut())
    }

    /// Apply a function over all pixels
    pub fn for_each_pixel<F>(&self, f: F)
    where
        F: FnMut(&Color<SPACE, WHITE>),
    {
        self.pixels().for_each(f);
    }

    /// Apply a function over all pixels with mutability
    pub fn for_each_pixel_mut<F>(&mut self, f: F)
    where
        F: FnMut(&mut Color<SPACE, WHITE>),
    {
        self.pixels_mut().for_each(f);
    }

    /// Map a function over pixels and return a Color
    pub fn map_pixels<'a, F>(
        &'a self,
        f: F,
    ) -> impl Iterator<Item = Color<SPACE, WHITE>> + 'a
    where
        F: FnMut(&Color<SPACE, WHITE>) -> Color<SPACE, WHITE> + 'a,
    {
        self.pixels().map(f)
    }

    /// Map a function over pixels and return an f64
    pub fn map_f<'a, F>(&'a self, f: F) -> impl Iterator<Item = f64> + 'a
    where
        F: FnMut(&Color<SPACE, WHITE>) -> f64 + 'a,
    {
        self.pixels().map(f)
    }

    /// Map a function over pixels zipped with another image and return a Color
    pub fn map_pixels_with<'a, F>(
        &'a self,
        image: &'a Image<SPACE, WHITE>,
        f: F,
    ) -> impl Iterator<Item = Color<SPACE, WHITE>> + 'a
    where
        F: FnMut(
                (&Color<SPACE, WHITE>, &Color<SPACE, WHITE>),
            ) -> Color<SPACE, WHITE>
            + 'a,
    {
        self.pixels_zip(image).map(f)
    }

    /// Map a function over pixels and return a Color in a new space
    pub fn map_pixels_to<'a, NEWSPACE, F>(
        &'a self,
        f: F,
    ) -> impl Iterator<Item = Color<NEWSPACE, WHITE>> + 'a
    where
        NEWSPACE: ColorType + FromColorType<SPACE> + Clone + Copy + Send + Sync,
        F: FnMut(&Color<SPACE, WHITE>) -> Color<NEWSPACE, WHITE> + 'a,
    {
        self.pixels().map(f)
    }

    /// Map a function over pixels zipped with another image and return a Color in a new space
    pub fn map_pixels_with_to<'a, NEWSPACE, F>(
        &'a self,
        image: &'a Image<SPACE, WHITE>,
        f: F,
    ) -> impl Iterator<Item = Color<NEWSPACE, WHITE>> + 'a
    where
        NEWSPACE: ColorType + FromColorType<SPACE> + Clone + Copy + Send + Sync,
        F: FnMut(
                (&Color<SPACE, WHITE>, &Color<SPACE, WHITE>),
            ) -> Color<NEWSPACE, WHITE>
            + 'a,
    {
        self.pixels_zip(image).map(f)
    }

    /// Return a row as a slice
    pub fn row(&self, n: usize) -> &[Color<SPACE, WHITE>] {
        let w = self.width();
        // self.pixels().
        &(&self.data)[(n * w)..(n + 1) * w]
    }

    /// Return a specific channel of all colors as a Vec
    pub fn channel(&self, n: usize) -> Vec<f64> {
        match n {
            0 => self.map_f(|x| x.0).collect(),
            1 => self.map_f(|x| x.1).collect(),
            2 => self.map_f(|x| x.2).collect(),
            3 => self.map_f(|x| x.3).collect(),
            _ => self.map_f(|x| x.0).collect(),
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
    pub fn put_pixel(&mut self, (x, y): (usize, usize), pixel: Color<SPACE, WHITE>) {
        let w = self.width();
        if let Some(p) = self.data.get_mut(x + (y * w)) {
            *p = pixel;
        }
    }

    /// Get the pixel at (x,y)
    pub fn get_pixel(&self, (x, y): (usize, usize)) -> Option<&Color<SPACE, WHITE>> {
        let w = self.width();
        self.data.get(x + (y * w))
    }

    /// Convert all colors in the image
    #[rustfmt::skip]
    pub fn convert<NEWSPACE: ColorType + FromColorType<SPACE> + Clone + Copy + Send + Sync>(&self) -> Image<NEWSPACE, WHITE> {
        Image {
            data: self.pixels().map(
                |x| NEWSPACE::from_color(*x)
            ).collect::<Vec<Color<NEWSPACE,  WHITE>>>(),
            size: self.size,
        }
    }

    /// Return a region of the Image
    pub fn crop(&self, offset: (usize, usize), size: (usize, usize)) -> Image<SPACE, WHITE> {
        let w = self.size.0;
        let (x_range, y_range) = (
            offset.0..(size.0 + offset.0),
            (offset.1 * w)..((offset.1 * w) + (w * size.1)),
        );
        let mut new_data = vec![Color::<SPACE, WHITE>::new([0.0, 0.0, 0.0, 0.0]); 0];
        for row in self.data[y_range].chunks_exact(w) {
            let new = row.to_vec()[x_range.clone()].to_vec();
            new_data.extend(new);
        }
        Image {
            data: new_data,
            size,
        }
    }

    /// Return a cropped region aligned
    pub fn crop_align(
        &self,
        mode: (Align, Align),
        size: (usize, usize),
    ) -> Image<SPACE, WHITE> {
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

    /// Return the min and max colors
    pub fn range(&self) -> (Color<SPACE, WHITE>, Color<SPACE, WHITE>) {
        let (mut min, mut max) = ([100000.0; 4], [-100000.0; 4]);
        for pixel in self.pixels() {
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
            Color::<SPACE, WHITE>::new([min[0], min[1], min[2], min[3]]),
            Color::<SPACE, WHITE>::new([max[0], max[1], max[2], max[3]]),
        )
    }

    /// Return the min and max colors in a given color space
    pub fn range_in_space<NEWSPACE: ColorType + FromColorType<SPACE> + Clone + Copy>(
        &self,
    ) -> (Color<NEWSPACE, WHITE>, Color<NEWSPACE, WHITE>) {
        let (mut min, mut max) = ([100000.0; 4], [-100000.0; 4]);
        for pixel in self.pixels() {
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
            Color::<NEWSPACE, WHITE>::new([min[0], min[1], min[2], min[3]]),
            Color::<NEWSPACE, WHITE>::new([max[0], max[1], max[2], max[3]]),
        )
    }

    /// The mean of all colors
    pub fn mean(&self) -> Color<SPACE, WHITE> {
        let mut avg = Color::<SPACE, WHITE>::new([0.0, 0.0, 0.0, 0.0]);
        let mut i = 0;
        for color in self.pixels() {
            i += 1;
            avg += *color;
        }
        avg / i as f64
    }

    /// Compute the mean of all colors for two images at once
    pub fn mean_with(
        &self,
        image: &Image<SPACE, WHITE>,
    ) -> (Color<SPACE, WHITE>, Color<SPACE, WHITE>) {
        let mut avg = (
            Color::<SPACE, WHITE>::new([0.0, 0.0, 0.0, 0.0]),
            Color::<SPACE, WHITE>::new([0.0, 0.0, 0.0, 0.0]),
        );
        let mut i = 0;
        for color in self.pixels_zip(image) {
            i += 1;
            avg.0 += *color.0;
            avg.1 += *color.1;
        }
        (avg.0 / i as f64, avg.1 / i as f64)
    }

    /// The mean of all colors in a given color space
    pub fn mean_in_space<NEWSPACE: ColorType + FromColorType<SPACE> + Clone + Copy>(
        &self,
    ) -> Color<NEWSPACE, WHITE> {
        let mut avg = Color::<NEWSPACE, WHITE>::new([0.0, 0.0, 0.0, 0.0]);
        let mut i = 0;
        for color in self.pixels() {
            i += 1;
            avg += NEWSPACE::from_color(*color);
        }
        avg / i as f64
    }

    /// Return the variance of color
    pub fn variance(&self) -> Color<SPACE, WHITE> {
        let mean = self.mean();

        let mut v = Color::<SPACE, WHITE>::new([0.0, 0.0, 0.0, 0.0]);
        let mut i = 0;
        for color in self.pixels() {
            i += 1;
            let diff = *color - mean;
            v += diff * diff;
        }

        v / i as f64
    }

    /// Return the variance of color in two images at once
    pub fn variance_with(
        &self,
        image: &Image<SPACE, WHITE>,
    ) -> (Color<SPACE, WHITE>, Color<SPACE, WHITE>) {
        let mean = self.mean_with(image);

        let mut v = (
            Color::<SPACE, WHITE>::new([0.0, 0.0, 0.0, 0.0]),
            Color::<SPACE, WHITE>::new([0.0, 0.0, 0.0, 0.0]),
        );
        let mut i = 0;
        for color in self.pixels_zip(image) {
            i += 1;
            let diff = (*color.0 - mean.0, *color.1 - mean.1);
            v.0 += diff.0 * diff.0;
            v.1 += diff.1 * diff.1;
        }

        (v.0 / i as f64, v.1 / i as f64)
    }

    /// Return the variance of color in a given color space
    pub fn variance_in_space<NEWSPACE: ColorType + FromColorType<SPACE> + Clone + Copy>(
        &self,
    ) -> Color<NEWSPACE, WHITE> {
        let mean = self.mean_in_space::<NEWSPACE>();

        let mut v = Color::<NEWSPACE, WHITE>::new([0.0, 0.0, 0.0, 0.0]);
        let mut i = 0;
        for color in self.pixels() {
            i += 1;
            let diff = NEWSPACE::from_color(*color) - mean;
            v += diff * diff;
        }

        v / i as f64
    }

    /// Return the covariance of color with another Image
    pub fn covariance(&self, image: &Image<SPACE, WHITE>) -> Color<SPACE, WHITE> {
        let mean1 = self.mean();
        let mean2 = image.mean();

        let mut v = Color::<SPACE, WHITE>::new([0.0, 0.0, 0.0, 0.0]);
        let mut i = 0;
        for (color1, color2) in self.pixels_zip(image) {
            i += 1;
            v += (*color1 - mean1) * (*color2 - mean2);
        }

        v / i as f64
    }

    /// Return the covariance of color with another Image in a given color space
    pub fn covariance_in_space<
        NEWSPACE: 'static + ColorType + FromColorType<SPACE> + Clone + Copy,
    >(
        &self,
        image: &Image<SPACE, WHITE>,
    ) -> Color<NEWSPACE, WHITE> {
        let mean1 = self.mean_in_space::<NEWSPACE>();
        let mean2 = image.mean_in_space::<NEWSPACE>();

        let mut v = Color::<NEWSPACE, WHITE>::new([0.0, 0.0, 0.0, 0.0]);
        let mut i = 0;

        for (color1, color2) in self.pixels().zip(image.pixels()) {
            i += 1;
            v += (NEWSPACE::from_color(*color1) - mean1) * (NEWSPACE::from_color(*color2) - mean2);
        }

        v / i as f64
    }

    /// Structural similarity index with another image
    pub fn ssim(&self, image: &Image<SPACE, WHITE>) -> Color<SPACE, WHITE> {
        let meanx = self.mean();
        let meany = image.mean();

        let variancex = self.variance();
        let variancey = image.variance();

        let covariance = self.covariance(image);

        let (c1, c2) = (
            Color::new([(0.01 * (2f64.powi(16) - 1.0)).powi(2); 4]),
            Color::new([(0.03 * (2f64.powi(16) - 1.0)).powi(2); 4]),
        );

        (((2.0 * meanx * meany) + c1) * ((covariance + c2) * 2.0))
            / (((meanx * meanx) + (meany * meany) + c1) * (variancex + variancey + c2))
    }

    /// Structural similarity index with another image, in a specific color space
    pub fn ssim_in_space<NEWSPACE: ColorType>(
        &self,
        image: &Image<SPACE, WHITE>,
    ) -> Color<NEWSPACE, WHITE>
    where
        NEWSPACE: FromColorType<SPACE>,
    {
        let self_space = self.convert::<NEWSPACE>();
        let img_space = image.convert::<NEWSPACE>();

        let meanx = self_space.mean();
        let meany = img_space.mean();

        let variancex = self_space.variance();
        let variancey = img_space.variance();

        let covariance = self_space.covariance(&img_space);

        let (c1, c2) = (
            (0.01 * (2f64.powi(16) - 1.0)).powi(2),
            (0.03 * (2f64.powi(16) - 1.0)).powi(2),
        );

        (((2.0 * meanx * meany) + c1) * ((covariance + c2) * 2.0))
            / (((meanx * meanx) + (meany * meany) + c1) * (variancex + variancey + c2))
    }
}

impl<SPACE: ColorType + Clone + Copy, const WHITE: White> std::ops::Neg
    for Image<SPACE, WHITE>
{
    type Output = Image<SPACE, WHITE>;
    fn neg(self) -> Self::Output {
        let mut new_img = self;
        new_img.pixels_mut().for_each(|x| *x = -*x);
        new_img
    }
}

impl<SPACE: ColorType + Clone + Copy, const WHITE: White>
    std::ops::Add<Image<SPACE, WHITE>> for Image<SPACE, WHITE>
{
    type Output = Image<SPACE, WHITE>;
    fn add(self, rhs: Image<SPACE, WHITE>) -> Self::Output {
        let mut new_img = self;
        new_img.pixels_mut_zip(&rhs).for_each(|x| *x.0 += *x.1);
        new_img
    }
}

impl<SPACE: ColorType + Clone + Copy, const WHITE: White>
    std::ops::Sub<Image<SPACE, WHITE>> for Image<SPACE, WHITE>
{
    type Output = Image<SPACE, WHITE>;
    fn sub(self, rhs: Image<SPACE, WHITE>) -> Self::Output {
        let mut new_img = self;
        new_img.pixels_mut_zip(&rhs).for_each(|x| *x.0 -= *x.1);
        new_img
    }
}

impl<SPACE: ColorType + Clone + Copy, const WHITE: White>
    std::ops::Mul<Image<SPACE, WHITE>> for Image<SPACE, WHITE>
{
    type Output = Image<SPACE, WHITE>;
    fn mul(self, rhs: Image<SPACE, WHITE>) -> Self::Output {
        let mut new_img = self;
        new_img.pixels_mut_zip(&rhs).for_each(|x| *x.0 *= *x.1);
        new_img
    }
}

impl<SPACE: ColorType + Clone + Copy, const WHITE: White>
    std::ops::Div<Image<SPACE, WHITE>> for Image<SPACE, WHITE>
{
    type Output = Image<SPACE, WHITE>;
    fn div(self, rhs: Image<SPACE, WHITE>) -> Self::Output {
        let mut new_img = self;
        new_img.pixels_mut_zip(&rhs).for_each(|x| *x.0 /= *x.1);
        new_img
    }
}

impl<SPACE: ColorType + Clone + Copy, const WHITE: White>
    std::ops::AddAssign<Image<SPACE, WHITE>> for Image<SPACE, WHITE>
{
    fn add_assign(&mut self, rhs: Image<SPACE, WHITE>) {
        self.pixels_mut_zip(&rhs).for_each(|x| *x.0 += *x.1);
    }
}

impl<SPACE: ColorType + Clone + Copy, const WHITE: White>
    std::ops::SubAssign<Image<SPACE, WHITE>> for Image<SPACE, WHITE>
{
    fn sub_assign(&mut self, rhs: Image<SPACE, WHITE>) {
        self.pixels_mut_zip(&rhs).for_each(|x| *x.0 -= *x.1);
    }
}

impl<SPACE: ColorType + Clone + Copy, const WHITE: White>
    std::ops::MulAssign<Image<SPACE, WHITE>> for Image<SPACE, WHITE>
{
    fn mul_assign(&mut self, rhs: Image<SPACE, WHITE>) {
        self.pixels_mut_zip(&rhs).for_each(|x| *x.0 *= *x.1);
    }
}

impl<SPACE: ColorType + Clone + Copy, const WHITE: White>
    std::ops::DivAssign<Image<SPACE, WHITE>> for Image<SPACE, WHITE>
{
    fn div_assign(&mut self, rhs: Image<SPACE, WHITE>) {
        self.pixels_mut_zip(&rhs).for_each(|x| *x.0 /= *x.1);
    }
}

impl<SPACE: ColorType + Clone + Copy, const WHITE: White>
    std::ops::Add<Color<SPACE, WHITE>> for Image<SPACE, WHITE>
{
    type Output = Image<SPACE, WHITE>;
    fn add(self, rhs: Color<SPACE, WHITE>) -> Self::Output {
        let mut new_img = self;
        new_img.pixels_mut().for_each(|x| *x += rhs);
        new_img
    }
}

impl<SPACE: ColorType + Clone + Copy, const WHITE: White>
    std::ops::Sub<Color<SPACE, WHITE>> for Image<SPACE, WHITE>
{
    type Output = Image<SPACE, WHITE>;
    fn sub(self, rhs: Color<SPACE, WHITE>) -> Self::Output {
        let mut new_img = self;
        new_img.pixels_mut().for_each(|x| *x -= rhs);
        new_img
    }
}

impl<SPACE: ColorType + Clone + Copy, const WHITE: White>
    std::ops::Mul<Color<SPACE, WHITE>> for Image<SPACE, WHITE>
{
    type Output = Image<SPACE, WHITE>;
    fn mul(self, rhs: Color<SPACE, WHITE>) -> Self::Output {
        let mut new_img = self;
        new_img.pixels_mut().for_each(|x| *x *= rhs);
        new_img
    }
}

impl<SPACE: ColorType + Clone + Copy, const WHITE: White>
    std::ops::Div<Color<SPACE, WHITE>> for Image<SPACE, WHITE>
{
    type Output = Image<SPACE, WHITE>;
    fn div(self, rhs: Color<SPACE, WHITE>) -> Self::Output {
        let mut new_img = self;
        new_img.pixels_mut().for_each(|x| *x /= rhs);
        new_img
    }
}

impl<SPACE: ColorType + Clone + Copy, const WHITE: White>
    std::ops::AddAssign<Color<SPACE, WHITE>> for Image<SPACE, WHITE>
{
    fn add_assign(&mut self, rhs: Color<SPACE, WHITE>) {
        self.pixels_mut().for_each(|x| *x += rhs);
    }
}

impl<SPACE: ColorType + Clone + Copy, const WHITE: White>
    std::ops::SubAssign<Color<SPACE, WHITE>> for Image<SPACE, WHITE>
{
    fn sub_assign(&mut self, rhs: Color<SPACE, WHITE>) {
        self.pixels_mut().for_each(|x| *x -= rhs);
    }
}

impl<SPACE: ColorType + Clone + Copy, const WHITE: White>
    std::ops::MulAssign<Color<SPACE, WHITE>> for Image<SPACE, WHITE>
{
    fn mul_assign(&mut self, rhs: Color<SPACE, WHITE>) {
        self.pixels_mut().for_each(|x| *x *= rhs);
    }
}

impl<SPACE: ColorType + Clone + Copy, const WHITE: White>
    std::ops::DivAssign<Color<SPACE, WHITE>> for Image<SPACE, WHITE>
{
    fn div_assign(&mut self, rhs: Color<SPACE, WHITE>) {
        self.pixels_mut().for_each(|x| *x /= rhs);
    }
}

impl<SPACE: ColorType + Clone + Copy, const WHITE: White>
    std::ops::Mul<f64> for Image<SPACE, WHITE>
{
    type Output = Image<SPACE, WHITE>;
    fn mul(self, rhs: f64) -> Self::Output {
        let mut new_img = self;
        new_img.pixels_mut().for_each(|x| *x *= rhs);
        new_img
    }
}

impl<SPACE: ColorType + Clone + Copy, const WHITE: White>
    std::ops::Div<f64> for Image<SPACE, WHITE>
{
    type Output = Image<SPACE, WHITE>;
    fn div(self, rhs: f64) -> Self::Output {
        let mut new_img = self;
        new_img.pixels_mut().for_each(|x| *x /= rhs);
        new_img
    }
}

impl<SPACE: ColorType + Clone + Copy, const WHITE: White>
    std::ops::MulAssign<f64> for Image<SPACE, WHITE>
{
    fn mul_assign(&mut self, rhs: f64) {
        self.pixels_mut().for_each(|x| *x *= rhs);
    }
}

impl<SPACE: ColorType + Clone + Copy, const WHITE: White>
    std::ops::DivAssign<f64> for Image<SPACE, WHITE>
{
    fn div_assign(&mut self, rhs: f64) {
        self.pixels_mut().for_each(|x| *x /= rhs);
    }
}

impl<SPACE: ColorType + Clone + Copy, const WHITE: White>
    std::ops::Div<Image<SPACE, WHITE>> for f64
{
    type Output = Image<SPACE, WHITE>;
    fn div(self, rhs: Image<SPACE, WHITE>) -> Self::Output {
        let mut new_img = rhs;
        for pixel in new_img.pixels_mut() {
            *pixel *= 1.0 / self;
        }
        new_img
    }
}

impl<SPACE: ColorType + Clone + Copy, const WHITE: White>
    std::ops::Mul<Image<SPACE, WHITE>> for f64
{
    type Output = Image<SPACE, WHITE>;
    fn mul(self, rhs: Image<SPACE, WHITE>) -> Self::Output {
        let mut new_img = rhs;
        for pixel in new_img.pixels_mut() {
            *pixel *= self;
        }
        new_img
    }
}

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
