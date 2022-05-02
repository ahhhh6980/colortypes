use crate::{methods::CIELaba, types::FromColorType};

use super::types::{Color, ColorType, RefWhite};
use std::{fmt, marker::PhantomData};

impl<SPACE: ColorType + Clone + Copy, const WHITE: RefWhite> fmt::Display for Color<SPACE, WHITE> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "[{:.4}, {:.4}, {:.4}, {:.4}]",
            self.0, self.1, self.2, self.3
        )
    }
}
impl<SPACE: ColorType + Clone + Copy, const WHITE: RefWhite> Default for Color<SPACE, WHITE> {
    fn default() -> Color<SPACE, WHITE> {
        Color::new([0.0, 0.0, 0.0, 1.0])
    }
}

#[allow(dead_code)]
impl<SPACE: ColorType + Clone + Copy, const WHITE: RefWhite> Color<SPACE, WHITE> {
    /// Return the reference white point
    pub fn white() -> RefWhite {
        WHITE
    }

    /// Construct a new color
    pub fn new(ch: [f64; 4]) -> Color<SPACE, WHITE> {
        Color::<SPACE, WHITE>(ch[0], ch[1], ch[2], ch[3], PhantomData)
    }

    /// Color to (\[f64;4\], RefWhite, ColorSpace)
    pub fn to_arr(&self) -> ([f64; 4], RefWhite) {
        ([self.0, self.1, self.2, self.3], WHITE)
    }

    /// Color to (\[u8;4\], RefWhite, ColorSpace)
    pub fn to_arr8(&self) -> ([u8; 4], RefWhite) {
        (
            [
                (self.0 * u8::MAX as f64).round() as u8,
                (self.1 * u8::MAX as f64).round() as u8,
                (self.2 * u8::MAX as f64).round() as u8,
                (self.3 * u8::MAX as f64).round() as u8,
            ],
            WHITE,
        )
    }

    /// Color to (\[u16;4\], RefWhite, ColorSpace)
    pub fn to_arr16(&self) -> ([u16; 4], RefWhite) {
        (
            [
                (self.0 * u16::MAX as f64).round() as u16,
                (self.1 * u16::MAX as f64).round() as u16,
                (self.2 * u16::MAX as f64).round() as u16,
                (self.3 * u16::MAX as f64).round() as u16,
            ],
            WHITE,
        )
    }

    // Color difference for acceptability
    pub fn delta_e_a(self, color: Color<SPACE, WHITE>) -> f64
    where
        CIELaba: FromColorType<SPACE>,
    {
        self.delta_e(color, (2.0, 1.0))
    }

    // Color difference for perceptibility
    pub fn delta_e_p(self, color: Color<SPACE, WHITE>) -> f64
    where
        CIELaba: FromColorType<SPACE>,
    {
        self.delta_e(color, (1.0, 1.0))
    }

    /// Color difference
    pub fn delta_e(self, color: Color<SPACE, WHITE>, (l, c): (f64, f64)) -> f64
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
    pub fn within_u16_sqrd_precision_of(&self, rhs: Color<SPACE, WHITE>) -> [bool; 4] {
        [
            (self.0 - rhs.0).abs() < (u16::MAX as f64).powi(2).recip(),
            (self.1 - rhs.1).abs() < (u16::MAX as f64).powi(2).recip(),
            (self.2 - rhs.2).abs() < (u16::MAX as f64).powi(2).recip(),
            (self.3 - rhs.3).abs() < (u16::MAX as f64).powi(2).recip(),
        ]
    }
}

impl<SPACE: ColorType + Clone + Copy, const WHITE: RefWhite> std::ops::Add<Color<SPACE, WHITE>>
    for Color<SPACE, WHITE>
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

impl<SPACE: ColorType + Clone + Copy, const WHITE: RefWhite>
    std::ops::AddAssign<Color<SPACE, WHITE>> for Color<SPACE, WHITE>
{
    fn add_assign(&mut self, rhs: Color<SPACE, WHITE>) {
        self.0 += rhs.0;
        self.1 += rhs.1;
        self.2 += rhs.2;
        self.3 += rhs.3;
    }
}

impl<SPACE: ColorType + Clone + Copy, const WHITE: RefWhite> std::ops::Sub<Color<SPACE, WHITE>>
    for Color<SPACE, WHITE>
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

impl<SPACE: ColorType + Clone + Copy, const WHITE: RefWhite>
    std::ops::SubAssign<Color<SPACE, WHITE>> for Color<SPACE, WHITE>
{
    fn sub_assign(&mut self, rhs: Color<SPACE, WHITE>) {
        self.0 -= rhs.0;
        self.1 -= rhs.1;
        self.2 -= rhs.2;
        self.3 -= rhs.3;
    }
}

impl<SPACE: ColorType + Clone + Copy, const WHITE: RefWhite> std::ops::Mul<Color<SPACE, WHITE>>
    for Color<SPACE, WHITE>
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

impl<SPACE: ColorType + Clone + Copy, const WHITE: RefWhite>
    std::ops::MulAssign<Color<SPACE, WHITE>> for Color<SPACE, WHITE>
{
    fn mul_assign(&mut self, rhs: Color<SPACE, WHITE>) {
        self.0 *= rhs.0;
        self.1 *= rhs.1;
        self.2 *= rhs.2;
        self.3 *= rhs.3;
    }
}

impl<SPACE: ColorType + Clone + Copy, const WHITE: RefWhite> std::ops::Div<Color<SPACE, WHITE>>
    for Color<SPACE, WHITE>
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

impl<SPACE: ColorType + Clone + Copy, const WHITE: RefWhite>
    std::ops::DivAssign<Color<SPACE, WHITE>> for Color<SPACE, WHITE>
{
    fn div_assign(&mut self, rhs: Color<SPACE, WHITE>) {
        self.0 /= rhs.0;
        self.1 /= rhs.1;
        self.2 /= rhs.2;
        self.3 /= rhs.3;
    }
}

impl<SPACE: ColorType + Clone + Copy, const WHITE: RefWhite> std::ops::Mul<f64>
    for Color<SPACE, WHITE>
{
    type Output = Color<SPACE, WHITE>;
    fn mul(self, rhs: f64) -> Color<SPACE, WHITE> {
        Self::new([self.0 * rhs, self.1 * rhs, self.2 * rhs, self.3 * rhs])
    }
}

impl<SPACE: ColorType + Clone + Copy, const WHITE: RefWhite> std::ops::MulAssign<f64>
    for Color<SPACE, WHITE>
{
    // type Output = Color<SPACE, WHITE>;
    fn mul_assign(&mut self, rhs: f64) {
        self.0 *= rhs;
        self.1 *= rhs;
        self.2 *= rhs;
        self.3 *= rhs;
    }
}

impl<SPACE: ColorType + Clone + Copy, const WHITE: RefWhite> std::ops::Mul<Color<SPACE, WHITE>>
    for f64
{
    type Output = Color<SPACE, WHITE>;
    fn mul(self, rhs: Color<SPACE, WHITE>) -> Color<SPACE, WHITE> {
        Color::new([self * rhs.0, self * rhs.1, self * rhs.2, self * rhs.3])
    }
}

impl<SPACE: ColorType + Clone + Copy, const WHITE: RefWhite> std::ops::Div<f64>
    for Color<SPACE, WHITE>
{
    type Output = Color<SPACE, WHITE>;
    fn div(self, rhs: f64) -> Color<SPACE, WHITE> {
        Self::new([self.0 / rhs, self.1 / rhs, self.2 / rhs, self.3 / rhs])
    }
}

impl<SPACE: ColorType + Clone + Copy, const WHITE: RefWhite> std::ops::DivAssign<f64>
    for Color<SPACE, WHITE>
{
    // type Output = Color<SPACE, WHITE>;
    fn div_assign(&mut self, rhs: f64) {
        self.0 /= rhs;
        self.1 /= rhs;
        self.2 /= rhs;
        self.3 /= rhs;
    }
}

impl<SPACE: ColorType + Clone + Copy, const WHITE: RefWhite> std::ops::Div<Color<SPACE, WHITE>>
    for f64
{
    type Output = Color<SPACE, WHITE>;
    fn div(self, rhs: Color<SPACE, WHITE>) -> Color<SPACE, WHITE> {
        Color::new([self / rhs.0, self / rhs.1, self / rhs.2, self / rhs.3])
    }
}
