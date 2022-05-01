use super::types::{Color, ColorType, RefWhite};
use std::{fmt, marker::PhantomData};

impl<SPACE: ColorType, const WHITE: RefWhite> fmt::Display for Color<SPACE, WHITE> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "[{:.4}, {:.4}, {:.4}, {:.4}]",
            self.0, self.1, self.2, self.3
        )
    }
}
impl<SPACE: ColorType, const WHITE: RefWhite> Default for Color<SPACE, WHITE> {
    fn default() -> Color<SPACE, WHITE> {
        Color::new([0.0, 0.0, 0.0, 1.0])
    }
}

#[allow(dead_code)]
impl<SPACE: ColorType, const WHITE: RefWhite> Color<SPACE, WHITE> {
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

impl<SPACE: ColorType, const WHITE: RefWhite> std::ops::Add<Color<SPACE, WHITE>>
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

impl<SPACE: ColorType, const WHITE: RefWhite> std::ops::AddAssign<Color<SPACE, WHITE>>
    for Color<SPACE, WHITE>
{
    fn add_assign(&mut self, rhs: Color<SPACE, WHITE>) {
        self.0 += rhs.0;
        self.1 += rhs.1;
        self.2 += rhs.2;
        self.3 += rhs.3;
    }
}

impl<SPACE: ColorType, const WHITE: RefWhite> std::ops::Sub<Color<SPACE, WHITE>>
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

impl<SPACE: ColorType, const WHITE: RefWhite> std::ops::SubAssign<Color<SPACE, WHITE>>
    for Color<SPACE, WHITE>
{
    fn sub_assign(&mut self, rhs: Color<SPACE, WHITE>) {
        self.0 -= rhs.0;
        self.1 -= rhs.1;
        self.2 -= rhs.2;
        self.3 -= rhs.3;
    }
}

impl<SPACE: ColorType, const WHITE: RefWhite> std::ops::Mul<Color<SPACE, WHITE>>
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

impl<SPACE: ColorType, const WHITE: RefWhite> std::ops::MulAssign<Color<SPACE, WHITE>>
    for Color<SPACE, WHITE>
{
    fn mul_assign(&mut self, rhs: Color<SPACE, WHITE>) {
        self.0 *= rhs.0;
        self.1 *= rhs.1;
        self.2 *= rhs.2;
        self.3 *= rhs.3;
    }
}

impl<SPACE: ColorType, const WHITE: RefWhite> std::ops::Div<Color<SPACE, WHITE>>
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

impl<SPACE: ColorType, const WHITE: RefWhite> std::ops::DivAssign<Color<SPACE, WHITE>>
    for Color<SPACE, WHITE>
{
    fn div_assign(&mut self, rhs: Color<SPACE, WHITE>) {
        self.0 /= rhs.0;
        self.1 /= rhs.1;
        self.2 /= rhs.2;
        self.3 /= rhs.3;
    }
}

impl<SPACE: ColorType, const WHITE: RefWhite> std::ops::Mul<f64> for Color<SPACE, WHITE> {
    type Output = Color<SPACE, WHITE>;
    fn mul(self, rhs: f64) -> Color<SPACE, WHITE> {
        Self::new([self.0 * rhs, self.1 * rhs, self.2 * rhs, self.3 * rhs])
    }
}

impl<SPACE: ColorType, const WHITE: RefWhite> std::ops::MulAssign<f64> for Color<SPACE, WHITE> {
    // type Output = Color<SPACE, WHITE>;
    fn mul_assign(&mut self, rhs: f64) {
        self.0 *= rhs;
        self.1 *= rhs;
        self.2 *= rhs;
        self.3 *= rhs;
    }
}

impl<SPACE: ColorType, const WHITE: RefWhite> std::ops::Mul<Color<SPACE, WHITE>> for f64 {
    type Output = Color<SPACE, WHITE>;
    fn mul(self, rhs: Color<SPACE, WHITE>) -> Color<SPACE, WHITE> {
        Color::new([self * rhs.0, self * rhs.1, self * rhs.2, self * rhs.3])
    }
}

impl<SPACE: ColorType, const WHITE: RefWhite> std::ops::Div<f64> for Color<SPACE, WHITE> {
    type Output = Color<SPACE, WHITE>;
    fn div(self, rhs: f64) -> Color<SPACE, WHITE> {
        Self::new([self.0 / rhs, self.1 / rhs, self.2 / rhs, self.3 / rhs])
    }
}

impl<SPACE: ColorType, const WHITE: RefWhite> std::ops::DivAssign<f64> for Color<SPACE, WHITE> {
    // type Output = Color<SPACE, WHITE>;
    fn div_assign(&mut self, rhs: f64) {
        self.0 /= rhs;
        self.1 /= rhs;
        self.2 /= rhs;
        self.3 /= rhs;
    }
}

impl<SPACE: ColorType, const WHITE: RefWhite> std::ops::Div<Color<SPACE, WHITE>> for f64 {
    type Output = Color<SPACE, WHITE>;
    fn div(self, rhs: Color<SPACE, WHITE>) -> Color<SPACE, WHITE> {
        Color::new([self / rhs.0, self / rhs.1, self / rhs.2, self / rhs.3])
    }
}
