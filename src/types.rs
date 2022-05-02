use std::{fmt, marker::PhantomData};

use crate::colors::CIELaba;

pub trait ColorType {}

/// Method for safely converting between ColorType structs
pub trait FromColorType<SPACE>: ColorType
where
    SPACE: ColorType + Clone + Copy,
{
    fn from_color<const WHITE: RefWhite>(_: Color<SPACE, WHITE>) -> Color<Self, WHITE>
    where
        Self: std::marker::Sized;
}

/// An abstract color type that has operator overloading
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub struct Color<SPACE: ColorType, const WHITE: RefWhite>(
    pub f64,
    pub f64,
    pub f64,
    pub f64,
    pub std::marker::PhantomData<SPACE>,
);

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

///

#[allow(dead_code)]
/// The reference white point of the color. Default: D65
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Hash)]
pub enum RefWhite {
    D50,
    D65,
    E,
    Custom(u16),
}

/// Gamut may change a little bit during development
pub trait Gamut {
    fn white(&self) -> RefWhite;
}

/// ColorGamut may change a little bit during development
///
/// This holds tristimulus values for color specifications
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub struct ColorGamut<const WHITE: RefWhite> {
    pub gamma: f64,
    /// The color space primaries
    pub primaries_xyy: [Col3; 3],
    pub transfer_inv: fn(v: f64) -> f64,
    pub transfer: fn(v: f64) -> f64,
    pub conversion: Mat3,
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
    pub fn new(c: [f64; 3]) -> Self {
        Col3(c[0], c[1], c[2])
    }
    pub fn mult(&self, rhs: Col3) -> Col3 {
        Col3(self.0 * rhs.0, self.1 * rhs.1, self.2 * rhs.2)
    }
    pub fn div(&self, rhs: Col3) -> Col3 {
        Col3(
            self.0 * rhs.0.recip(),
            self.1 * rhs.1.recip(),
            self.2 * rhs.2.recip(),
        )
    }
    pub fn over(&self, f: fn(x: f64) -> f64) -> Col3 {
        Col3(f(self.0), f(self.1), f(self.2))
    }
    pub fn to_arr(&self) -> [f64; 3] {
        [self.0, self.1, self.2]
    }
}

#[allow(dead_code)]
impl Mat3 {
    pub fn new(m: [[f64; 3]; 3]) -> Self {
        Mat3(
            Col3(m[0][0], m[0][1], m[0][2]),
            Col3(m[1][0], m[1][1], m[1][2]),
            Col3(m[2][0], m[2][1], m[2][2]),
        )
    }
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
    pub fn identity() -> Mat3 {
        Mat3(Col3(1., 0., 0.), Col3(0., 1., 0.), Col3(0., 0., 1.))
    }
    /// Grab a row
    pub fn row(&self, n: usize) -> &Col3 {
        [&self.0, &self.1, &self.2][n % 3]
    }
    pub fn row_assign(&mut self, n: usize, new_row: Col3) {
        *[&mut self.0, &mut self.1, &mut self.2][n % 3] = new_row;
    }
    /// Inverse of matrix using cross products and the triple product
    /// https://en.wikipedia.org/wiki/Invertible_matrix#Inversion_of_3_%C3%97_3_matrices
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
    pub fn over_columns(&self, rhs: Col3, f: fn(a: Col3, b: Col3) -> Col3) -> Mat3 {
        Mat3(f(self.0, rhs), f(self.1, rhs), f(self.2, rhs))
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
pub struct Image<SPACE: ColorType + Clone + Copy, const WHITE: RefWhite> {
    pub data: Vec<Color<SPACE, WHITE>>,
    pub size: (usize, usize),
}

#[allow(dead_code)]
impl<SPACE: ColorType + Clone + Copy, const WHITE: RefWhite> Image<SPACE, WHITE> {
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
    pub fn from_vec(size: (usize, usize), data: Vec<Color<SPACE, WHITE>>) -> Image<SPACE, WHITE> {
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
    pub fn pixels(&self) -> &Vec<Color<SPACE, WHITE>> {
        &self.data
    }
    /// Get the pixels as mutable
    pub fn pixels_mut(&mut self) -> &mut Vec<Color<SPACE, WHITE>> {
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
    pub fn put_pixel(&mut self, (x, y): (usize, usize), pixel: Color<SPACE, WHITE>) {
        let w = self.width();
        self.pixels_mut()[x + (y * w)] = pixel;
    }
    /// Get the pixel at (x,y)
    pub fn get_pixel(&self, (x, y): (usize, usize)) -> Color<SPACE, WHITE> {
        let w = self.width();
        self.pixels()[x + (y * w)]
    }
    /// Convert all colors in the image
    #[rustfmt::skip]
    pub fn convert<NEWSPACE: ColorType + FromColorType<SPACE> + Clone + Copy>(&self) -> Image<NEWSPACE, WHITE> {
        Image {
            data: self.pixels().iter().map(
                |x| <NEWSPACE as FromColorType<SPACE>>::from_color::<WHITE>(*x)
            ).collect::<Vec<Color<NEWSPACE, WHITE>>>(),
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
    pub fn crop_align(&self, mode: (Align, Align), size: (usize, usize)) -> Image<SPACE, WHITE> {
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
    /// The mean of all colors
    pub fn mean(&self) -> Color<SPACE, WHITE> {
        let mut avg = Color::<SPACE, WHITE>::new([0.0, 0.0, 0.0, 0.0]);
        let mut i = 0;
        for color in self.pixels().iter() {
            i += 1;
            avg += *color;
        }
        avg / i as f64
    }
    /// Return the variance of color
    pub fn variance(&self) -> Color<SPACE, WHITE> {
        let mean = self.mean();

        let mut v = Color::<SPACE, WHITE>::new([0.0, 0.0, 0.0, 0.0]);
        let mut i = 0;
        for color in self.pixels().iter() {
            i += 1;
            let diff = *color - mean;
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
        for (color1, color2) in self.pixels().iter().zip(image.pixels().iter()) {
            i += 1;
            v += (*color1 - mean1) * (*color2 - mean2);
        }

        v / i as f64
    }
    // pub fn stdev() -> Color<SPACE, WHITE> {}
    // pub fn luminance() -> f64 {
    //     0.0
    // }
}

impl<SPACE: ColorType + Clone + Copy, const WHITE: RefWhite> std::ops::Add<Image<SPACE, WHITE>>
    for Image<SPACE, WHITE>
{
    type Output = Image<SPACE, WHITE>;
    fn add(self, rhs: Image<SPACE, WHITE>) -> Self::Output {
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

impl<SPACE: ColorType + Clone + Copy, const WHITE: RefWhite> std::ops::Sub<Image<SPACE, WHITE>>
    for Image<SPACE, WHITE>
{
    type Output = Image<SPACE, WHITE>;
    fn sub(self, rhs: Image<SPACE, WHITE>) -> Self::Output {
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

impl<SPACE: ColorType + Clone + Copy, const WHITE: RefWhite> std::ops::Mul<Image<SPACE, WHITE>>
    for Image<SPACE, WHITE>
{
    type Output = Image<SPACE, WHITE>;
    fn mul(self, rhs: Image<SPACE, WHITE>) -> Self::Output {
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

impl<SPACE: ColorType + Clone + Copy, const WHITE: RefWhite> std::ops::Div<Image<SPACE, WHITE>>
    for Image<SPACE, WHITE>
{
    type Output = Image<SPACE, WHITE>;
    fn div(self, rhs: Image<SPACE, WHITE>) -> Self::Output {
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

impl<SPACE: ColorType + Clone + Copy, const WHITE: RefWhite> std::ops::Mul<f64>
    for Image<SPACE, WHITE>
{
    type Output = Image<SPACE, WHITE>;
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

impl<SPACE: ColorType + Clone + Copy, const WHITE: RefWhite> std::ops::Div<f64>
    for Image<SPACE, WHITE>
{
    type Output = Image<SPACE, WHITE>;
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

impl<SPACE: ColorType + Clone + Copy, const WHITE: RefWhite> std::ops::Div<Image<SPACE, WHITE>>
    for f64
{
    type Output = Image<SPACE, WHITE>;
    fn div(self, rhs: Image<SPACE, WHITE>) -> Self::Output {
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

impl<SPACE: ColorType + Clone + Copy, const WHITE: RefWhite> std::ops::Mul<Image<SPACE, WHITE>>
    for f64
{
    type Output = Image<SPACE, WHITE>;
    fn mul(self, rhs: Image<SPACE, WHITE>) -> Self::Output {
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
