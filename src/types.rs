pub trait ColorType {}

/// Method for safely converting between ColorType structs
pub trait FromColorType<SPACE>: ColorType
where
    SPACE: ColorType,
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

/// System may change a little bit during development
pub trait Gamut {
    fn white(&self) -> RefWhite;
}

/// ColorSpace may change a little bit during development
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

/// Image may change during development
///
/// An image that can do math and has operator overloading
#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Image<SPACE: ColorType, const WHITE: RefWhite> {
    pub data: Vec<Color<SPACE, WHITE>>,
    pub size: (usize, usize),
}

/// ImageInit is very likely to change during development
///
/// What kind of values to init Image with
#[allow(dead_code)]
pub enum ImageInit<SPACE: ColorType, const WHITE: RefWhite> {
    Zero,
    One,
    Custom(Color<SPACE, WHITE>),
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
