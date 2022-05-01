use super::types::{Col3, Mat3};
use std::fmt;
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

impl Default for Mat3 {
    fn default() -> Mat3 {
        Mat3::identity()
    }
}

impl fmt::Display for Col3 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "│{: >+9.4} {: ^+9.4} {: <+9.4}│", self.0, self.1, self.2)
    }
}

impl Default for Col3 {
    fn default() -> Col3 {
        Col3(0.0, 0.0, 0.0)
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
        Col3(self.0 / rhs.0, self.1 / rhs.1, self.2 / rhs.2)
    }
    pub fn over(&self, f: fn(x: f64) -> f64) -> Col3 {
        Col3(f(self.0), f(self.1), f(self.2))
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
    /// Return the 3x3 identity matrix
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
    /// <https://en.wikipedia.org/wiki/Invertible_matrix#Inversion_of_3_%C3%97_3_matrices>
    pub fn inverse(&self) -> Mat3 {
        let det: f64 = self.0 * (self.1 ^ self.2);
        Mat3(
            (self.1 ^ self.2) / det,
            (self.2 ^ self.0) / det,
            (self.0 ^ self.1) / det,
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
            (self.1 * rhs.2) - (self.2 * rhs.1),
          -((self.0 * rhs.2) - (self.2 * rhs.0)),
            (self.0 * rhs.1) - (self.1 * rhs.0),
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
        Col3(self.0 / rhs, self.1 / rhs, self.2 / rhs)
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
        Col3(self / rhs.0, self / rhs.1, self / rhs.2)
    }
}
