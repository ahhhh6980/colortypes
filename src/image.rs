use super::types::{Align, Color, ColorType, FromColorType, Image, ImageInit, RefWhite};
#[allow(dead_code)]
impl<SPACE: ColorType + Clone, const WHITE: RefWhite> Image<SPACE, WHITE> {
    fn default() -> Image<SPACE, WHITE> {
        Image {
            data: Vec::new(),
            size: (0, 0),
        }
    }
    pub fn new((width, height): (usize, usize)) -> Image<SPACE, WHITE> {
        Image {
            data: vec![Color::new([0.0, 0.0, 0.0, 1.0]); width * height],
            size: (width, height),
        }
    }
    pub fn new_with(
        (width, height): (usize, usize),
        fill: ImageInit<SPACE, WHITE>,
    ) -> Image<SPACE, WHITE> {
        Image {
            data: vec![
                match fill {
                    ImageInit::Zero => Color::new([0.0, 0.0, 0.0, 1.0]),
                    ImageInit::One => Color::new([1.0, 1.0, 1.0, 1.0]),
                    ImageInit::Custom(color) => color,
                };
                width * height
            ],
            size: (width, height),
        }
    }
    pub fn from_vec(size: (usize, usize), data: Vec<Color<SPACE, WHITE>>) -> Image<SPACE, WHITE> {
        Image { data, size }
    }
    pub fn width(&self) -> usize {
        self.size.0
    }
    pub fn height(&self) -> usize {
        self.size.1
    }
    pub fn pixels(&self) -> &Vec<Color<SPACE, WHITE>> {
        &self.data
    }
    pub fn pixels_mut(&mut self) -> &mut Vec<Color<SPACE, WHITE>> {
        &mut self.data
    }
    pub fn to_vec(&self) -> Vec<f64> {
        let mut vec: Vec<f64> = Vec::new();
        for col in self.data.iter() {
            vec.extend(col.to_arr().0);
        }
        vec
    }
    pub fn put_pixel(&mut self, (x, y): (usize, usize), pixel: Color<SPACE, WHITE>) {
        let w = self.width();
        self.pixels_mut()[x + (y * w)] = pixel;
    }
    pub fn get_pixel(&self, (x, y): (usize, usize)) -> Color<SPACE, WHITE> {
        let w = self.width();
        self.pixels()[x + (y * w)].clone()
    }
    #[rustfmt::skip]
    pub fn convert<NEWSPACE: ColorType + FromColorType<SPACE>>(&self) -> Image<NEWSPACE, WHITE> {
        Image {
            data: self.pixels().iter().map(
                |x| <NEWSPACE as FromColorType<SPACE>>::from_color::<WHITE>(x.clone())
            ).collect::<Vec<Color<NEWSPACE, WHITE>>>(),
            size: self.size,
        }
    }
    pub fn crop(&mut self, offset: (usize, usize), size: (usize, usize)) {
        let w = self.size.0;
        let (x_range, y_range) = (
            offset.0..(size.0 + offset.0),
            (offset.1 * w)..((offset.1 * w) + (w * size.1)),
        );
        let mut new_data = vec![Color::<SPACE, WHITE>::new([0.0, 0.0, 0.0, 0.0]); 0];
        for row in self.pixels_mut()[y_range].chunks_exact_mut(w) {
            let new = row.to_vec()[x_range.clone()].to_vec();
            new_data.extend(new);
        }
        self.size = size;
        self.data = new_data;
    }
    pub fn crop_align(&mut self, mode: (Align, Align), size: (usize, usize)) {
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
        self.crop(offset, size);
    }
}

impl<SPACE: ColorType + Clone, const WHITE: RefWhite> std::ops::Add<Image<SPACE, WHITE>>
    for Image<SPACE, WHITE>
{
    type Output = Image<SPACE, WHITE>;
    fn add(self, rhs: Image<SPACE, WHITE>) -> Self::Output {
        let mut new_img = Image {
            data: self.pixels().clone(),
            size: self.size,
        };
        for (pixel, rhs_px) in new_img.pixels_mut().iter_mut().zip(rhs.pixels().iter()) {
            *pixel += rhs_px.clone();
        }
        new_img
    }
}

impl<SPACE: ColorType + Clone, const WHITE: RefWhite> std::ops::Sub<Image<SPACE, WHITE>>
    for Image<SPACE, WHITE>
{
    type Output = Image<SPACE, WHITE>;
    fn sub(self, rhs: Image<SPACE, WHITE>) -> Self::Output {
        let mut new_img = Image {
            data: self.pixels().clone(),
            size: self.size,
        };
        for (pixel, rhs_px) in new_img.pixels_mut().iter_mut().zip(rhs.pixels().iter()) {
            *pixel -= rhs_px.clone();
        }
        new_img
    }
}

impl<SPACE: ColorType + Clone, const WHITE: RefWhite> std::ops::Mul<Image<SPACE, WHITE>>
    for Image<SPACE, WHITE>
{
    type Output = Image<SPACE, WHITE>;
    fn mul(self, rhs: Image<SPACE, WHITE>) -> Self::Output {
        let mut new_img = Image {
            data: self.pixels().clone(),
            size: self.size,
        };
        for (pixel, rhs_px) in new_img.pixels_mut().iter_mut().zip(rhs.pixels().iter()) {
            *pixel *= rhs_px.clone();
        }
        new_img
    }
}

impl<SPACE: ColorType + Clone, const WHITE: RefWhite> std::ops::Div<Image<SPACE, WHITE>>
    for Image<SPACE, WHITE>
{
    type Output = Image<SPACE, WHITE>;
    fn div(self, rhs: Image<SPACE, WHITE>) -> Self::Output {
        let mut new_img = Image {
            data: self.pixels().clone(),
            size: self.size,
        };
        for (pixel, rhs_px) in new_img.pixels_mut().iter_mut().zip(rhs.pixels().iter()) {
            *pixel /= rhs_px.clone();
        }
        new_img
    }
}

///

impl<SPACE: ColorType + Clone, const WHITE: RefWhite> std::ops::Mul<f64> for Image<SPACE, WHITE> {
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

impl<SPACE: ColorType + Clone, const WHITE: RefWhite> std::ops::Div<f64> for Image<SPACE, WHITE> {
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

impl<SPACE: ColorType + Clone, const WHITE: RefWhite> std::ops::Div<Image<SPACE, WHITE>> for f64 {
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

impl<SPACE: ColorType + Clone, const WHITE: RefWhite> std::ops::Mul<Image<SPACE, WHITE>> for f64 {
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
