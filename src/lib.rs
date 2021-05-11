#![deny(clippy::all, clippy::pedantic)]
#![allow(clippy::unused_self)]

use std::{
    convert::TryInto,
    error,
    ffi::{c_void, CStr},
    fmt::Display,
    marker::PhantomData,
    mem::MaybeUninit,
    slice,
    sync::atomic::{AtomicU8, Ordering},
};

use bitflags::bitflags;
use vtflib_sys as ffi;

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    InvalidLength,
    InvalidParameters,
    InvalidFormat,
    LengthOverflow,
    VtfLib(String),
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::InvalidLength => {
                f.write_str("image data has invalid length for the specified size")
            }
            Error::InvalidParameters => f.write_str("invalid parameters for the current image"),
            Error::InvalidFormat => f.write_str("invalid format or no image is loaded"),
            Error::LengthOverflow => f.write_str("length doesn't fit into `u32`"),
            Error::VtfLib(s) => f.write_str(s),
        }
    }
}

impl error::Error for Error {}

pub type Result<T> = std::result::Result<T, Error>;

#[cold]
unsafe fn get_last_error() -> Error {
    let cstr = CStr::from_ptr(ffi::vlGetLastError());
    Error::VtfLib(cstr.to_string_lossy().into_owned())
}

#[inline]
fn ffi_bool(bool: bool) -> ffi::vlBool {
    if bool {
        ffi::vlTrue
    } else {
        ffi::vlFalse
    }
}

#[inline]
unsafe fn ffi_result<P>(pointer: *mut P) -> Result<*mut P> {
    if pointer.is_null() {
        Err(get_last_error())
    } else {
        Ok(pointer)
    }
}

macro_rules! ffi_try {
    ($expr: expr) => {
        if $expr == ffi::vlFalse {
            return Err(get_last_error());
        }
    };
}

macro_rules! ffi_enum {
    (
        #[repr($repr:ident)]
        $(#[$meta:meta]) *
        $vis:vis enum $name:ident {
            $(
                $tag:ident = $value:path
            ),+
        }
    ) => {
        $(#[$meta] )*
        #[repr($repr)]
        $vis enum $name {
            $(
                $tag = $value
            ),*
        }

        impl $name {
            fn from_ffi(value: $repr) -> Option<Self> {
                match value {
                    $(
                        $value => Some(Self::$tag),
                    )*
                    _ => None,
                }
            }
        }
    };
}

macro_rules! ffi_getters {
    (
        $(
            $(#[$meta:meta]) *
            $vis:vis fn $name:ident(&self$(,$arg:ident: $argty:ty)*) -> $ty:ty => $expr:expr
        ),*
    ) => {
        $(
            $(#[$meta] )* #[must_use] $vis fn $name(&self$(, $arg: $argty)*) -> $ty {
                unsafe {
                    $expr
                }
            }
        )*
    };
}

macro_rules! ffi_setters {
    (
        $(
            $(#[$meta:meta]) *
            $vis:vis fn $name:ident(&mut self$(,$arg:ident: $argty:ty)*) => $stmt:stmt
        ),*
    ) => {
        $(
            $(#[$meta] )* $vis fn $name(&mut self$(, $arg: $argty)*) {
                unsafe {
                    $stmt
                };
            }
        )*
    };
}

macro_rules! builder {
    (
        $(
            $(#[$meta:meta]) *
            $vis:vis fn $name:ident(mut $self:ident$(,$arg:ident: $argty:ty)*) -> Self => $stmt:stmt
        ),*
    ) => {
        $(
            $(#[$meta] )* $vis fn $name(mut $self$(, $arg: $argty)*) -> Self {
                $stmt
                $self
            }
        )*
    };
}

// 0 if library is uninitialized
static UNINIT_COUNTER: AtomicU8 = AtomicU8::new(0);

fn uninit_decrement() {
    match UNINIT_COUNTER.fetch_sub(1, Ordering::AcqRel) {
        0 => panic!("uninit counter overflowed"),
        1 => unsafe {
            ffi::vlShutdown();
        },
        _ => {}
    }
}

#[repr(i32)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum IntegerOption {
    DxtQuality = ffi::tagVTFLibOption_VTFLIB_DXT_QUALITY,
    BluescreenMaskR = ffi::tagVTFLibOption_VTFLIB_BLUESCREEN_MASK_R,
    BluescreenMaskG = ffi::tagVTFLibOption_VTFLIB_BLUESCREEN_MASK_G,
    BluescreenMaskB = ffi::tagVTFLibOption_VTFLIB_BLUESCREEN_MASK_B,
    BluescreenClearR = ffi::tagVTFLibOption_VTFLIB_BLUESCREEN_CLEAR_R,
    BluescreenClearG = ffi::tagVTFLibOption_VTFLIB_BLUESCREEN_CLEAR_G,
    BluescreenClearB = ffi::tagVTFLibOption_VTFLIB_BLUESCREEN_CLEAR_B,
    VmtParseMode = ffi::tagVTFLibOption_VTFLIB_VMT_PARSE_MODE,
}

#[repr(i32)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FloatOption {
    LuminanceWeightR = ffi::tagVTFLibOption_VTFLIB_LUMINANCE_WEIGHT_R,
    LuminanceWeightG = ffi::tagVTFLibOption_VTFLIB_LUMINANCE_WEIGHT_G,
    LuminanceWeightB = ffi::tagVTFLibOption_VTFLIB_LUMINANCE_WEIGHT_B,
    Fp16HdrKey = ffi::tagVTFLibOption_VTFLIB_FP16_HDR_KEY,
    Fp16HdrShift = ffi::tagVTFLibOption_VTFLIB_FP16_HDR_SHIFT,
    Fp16HdrGamma = ffi::tagVTFLibOption_VTFLIB_FP16_HDR_GAMMA,
    UnsharpenRadius = ffi::tagVTFLibOption_VTFLIB_UNSHARPEN_RADIUS,
    UnsharpenAmount = ffi::tagVTFLibOption_VTFLIB_UNSHARPEN_AMOUNT,
    UnsharpenThreshold = ffi::tagVTFLibOption_VTFLIB_UNSHARPEN_THRESHOLD,
    XSharpenStrength = ffi::tagVTFLibOption_VTFLIB_XSHARPEN_STRENGTH,
    XSharpenThreshold = ffi::tagVTFLibOption_VTFLIB_XSHARPEN_THRESHOLD,
}

// *const () makes the struct !Send + !Sync, VTFLib is not thread safe
#[must_use]
#[derive(Debug)]
pub struct VtfLib(PhantomData<*const ()>);

impl VtfLib {
    /// Initialize the library.
    /// Returns [`None`] if it's already initialized.
    /// Uninitialization happens when both returned structs are dropped.
    #[must_use]
    pub fn new() -> Option<(Self, VtfGuard)> {
        UNINIT_COUNTER
            .compare_exchange(0, 2, Ordering::Acquire, Ordering::Acquire)
            .ok()?;
        unsafe {
            if ffi::vlInitialize() == ffi::vlFalse {
                UNINIT_COUNTER.store(0, Ordering::Release);
                return None;
            }
        }
        Some((Self(PhantomData), VtfGuard))
    }

    #[must_use]
    pub fn get_version() -> u32 {
        unsafe { ffi::vlGetVersion() }
    }

    #[must_use]
    #[allow(clippy::missing_panics_doc)]
    pub fn get_version_string() -> &'static str {
        unsafe { CStr::from_ptr(ffi::vlGetVersionString()) }
            .to_str()
            .unwrap()
    }

    #[must_use]
    pub fn get_integer(&self, option: IntegerOption) -> i32 {
        unsafe { ffi::vlGetInteger(option as i32) }
    }

    pub fn set_integer(&self, option: IntegerOption, value: i32) {
        unsafe {
            ffi::vlSetInteger(option as i32, value);
        }
    }

    #[must_use]
    pub fn get_float(&self, option: FloatOption) -> f32 {
        unsafe { ffi::vlGetFloat(option as i32) }
    }

    pub fn set_float(&self, option: FloatOption, value: f32) {
        unsafe {
            ffi::vlSetFloat(option as i32, value);
        }
    }

    /// # Panics
    ///
    /// Panics if the vtf creation fails. This should not happen.
    pub fn new_vtf_file(&self) -> VtfFile {
        let mut handle = 0;
        unsafe {
            assert_eq!(
                ffi::vlCreateImage(&mut handle),
                ffi::vlTrue,
                "image creation failed"
            );
        }
        VtfFile {
            marker: PhantomData,
            handle,
        }
    }
}

impl Drop for VtfLib {
    fn drop(&mut self) {
        uninit_decrement()
    }
}

/// Enforces that only one [`VtfFile`] is bound at a time
#[derive(Debug)]
pub struct VtfGuard;

impl Drop for VtfGuard {
    fn drop(&mut self) {
        uninit_decrement()
    }
}

ffi_enum! {
    #[repr(i32)]
    #[derive(Debug, Clone, Copy, PartialEq)]
    pub enum ImageFormat {
        Rgba8888 = ffi::tagVTFImageFormat_IMAGE_FORMAT_RGBA8888,
        Agbr8888 = ffi::tagVTFImageFormat_IMAGE_FORMAT_ABGR8888,
        Rgb888 = ffi::tagVTFImageFormat_IMAGE_FORMAT_RGB888,
        Bgr888 = ffi::tagVTFImageFormat_IMAGE_FORMAT_BGR888,
        Rgb565 = ffi::tagVTFImageFormat_IMAGE_FORMAT_RGB565,
        I8 = ffi::tagVTFImageFormat_IMAGE_FORMAT_I8,
        Ia88 = ffi::tagVTFImageFormat_IMAGE_FORMAT_IA88,
        P8 = ffi::tagVTFImageFormat_IMAGE_FORMAT_P8,
        A8 = ffi::tagVTFImageFormat_IMAGE_FORMAT_A8,
        Rgb888Bluescreen = ffi::tagVTFImageFormat_IMAGE_FORMAT_RGB888_BLUESCREEN,
        Bgr888Bluescreen = ffi::tagVTFImageFormat_IMAGE_FORMAT_BGR888_BLUESCREEN,
        Argb8888 = ffi::tagVTFImageFormat_IMAGE_FORMAT_ARGB8888,
        Bgra8888 = ffi::tagVTFImageFormat_IMAGE_FORMAT_BGRA8888,
        Dxt1 = ffi::tagVTFImageFormat_IMAGE_FORMAT_DXT1,
        Dxt3 = ffi::tagVTFImageFormat_IMAGE_FORMAT_DXT3,
        Dxt5 = ffi::tagVTFImageFormat_IMAGE_FORMAT_DXT5,
        Bgrx8888 = ffi::tagVTFImageFormat_IMAGE_FORMAT_BGRX8888,
        Bgr565 = ffi::tagVTFImageFormat_IMAGE_FORMAT_BGR565,
        Bgrx5551 = ffi::tagVTFImageFormat_IMAGE_FORMAT_BGRX5551,
        Bgra4444 = ffi::tagVTFImageFormat_IMAGE_FORMAT_BGRA4444,
        Dxt1OneBitAlpha = ffi::tagVTFImageFormat_IMAGE_FORMAT_DXT1_ONEBITALPHA,
        Bgra5551 = ffi::tagVTFImageFormat_IMAGE_FORMAT_BGRA5551,
        Uv88 = ffi::tagVTFImageFormat_IMAGE_FORMAT_UV88,
        Uvwq8888 = ffi::tagVTFImageFormat_IMAGE_FORMAT_UVWQ8888,
        Rgba16161616F = ffi::tagVTFImageFormat_IMAGE_FORMAT_RGBA16161616F,
        Rgba16161616 = ffi::tagVTFImageFormat_IMAGE_FORMAT_RGBA16161616,
        Uvlx8888 = ffi::tagVTFImageFormat_IMAGE_FORMAT_UVLX8888,
        R32F = ffi::tagVTFImageFormat_IMAGE_FORMAT_R32F,
        Rgb323232F = ffi::tagVTFImageFormat_IMAGE_FORMAT_RGB323232F,
        Rgba32323232F = ffi::tagVTFImageFormat_IMAGE_FORMAT_RGBA32323232F,
        NvDst16 = ffi::tagVTFImageFormat_IMAGE_FORMAT_NV_DST16,
        NvDst24 = ffi::tagVTFImageFormat_IMAGE_FORMAT_NV_DST24,
        NvIntz = ffi::tagVTFImageFormat_IMAGE_FORMAT_NV_INTZ,
        NvRawz = ffi::tagVTFImageFormat_IMAGE_FORMAT_NV_RAWZ,
        AtiDst16 = ffi::tagVTFImageFormat_IMAGE_FORMAT_ATI_DST16,
        AtiDst24 = ffi::tagVTFImageFormat_IMAGE_FORMAT_ATI_DST24,
        NvNull = ffi::tagVTFImageFormat_IMAGE_FORMAT_NV_NULL,
        Ati2N = ffi::tagVTFImageFormat_IMAGE_FORMAT_ATI2N,
        Ati1N = ffi::tagVTFImageFormat_IMAGE_FORMAT_ATI1N
    }
}

impl ImageFormat {
    #[must_use]
    pub fn info(self) -> Option<ImageFormatInfo> {
        let info = unsafe {
            let mut pointer = MaybeUninit::uninit();
            if ffi::vlImageGetImageFormatInfoEx(self as i32, pointer.as_mut_ptr()) == ffi::vlFalse {
                return None;
            }
            pointer.assume_init()
        };
        Some(ImageFormatInfo::new(info))
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ImageFormatInfo {
    name: &'static str,
    bits_per_pixel: u32,
    bytes_per_pixel: u32,
    red_bits_per_pixel: u32,
    green_bits_per_pixel: u32,
    blue_bits_per_pixel: u32,
    alpha_bits_per_pixel: u32,
    compressed: bool,
    supported: bool,
}

impl ImageFormatInfo {
    fn new(format: ffi::tagSVTFImageFormatInfo) -> Self {
        let name = unsafe { CStr::from_ptr(format.lpName).to_str().unwrap() };
        Self {
            name,
            bits_per_pixel: format.uiBitsPerPixel,
            bytes_per_pixel: format.uiBytesPerPixel,
            red_bits_per_pixel: format.uiRedBitsPerPixel,
            green_bits_per_pixel: format.uiGreenBitsPerPixel,
            blue_bits_per_pixel: format.uiBlueBitsPerPixel,
            alpha_bits_per_pixel: format.uiAlphaBitsPerPixel,
            compressed: format.bIsCompressed == ffi::vlTrue,
            supported: format.bIsSupported == ffi::vlTrue,
        }
    }
}

bitflags! {
    pub struct ImageFlags: u32 {
        const POINT_SAMPLE = ffi::tagVTFImageFlag_TEXTUREFLAGS_POINTSAMPLE as u32;
        const TRILINEAR = ffi::tagVTFImageFlag_TEXTUREFLAGS_TRILINEAR as u32;
        const CLAMP_S = ffi::tagVTFImageFlag_TEXTUREFLAGS_CLAMPS as u32;
        const CLAMP_T = ffi::tagVTFImageFlag_TEXTUREFLAGS_CLAMPT as u32;
        const ANISOTROPIC = ffi::tagVTFImageFlag_TEXTUREFLAGS_ANISOTROPIC as u32;
        const HINT_DXT5 = ffi::tagVTFImageFlag_TEXTUREFLAGS_HINT_DXT5 as u32;
        const SRGB = ffi::tagVTFImageFlag_TEXTUREFLAGS_SRGB as u32;
        const DEPRECATED_NOCOMPRESS = ffi::tagVTFImageFlag_TEXTUREFLAGS_DEPRECATED_NOCOMPRESS as u32;
        const NORMAL = ffi::tagVTFImageFlag_TEXTUREFLAGS_NORMAL as u32;
        const NO_MIP = ffi::tagVTFImageFlag_TEXTUREFLAGS_NOMIP as u32;
        const NO_LOD = ffi::tagVTFImageFlag_TEXTUREFLAGS_NOLOD as u32;
        const MIN_MIP = ffi::tagVTFImageFlag_TEXTUREFLAGS_MINMIP as u32;
        const PROCEDURAL = ffi::tagVTFImageFlag_TEXTUREFLAGS_PROCEDURAL as u32;
        const ONE_BIT_ALPHA = ffi::tagVTFImageFlag_TEXTUREFLAGS_ONEBITALPHA as u32;
        const EIGHT_BIT_ALPHA = ffi::tagVTFImageFlag_TEXTUREFLAGS_EIGHTBITALPHA as u32;
        const ENV_MAP = ffi::tagVTFImageFlag_TEXTUREFLAGS_ENVMAP as u32;
        const RENDER_TARGET = ffi::tagVTFImageFlag_TEXTUREFLAGS_RENDERTARGET as u32;
        const DEPTH_RENDER_TARGET = ffi::tagVTFImageFlag_TEXTUREFLAGS_DEPTHRENDERTARGET as u32;
        const NO_DEBUG_OVERRIDE = ffi::tagVTFImageFlag_TEXTUREFLAGS_NODEBUGOVERRIDE as u32;
        const SINGLE_COPY = ffi::tagVTFImageFlag_TEXTUREFLAGS_SINGLECOPY as u32;
        const UNUSED_0 = ffi::tagVTFImageFlag_TEXTUREFLAGS_UNUSED0 as u32;
        const DEPRECATED_ONE_OVER_MIP_LEVEL_IN_ALPHA = ffi::tagVTFImageFlag_TEXTUREFLAGS_DEPRECATED_ONEOVERMIPLEVELINALPHA as u32;
        const UNUSED_1 = ffi::tagVTFImageFlag_TEXTUREFLAGS_UNUSED1 as u32;
        const DEPRECATED_PREMULT_COLOR_BY_ONE_OVER_MIP_LEVEL = ffi::tagVTFImageFlag_TEXTUREFLAGS_DEPRECATED_PREMULTCOLORBYONEOVERMIPLEVEL as u32;
        const UNUSED_2 = ffi::tagVTFImageFlag_TEXTUREFLAGS_UNUSED2 as u32;
        const DEPRECATED_NORMAL_TO_DUDV = ffi::tagVTFImageFlag_TEXTUREFLAGS_DEPRECATED_NORMALTODUDV as u32;
        const UNUSED_3 = ffi::tagVTFImageFlag_TEXTUREFLAGS_UNUSED3 as u32;
        const DEPRECATED_ALPHA_TEST_MIP_GENERATION = ffi::tagVTFImageFlag_TEXTUREFLAGS_DEPRECATED_ALPHATESTMIPGENERATION as u32;
        const NO_DEPTH_BUFFER = ffi::tagVTFImageFlag_TEXTUREFLAGS_NODEPTHBUFFER as u32;
        const UNUSED_4 = ffi::tagVTFImageFlag_TEXTUREFLAGS_UNUSED4 as u32;
        const DEPRECATED_NICE_FILTERED = ffi::tagVTFImageFlag_TEXTUREFLAGS_DEPRECATED_NICEFILTERED as u32;
        const CLAMP_U = ffi::tagVTFImageFlag_TEXTUREFLAGS_CLAMPU as u32;
        const VERTEX_TEXTURE = ffi::tagVTFImageFlag_TEXTUREFLAGS_VERTEXTEXTURE as u32;
        const SSBUMP = ffi::tagVTFImageFlag_TEXTUREFLAGS_SSBUMP as u32;
        const UNUSED_5 = ffi::tagVTFImageFlag_TEXTUREFLAGS_UNUSED5 as u32;
        const DEPRECATED_UNFILTERABLE_OK = ffi::tagVTFImageFlag_TEXTUREFLAGS_DEPRECATED_UNFILTERABLE_OK as u32;
        const BORDER = ffi::tagVTFImageFlag_TEXTUREFLAGS_BORDER as u32;
        const DEPRECATED_SPECVAR_RED = ffi::tagVTFImageFlag_TEXTUREFLAGS_DEPRECATED_SPECVAR_RED as u32;
        #[allow(clippy::cast_sign_loss)]
        const DEPRECATED_SPECVAR_ALPHA = ffi::tagVTFImageFlag_TEXTUREFLAGS_DEPRECATED_SPECVAR_ALPHA as u32;
    }
}

#[repr(i32)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum MipmapFilter {
    Point = ffi::tagVTFMipmapFilter_MIPMAP_FILTER_POINT,
    Box = ffi::tagVTFMipmapFilter_MIPMAP_FILTER_BOX,
    Triangle = ffi::tagVTFMipmapFilter_MIPMAP_FILTER_TRIANGLE,
    Quadratic = ffi::tagVTFMipmapFilter_MIPMAP_FILTER_QUADRATIC,
    Cubic = ffi::tagVTFMipmapFilter_MIPMAP_FILTER_CUBIC,
    Catrom = ffi::tagVTFMipmapFilter_MIPMAP_FILTER_CATROM,
    Mitchell = ffi::tagVTFMipmapFilter_MIPMAP_FILTER_MITCHELL,
    Gaussian = ffi::tagVTFMipmapFilter_MIPMAP_FILTER_GAUSSIAN,
    Sinc = ffi::tagVTFMipmapFilter_MIPMAP_FILTER_SINC,
    Bessel = ffi::tagVTFMipmapFilter_MIPMAP_FILTER_BESSEL,
    Hanning = ffi::tagVTFMipmapFilter_MIPMAP_FILTER_HANNING,
    Hamming = ffi::tagVTFMipmapFilter_MIPMAP_FILTER_HAMMING,
    Blackman = ffi::tagVTFMipmapFilter_MIPMAP_FILTER_BLACKMAN,
    Kaiser = ffi::tagVTFMipmapFilter_MIPMAP_FILTER_KAISER,
}

#[repr(i32)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SharpenFilter {
    None = ffi::tagVTFSharpenFilter_SHARPEN_FILTER_NONE,
    Negative = ffi::tagVTFSharpenFilter_SHARPEN_FILTER_NEGATIVE,
    Lighter = ffi::tagVTFSharpenFilter_SHARPEN_FILTER_LIGHTER,
    Darker = ffi::tagVTFSharpenFilter_SHARPEN_FILTER_DARKER,
    ContrastMore = ffi::tagVTFSharpenFilter_SHARPEN_FILTER_CONTRASTMORE,
    ContrastLess = ffi::tagVTFSharpenFilter_SHARPEN_FILTER_CONTRASTLESS,
    Smoothen = ffi::tagVTFSharpenFilter_SHARPEN_FILTER_SMOOTHEN,
    SharpenSoft = ffi::tagVTFSharpenFilter_SHARPEN_FILTER_SHARPENSOFT,
    SharpenMedium = ffi::tagVTFSharpenFilter_SHARPEN_FILTER_SHARPENMEDIUM,
    SharpenStrong = ffi::tagVTFSharpenFilter_SHARPEN_FILTER_SHARPENSTRONG,
    FindEdges = ffi::tagVTFSharpenFilter_SHARPEN_FILTER_FINDEDGES,
    Contour = ffi::tagVTFSharpenFilter_SHARPEN_FILTER_CONTOUR,
    EdgeDetect = ffi::tagVTFSharpenFilter_SHARPEN_FILTER_EDGEDETECT,
    EdgeDetectSoft = ffi::tagVTFSharpenFilter_SHARPEN_FILTER_EDGEDETECTSOFT,
    Emboss = ffi::tagVTFSharpenFilter_SHARPEN_FILTER_EMBOSS,
    MeanRemoval = ffi::tagVTFSharpenFilter_SHARPEN_FILTER_MEANREMOVAL,
    Unsharp = ffi::tagVTFSharpenFilter_SHARPEN_FILTER_UNSHARP,
    XSharpen = ffi::tagVTFSharpenFilter_SHARPEN_FILTER_XSHARPEN,
    WarpSharp = ffi::tagVTFSharpenFilter_SHARPEN_FILTER_WARPSHARP,
}

#[repr(i32)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum KernelFilter {
    Filter4X = ffi::tagVTFKernelFilter_KERNEL_FILTER_4X,
    Filter3X3 = ffi::tagVTFKernelFilter_KERNEL_FILTER_3X3,
    Filter5X5 = ffi::tagVTFKernelFilter_KERNEL_FILTER_5X5,
    Filter7X7 = ffi::tagVTFKernelFilter_KERNEL_FILTER_7X7,
    Filter9X9 = ffi::tagVTFKernelFilter_KERNEL_FILTER_9X9,
    FilterDuDv = ffi::tagVTFKernelFilter_KERNEL_FILTER_DUDV,
}

#[repr(i32)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum HeightConversionMethod {
    Alpha = ffi::tagVTFHeightConversionMethod_HEIGHT_CONVERSION_METHOD_ALPHA,
    AverageRgb = ffi::tagVTFHeightConversionMethod_HEIGHT_CONVERSION_METHOD_AVERAGE_RGB,
    BiasedRgb = ffi::tagVTFHeightConversionMethod_HEIGHT_CONVERSION_METHOD_BIASED_RGB,
    Red = ffi::tagVTFHeightConversionMethod_HEIGHT_CONVERSION_METHOD_RED,
    Green = ffi::tagVTFHeightConversionMethod_HEIGHT_CONVERSION_METHOD_GREEN,
    Blue = ffi::tagVTFHeightConversionMethod_HEIGHT_CONVERSION_METHOD_BLUE,
    MaxRgb = ffi::tagVTFHeightConversionMethod_HEIGHT_CONVERSION_METHOD_MAX_RGB,
    ColorSpace = ffi::tagVTFHeightConversionMethod_HEIGHT_CONVERSION_METHOD_COLORSPACE,
}

#[repr(i32)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum NormalAlphaResult {
    NoChange = ffi::tagVTFNormalAlphaResult_NORMAL_ALPHA_RESULT_NOCHANGE,
    Height = ffi::tagVTFNormalAlphaResult_NORMAL_ALPHA_RESULT_HEIGHT,
    Black = ffi::tagVTFNormalAlphaResult_NORMAL_ALPHA_RESULT_BLACK,
    White = ffi::tagVTFNormalAlphaResult_NORMAL_ALPHA_RESULT_WHITE,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ResizeMethod {
    NearestPower2,
    BiggestPower2,
    SmallestPower2,
    Custom { width: u32, height: u32 },
}

ffi_enum! {
    #[repr(i32)]
    #[derive(Debug, Clone, Copy, PartialEq)]
    pub enum ResourceType {
        LegacyLowResImage = ffi::tagVTFResourceEntryType_VTF_LEGACY_RSRC_LOW_RES_IMAGE,
        LegacyImage = ffi::tagVTFResourceEntryType_VTF_LEGACY_RSRC_IMAGE,
        Sheet = ffi::tagVTFResourceEntryType_VTF_RSRC_SHEET,
        Crc = ffi::tagVTFResourceEntryType_VTF_RSRC_CRC,
        TextureLodSettings = ffi::tagVTFResourceEntryType_VTF_RSRC_TEXTURE_LOD_SETTINGS,
        TextureSettingsEx = ffi::tagVTFResourceEntryType_VTF_RSRC_TEXTURE_SETTINGS_EX,
        KeyValueData = ffi::tagVTFResourceEntryType_VTF_RSRC_KEY_VALUE_DATA
    }
}

#[must_use]
#[derive(Debug)]
pub struct VtfFile<'a> {
    // cannot outlive the library
    marker: PhantomData<&'a VtfLib>,
    handle: u32,
}

impl<'a> VtfFile<'a> {
    /// # Panics
    ///
    /// Panics if the binding fails. This should not happen.
    pub fn bind(self, _guard: &mut VtfGuard) -> BoundVtfFile<'a, '_> {
        unsafe {
            assert_eq!(
                ffi::vlBindImage(self.handle),
                ffi::vlTrue,
                "image binding failed"
            );
        }
        BoundVtfFile {
            marker: PhantomData,
            vtf_file: self,
        }
    }

    #[must_use]
    pub fn compute_image_size(
        width: u32,
        height: u32,
        depth: u32,
        mipmaps: u32,
        format: ImageFormat,
    ) -> u32 {
        unsafe { ffi::vlImageComputeImageSize(width, height, depth, mipmaps, format as i32) }
    }

    #[must_use]
    pub fn compute_mipmap_count(width: u32, height: u32, depth: u32) -> u32 {
        unsafe { ffi::vlImageComputeMipmapCount(width, height, depth) }
    }

    #[must_use]
    pub fn compute_mipmap_dimensions(
        width: u32,
        height: u32,
        depth: u32,
        mipmap_level: u32,
    ) -> (u32, u32, u32) {
        let mut w = 0;
        let mut h = 0;
        let mut d = 0;
        unsafe {
            ffi::vlImageComputeMipmapDimensions(
                width,
                height,
                depth,
                mipmap_level,
                &mut w,
                &mut h,
                &mut d,
            );
        }
        (w, h, d)
    }

    #[must_use]
    pub fn compute_mipmap_size(
        width: u32,
        height: u32,
        depth: u32,
        mipmap_level: u32,
        format: ImageFormat,
    ) -> u32 {
        unsafe { ffi::vlImageComputeMipmapSize(width, height, depth, mipmap_level, format as i32) }
    }
}

impl<'a> Drop for VtfFile<'a> {
    fn drop(&mut self) {
        unsafe {
            ffi::vlDeleteImage(self.handle);
        }
    }
}

#[must_use]
pub struct BoundVtfFile<'a, 'b> {
    // mutable borrow ensures only one vtf file is bound at a time
    marker: PhantomData<&'b mut VtfGuard>,
    vtf_file: VtfFile<'a>,
}

impl<'a, 'b> BoundVtfFile<'a, 'b> {
    pub fn unbind(self) -> VtfFile<'a> {
        self.vtf_file
    }

    /// Build a new empty image.
    pub fn build_empty(&mut self, width: u32, height: u32) -> EmptyImageBuilder {
        EmptyImageBuilder::new(width, height)
    }

    /// Load an image from existing RGBA8888 data.
    pub fn from_rgba8888(&mut self, width: u32, height: u32) -> Rgba8888ImageBuilder {
        Rgba8888ImageBuilder::new(width, height)
    }

    /// Destroy the contained image
    pub fn destroy(&mut self) {
        unsafe {
            ffi::vlImageDestroy();
        }
    }

    /// Load a VTF image from `bytes`.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the image loading fails
    /// or if the image is too large for the length fit into `u32`.
    pub fn load(&mut self, bytes: &[u8]) -> Result<()> {
        self.load_impl(bytes, false)
    }

    /// Load a VTF image header from `bytes`,
    /// to save memory while only getting information from the file, for example.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the image loading fails
    /// or if the image is too large for the length fit into `u32`.
    pub fn load_header(&mut self, bytes: &[u8]) -> Result<()> {
        self.load_impl(bytes, true)
    }

    fn load_impl(&mut self, bytes: &[u8], header_only: bool) -> Result<()> {
        let len = bytes.len().try_into().map_err(|_| Error::LengthOverflow)?;
        unsafe {
            ffi_try!(ffi::vlImageLoadLump(
                bytes.as_ptr().cast(),
                len,
                ffi_bool(header_only)
            ));
        }
        Ok(())
    }

    /// Save a VTF image into `buffer`. Returns bytes written.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the image saving fails
    /// or if the buffer is too large for the length fit into `u32`.
    pub fn save(&self, buffer: &mut [u8]) -> Result<usize> {
        let len = buffer.len().try_into().map_err(|_| Error::LengthOverflow)?;
        let mut written = 0;
        unsafe {
            ffi_try!(ffi::vlImageSaveLump(
                buffer.as_mut_ptr().cast(),
                len,
                &mut written
            ));
        }
        Ok(written as usize)
    }

    /// Save a VTF image into a [`Vec`].
    ///
    /// # Errors
    ///
    /// Returns `Err` if the image saving fails.
    #[allow(clippy::cast_possible_truncation)]
    pub fn save_to_vec(&self) -> Result<Vec<u8>> {
        let len = self.size();
        let mut buffer: Vec<u8> = Vec::with_capacity(len);
        let mut written = 0;
        unsafe {
            ffi_try!(ffi::vlImageSaveLump(
                buffer.as_mut_ptr().cast(),
                len as u32,
                &mut written
            ));
            buffer.set_len(written as usize);
        }
        Ok(buffer)
    }

    ffi_getters!(
        #[allow(clippy::cast_possible_truncation)]
        pub fn has_image(&self) -> bool => ffi::vlImageGetHasImage() as u8 == ffi::vlTrue,

        pub fn major_version(&self) -> u32 => ffi::vlImageGetMajorVersion(),
        pub fn minor_version(&self) -> u32 => ffi::vlImageGetMinorVersion(),
        pub fn size(&self) -> usize => ffi::vlImageGetSize() as usize,

        pub fn width(&self) -> u32 => ffi::vlImageGetWidth(),
        pub fn height(&self) -> u32 => ffi::vlImageGetHeight(),
        pub fn depth(&self) -> u32 => ffi::vlImageGetDepth(),

        pub fn frame_count(&self) -> u32 => ffi::vlImageGetFrameCount(),
        pub fn face_count(&self) -> u32 => ffi::vlImageGetFaceCount(),
        pub fn mipmap_count(&self) -> u32 => ffi::vlImageGetMipmapCount(),

        pub fn start_frame(&self) -> u32 => ffi::vlImageGetStartFrame(),

        pub fn flags(&self) -> ImageFlags => ImageFlags::from_bits_truncate(ffi::vlImageGetFlags()),

        #[allow(clippy::cast_possible_wrap)]
        pub fn flag(&self, flag: ImageFlags) -> bool => ffi::vlImageGetFlag(flag.bits as i32) == ffi::vlTrue,

        pub fn bumpmap_scale(&self) -> f32 => ffi::vlImageGetBumpmapScale(),

        pub fn format(&self) -> Option<ImageFormat> => ImageFormat::from_ffi(ffi::vlImageGetFormat()),

        pub fn has_thumbnail(&self) -> bool => ffi::vlImageGetHasThumbnail() == ffi::vlTrue,
        pub fn thumbnail_width(&self) -> u32 => ffi::vlImageGetThumbnailWidth(),
        pub fn thumbnail_height(&self) -> u32 => ffi::vlImageGetThumbnailHeight(),
        pub fn thumbnail_format(&self) -> Option<ImageFormat> => ImageFormat::from_ffi(ffi::vlImageGetThumbnailFormat()),

        pub fn supports_resource(&self) -> bool => ffi::vlImageGetSupportsResources() == ffi::vlTrue,
        pub fn resource_count(&self) -> u32 => ffi::vlImageGetResourceCount(),

        #[allow(clippy::cast_possible_wrap)]
        pub fn resource_type(&self, index: u32) -> Option<ResourceType> => ResourceType::from_ffi(ffi::vlImageGetResourceType(index) as i32),

        pub fn has_resource(&self, resource: ResourceType) -> bool => ffi::vlImageGetHasResource(resource as u32) == ffi::vlTrue
    );

    ffi_setters! {
        pub fn set_start_frame(&mut self, start_frame: u32) => ffi::vlImageSetStartFrame(start_frame),
        pub fn set_flags(&mut self, flags: ImageFlags) => ffi::vlImageSetFlags(flags.bits),
        #[allow(clippy::cast_possible_wrap)]
        pub fn set_flag(&mut self, flag: ImageFlags, state: bool) => ffi::vlImageSetFlag(flag.bits as i32, ffi_bool(state)),
        pub fn set_bumpmap_scale(&mut self, scale: f32) => ffi::vlImageSetBumpmapScale(scale),
        pub fn set_reflectivity(&mut self, x: f32, y: f32, z: f32) => ffi::vlImageSetReflectivity(x, y, z)
    }

    #[must_use]
    pub fn reflectivity(&self) -> (f32, f32, f32) {
        let mut x = 0.0;
        let mut y = 0.0;
        let mut z = 0.0;
        unsafe {
            ffi::vlImageGetReflectivity(&mut x, &mut y, &mut z);
        }
        (x, y, z)
    }

    fn data_len(&self, mipmap_level: u32) -> Option<usize> {
        self.format().map(|f| {
            VtfFile::compute_mipmap_size(self.width(), self.height(), 1, mipmap_level, f) as usize
        })
    }

    fn verify_data_invariants(&self, frame: u32, face: u32, slice: u32, mipmap_level: u32) -> bool {
        if frame >= self.frame_count() {
            return false;
        }
        if face >= self.face_count() {
            return false;
        }
        if slice >= self.depth() {
            return false;
        }
        if mipmap_level >= self.mipmap_count() {
            return false;
        }
        true
    }

    #[must_use]
    pub fn data(&self, frame: u32, face: u32, slice: u32, mipmap_level: u32) -> Option<&[u8]> {
        if !self.verify_data_invariants(frame, face, slice, mipmap_level) {
            return None;
        }
        return unsafe { self.data_unchecked(frame, face, slice, mipmap_level) };
    }

    /// # Safety
    ///
    /// If any parameters are invalid for the current image, undefined behaviour results.
    #[must_use]
    pub unsafe fn data_unchecked(
        &self,
        frame: u32,
        face: u32,
        slice: u32,
        mipmap_level: u32,
    ) -> Option<&[u8]> {
        let data_len = self.data_len(mipmap_level)?;
        let data = ffi::vlImageGetData(frame, face, slice, mipmap_level);
        if data.is_null() {
            return None;
        }
        Some(slice::from_raw_parts(data, data_len))
    }

    /// # Errors
    ///
    /// Returns `Err` if the data length is not correct,
    /// or if the parameters are not valid for the current image.
    pub fn set_data(
        &mut self,
        frame: u32,
        face: u32,
        slice: u32,
        mipmap_level: u32,
        data: &[u8],
    ) -> Result<()> {
        let data_len = self.data_len(mipmap_level).ok_or(Error::InvalidFormat)?;
        if data.len() < data_len {
            return Err(Error::InvalidLength);
        }
        if !self.verify_data_invariants(frame, face, slice, mipmap_level) {
            return Err(Error::InvalidParameters);
        }
        unsafe { self.set_data_unchecked(frame, face, slice, mipmap_level, data) }
        Ok(())
    }

    /// # Safety
    ///
    /// If any parameters are invalid for the current image,
    /// or if data length is invalid, undefined behaviour results.
    pub unsafe fn set_data_unchecked(
        &mut self,
        frame: u32,
        face: u32,
        slice: u32,
        mipmap_level: u32,
        data: &[u8],
    ) {
        // this definitely doesn't need mutable data, bad library
        ffi::vlImageSetData(frame, face, slice, mipmap_level, data.as_ptr() as *mut u8);
    }

    fn thumbnail_data_len(&self) -> Option<usize> {
        self.thumbnail_format().map(|f| {
            VtfFile::compute_image_size(self.thumbnail_width(), self.thumbnail_height(), 1, 1, f)
                as usize
        })
    }

    #[must_use]
    pub fn thumbnail_data(&self) -> Option<&[u8]> {
        let data_len = self.thumbnail_data_len()?;
        unsafe {
            let data = ffi::vlImageGetThumbnailData();
            if data.is_null() {
                return None;
            }
            Some(slice::from_raw_parts(data, data_len))
        }
    }

    /// # Errors
    ///
    /// Returns `Err` if the data length is not correct.
    pub fn set_thumbnail_data(&mut self, data: &[u8]) -> Result<()> {
        let data_len = self.thumbnail_data_len().ok_or(Error::InvalidFormat)?;
        if data.len() < data_len {
            return Err(Error::InvalidLength);
        }
        unsafe {
            ffi::vlImageSetThumbnailData(data.as_ptr() as *mut u8);
        }
        Ok(())
    }

    /// # Errors
    ///
    /// Returns `Err` if the resource data cannot be get.
    pub fn resource_data(&self, resource: ResourceType) -> Result<&[u8]> {
        let mut data_len = 0;
        unsafe {
            let data = ffi_result(ffi::vlImageGetResourceData(resource as u32, &mut data_len))?;
            Ok(slice::from_raw_parts(data.cast(), data_len as usize))
        }
    }

    /// # Errors
    ///
    /// Returns `Err` if the resource data cannot be set.
    pub fn set_resource_data(&mut self, resource: ResourceType, data: &[u8]) -> Result<()> {
        let len = data.len().try_into().map_err(|_| Error::LengthOverflow)?;
        unsafe {
            ffi_result(ffi::vlImageSetResourceData(
                resource as u32,
                len,
                data.as_ptr() as *mut c_void,
            ))?;
        }
        Ok(())
    }

    /// # Errors
    ///
    /// Returns `Err` if the mipmap generation fails,
    /// or if the parameters are incorrect for the current image.
    pub fn generate_mipmaps(
        &mut self,
        face: u32,
        frame: u32,
        filter: MipmapFilter,
        sharpen_filter: SharpenFilter,
    ) -> Result<()> {
        if face <= self.face_count() {
            return Err(Error::InvalidParameters);
        }
        if frame <= self.frame_count() {
            return Err(Error::InvalidParameters);
        }
        unsafe {
            ffi_try!(ffi::vlImageGenerateMipmaps(
                face,
                frame,
                filter as i32,
                sharpen_filter as i32,
            ));
        }
        Ok(())
    }

    /// # Errors
    ///
    /// Returns `Err` if the mipmap generation fails.
    pub fn generate_all_mipmaps(
        &mut self,
        filter: MipmapFilter,
        sharpen_filter: SharpenFilter,
    ) -> Result<()> {
        unsafe {
            ffi_try!(ffi::vlImageGenerateAllMipmaps(
                filter as i32,
                sharpen_filter as i32,
            ));
        }
        Ok(())
    }

    /// # Errors
    ///
    /// Returns `Err` if the thumbnail generation fails.
    pub fn generate_thumbnail(&mut self) -> Result<()> {
        unsafe {
            ffi_try!(ffi::vlImageGenerateThumbnail());
        }
        Ok(())
    }

    /// # Errors
    ///
    /// Returns `Err` if the normal map generation fails,
    /// or if the parameters are incorrect for the current image.
    pub fn generate_normal_map(
        &mut self,
        frame: u32,
        kernel_filter: KernelFilter,
        height_conversion_method: HeightConversionMethod,
        normal_alpha_result: NormalAlphaResult,
    ) -> Result<()> {
        if frame <= self.frame_count() {
            return Err(Error::InvalidParameters);
        }
        unsafe {
            ffi_try!(ffi::vlImageGenerateNormalMap(
                frame,
                kernel_filter as i32,
                height_conversion_method as i32,
                normal_alpha_result as i32
            ));
        }
        Ok(())
    }

    /// # Errors
    ///
    /// Returns `Err` if the normal map generation fails.
    pub fn generate_all_normal_maps(
        &mut self,
        kernel_filter: KernelFilter,
        height_conversion_method: HeightConversionMethod,
        normal_alpha_result: NormalAlphaResult,
    ) -> Result<()> {
        unsafe {
            ffi_try!(ffi::vlImageGenerateAllNormalMaps(
                kernel_filter as i32,
                height_conversion_method as i32,
                normal_alpha_result as i32
            ));
        }
        Ok(())
    }

    /// # Errors
    ///
    /// Returns `Err` if the sphere map generation fails.
    pub fn generate_sphere_map(&mut self) -> Result<()> {
        unsafe {
            ffi_try!(ffi::vlImageGenerateSphereMap());
        }
        Ok(())
    }

    /// # Errors
    ///
    /// Returns `Err` if the reflectivity can't be computed.
    pub fn compute_reflectivity(&mut self) -> Result<()> {
        unsafe {
            ffi_try!(ffi::vlImageComputeReflectivity());
        }
        Ok(())
    }
}

#[must_use]
pub struct EmptyImageBuilder<'a, 'b, 'c> {
    marker: PhantomData<&'c mut BoundVtfFile<'a, 'b>>,
    width: u32,
    height: u32,
    frames: u32,
    faces: u32,
    slices: u32,
    format: ImageFormat,
    thumbnail: bool,
    mipmaps: bool,
    null_data: bool,
}

impl<'a, 'b, 'c> EmptyImageBuilder<'a, 'b, 'c> {
    fn new(width: u32, height: u32) -> Self {
        Self {
            marker: PhantomData,
            width,
            height,
            frames: 1,
            faces: 1,
            slices: 1,
            format: ImageFormat::Rgba8888,
            thumbnail: true,
            mipmaps: true,
            null_data: false,
        }
    }

    builder! {
        pub fn frames(mut self, frames: u32) -> Self => self.frames = frames,
        pub fn faces(mut self, faces: u32) -> Self => self.faces = faces,
        pub fn slices(mut self, slices: u32) -> Self => self.slices = slices,
        pub fn format(mut self, format: ImageFormat) -> Self => self.format = format,
        pub fn thumbnail(mut self, thumbnail: bool) -> Self => self.thumbnail = thumbnail,
        pub fn mipmaps(mut self, mipmaps: bool) -> Self => self.mipmaps = mipmaps,
        pub fn null_data(mut self, null_data: bool) -> Self => self.null_data = null_data
    }

    /// Create the image
    ///
    /// # Errors
    ///
    /// Returns `Err` if the image creation fails.
    pub fn create(self) -> Result<()> {
        unsafe {
            ffi_try!(ffi::vlImageCreate(
                self.width,
                self.height,
                self.frames,
                self.faces,
                self.slices,
                self.format as i32,
                ffi_bool(self.thumbnail),
                ffi_bool(self.mipmaps),
                ffi_bool(self.null_data)
            ));
        }
        Ok(())
    }
}

#[must_use]
pub struct Rgba8888ImageBuilder<'a, 'b, 'c> {
    marker: PhantomData<&'c mut BoundVtfFile<'a, 'b>>,
    width: u32,
    height: u32,
    options: ffi::SVTFCreateOptions,
}

impl<'a, 'b, 'c> Rgba8888ImageBuilder<'a, 'b, 'c> {
    fn new(width: u32, height: u32) -> Self {
        let options = unsafe {
            let mut options = MaybeUninit::uninit();
            ffi::vlImageCreateDefaultCreateStructure(options.as_mut_ptr());
            options.assume_init()
        };
        Self {
            marker: PhantomData,
            width,
            height,
            options,
        }
    }

    builder! {
        pub fn version(mut self, major: u32, minor: u32) -> Self => self.options.uiVersion = [major, minor],
        pub fn format(mut self, format: ImageFormat) -> Self => self.options.ImageFormat = format as i32,
        pub fn flags(mut self, flags: ImageFlags) -> Self => self.options.uiFlags = flags.bits,
        pub fn with_flag(mut self, flag: ImageFlags) -> Self => self.options.uiFlags |= flag.bits,
        pub fn without_flag(mut self, flag: ImageFlags) -> Self => self.options.uiFlags &= !flag.bits,
        pub fn start_frame(mut self, frame: u32) -> Self => self.options.uiStartFrame = frame,
        pub fn bump_scale(mut self, scale: f32) -> Self => self.options.sBumpScale = scale,
        pub fn reflectivity(mut self, reflectivity: [f32; 3]) -> Self => self.options.sReflectivity = reflectivity,
        pub fn generate_mipmaps(mut self, mipmaps: bool) -> Self => self.options.bMipmaps = ffi_bool(mipmaps),
        pub fn mipmap_filter(mut self, filter: MipmapFilter) -> Self => self.options.MipmapFilter = filter as i32,
        pub fn mipmap_sharpen_filter(mut self, filter: SharpenFilter) -> Self => self.options.MipmapSharpenFilter = filter as i32,
        pub fn generate_thumbnail(mut self, thumbnail: bool) -> Self => self.options.bThumbnail = ffi_bool(thumbnail),
        pub fn compute_reflectivity(mut self, reflectivity: bool) -> Self => self.options.bReflectivity = ffi_bool(reflectivity),
        pub fn resize(mut self, resize: bool) -> Self => self.options.bResize = ffi_bool(resize),
        pub fn resize_method(mut self, method: ResizeMethod) -> Self => match method {
            ResizeMethod::NearestPower2 => self.options.ResizeMethod = ffi::tagVTFResizeMethod_RESIZE_NEAREST_POWER2,
            ResizeMethod::BiggestPower2 => self.options.ResizeMethod = ffi::tagVTFResizeMethod_RESIZE_BIGGEST_POWER2,
            ResizeMethod::SmallestPower2 => self.options.ResizeMethod = ffi::tagVTFResizeMethod_RESIZE_SMALLEST_POWER2,
            ResizeMethod::Custom { width, height } => {
                self.options.ResizeMethod = ffi::tagVTFResizeMethod_RESIZE_SET;
                self.options.uiResizeWidth = width;
                self.options.uiResizeHeight = height;
            }
        },
        pub fn resize_filter(mut self, filter: MipmapFilter) -> Self => self.options.ResizeFilter = filter as i32,
        pub fn resize_sharpen_filter(mut self, filter: MipmapFilter) -> Self => self.options.ResizeSharpenFilter = filter as i32,
        pub fn resize_clamp(mut self, clamp: bool) -> Self => self.options.bResizeClamp = ffi_bool(clamp),
        pub fn resize_clamp_dimensions(mut self, width: u32, heigth: u32) -> Self => {
            self.options.uiResizeClampWidth = width;
            self.options.uiResizeClampHeight = heigth;
        },
        pub fn gamma_correction(mut self, correction: bool) -> Self => self.options.bGammaCorrection = ffi_bool(correction),
        pub fn gamma_correction_amount(mut self, amount: f32) -> Self => self.options.sGammaCorrection = amount,
        pub fn normal_map(mut self, normal_map: bool) -> Self => self.options.bNormalMap = ffi_bool(normal_map),
        pub fn kernel_filter(mut self, filter: KernelFilter) -> Self => self.options.KernelFilter = filter as i32,
        pub fn height_conversion_method(mut self, method: HeightConversionMethod) -> Self => self.options.HeightConversionMethod = method as i32,
        pub fn normal_alpha_result(mut self, result: NormalAlphaResult) -> Self => self.options.NormalAlphaResult = result as i32,
        pub fn normal_minimum_z(mut self, min: u8) -> Self => self.options.bNormalMinimumZ = min,
        pub fn normal_scale(mut self, scale: f32) -> Self => self.options.sNormalScale = scale,
        pub fn normal_wrap(mut self, wrap: bool) -> Self => self.options.bNormalWrap = ffi_bool(wrap),
        pub fn normal_invert_x(mut self, invert: bool) -> Self => self.options.bNormalInvertX = ffi_bool(invert),
        pub fn normal_invert_y(mut self, invert: bool) -> Self => self.options.bNormalInvertY = ffi_bool(invert),
        pub fn normal_invert_z(mut self, invert: bool) -> Self => self.options.bNormalInvertZ = ffi_bool(invert),
        pub fn sphere_map(mut self, sphere_map: bool) -> Self => self.options.bSphereMap = ffi_bool(sphere_map)
    }

    /// Create a single frame from `data`.
    ///
    /// # Errors
    ///
    /// Returns `Err` if `data` has the wrong length or the image creation fails.
    pub fn create(mut self, data: &mut [u8]) -> Result<()> {
        if data.len()
            < VtfFile::compute_image_size(self.width, self.height, 1, 1, ImageFormat::Rgba8888)
                as usize
        {
            return Err(Error::InvalidLength);
        }
        unsafe {
            ffi_try!(ffi::vlImageCreateSingle(
                self.width,
                self.height,
                data.as_mut_ptr(),
                &mut self.options
            ));
        }
        Ok(())
    }

    /// Create a multi frame or cubemap image from `data`.
    ///
    /// # Errors
    ///
    /// Returns `Err` if `data` has the wrong length or the image creation fails.
    pub fn create_multi(
        mut self,
        frames: u32,
        faces: u32,
        slices: u32,
        data: &mut [&mut [u8]],
    ) -> Result<()> {
        if data.len() < frames.max(faces.max(slices)) as usize {
            return Err(Error::InvalidLength);
        }
        let image_size =
            VtfFile::compute_image_size(self.width, self.height, 1, 1, ImageFormat::Rgba8888)
                as usize;
        for slice in data.iter() {
            if slice.len() < image_size {
                return Err(Error::InvalidLength);
            }
        }
        unsafe {
            let mut data: Vec<_> = data.iter_mut().map(|e| e.as_mut_ptr()).collect();
            ffi_try!(ffi::vlImageCreateMultiple(
                self.width,
                self.height,
                frames,
                faces,
                slices,
                data.as_mut_ptr(),
                &mut self.options,
            ));
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn initialization() {
        assert_eq!(UNINIT_COUNTER.load(Ordering::Acquire), 0);
        let (vtflib, guard) = VtfLib::new().unwrap();
        assert!(VtfLib::new().is_none());
        assert_eq!(UNINIT_COUNTER.load(Ordering::Acquire), 2);
        drop(vtflib);
        assert!(VtfLib::new().is_none());
        assert_eq!(UNINIT_COUNTER.load(Ordering::Acquire), 1);
        drop(guard);
        assert_eq!(UNINIT_COUNTER.load(Ordering::Acquire), 0);
        assert!(VtfLib::new().is_some());
    }
}
