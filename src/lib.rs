#![deny(clippy::all, clippy::pedantic, clippy::cargo)]
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[non_exhaustive]
pub enum Error {
    /// Image is not loaded or only the header was loaded.
    ImageNotLoaded,
    /// Provided image data buffer was too short to contain an image
    /// with the specified width, height and format.
    InvalidLength,
    /// Provided parameters were invalid for the bound image.
    /// Eg. provided `frame`, `face` or `slice` didn't exist.
    InvalidParameters,
    /// The bound image had an unsupported format.
    InvalidFormat,
    /// Provided buffer was too long to fit into `u32`.
    /// VTFLib does not have 64-bit support.
    LengthOverflow,
    /// An error that was returned by VTFLib.
    VtfLib(String),
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::ImageNotLoaded => f.write_str("image is not loaded"),
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
                $(#[$tagmeta:meta]) *
                $tag:ident = $value:path
            ),+
        }
    ) => {
        $(#[$meta] )*
        #[repr($repr)]
        $vis enum $name {
            $(
                $(#[$tagmeta] )*
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
        2 => {}
        _ => panic!("unexpected uninit counter value"),
    }
}

/// Integer library configuration options.
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

/// Floating point library configuration options.
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

/// Represents initialized library.
/// Only one of these can exist at any given time.
/// This struct is `!Send + !Sync` since the library is not thread-safe.
#[must_use]
#[derive(Debug)]
pub struct VtfLib(PhantomData<*const ()>);

impl VtfLib {
    /// Initialize the library.
    /// Returns [`None`] if it's already initialized.
    /// Uninitialization happens when both returned structs are dropped.
    #[must_use]
    pub fn initialize() -> Option<(Self, VtfGuard)> {
        UNINIT_COUNTER
            .compare_exchange(0, 2, Ordering::Acquire, Ordering::Acquire)
            .ok()?;
        unsafe {
            if ffi::vlInitialize() == ffi::vlFalse {
                UNINIT_COUNTER.store(0, Ordering::Release);
                return None;
            }
        }
        Some((Self(PhantomData), VtfGuard(PhantomData)))
    }

    /// Get the library's version number.
    #[must_use]
    pub fn get_version() -> u32 {
        unsafe { ffi::vlGetVersion() }
    }

    /// Get the library's version number string.
    #[must_use]
    #[allow(clippy::missing_panics_doc)]
    pub fn get_version_string() -> &'static str {
        unsafe { CStr::from_ptr(ffi::vlGetVersionString()) }
            .to_str()
            .unwrap()
    }

    /// Get the value of an integer configuration option.
    #[must_use]
    pub fn get_integer(&self, option: IntegerOption) -> i32 {
        unsafe { ffi::vlGetInteger(option as i32) }
    }

    /// Set the value of an integer configuration option.
    pub fn set_integer(&self, option: IntegerOption, value: i32) {
        unsafe {
            ffi::vlSetInteger(option as i32, value);
        }
    }

    /// Get the value of a floating point configuration option.
    #[must_use]
    pub fn get_float(&self, option: FloatOption) -> f32 {
        unsafe { ffi::vlGetFloat(option as i32) }
    }

    /// Set the value of a floating point configuration option.
    pub fn set_float(&self, option: FloatOption, value: f32) {
        unsafe {
            ffi::vlSetFloat(option as i32, value);
        }
    }

    /// Create a vtf file.
    /// The [`VtfFile`] needs to be bound before it can be worked on.
    ///
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
        uninit_decrement();
    }
}

/// Enforces that only one [`VtfFile`] is bound at a time.
/// Only one of these can exist at any given time.
/// This struct is `!Send + !Sync` since the library is not thread-safe.
#[derive(Debug)]
pub struct VtfGuard(PhantomData<*const ()>);

impl Drop for VtfGuard {
    fn drop(&mut self) {
        uninit_decrement();
    }
}

ffi_enum! {
    #[repr(i32)]
    #[doc = "VTFLib's supported image formats."]
    #[derive(Debug, Clone, Copy, PartialEq)]
    pub enum ImageFormat {
        #[doc = "Red, green, blue, alpha - 32 bpp"]
        Rgba8888 = ffi::tagVTFImageFormat_IMAGE_FORMAT_RGBA8888,
        #[doc = "Alpha, blue, green, red - 32 bpp"]
        Agbr8888 = ffi::tagVTFImageFormat_IMAGE_FORMAT_ABGR8888,
        #[doc = "Red, green, blue - 24 bpp"]
        Rgb888 = ffi::tagVTFImageFormat_IMAGE_FORMAT_RGB888,
        #[doc = "Blue, green, red - 24 bpp"]
        Bgr888 = ffi::tagVTFImageFormat_IMAGE_FORMAT_BGR888,
        #[doc = "Red, green, blue - 16 bpp"]
        Rgb565 = ffi::tagVTFImageFormat_IMAGE_FORMAT_RGB565,
        #[doc = "Luminance - 8 bpp"]
        I8 = ffi::tagVTFImageFormat_IMAGE_FORMAT_I8,
        #[doc = "Luminance, alpha - 8 bpp"]
        Ia88 = ffi::tagVTFImageFormat_IMAGE_FORMAT_IA88,
        #[doc = "Paletted, 8 bpp"]
        P8 = ffi::tagVTFImageFormat_IMAGE_FORMAT_P8,
        #[doc = "Alpha, 8 bpp"]
        A8 = ffi::tagVTFImageFormat_IMAGE_FORMAT_A8,
        #[doc = "Red, green, blue, bluescreen alpha - 24 bpp"]
        Rgb888Bluescreen = ffi::tagVTFImageFormat_IMAGE_FORMAT_RGB888_BLUESCREEN,
        #[doc = "Blue, green, red, bluescreen alpha - 24 bpp"]
        Bgr888Bluescreen = ffi::tagVTFImageFormat_IMAGE_FORMAT_BGR888_BLUESCREEN,
        #[doc = "Alpha, red, green, blue - 32 bpp"]
        Argb8888 = ffi::tagVTFImageFormat_IMAGE_FORMAT_ARGB8888,
        #[doc = "Blue, green, red, alpha - 32 bpp"]
        Bgra8888 = ffi::tagVTFImageFormat_IMAGE_FORMAT_BGRA8888,
        #[doc = "DXT1 compressed - 4 bpp"]
        Dxt1 = ffi::tagVTFImageFormat_IMAGE_FORMAT_DXT1,
        #[doc = "DXT3 compressed - 8 bpp"]
        Dxt3 = ffi::tagVTFImageFormat_IMAGE_FORMAT_DXT3,
        #[doc = "DXT5 compressed - 8 bpp"]
        Dxt5 = ffi::tagVTFImageFormat_IMAGE_FORMAT_DXT5,
        #[doc = "Blue, green, red, unused - 32 bpp"]
        Bgrx8888 = ffi::tagVTFImageFormat_IMAGE_FORMAT_BGRX8888,
        #[doc = "Blue, green, red - 16 bpp"]
        Bgr565 = ffi::tagVTFImageFormat_IMAGE_FORMAT_BGR565,
        #[doc = "Blue, green, red, unused - 16 bpp"]
        Bgrx5551 = ffi::tagVTFImageFormat_IMAGE_FORMAT_BGRX5551,
        #[doc = "Blue, green, red, alpha - 16 bpp"]
        Bgra4444 = ffi::tagVTFImageFormat_IMAGE_FORMAT_BGRA4444,
        #[doc = "DXT1 compressed, 1-bit alpha - 4 bpp"]
        Dxt1OneBitAlpha = ffi::tagVTFImageFormat_IMAGE_FORMAT_DXT1_ONEBITALPHA,
        #[doc = "Blue, green, red, alpha - 16 bpp"]
        Bgra5551 = ffi::tagVTFImageFormat_IMAGE_FORMAT_BGRA5551,
        #[doc = "2-channel format for DuDv/normal maps - 16 bpp"]
        Uv88 = ffi::tagVTFImageFormat_IMAGE_FORMAT_UV88,
        #[doc = "4-channel format for DuDv/normal maps - 32 bpp"]
        Uvwq8888 = ffi::tagVTFImageFormat_IMAGE_FORMAT_UVWQ8888,
        #[doc = "Red, green, blue, alpha - 64 bpp"]
        Rgba16161616F = ffi::tagVTFImageFormat_IMAGE_FORMAT_RGBA16161616F,
        #[doc = "Red, green, blue, alpha signed with mantissa - 64 bpp"]
        Rgba16161616 = ffi::tagVTFImageFormat_IMAGE_FORMAT_RGBA16161616,
        #[doc = "4-channel format for DuDv/normal maps - 32 bpp"]
        Uvlx8888 = ffi::tagVTFImageFormat_IMAGE_FORMAT_UVLX8888,
        #[doc = "Luminance - 32 bpp"]
        R32F = ffi::tagVTFImageFormat_IMAGE_FORMAT_R32F,
        #[doc = "Red, green, blue - 96 bpp"]
        Rgb323232F = ffi::tagVTFImageFormat_IMAGE_FORMAT_RGB323232F,
        #[doc = "Red, green, blue, alpha - 128 bpp"]
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
    /// Get information about the image format.
    /// Returns `None` if the image format is unknown.
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

/// Information about an image format.
#[derive(Debug, Clone)]
pub struct ImageFormatInfo {
    name: &'static str,
    info: ffi::tagSVTFImageFormatInfo,
}

impl ImageFormatInfo {
    fn new(info: ffi::tagSVTFImageFormatInfo) -> Self {
        let name = unsafe { CStr::from_ptr(info.lpName).to_str().unwrap() };
        Self { name, info }
    }

    #[must_use]
    pub fn name(&self) -> &'static str {
        self.name
    }

    #[must_use]
    pub fn bits_per_pixel(&self) -> u32 {
        self.info.uiBitsPerPixel
    }

    #[must_use]
    pub fn bytes_per_pixel(&self) -> u32 {
        self.info.uiBytesPerPixel
    }

    #[must_use]
    pub fn red_bits_per_pixel(&self) -> u32 {
        self.info.uiRedBitsPerPixel
    }

    #[must_use]
    pub fn green_bits_per_pixel(&self) -> u32 {
        self.info.uiGreenBitsPerPixel
    }

    #[must_use]
    pub fn blue_bits_per_pixel(&self) -> u32 {
        self.info.uiBlueBitsPerPixel
    }

    #[must_use]
    pub fn compressed(&self) -> bool {
        self.info.bIsCompressed == ffi::vlTrue
    }

    #[must_use]
    pub fn supported(&self) -> bool {
        self.info.bIsSupported == ffi::vlTrue
    }
}

bitflags! {
    #[doc = "VTF image header flags"]
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

/// VTF image cubemap face indices
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CubeMapFace {
    Right = ffi::tagVTFCubeMapFace_CUBEMAP_FACE_RIGHT as u32,
    Left = ffi::tagVTFCubeMapFace_CUBEMAP_FACE_LEFT as u32,
    Back = ffi::tagVTFCubeMapFace_CUBEMAP_FACE_BACK as u32,
    Front = ffi::tagVTFCubeMapFace_CUBEMAP_FACE_FRONT as u32,
    Up = ffi::tagVTFCubeMapFace_CUBEMAP_FACE_UP as u32,
    Down = ffi::tagVTFCubeMapFace_CUBEMAP_FACE_DOWN as u32,
    SphereMap = ffi::tagVTFCubeMapFace_CUBEMAP_FACE_SPHERE_MAP as u32,
}
/// Mipmap reduction filters
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

/// Mipmap sharpen filters
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
pub enum DxtQuality {
    Low = ffi::tagDXTQuality_DXT_QUALITY_LOW,
    Medium = ffi::tagDXTQuality_DXT_QUALITY_MEDIUM,
    High = ffi::tagDXTQuality_DXT_QUALITY_HIGH,
    Highest = ffi::tagDXTQuality_DXT_QUALITY_HIGHEST,
}

/// Normal map creation kernel sizes
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

/// Normal map height conversion methods
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

/// Normal map alpha channel handling
#[repr(i32)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum NormalAlphaResult {
    NoChange = ffi::tagVTFNormalAlphaResult_NORMAL_ALPHA_RESULT_NOCHANGE,
    Height = ffi::tagVTFNormalAlphaResult_NORMAL_ALPHA_RESULT_HEIGHT,
    Black = ffi::tagVTFNormalAlphaResult_NORMAL_ALPHA_RESULT_BLACK,
    White = ffi::tagVTFNormalAlphaResult_NORMAL_ALPHA_RESULT_WHITE,
}

/// Image resize methods
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ResizeMethod {
    NearestPower2,
    BiggestPower2,
    SmallestPower2,
    Custom { width: u32, height: u32 },
}

ffi_enum! {
    #[repr(i32)]
    #[doc = "Resource type identifiers"]
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

/// A VTF file.
/// Must be bound before it can be worked on.
#[must_use]
#[derive(Debug)]
pub struct VtfFile<'a> {
    // cannot outlive the library
    marker: PhantomData<&'a VtfLib>,
    handle: u32,
}

impl<'a> VtfFile<'a> {
    /// Bind the VTF file to work on it. Only one VTF file can be bound at a time.
    /// This is a limitation of the underlying library's C API.
    /// Mutably borrows the [`VtfGuard`] obtained from library initialization to enforce this.
    ///
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

    /// Compute how much memory a specified image needs in bytes.
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

    /// Compute the number of mip levels needed for an image of given dimensions.
    /// The number includes the original image, and counts mipmaps down to 1x1 pixels.
    #[must_use]
    pub fn compute_mipmap_count(width: u32, height: u32, depth: u32) -> u32 {
        unsafe { ffi::vlImageComputeMipmapCount(width, height, depth) }
    }

    /// Compute the dimensions of a given mip level for an image of given dimensions.
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

    /// Compute how much memory a given mipmap level needs in bytes.
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

    /// Convert an image stored in a given format to [`ImageFormat::Rgba8888`].
    ///
    /// # Errors
    ///
    /// Returns `Err` if `source` has invalid length or if the conversion fails.
    pub fn convert_image_to_rgba8888(
        source: &[u8],
        width: u32,
        height: u32,
        source_format: ImageFormat,
    ) -> Result<Vec<u8>> {
        let source_size = VtfFile::compute_image_size(width, height, 1, 1, source_format) as usize;
        let target_size =
            VtfFile::compute_image_size(width, height, 1, 1, ImageFormat::Rgba8888) as usize;
        if source.len() < source_size {
            return Err(Error::InvalidLength);
        }
        let mut buf = Vec::with_capacity(target_size);
        unsafe {
            ffi_try!(ffi::vlImageConvertToRGBA8888(
                source.as_ptr() as *mut u8,
                buf.as_mut_ptr(),
                width,
                height,
                source_format as i32
            ));
            buf.set_len(target_size);
        }
        Ok(buf)
    }

    /// Convert an image stored in [`ImageFormat::Rgba8888`] to a given format.
    ///
    /// # Errors
    ///
    /// Returns `Err` if `source` has invalid length or if the conversion fails.
    pub fn convert_image_from_rgba8888(
        source: &[u8],
        width: u32,
        height: u32,
        dest_format: ImageFormat,
    ) -> Result<Vec<u8>> {
        let source_size =
            VtfFile::compute_image_size(width, height, 1, 1, ImageFormat::Rgba8888) as usize;
        let target_size = VtfFile::compute_image_size(width, height, 1, 1, dest_format) as usize;
        if source.len() < source_size {
            return Err(Error::InvalidLength);
        }
        let mut buf = Vec::with_capacity(target_size);
        unsafe {
            ffi_try!(ffi::vlImageConvertFromRGBA8888(
                source.as_ptr() as *mut u8,
                buf.as_mut_ptr(),
                width,
                height,
                dest_format as i32
            ));
            buf.set_len(target_size);
        }
        Ok(buf)
    }

    /// Convert between supported image formats.
    ///
    /// # Errors
    ///
    /// Returns `Err` if `source` has invalid length or if the conversion fails.
    pub fn convert_image(
        source: &[u8],
        width: u32,
        height: u32,
        source_format: ImageFormat,
        dest_format: ImageFormat,
    ) -> Result<Vec<u8>> {
        let source_size = VtfFile::compute_image_size(width, height, 1, 1, source_format) as usize;
        let target_size = VtfFile::compute_image_size(width, height, 1, 1, dest_format) as usize;
        if source.len() < source_size {
            return Err(Error::InvalidLength);
        }
        let mut buf = Vec::with_capacity(target_size);
        unsafe {
            ffi_try!(ffi::vlImageConvert(
                source.as_ptr() as *mut u8,
                buf.as_mut_ptr(),
                width,
                height,
                source_format as i32,
                dest_format as i32
            ));
            buf.set_len(target_size);
        }
        Ok(buf)
    }

    /// Convert an image stored in [`ImageFormat::Rgba8888`] to a normal map.
    ///
    /// # Errors
    ///
    /// Returns `Err` if `source` has invalid length or if the conversion fails.
    pub fn convert_image_to_normal_map(
        source: &[u8],
        width: u32,
        height: u32,
        settings: &NormalMapConversionSettings,
    ) -> Result<Vec<u8>> {
        let size = VtfFile::compute_image_size(width, height, 1, 1, ImageFormat::Rgba8888) as usize;
        if source.len() < size {
            return Err(Error::InvalidLength);
        }
        let mut buf = Vec::with_capacity(size);
        unsafe {
            ffi_try!(ffi::vlImageConvertToNormalMap(
                source.as_ptr() as *mut u8,
                buf.as_mut_ptr(),
                width,
                height,
                settings.kernel_filter as i32,
                settings.height_conversion_method as i32,
                settings.normal_alpha_result as i32,
                settings.minimum_z,
                settings.scale,
                ffi_bool(settings.wrap),
                ffi_bool(settings.invert_x),
                ffi_bool(settings.invert_y),
            ));
            buf.set_len(size);
        }
        Ok(buf)
    }

    /// Resize an image stored in [`ImageFormat::Rgba8888`] to the given dimensions.
    ///
    /// # Errors
    ///
    /// Returns `Err` if `source` has invalid length or if the resize fails.
    pub fn resize_image(
        source: &[u8],
        source_width: u32,
        source_height: u32,
        target_width: u32,
        target_height: u32,
        resize_filter: MipmapFilter,
        sharpen_filter: SharpenFilter,
    ) -> Result<Vec<u8>> {
        let source_size =
            VtfFile::compute_image_size(source_width, source_height, 1, 1, ImageFormat::Rgba8888)
                as usize;
        let target_size =
            VtfFile::compute_image_size(target_width, target_height, 1, 1, ImageFormat::Rgba8888)
                as usize;
        if source.len() < source_size {
            return Err(Error::InvalidLength);
        }
        let mut buf = Vec::with_capacity(target_size);
        unsafe {
            ffi_try!(ffi::vlImageResize(
                source.as_ptr() as *mut u8,
                buf.as_mut_ptr(),
                source_width,
                source_height,
                target_width,
                target_height,
                resize_filter as i32,
                sharpen_filter as i32,
            ));
            buf.set_len(target_size);
        }
        Ok(buf)
    }

    /// Apply gamma correction to an image stored in [`ImageFormat::Rgba8888`].
    ///
    /// # Errors
    ///
    /// Returns `Err` if `data` has invalid length.
    pub fn correct_image_gamma(
        data: &mut [u8],
        width: u32,
        height: u32,
        gamma_correction: f32,
    ) -> Result<()> {
        let size = VtfFile::compute_image_size(width, height, 1, 1, ImageFormat::Rgba8888) as usize;
        if data.len() < size {
            return Err(Error::InvalidLength);
        }
        unsafe {
            ffi::vlImageCorrectImageGamma(data.as_mut_ptr(), width, height, gamma_correction);
        }
        Ok(())
    }

    /// Compute the reflectivity of an image stored in [`ImageFormat::Rgba8888`].
    ///
    /// # Errors
    ///
    /// Returns `Err` if `data` has invalid length.
    pub fn compute_image_reflectivity(
        data: &[u8],
        width: u32,
        height: u32,
    ) -> Result<(f32, f32, f32)> {
        let size = VtfFile::compute_image_size(width, height, 1, 1, ImageFormat::Rgba8888) as usize;
        if data.len() < size {
            return Err(Error::InvalidLength);
        }
        let mut x = 0.0;
        let mut y = 0.0;
        let mut z = 0.0;
        unsafe {
            ffi::vlImageComputeImageReflectivity(
                data.as_ptr() as *mut u8,
                width,
                height,
                &mut x,
                &mut y,
                &mut z,
            );
        }
        Ok((x, y, z))
    }

    /// Flip an image stored in [`ImageFormat::Rgba8888`] vertically along its X-axis.
    ///
    /// # Errors
    ///
    /// Returns `Err` if `data` has invalid length.
    pub fn flip_image(data: &mut [u8], width: u32, height: u32) -> Result<()> {
        let size = VtfFile::compute_image_size(width, height, 1, 1, ImageFormat::Rgba8888) as usize;
        if data.len() < size {
            return Err(Error::InvalidLength);
        }
        unsafe {
            ffi::vlImageFlipImage(data.as_mut_ptr(), width, height);
        }
        Ok(())
    }

    /// Flip an image stored in [`ImageFormat::Rgba8888`] horizontally along its Y-axis.
    ///
    /// # Errors
    ///
    /// Returns `Err` if `data` has invalid length.
    pub fn mirror_image(data: &mut [u8], width: u32, height: u32) -> Result<()> {
        let size = VtfFile::compute_image_size(width, height, 1, 1, ImageFormat::Rgba8888) as usize;
        if data.len() < size {
            return Err(Error::InvalidLength);
        }
        unsafe {
            ffi::vlImageMirrorImage(data.as_mut_ptr(), width, height);
        }
        Ok(())
    }
}

impl<'a> Drop for VtfFile<'a> {
    fn drop(&mut self) {
        unsafe {
            ffi::vlDeleteImage(self.handle);
        }
    }
}

/// A bound vtf file ready for manipulation.
/// Only one vtf file can be bound at a time.
/// A bound vtf file can be either manually unbound,
/// or automatically when it is dropped.
#[must_use]
pub struct BoundVtfFile<'a, 'b> {
    // mutable borrow ensures only one vtf file is bound at a time
    marker: PhantomData<&'b mut VtfGuard>,
    vtf_file: VtfFile<'a>,
}

impl<'a, 'b> BoundVtfFile<'a, 'b> {
    /// Unbind the image.
    /// Allows other images to be bound.
    pub fn unbind(self) -> VtfFile<'a> {
        self.vtf_file
    }

    /// Build a new empty image.
    pub fn new_empty(&mut self, width: u32, height: u32) -> EmptyImageBuilder {
        EmptyImageBuilder::new(width, height)
    }

    /// Load an image from existing [`ImageFormat::Rgba8888`] data.
    pub fn from_rgba8888(&mut self, width: u32, height: u32) -> Rgba8888ImageBuilder {
        Rgba8888ImageBuilder::new(width, height)
    }

    /// Destroy the contained image, if any.
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

    /// Save the image into `buffer`. Returns bytes written.
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

    /// Save the image into a [`Vec`].
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
        #[doc = "Check if image data has been loaded."]
        #[doc = "Returns false if a VTF file was loaded with header only."]
        pub fn has_image(&self) -> bool => ffi::vlImageGetHasImage() as u8 == ffi::vlTrue,

        #[doc = "Returns the VTF file major version number."]
        pub fn major_version(&self) -> u32 => ffi::vlImageGetMajorVersion(),
        #[doc = "Returns the VTF file minor version number."]
        pub fn minor_version(&self) -> u32 => ffi::vlImageGetMinorVersion(),
        #[doc = "Returns the VTF file size in bytes."]
        pub fn size(&self) -> usize => ffi::vlImageGetSize() as usize,

        #[doc = "Returns the width of the image in pixels."]
        pub fn width(&self) -> u32 => ffi::vlImageGetWidth(),
        #[doc = "Returns the height of the image in pixels."]
        pub fn height(&self) -> u32 => ffi::vlImageGetHeight(),
        #[doc = "Returns the depth of the image in pixels."]
        pub fn depth(&self) -> u32 => ffi::vlImageGetDepth(),

        #[doc = "Returns the frame count of the image."]
        pub fn frame_count(&self) -> u32 => ffi::vlImageGetFrameCount(),
        #[doc = "Returns the face count of the image."]
        pub fn face_count(&self) -> u32 => ffi::vlImageGetFaceCount(),
        #[doc = "Returns the number of mip levels in the image."]
        pub fn mipmap_count(&self) -> u32 => ffi::vlImageGetMipmapCount(),

        #[doc = "Returns the start frame of the image."]
        pub fn start_frame(&self) -> u32 => ffi::vlImageGetStartFrame(),

        #[doc = "Returns the image flags."]
        pub fn flags(&self) -> ImageFlags => ImageFlags::from_bits_truncate(ffi::vlImageGetFlags()),

        #[doc = "Check if the given flag is set."]
        #[allow(clippy::cast_possible_wrap)]
        pub fn flag(&self, flag: ImageFlags) -> bool => ffi::vlImageGetFlag(flag.bits as i32) == ffi::vlTrue,

        #[doc = "Returns the bump scale value."]
        pub fn bumpmap_scale(&self) -> f32 => ffi::vlImageGetBumpmapScale(),

        #[doc = "Returns the format of the image."]
        #[doc = "Returns `None` if the format is unknown or if an image is not loaded."]
        pub fn format(&self) -> Option<ImageFormat> => ImageFormat::from_ffi(ffi::vlImageGetFormat()),

        #[doc = "Check whether the current image has a thumbnail."]
        pub fn has_thumbnail(&self) -> bool => ffi::vlImageGetHasThumbnail() == ffi::vlTrue,
        #[doc = "Returns the width the thumbnail in pixels."]
        pub fn thumbnail_width(&self) -> u32 => ffi::vlImageGetThumbnailWidth(),
        #[doc = "Returns the height the thumbnail in pixels."]
        pub fn thumbnail_height(&self) -> u32 => ffi::vlImageGetThumbnailHeight(),
        #[doc = "Returns the format of the thumbnail."]
        #[doc = "Returns `None` if the format is unknown or if there is no thumbnail."]
        pub fn thumbnail_format(&self) -> Option<ImageFormat> => ImageFormat::from_ffi(ffi::vlImageGetThumbnailFormat()),

        #[doc = "Check whether the current VTF version supports resources."]
        pub fn supports_resource(&self) -> bool => ffi::vlImageGetSupportsResources() == ffi::vlTrue,
        #[doc = "Returns the number of resource contained within the VTF file."]
        pub fn resource_count(&self) -> u32 => ffi::vlImageGetResourceCount(),

        #[allow(clippy::cast_possible_wrap)]
        #[doc = "Returns the type of the resource at the given index."]
        #[doc = "Returns `None` if the given index doesn't exist or if resources are not supported."]
        pub fn resource_type(&self, index: u32) -> Option<ResourceType> => ResourceType::from_ffi(ffi::vlImageGetResourceType(index) as i32),

        #[doc = "Check whether the resource of the given type exists."]
        pub fn has_resource(&self, resource: ResourceType) -> bool => ffi::vlImageGetHasResource(resource as u32) == ffi::vlTrue
    );

    ffi_setters! {
        #[doc = "Sets the start frame of the image."]
        pub fn set_start_frame(&mut self, start_frame: u32) => ffi::vlImageSetStartFrame(start_frame),
        #[doc = "Sets the flags of the image."]
        pub fn set_flags(&mut self, flags: ImageFlags) => ffi::vlImageSetFlags(flags.bits),
        #[allow(clippy::cast_possible_wrap)]
        #[doc = "Sets the state of a specific flag in the image"]
        pub fn set_flag(&mut self, flag: ImageFlags, state: bool) => ffi::vlImageSetFlag(flag.bits as i32, ffi_bool(state)),
        #[doc = "Sets the bump scale value."]
        pub fn set_bumpmap_scale(&mut self, scale: f32) => ffi::vlImageSetBumpmapScale(scale),
        #[doc = "Sets the reflectivity values of the image."]
        pub fn set_reflectivity(&mut self, x: f32, y: f32, z: f32) => ffi::vlImageSetReflectivity(x, y, z)
    }

    /// Gets the reflectivity values of the image.
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

    /// Get a reference to the image data of a specific image.
    /// The data is in the format returned by `format()`.
    /// Returns `None` if the specified data doesn't exist or if an image is not loaded.
    #[must_use]
    pub fn data(&self, frame: u32, face: u32, slice: u32, mipmap_level: u32) -> Option<&[u8]> {
        if !self.has_image() {
            return None;
        }
        if !self.verify_data_invariants(frame, face, slice, mipmap_level) {
            return None;
        }
        return unsafe { self.data_unchecked(frame, face, slice, mipmap_level) };
    }

    /// Get an unchecked reference to the image data of the specified image.
    /// The data is in the format returned by `format()`.
    ///
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

    /// Sets the image data of the specified image.
    /// The data must be in the format returned by `format()`.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the data length is not correct, if the format is invalid,
    /// if the parameters are not valid or if an image is not loaded.
    pub fn set_data(
        &mut self,
        frame: u32,
        face: u32,
        slice: u32,
        mipmap_level: u32,
        data: &[u8],
    ) -> Result<()> {
        if !self.has_image() {
            return Err(Error::ImageNotLoaded);
        }
        if !self.verify_data_invariants(frame, face, slice, mipmap_level) {
            return Err(Error::InvalidParameters);
        }
        let data_len = self.data_len(mipmap_level).ok_or(Error::InvalidFormat)?;
        if data.len() < data_len {
            return Err(Error::InvalidLength);
        }
        unsafe { self.set_data_unchecked(frame, face, slice, mipmap_level, data) }
        Ok(())
    }

    /// Sets the image data of the specified image.
    /// The data must be in the format returned by `format()`.
    ///
    /// # Safety
    ///
    /// If any parameters are invalid,
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

    /// Get a reference to the thumbnail data.
    /// The data is in the format returned by `thumbnail_format()`.
    /// Returns `None` if there is no thumbnail data or if an image is not loaded.
    #[must_use]
    pub fn thumbnail_data(&self) -> Option<&[u8]> {
        if !self.has_image() {
            return None;
        }
        let data_len = self.thumbnail_data_len()?;
        unsafe {
            let data = ffi::vlImageGetThumbnailData();
            if data.is_null() {
                return None;
            }
            Some(slice::from_raw_parts(data, data_len))
        }
    }

    /// Sets the thumbnail image data.
    /// The data must be in the format returned by `thumbnail_format()`.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the data length is not correct, if the format is invalid or if an image is not loaded.
    pub fn set_thumbnail_data(&mut self, data: &[u8]) -> Result<()> {
        if !self.has_image() {
            return Err(Error::ImageNotLoaded);
        }
        let data_len = self.thumbnail_data_len().ok_or(Error::InvalidFormat)?;
        if data.len() < data_len {
            return Err(Error::InvalidLength);
        }
        unsafe {
            ffi::vlImageSetThumbnailData(data.as_ptr() as *mut u8);
        }
        Ok(())
    }

    /// Get a reference to the given resource data.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the resource data cannot be get or if an image is not loaded.
    pub fn resource_data(&self, resource: ResourceType) -> Result<&[u8]> {
        if !self.has_image() {
            return Err(Error::ImageNotLoaded);
        }
        let mut data_len = 0;
        unsafe {
            let data = ffi_result(ffi::vlImageGetResourceData(resource as u32, &mut data_len))?;
            Ok(slice::from_raw_parts(data.cast(), data_len as usize))
        }
    }

    /// Sets the given resource data.
    /// This creates the resource if it doesn't exist.
    /// Passing an empty slice deletes the resource.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the resource data cannot be set or if an image is not loaded.
    pub fn set_resource_data(&mut self, resource: ResourceType, data: &[u8]) -> Result<()> {
        if !self.has_image() {
            return Err(Error::ImageNotLoaded);
        }
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

    /// Generates mipmaps for the given face and frame.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the mipmap generation fails,
    /// or if the parameters are invalid.
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

    /// Generates mipmaps for all faces and frames.
    ///
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

    /// Generates the thumbnail.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the thumbnail generation fails.
    pub fn generate_thumbnail(&mut self) -> Result<()> {
        unsafe {
            ffi_try!(ffi::vlImageGenerateThumbnail());
        }
        Ok(())
    }

    /// Converts the image to a normal map using the data in the given frame.
    ///
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

    /// Converts the image to a normal map using mip level 0 as the source.
    ///
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

    /// Generates a shere map using the 6 cubemap faces of the image.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the sphere map generation fails.
    pub fn generate_sphere_map(&mut self) -> Result<()> {
        unsafe {
            ffi_try!(ffi::vlImageGenerateSphereMap());
        }
        Ok(())
    }

    /// Computes and sets the reflectivity values of the image based on pixels' color averages.
    ///
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

/// A builder for a new empty image.
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
        #[doc = "Sets the number of frames in the image."]
        pub fn frames(mut self, frames: u32) -> Self => self.frames = frames,
        #[doc = "Sets the number of faces in the image."]
        pub fn faces(mut self, faces: u32) -> Self => self.faces = faces,
        #[doc = "Sets the number of z slices in the image."]
        pub fn slices(mut self, slices: u32) -> Self => self.slices = slices,
        #[doc = "Sets the format of the image."]
        pub fn format(mut self, format: ImageFormat) -> Self => self.format = format,
        #[doc = "Sets whether the image will contain a thumbnail."]
        pub fn thumbnail(mut self, thumbnail: bool) -> Self => self.thumbnail = thumbnail,
        #[doc = "Sets whether the image will contain mipmaps."]
        pub fn mipmaps(mut self, mipmaps: bool) -> Self => self.mipmaps = mipmaps,
        #[doc = "Sets whether the image data will be zeroed on creation."]
        pub fn null_data(mut self, null_data: bool) -> Self => self.null_data = null_data
    }

    /// Create the empty image.
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

/// A builder for an image from existing RGBA8888 data.
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
        #[doc = "Sets the version of the created image."]
        pub fn version(mut self, major: u32, minor: u32) -> Self => self.options.uiVersion = [major, minor],
        #[doc = "Sets the format of the created image."]
        pub fn format(mut self, format: ImageFormat) -> Self => self.options.ImageFormat = format as i32,
        #[doc = "Sets the flags of the created image."]
        pub fn flags(mut self, flags: ImageFlags) -> Self => self.options.uiFlags = flags.bits,
        #[doc = "Enables the given flag in the created image."]
        pub fn with_flag(mut self, flag: ImageFlags) -> Self => self.options.uiFlags |= flag.bits,
        #[doc = "Disables the given flag in the created image."]
        pub fn without_flag(mut self, flag: ImageFlags) -> Self => self.options.uiFlags &= !flag.bits,
        #[doc = "Sets the start frame of the created image."]
        pub fn start_frame(mut self, frame: u32) -> Self => self.options.uiStartFrame = frame,
        #[doc = "Sets the bump scale of the created image."]
        pub fn bump_scale(mut self, scale: f32) -> Self => self.options.sBumpScale = scale,
        #[doc = "Sets the reflectivity values of the created image."]
        pub fn reflectivity(mut self, reflectivity: [f32; 3]) -> Self => self.options.sReflectivity = reflectivity,
        #[doc = "Sets whether to generate mipmaps."]
        pub fn generate_mipmaps(mut self, mipmaps: bool) -> Self => self.options.bMipmaps = ffi_bool(mipmaps),
        #[doc = "Sets the mipmap resize filter."]
        pub fn mipmap_filter(mut self, filter: MipmapFilter) -> Self => self.options.MipmapFilter = filter as i32,
        #[doc = "Sets the mipmap sharpen filter"]
        pub fn mipmap_sharpen_filter(mut self, filter: SharpenFilter) -> Self => self.options.MipmapSharpenFilter = filter as i32,
        #[doc = "Sets whether to generate a thumbnail."]
        pub fn generate_thumbnail(mut self, thumbnail: bool) -> Self => self.options.bThumbnail = ffi_bool(thumbnail),
        #[doc = "Sets whether to compute the reflectivity of the image."]
        pub fn compute_reflectivity(mut self, reflectivity: bool) -> Self => self.options.bReflectivity = ffi_bool(reflectivity),
        #[doc = "Sets whether to resize the input image."]
        pub fn resize(mut self, resize: bool) -> Self => self.options.bResize = ffi_bool(resize),
        #[doc = "Sets the resize method."]
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
        #[doc = "Sets the resize filter."]
        pub fn resize_filter(mut self, filter: MipmapFilter) -> Self => self.options.ResizeFilter = filter as i32,
        #[doc = "Sets the resize sharpen filter."]
        pub fn resize_sharpen_filter(mut self, filter: MipmapFilter) -> Self => self.options.ResizeSharpenFilter = filter as i32,
        #[doc = "Sets whether to clamp the resize size."]
        pub fn resize_clamp(mut self, clamp: bool) -> Self => self.options.bResizeClamp = ffi_bool(clamp),
        #[doc = "Sets the dimensions to clamp the resize to."]
        pub fn resize_clamp_dimensions(mut self, width: u32, heigth: u32) -> Self => {
            self.options.uiResizeClampWidth = width;
            self.options.uiResizeClampHeight = heigth;
        },
        #[doc = "Apply gamma correction to the input image."]
        pub fn gamma_correct(mut self, amount: f32) -> Self => {
            self.options.bGammaCorrection = ffi::vlTrue;
            self.options.sGammaCorrection = amount;
        },
        #[doc = "Convert the input image into a normal map."]
        pub fn normal_map(mut self, settings: NormalMapConversionSettings) -> Self => {
            self.options.KernelFilter = settings.kernel_filter as i32;
            self.options.HeightConversionMethod = settings.height_conversion_method as i32;
            self.options.NormalAlphaResult = settings.normal_alpha_result as i32;
            self.options.bNormalMinimumZ = settings.minimum_z;
            self.options.sNormalScale = settings.scale;
            self.options.bNormalWrap = ffi_bool(settings.wrap);
            self.options.bNormalInvertX = ffi_bool(settings.invert_x);
            self.options.bNormalInvertY = ffi_bool(settings.invert_y);
        },
        #[doc = "Sets whether to generate a sphere map for six-faced cubemap images."]
        pub fn sphere_map(mut self, sphere_map: bool) -> Self => self.options.bSphereMap = ffi_bool(sphere_map)
    }

    /// Create a single frame image from `data`.
    /// Data needs to be `mut` because some creation options are applied in place.
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
    /// Data needs to be `mut` because some creation options are applied in place.
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

/// Settings for normal map generation.
#[must_use]
#[derive(Debug, Clone, PartialEq)]
pub struct NormalMapConversionSettings {
    kernel_filter: KernelFilter,
    height_conversion_method: HeightConversionMethod,
    normal_alpha_result: NormalAlphaResult,
    minimum_z: u8,
    scale: f32,
    wrap: bool,
    invert_x: bool,
    invert_y: bool,
}

impl NormalMapConversionSettings {
    pub fn new() -> Self {
        Self {
            kernel_filter: KernelFilter::Filter3X3,
            height_conversion_method: HeightConversionMethod::AverageRgb,
            normal_alpha_result: NormalAlphaResult::White,
            minimum_z: 0,
            scale: 2.0,
            wrap: false,
            invert_x: false,
            invert_y: false,
        }
    }

    builder! {
        #[doc = "Sets the normal map generation kernel."]
        pub fn kernel_filter(mut self, filter: KernelFilter) -> Self => self.kernel_filter = filter,
        #[doc = "Sets the method of determining height from input image."]
        pub fn height_conversion_method(mut self, method: HeightConversionMethod) -> Self => self.height_conversion_method = method,
        #[doc = "Sets the output image alpha channel handling."]
        pub fn alpha_result(mut self, result: NormalAlphaResult) -> Self => self.normal_alpha_result = result,
        #[doc = "Sets the minimum normal Z value."]
        pub fn minimum_z(mut self, min: u8) -> Self => self.minimum_z = min,
        #[doc = "Sets the normal map scale."]
        pub fn scale(mut self, scale: f32) -> Self => self.scale = scale,
        #[doc = "Sets whether to wrap the normal map."]
        pub fn wrap(mut self, wrap: bool) -> Self => self.wrap = wrap,
        #[doc = "Sets whether to invert the normal X component."]
        pub fn invert_x(mut self, invert: bool) -> Self => self.invert_x = invert,
        #[doc = "Sets whether to invert the normal Y component."]
        pub fn invert_y(mut self, invert: bool) -> Self => self.invert_y = invert
    }
}

impl Default for NormalMapConversionSettings {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn initialization() {
        assert_eq!(UNINIT_COUNTER.load(Ordering::Acquire), 0);
        let (vtflib, guard) = VtfLib::initialize().unwrap();
        assert!(VtfLib::initialize().is_none());
        assert_eq!(UNINIT_COUNTER.load(Ordering::Acquire), 2);
        drop(vtflib);
        assert!(VtfLib::initialize().is_none());
        assert_eq!(UNINIT_COUNTER.load(Ordering::Acquire), 1);
        drop(guard);
        assert_eq!(UNINIT_COUNTER.load(Ordering::Acquire), 0);
        assert!(VtfLib::initialize().is_some());
    }
}
