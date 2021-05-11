use serial_test::serial;
use vtflib::{ImageFormat, VtfLib};

#[test]
#[serial]
fn vtf_create_empty() {
    let (vtflib, mut guard) = VtfLib::new().unwrap();

    let vtf = vtflib.new_vtf_file();

    let mut vtf = vtf.bind(&mut guard);
    vtf.build_empty(512, 512).null_data(true).create().unwrap();
    assert!(vtf.has_image());
}

#[test]
#[serial]
fn vtf_load_save() {
    let (vtflib, mut guard) = VtfLib::new().unwrap();

    let vtf = vtflib.new_vtf_file();

    let original = include_bytes!("test.vtf");

    let mut vtf = vtf.bind(&mut guard);
    vtf.load(original).unwrap();
    assert!(vtf.has_image());
    assert_eq!(vtf.width(), 1024);
    assert_eq!(vtf.height(), 1024);
    assert_eq!(vtf.format(), Some(ImageFormat::Dxt1));

    let mut buf = vec![0; vtf.size()];
    vtf.save(buf.as_mut()).unwrap();
    assert_eq!(buf, original);
}

#[test]
#[serial]
fn vtf_two_vtfs() {
    let (vtflib, mut guard) = VtfLib::new().unwrap();

    let vtf1 = vtflib.new_vtf_file();
    let vtf2 = vtflib.new_vtf_file();

    let mut vtf1 = vtf1.bind(&mut guard);
    vtf1.build_empty(512, 512).null_data(true).create().unwrap();
    assert!(vtf1.has_image());

    let mut vtf2 = vtf2.bind(&mut guard);
    vtf2.load(include_bytes!("test.vtf")).unwrap();
    assert_eq!(vtf2.width(), 1024);
    assert_eq!(vtf2.height(), 1024);
    assert_eq!(vtf2.format(), Some(ImageFormat::Dxt1));
}
