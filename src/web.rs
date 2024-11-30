use eframe::{wasm_bindgen, wasm_bindgen::prelude::*, wasm_bindgen::JsCast, web_sys};
use web_sys::{js_sys::Array, Blob};

#[wasm_bindgen]
pub fn save_file(text: &str, filename: &str) {
    let window = web_sys::window().expect("window should exist");
    let document = window.document().expect("document should exist on window");

    // Create blob with text data
    let array = Array::new();
    array.push(&JsValue::from_str(text));
    let blob = Blob::new_with_str_sequence(&array).expect("Failed to create blob");

    // Create an object URL
    let url =
        web_sys::Url::create_object_url_with_blob(&blob).expect("Failed to create object URL");

    // Create an anchor element
    let a = document
        .create_element("a")
        .expect("Failed to create anchor element");
    let a = a
        .dyn_into::<web_sys::HtmlAnchorElement>()
        .expect("Failed to cast to anchor");

    a.set_href(&url);
    a.set_download(filename);
    a.click();
    // Clean up the object URL
    let _ = web_sys::Url::revoke_object_url(&url);
}
