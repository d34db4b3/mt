use base64::Engine;
use ecdsa::signature::digest::Digest;
use ecdsa::signature::digest::FixedOutput;
use ecdsa::{
    elliptic_curve::ScalarPrimitive,
    hazmat::{DigestPrimitive, SignPrimitive},
};
use p256::{ecdsa::Signature, NistP256, Scalar};

fn main() {
    // let key = SigningKey::random(&mut OsRng);
    let da: Scalar = ScalarPrimitive::<NistP256>::from_bytes(
        &[
            59u8, 180, 10, 132, 238, 210, 114, 98, 32, 88, 154, 250, 41, 45, 157, 206, 73, 119, 5,
            160, 151, 201, 244, 30, 166, 24, 2, 215, 156, 200, 5, 53,
        ]
        .into(),
    )
    .unwrap()
    .into();
    let k: Scalar = ScalarPrimitive::<NistP256>::from_bytes(
        &[
            170, 139, 84, 161, 152, 234, 233, 151, 60, 193, 131, 139, 5, 79, 248, 76, 29, 40, 209,
            138, 131, 121, 5, 131, 47, 62, 2, 133, 138, 225, 163, 55,
        ]
        .into(),
    )
    .unwrap()
    .into();

    println!("da: {}", hex::encode(da.to_bytes()));
    println!("k: {}", hex::encode(k.to_bytes()));
    let msg1 = b"msg1";
    let msg2 = b"msg2";
    // ecdsa::hazmat::SignPrimitive::try_sign_prehashed(&self, k, z)
    let sig1: Signature = da
        .try_sign_prehashed(
            k,
            &<NistP256 as DigestPrimitive>::Digest::new_with_prefix(&msg1).finalize_fixed(),
        )
        .unwrap()
        .0;
    println!("r1: {}", hex::encode(sig1.r().to_bytes()));
    println!("s1: {}", hex::encode(sig1.s().to_bytes()));
    println!(
        "sig1: {}",
        base64::engine::general_purpose::URL_SAFE_NO_PAD.encode(&sig1.to_bytes())
    );
    let sig2: Signature = da
        .try_sign_prehashed(
            k,
            &<NistP256 as DigestPrimitive>::Digest::new_with_prefix(&msg2).finalize_fixed(),
        )
        .unwrap()
        .0;
    println!("r2: {}", hex::encode(sig2.r().to_bytes()));
    println!("s2: {}", hex::encode(sig2.s().to_bytes()));
    println!(
        "sig2: {}",
        base64::engine::general_purpose::URL_SAFE_NO_PAD.encode(&sig2.to_bytes())
    );
}
