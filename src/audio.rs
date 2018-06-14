use err::Error;
use simplemad::Decoder;
use std::io::{Read, Write};
use vorbis::{Encoder, VorbisQuality};

#[cfg(feature = "mp3_to_vorbis")]
pub fn mp3_to_vorbis<R: Read, H: Into<Option<usize>>>(
    mp3_buf: R,
    size_hint: H,
) -> Result<Vec<u8>, Error> {
    let decoder = Decoder::decode(mp3_buf)?;

    let mut decoded_data = Vec::with_capacity(size_hint.into().unwrap_or(64));

    let mut target_rate: Option<u32> = None;
    for decoding_result in decoder {
        let frame = match decoding_result {
            Ok(f) => f,
            Err(e) => {
                #[cfg(debug_assertions)]
                eprintln!("{:?}", e);

                continue;
            },
        };

        if let Some(tr) = target_rate {
            //assert_eq!(tr, frame.sample_rate);
            if tr != frame.sample_rate {
                // Dummy error
                return Err(Error::Mad(::simplemad::SimplemadError::EOF));
            }
        } else {
            target_rate = Some(frame.sample_rate);
        }

        //assert_eq!(frame.samples.len(), 2);
        if frame.samples.len() != 2 {
            // Dummy error
            return Err(Error::Mad(::simplemad::SimplemadError::EOF));
        }
        let (l_channel, r_channel) = (&frame.samples[0], &frame.samples[1]);

        for (l, r) in l_channel.iter().zip(r_channel) {
            decoded_data.push(l.to_i16());
            decoded_data.push(r.to_i16());
        }
    }

    let mut encoder = Encoder::new(
        2,
        target_rate
            .ok_or(Error::Mad(::simplemad::SimplemadError::EOF))?
            .into(),
        VorbisQuality::HighQuality,
    )?;

    encoder.encode(&decoded_data).map_err(|e| e.into())
}

pub fn ffmpeg_mp3_to_vorbis(
    a: ::nx::audio::Audio,
    audio_id: u32,
) -> Result<Vec<u8>, Error> {
    let mp3_f_name = format!("__nx_edit__TEMP__0{}.mp3", audio_id);
    let ogg_f_name = format!("__nx_edit__TEMP__0{}.ogg", audio_id);

    {
        let mut mp3_f = ::std::fs::File::create(&mp3_f_name)?;

        mp3_f.write_all(a.data())?;
        mp3_f.sync_all()?;

        if !::std::process::Command::new("ffmpeg")
            .arg("-i")
            .arg(&mp3_f_name)
            .arg("-c:a")
            .arg("libvorbis")
            .arg("-q:a")
            .arg("8")
            .arg(&ogg_f_name)
            .stdout(::std::process::Stdio::null())
            .stderr(::std::process::Stdio::null())
            .spawn()?
            .wait()?
            .success()
        {
            eprintln!(
                "Not even ffmpeg can handle your shitty MP3, so it is being \
                 trashed."
            );

            ::std::fs::remove_file(&mp3_f_name);
            ::std::fs::remove_file(&ogg_f_name);

            return Ok(Vec::new());
        }
    }

    ::std::fs::remove_file(&mp3_f_name)?;

    let ogg_data = {
        let mut ogg_f = ::std::fs::File::open(&ogg_f_name)?;

        let mut ogg_f_data: Vec<u8> =
            Vec::with_capacity(ogg_f.metadata()?.len() as usize);
        ogg_f.read_to_end(&mut ogg_f_data)?;

        ogg_f_data
    };

    ::std::fs::remove_file(&ogg_f_name)?;

    Ok(ogg_data)
}
