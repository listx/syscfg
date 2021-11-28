use config::{Config, ConfigError};

#[derive(Debug, serde::Deserialize)]
pub struct Server {
    pub domain: String,
    pub port: u16,
}

#[derive(Debug, serde::Deserialize)]
pub struct Settings {
    pub server: Server,
}

impl Settings {
    pub fn new() -> Result<Self, ConfigError> {
        let mut s = Config::default();
        // Add in `./settings.yaml`
        s.merge(config::File::new(
            &format!("{}/syscfg/lhc/settings.yaml", env!("HOME")),
            config::FileFormat::Yaml,
        ))?;
        // Add in settings from the environment (with a prefix of LHC)
        // Eg.. `LHC_DEBUG=1 ./target/app` would set the `debug` key
        s.merge(config::Environment::with_prefix("LHC"))?;

        s.try_into()
    }
}
