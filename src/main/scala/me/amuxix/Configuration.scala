package me.amuxix

import com.typesafe.config.{Config, ConfigFactory}
import pureconfig.ConfigSource
import pureconfig.generic.auto._

case class Configuration(
  token: String,
)

object Configuration {
  def fromConfig(config: Config = ConfigFactory.load()): Configuration = ConfigSource
    .fromConfig(config)
    .loadOrThrow[Configuration]
}
