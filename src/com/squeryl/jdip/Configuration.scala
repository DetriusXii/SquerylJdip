package com.squeryl.jdip

case class Configuration(username: String, 
    password: String, 
    configFile: String,
    jdbcURL: String, 
    connection: () => java.sql.Connection)