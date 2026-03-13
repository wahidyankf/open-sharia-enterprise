package com.demobektkt.infrastructure

import com.demobektkt.infrastructure.tables.AttachmentsTable
import com.demobektkt.infrastructure.tables.ExpensesTable
import com.demobektkt.infrastructure.tables.TokensTable
import com.demobektkt.infrastructure.tables.UsersTable
import org.jetbrains.exposed.sql.Database
import org.jetbrains.exposed.sql.SchemaUtils
import org.jetbrains.exposed.sql.transactions.transaction

object DatabaseFactory {
  fun init(jdbcUrl: String, user: String, password: String) {
    Database.connect(url = jdbcUrl, user = user, password = password)
    transaction { SchemaUtils.create(UsersTable, TokensTable, ExpensesTable, AttachmentsTable) }
  }
}
