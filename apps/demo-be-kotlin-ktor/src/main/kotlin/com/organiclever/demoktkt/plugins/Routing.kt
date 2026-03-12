package com.organiclever.demoktkt.plugins

import com.organiclever.demoktkt.routes.AdminRoutes
import com.organiclever.demoktkt.routes.AttachmentRoutes
import com.organiclever.demoktkt.routes.AuthRoutes
import com.organiclever.demoktkt.routes.ExpenseRoutes
import com.organiclever.demoktkt.routes.ReportRoutes
import com.organiclever.demoktkt.routes.TokenRoutes
import com.organiclever.demoktkt.routes.UserRoutes
import io.ktor.server.application.Application
import io.ktor.server.auth.authenticate
import io.ktor.server.response.respond
import io.ktor.server.routing.delete
import io.ktor.server.routing.get
import io.ktor.server.routing.patch
import io.ktor.server.routing.post
import io.ktor.server.routing.put
import io.ktor.server.routing.route
import io.ktor.server.routing.routing

fun Application.configureRouting() {
  routing {
    get("/health") { call.respond(mapOf("status" to "UP")) }

    route("/.well-known") { get("/jwks.json") { TokenRoutes.jwks(call) } }

    route("/api/v1") {
      route("/auth") {
        post("/register") { AuthRoutes.register(call) }
        post("/login") { AuthRoutes.login(call) }
        post("/logout") { AuthRoutes.logout(call) }
        post("/refresh") { AuthRoutes.refresh(call) }
        authenticate("jwt-auth") { post("/logout-all") { AuthRoutes.logoutAll(call) } }
      }

      authenticate("jwt-auth") {
        route("/users/me") {
          get { UserRoutes.getProfile(call) }
          patch { UserRoutes.updateDisplayName(call) }
          post("/password") { UserRoutes.changePassword(call) }
          post("/deactivate") { UserRoutes.deactivate(call) }
        }

        route("/admin") {
          get("/users") { AdminRoutes.listUsers(call) }
          post("/users/{id}/disable") { AdminRoutes.disable(call) }
          post("/users/{id}/enable") { AdminRoutes.enable(call) }
          post("/users/{id}/unlock") { AdminRoutes.unlock(call) }
          post("/users/{id}/force-password-reset") { AdminRoutes.forcePasswordReset(call) }
        }

        route("/expenses") {
          post { ExpenseRoutes.create(call) }
          get { ExpenseRoutes.list(call) }
          // /summary MUST be before /{id} to avoid path shadowing
          get("/summary") { ExpenseRoutes.summary(call) }
          get("/{id}") { ExpenseRoutes.getById(call) }
          put("/{id}") { ExpenseRoutes.update(call) }
          delete("/{id}") { ExpenseRoutes.delete(call) }
          post("/{id}/attachments") { AttachmentRoutes.upload(call) }
          get("/{id}/attachments") { AttachmentRoutes.list(call) }
          delete("/{id}/attachments/{aid}") { AttachmentRoutes.delete(call) }
        }

        route("/tokens") { get("/claims") { TokenRoutes.claims(call) } }

        route("/reports") { get("/pl") { ReportRoutes.pl(call) } }
      }
    }
  }
}
