package com.organiclever.demojavx;

import io.vertx.core.Vertx;

public final class Main {

    private Main() {
    }

    public static void main(String[] args) {
        System.out.println("Starting demo-be-java-vertx...");
        Vertx vertx = Vertx.vertx();
        vertx.deployVerticle(new MainVerticle())
                .onSuccess(id -> System.out.println("Verticle deployed: " + id))
                .onFailure(err -> {
                    System.err.println("Failed to deploy verticle: " + err.getMessage());
                    err.printStackTrace();
                    vertx.close();
                });
    }
}
