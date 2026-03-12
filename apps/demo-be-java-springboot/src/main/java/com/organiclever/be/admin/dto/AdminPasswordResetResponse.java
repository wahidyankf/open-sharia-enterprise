package com.organiclever.be.admin.dto;

import com.fasterxml.jackson.annotation.JsonProperty;

public record AdminPasswordResetResponse(@JsonProperty("reset_token") String resetToken) {}
