package com.demobejasb.admin.dto;

import com.fasterxml.jackson.annotation.JsonProperty;

public record AdminPasswordResetResponse(@JsonProperty("reset_token") String resetToken) {}
