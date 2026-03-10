package com.organiclever.be.user.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import org.jspecify.annotations.Nullable;

public record UpdateProfileRequest(@Nullable @JsonProperty("display_name") String displayName) {}
