package com.organiclever.be.admin.dto;

import java.util.List;

public record AdminUserListResponse(List<AdminUserResponse> data, long total, int page) {}
