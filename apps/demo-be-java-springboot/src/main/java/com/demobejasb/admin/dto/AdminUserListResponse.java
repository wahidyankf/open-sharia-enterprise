package com.demobejasb.admin.dto;

import java.util.List;

public record AdminUserListResponse(List<AdminUserResponse> content, long totalElements, int page) {}
