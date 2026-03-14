package com.demobejasb.attachment.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.demobejasb.attachment.model.Attachment;
import java.util.UUID;

public record AttachmentResponse(
        UUID id,
        String filename,
        @JsonProperty("contentType") String contentType,
        long size,
        String url) {

    public static AttachmentResponse from(final Attachment attachment) {
        String url =
                "/api/v1/expenses/"
                        + attachment.getExpense().getId()
                        + "/attachments/"
                        + attachment.getId()
                        + "/download";
        return new AttachmentResponse(
                attachment.getId(),
                attachment.getFilename(),
                attachment.getContentType(),
                attachment.getSize(),
                url);
    }
}
