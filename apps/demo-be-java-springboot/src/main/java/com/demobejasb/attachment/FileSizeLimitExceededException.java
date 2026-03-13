package com.demobejasb.attachment;

/** Thrown when an uploaded file exceeds the maximum allowed size. */
public class FileSizeLimitExceededException extends RuntimeException {

    public FileSizeLimitExceededException() {
        super("File size exceeds the maximum allowed limit");
    }
}
