"use client";

interface YouTubeProps {
  videoId: string;
}

export function YouTube({ videoId }: YouTubeProps) {
  return (
    <div className="my-4 aspect-video w-full overflow-hidden rounded-lg">
      <iframe
        src={`https://www.youtube.com/embed/${videoId}`}
        title="YouTube video"
        allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture"
        allowFullScreen
        loading="lazy"
        className="h-full w-full border-0"
      />
    </div>
  );
}
