export interface ProgressRingProps {
  size?: number;
  stroke?: number;
  progress: number;
  color?: string;
  bg?: string;
}

export function ProgressRing({
  size = 80,
  stroke = 6,
  progress,
  color = "var(--hue-teal)",
  bg = "var(--warm-100)",
}: ProgressRingProps) {
  const radius = (size - stroke) / 2;
  const circ = 2 * Math.PI * radius;
  const clampedProgress = Math.max(0, Math.min(1, progress));

  return (
    <svg
      width={size}
      height={size}
      style={{ transform: "rotate(-90deg)" }}
      role="progressbar"
      aria-valuenow={Math.round(clampedProgress * 100)}
      aria-valuemin={0}
      aria-valuemax={100}
    >
      <circle cx={size / 2} cy={size / 2} r={radius} fill="none" stroke={bg} strokeWidth={stroke} />
      <circle
        cx={size / 2}
        cy={size / 2}
        r={radius}
        fill="none"
        stroke={color}
        strokeWidth={stroke}
        strokeLinecap="round"
        strokeDasharray={circ}
        strokeDashoffset={circ * (1 - clampedProgress)}
        style={{ transition: "stroke-dashoffset 1s linear, stroke 300ms" }}
      />
    </svg>
  );
}
