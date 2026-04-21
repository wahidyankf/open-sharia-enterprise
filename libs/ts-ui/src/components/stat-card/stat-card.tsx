import type { HueName } from "../hue-picker/hue-picker";
import { InfoTip } from "../info-tip/info-tip";
import { Icon, type IconName } from "../icon/icon";

export interface StatCardProps {
  label: string;
  value: string | number;
  unit: string;
  hue: HueName;
  icon: string;
  info?: string;
}

export function StatCard({ label, value, unit, hue, icon, info }: StatCardProps) {
  return (
    <div className="flex min-h-[96px] flex-col gap-2 rounded-[20px] border bg-card p-3.5 shadow-sm">
      <div className="flex items-center justify-between gap-2">
        <div
          style={{ backgroundColor: `var(--hue-${hue})` }}
          className="flex h-9 w-9 flex-shrink-0 items-center justify-center rounded-xl text-white"
        >
          <Icon name={icon as IconName} size={20} />
        </div>
        {info && <InfoTip title={label} text={info} />}
      </div>
      <div>
        <span className="font-mono text-2xl leading-none font-extrabold tracking-tight">{value}</span>
        <span className="ml-1 font-sans text-[13px] font-bold text-muted-foreground">{unit}</span>
      </div>
      <p className="text-xs text-muted-foreground">{label}</p>
    </div>
  );
}
