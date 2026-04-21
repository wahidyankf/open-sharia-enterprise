import type { Meta, StoryObj } from "@storybook/nextjs-vite";
import { StatCard } from "./stat-card";

const meta = {
  title: "Display/StatCard",
  component: StatCard,
  parameters: { layout: "centered" },
} satisfies Meta<typeof StatCard>;

export default meta;
type Story = StoryObj<typeof meta>;

export const TealTrend: Story = {
  args: { label: "Steps", value: "12,500", unit: "steps", hue: "teal", icon: "trend" },
};

export const WithInfo: Story = {
  args: {
    label: "Volume",
    value: "45,000",
    unit: "kg",
    hue: "sage",
    icon: "dumbbell",
    info: "Total weight lifted across all sets for this session.",
  },
};

export const WithoutInfo: Story = {
  args: { label: "Streak", value: "14", unit: "days", hue: "honey", icon: "flame" },
};
